#!/usr/bin/ol --run

(import
  (owl terminal))

(define logfd (open-output-file "led.log"))

(define (log . what)
   (print-to logfd what))

(define (trampoline)
  (print "main: " (wait-mail))
  (halt 1)
  (trampoline))

(define (move-cursor msg x y)
  (lets ((from msg msg))
    (if (and (eq? from 'terminal) (tuple? msg) (eq? (ref msg 1) 'arrow))
      (cond
        ((eq? (ref msg 2) 'up)
          (values x (max 1 (- y 1))))
        ((eq? (ref msg 2) 'down)
          (values x (+ y 1)))
        ((eq? (ref msg 2) 'left)
          (values (max 1 (- x 1)) y))
        (else
          (values (+ x 1) y)))
      (values x y))))

(define (buffer up down left right x y w h off meta)
   (tuple up down left right x y w h off meta))

(define (make-empty-state w h meta)
	(buffer null null null null 1 1 w h (cons 0 0) meta))

(define (make-file-state w h path meta)
  (let ((data (map string->list (force-ll (lines (open-input-file path))))))
    (if (pair? data)
      (buffer null (cdr data) null (car data) 1 1 w h (cons 0 0) 
        (put meta 'path path))
      (error "could not open " path))))

(define (screen-width buff) (ref buff 5))
(define (screen-height buff) (ref buff 6))

(define (debug . stuff)
   '(lets
      ((max-msg-width 60)
       (val (take (foldr render null stuff) max-msg-width)))
      (mail 'terminal
         (tio
            (cursor-save)
            (cursor-hide)
            (font-bold)
            (set-cursor 1 1)
            (clear-line-right)
            (raw val)
            (font-normal)
            (cursor-restore)
            (cursor-show)))))

(define (draw-lines-at-offset tl w dx y dy end lines)
   (cond
      ((null? lines) tl)
      ((eq? y end) tl)
      (else
         (let ((these (drop (car lines) dx)))
            (tio*
               (set-cursor 1 y)
               (clear-line-right)
               (raw (take these w))
               (draw-lines-at-offset w dx (+ y dy) dy end (cdr lines))
               tl)))))
                  
(define (update-screen buff)
   (lets 
      ((u d l r x y w h off meta buff)
       (this (append (reverse l) r)))
      (tio
          (clear-screen)
          (draw-lines-at-offset w (car off) y -1 0 (cons this u))
          (draw-lines-at-offset w (car off) (+ y 1) +1 (+ h 1) d)
          (set-cursor x y))))

(define (scroll-right buff)
   (lets 
      ((u d l r x y w h off meta buff)
       (dx dy off)
       (step (* 2 (div w 3)))
       (buff (buffer u d l r (- x step) y w h (cons (+ dx step) dy) meta)))
      (values buff (update-screen buff))))

(define (scroll-left buff)
   (lets 
      ((u d l r x y w h off meta buff)
       (dx dy off))
      (if (eq? dx 1)
        (values buff null)
        (lets
          ((step (min dx (* 2 (div w 3))))
           (buff (buffer u d l r (+ x step) y w h (cons (- dx step) dy) meta)))
          (values buff (update-screen buff))))))

(define (scroll-down buff)
   (lets 
    ((u d l r x y w h off meta buff)
     (step (+ 1 (* 2 (div h 3))))
     (dx dy off)
     (buff 
      (buffer u d l r x (- y step) w h (cons dx (+ dy step)) meta)))
    (values buff
      (update-screen buff))))

(define (scroll-up buff)
   (lets 
    ((u d l r x y w h off meta buff)
     (dx dy off)
     (step (min dy (+ 1 (* 2 (div h 3)))))
     (buff 
      (buffer u d l r x (+ y step) w h (cons dx (- dy step)) meta)))
    (values buff
      (update-screen buff))))

;; scroll window all the way left, if necessary
(define (reset-left buff)
   (lets 
      ((u d l r x y w h off meta buff)
       (dx dy off))
;      (if (eq? dx 1) (values buff null) ...)
         (let ((r (append (reverse l) r)))
            (values
               (buffer u d null r 1 y w h (cons 0 dy) meta)
               (tio
                  (draw-lines-at-offset w 0 (- y 1) -1 0 u)
                  (draw-lines-at-offset w 0 (+ y 1) +1 (+ h 1) d)
                  (set-cursor 1 y)
                  (clear-line-right)
                  (raw (take r w))
                  (set-cursor 1 y))))))

(define (log-buff buff)
  (lets
    ((u d l r x y w h off meta buff))
    (log "log: cursor at " (cons x y) " at offset " off ", line pos " (+ (car off) (- x 1)))))
        

(define (insert-handle-key buff k)
   (lets ((u d l r x y w h off meta buff))
      (if (eq? x w)
         (lets
            ((buff scroll-tio (scroll-right buff))
             (buff insert-tio (insert-handle-key buff k)))
            (values buff
               (append scroll-tio insert-tio)))
         (begin
            (log "insert of key " k " at " (cons x y))
            (values
               (buffer u d (cons k l) r (+ x 1) y w h off meta)
               (cons k
                  (if (null? r)
                     null
                     (tio
                        (clear-line-right)
                        (cursor-save)
                        (raw (take r (- w (+ x 1))))
                        (cursor-restore)))))))))

(define (insert-backspace buff)
   (lets ((u d l r x y w h off meta buff))
      (cond
         ((null? l)
            ;; no-op (could also backspace to line above)
            (values buff null))
         ((eq? x 1)
            (lets 
               ((buff scroll-tio (scroll-left buff))
                (buff bs-tio     (insert-backspace buff)))
               (values buff
                  (append scroll-tio bs-tio))))
         (else
            (values
               (buffer u d (cdr l) r (- x 1) y w h off meta)
               (tio
                  (cursor-left 1)
                  (clear-line-right)
                  (cursor-save)
                  (raw (take r (- w x)))
                  (cursor-restore)))))))

;; (a b c d ... n) 3 → (c b a) (d... n)
(define (line-seek line pos)
  (let loop ((line line) (pos pos) (l null))
    (cond
      ((eq? pos 0)
        (values l line))
      ((null? line)
        (error "empty line at line-seek: " pos))
      (else
        (loop (cdr line) (- pos 1) (cons (car line) l))))))

;; move line down within the same screen preserving cursor position if possible
(define (line-down buff)
   (lets ((u d l r x y w h off meta buff)
          (dx dy off)
          (_ (log "line-down starting from " (cons x y)))
          (line (append (reverse l) r))
          (u (cons line u))
          (y (+ y 1))
          (line d (uncons d null))
          (x (min x (+ 1 (- (length line) (car off)))))
          (line-pos (+ (- x 1) (car off)))
          (l r (line-seek line line-pos)))
        (log "line-down went to (x . y) " (cons x y))
        (log "next line length is " (length line) ", x=" x ", dx=" (car off) ", l='" (list->string l) "', r='" (list->string r) "'")
        (values
          (buffer u d l r x y w h off meta)
          x y)))

;; move line up within the same screen preserving cursor position if possible
(define (line-up buff)
   (lets ((u d l r x y w h off meta buff)
          (dx dy off)
          (line (append (reverse l) r))
          (d (cons line d))
          (y (- y 1))
          (line u (uncons u null))
          (x (min x (+ 1 (- (length line) (car off)))))
          (line-pos (+ (- x 1) (car off)))
          (l r (line-seek line line-pos)))
        (log "line-up went to (x . y) " (cons x y))
        (log "next line length is " (length line) ", x=" x ", dx=" (car off) ", l='" (list->string l) "', r='" (list->string r) "'")
        (values
          (buffer u d l r x y w h off meta)
          x y)))

(define (move-arrow buff dir)
   (lets ((u d l r x y w h off meta buff))
      (log "arrow " dir " from " (cons x y))
      (cond
         ((eq? dir 'up)
            (cond
               ((null? u)
                  (values buff null))
               ((eq? y 1)
                  (lets
                    ((buff tio-scroll (scroll-up buff))
                     (buff tio-move (move-arrow buff dir)))
                    (values buff
                      (append tio-scroll tio-move))))
              ((not (eq? 0 (car off))) ;; there is x-offset
                (let ((next-len (length (car u))))
                  (if (< next-len (car off)) ;; next line start not visible
                    (lets ;; dummy version
                      ((buff tio (move-arrow buff 'left))
                       (buff tio-this (move-arrow buff dir)))
                      (values buff (append tio tio-this)))
                    (lets ((buff x y (line-up buff)))
                      (values buff
                        (tio (set-cursor x y)))))))
               (else
                 (lets ((buff x y (line-up buff)))
                   (values buff
                     (tio (set-cursor x y)))))))
         ((eq? dir 'down)
            (cond
              ((null? d)
                (values buff null))
              ((eq? y h)
                (lets
                  ((buff tio-scroll (scroll-down buff))
                   (buff tio-move (move-arrow buff dir)))
                  (values buff
                    (append tio-scroll tio-move))))
              ((not (eq? 0 (car off))) ;; there is x-offset
                (let ((next-len (length (car d))))
                  (if (< next-len (car off)) ;; next line start not visible
                    (lets ;; dummy version
                      ((buff tio (move-arrow buff 'left))
                       (buff tio-this (move-arrow buff dir)))
                      (values buff (append tio tio-this)))
                    (lets ((buff x y (line-down buff)))
                      (values buff
                        (tio (set-cursor x y)))))))
              (else
                (lets ((buff x y (line-down buff)))
                  (values buff (tio (set-cursor x y)))))))
         ((eq? dir 'left)
            (cond
               ((null? l)
                  (values buff null))
               ((eq? x 1)
                  (lets
                    ((buff scroll-tio (scroll-left buff))
                     (buff move-tio (move-arrow buff dir)))
                    (values buff (append scroll-tio move-tio))))
               (else
                  (values
                     (buffer u d (cdr l) (cons (car l) r) (- x 1) y w h off meta)
                     (tio
                        (cursor-left 1))))))
         ((eq? dir 'right)
            (cond
               ((null? r)
                  (values buff null))
               ((eq? x w)
                  (lets
                    ((buff scroll-tio (scroll-right buff))
                     (buff move-tio (move-arrow buff dir)))
                    (values buff (append scroll-tio move-tio))))
               (else
                  (values 
                     (buffer u d (cons (car r) l) (cdr r) (+ x 1) y w h off meta)
                     (tio
                        (cursor-right 1))))))
         (else
            (log "odd line move: " dir)
            (values buff null)))))
      
(define (led-buffer buff undo mode)
   (log-buff buff)
   (lets ((envelope (wait-mail))
          (from msg envelope))
      (lets ((u d l r x y w h off meta buff))
        (log "cursor " (cons x y) ", offset " off ", event " envelope))
      (if (eq? from 'terminal)
         (if (eq? mode 'insert)
            (tuple-case msg
               ((key x)
                  (lets ((buff out (insert-handle-key buff x)))
                     (mail 'terminal out)
                     (led-buffer buff undo mode)))
               ((enter)
                  (lets 
                     ((u d l r x y w h off meta buff)
                      (u (cons (reverse l) u))
                      (buff (buffer u d null r 1 (+ y 1) w h off meta))
                      (buff reset-tio (reset-left buff)))
                     (mail 'terminal
                        (append reset-tio
                           (tio 
                              (set-cursor 1 (+ y 1)))))
                     (led-buffer buff undo mode)))
               ((backspace)
                  (lets ((buff out (insert-backspace buff)))
                     (mail 'terminal out)
                     (led-buffer buff undo mode)))
               ((arrow dir)
                  (lets ((buff out (move-arrow buff dir)))
                     (mail 'terminal out)
                     (led-buffer buff undo mode)))
               ((end-of-text)
                  (mail 'terminal 'stop)
                  0)
               (else
                  (debug "odd terminal msg " msg)
                  (led-buffer buff undo mode)))
            (begin
               (debug "odd mode " mode " for " envelope)
               (led-buffer buff undo mode)))
         (begin
            (debug "odd message " envelope)
            (led-buffer buff undo mode)))))

(define (start-led args)
  (log "started")
  (lets ((dimensions (interact 'terminal 'get-terminal-size))
         (w h dimensions))
    (log "dimensions " dimensions)
    (mail 'terminal
      (tio
        (clear-screen)
        (set-cursor (- (div w 2) 4) (div h 2))
        (raw (font-bold (render "LED 0.1" null)))
        ;(set-cursor 1 h)
        ;(output ": ")
        (font-normal)
        (set-cursor 1 1)
        ))
    (lets 
      ((buff 
        (if (= (length args) 1)
          (make-file-state w h (car args) #empty)
          (make-empty-state w h #empty))))
      (mail 'terminal (update-screen buff))
      (led-buffer buff null 'insert))))
    
(λ (args)
  (fork-linked-server 'terminal
    (λ () (terminal-server stdin 'led)))
  (fork-linked-server 'led 
    (λ () (start-led (cdr args))))
  (trampoline)
  0)
