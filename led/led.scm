#!/usr/bin/ol --run

(import
  (led terminal)
  (only (owl unicode) encode-point)
  (owl args))

(define version-str "led v0.1a")

(define (log . what)
   (mail 'logger what))

(define (output lst)
   (write-bytes stdout lst))

;;; Movement and insert mode edit operation

(define (buffer up down left right x y w h off meta)
   (tuple up down left right x y w h off meta))

(define (make-empty-buffer w h meta)
   (buffer null null null null 1 1 w h (cons 0 0) meta))

(define (buffer-meta buff) (ref buff 10))
(define (set-buffer-meta buff meta) (set buff 10 meta))

(define (put-buffer-meta buff key val)
   (set-buffer-meta buff (put (buffer-meta buff) key val)))

(define (get-buffer-meta buff key def)
   (get (buffer-meta buff) key def))

(define (buffer-x buff) (ref buff 5))
(define (buffer-y buff) (ref buff 6))

(define rp-node
   (tuple 'replace (list 41) 1 (tio (font-dim) (raw (list 41)) (font-normal))))

(define lp-node
   (tuple 'replace (list 40) 1 (tio (font-dim) (raw (list 40)) (font-normal))))

(define tab-node 
  (tuple 'replace 
   (list #\tab) 
   3 
   (tio
      (font-dim)
      ;(raw (list #\space #\▹ #\space))
      (raw (list #\_ #\_ #\_))
      (font-normal))))

(define (num->hex n)
   (lets ((n (number->string n 16))
          (l (string-length n)))
      (cond
         ((eq? l 1)
            (ilist #\0 #\x #\0 (string->list n)))
         ((eq? l 2)
            (ilist #\0 #\x (string->list n)))
         (else
            (error "num->hex: " n)))))

(define (hex-node n)
   (let ((node
      (tuple 'replace 
         (list n) 
         4 
         (tio
            (font-dim)
            (raw (num->hex n))
            (font-normal)))))
      (log "hex node of " n " is " node)
      node))

(define (whitespace? node)
   (cond
      ((eq? node #\space) #true)
      ((eq? node 13) #true)
      ((eq? node tab-node) #true)
      (else #false)))

(define (drop-leading-whitespace lst)
   (cond
      ((null? lst) lst)
      ((whitespace? (car lst))
         (drop-leading-whitespace (cdr lst)))
      (else lst)))
      
(define (untab meta)
   (let ((tab (get meta 'tab tab-node)))
      (λ (line)
         (map 
            (λ (node)   
               (cond
                  ((eq? node #\() lp-node)
                  ((eq? node #\)) rp-node)
                  ((eq? node #\tab) tab)
                  (else node)))
            line))))
      
(define (path->lines path meta)
   (let ((fd (open-input-file path)))
      (if fd
         (map (untab meta) (map string->list (force-ll (lines fd))))
         #false)))

(define (make-file-state w h path meta)
   (log "making file state out out of " path)
   (cond
      ((path->lines path meta) =>
         (lambda (data)
            (if (pair? data)
               (buffer null (cdr data) null (car data) 1 1 w h (cons 0 0) (put meta 'path path))
               (buffer null null null null 1 1 w h (cons 0 0) (put meta 'path path)))))
      ((open-output-file path) =>
         (lambda (fd)
            (log "opened new fd " fd)
            (close-port fd) ;; now input succeeds
            (log "created " path)
            (make-file-state w h path meta)))
      (else
         (log "Could not open " path)
         #false)))

(define (screen-width buff) (ref buff 7))

(define (screen-height buff) (ref buff 8))

(define (node-screen-width x) (ref x 3))

(define (node-screen-representation x) (ref x 4))

(define (node-width x)
   (cond
      ((eq? (type x) type-fix+) 1)
      ((tuple? x) (node-screen-width x))
      (else (error "node-width: " x))))

(define (take-printable line n)
  (cond
    ((eq? n 0) null)
    ((pair? line)
      (lets ((x line line))
        (cond
          ((eq? (type x) type-fix+)
            ;; a printable unicode code point
            (if (eq? 0 (fxband x #x80))
              ;; a printable ascii range thingie (usual suspect)
              (cons x (take-printable line (- n 1)))
              (encode-point x
                (take-printable line (- n 1)))))
          ((tuple? x)
            ;; #(type actual-codepoints width-on-screen screen-codepoints)
            (lets ((type cps width output x))
              (cond
                ((eq? type 'replace)
                    (append output
                       (take-printable line (- n width))))
                (else
                  (error "take-printable: what is " x)))))
          (else
            (error "take-printable: what is " x)))))
    (else
      null)))

(define (render-node node tl)
   (cond
      ((eq? (type node) type-fix+)
         (encode-point node tl))
      ((and (tuple? node) (eq? (ref node 1) 'replace))
         (foldr render-node tl (ref node 2)))
      (else
         (error "render-node: what is " node))))

(define (drop-printable line n)
  (cond
    ((eq? n 0) line)
    ((pair? line)
      (lets ((x line line))
        (cond
          ((eq? (type x) type-fix+)
            (drop-printable line (- n 1)))
          ((tuple? x)
            ;; #(type actual-codepoints width-on-screen screen-codepoints)
            (lets ((type cps width output x))
               ;(drop-printable (append output line) n)
               (drop-printable line (- n width))))
          (else
            (error "drop-printable: what is " x)))))
    (else
      null)))

(define (printable-length line)
   (fold (λ (n x) (+ n (node-width x))) 0 line))

(define empty-buffer-line
   (tio
      (font-dim)
      (raw (render "~" null))
      (font-normal)
      ))

(define (draw-lines-at-offset tl w dx y dy end lines)
   (cond
      ((null? lines) 
         (draw-lines-at-offset tl w dx y dy end
            (list empty-buffer-line)))
      ((eq? y end) tl)
      (else
         (let ((these (drop-printable (car lines) dx)))
            ;(log "printable after " dx " of " (car lines) " is " (drop-printable (car lines) dx))
            (tio*
               (set-cursor 1 y)
               (clear-line-right)
               (raw (take-printable these w))
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

(define (log-buff buff mode)
  (lets
    ((u d l r x y w h off meta buff)
     (dx dy off)
     (x (+ dx x))
     (y (+ dy y))
     (status (str "       " x "," y " " (if (eq? mode 'insert) "i" "c")))
     (poss (render status null)))
    ;(log "left " l ", right " r)
    ;(log "log: cursor at " (cons x y) " at offset " off ", line pos " (+ (car off) (- x 1)))
    (output
      (tio
         (cursor-save)
         (set-cursor (max 1 (- w (+ 1 (length poss)))) (+ h 1))
         (font-dim)
         (raw poss)
         (font-normal)
         (cursor-restore)))))

(define (notify buff txt)
   (lets ((u d l r x y w h off meta buff))
      (output
         (tio
            (set-cursor 1 (+ h 1))
            (clear-line)
            (font-dim)
            (raw (if (string? txt) (render txt null) txt))
            (font-normal)
            (cursor-restore)
            (set-cursor x y)))))
      
(define (key-node k)
   (log "key-node " k)
   (cond
      ((eq? k #\tab)
         tab-node)
      ((eq? k 40) ;; lp
         ;k
         lp-node
         )
      ((eq? k 41) ;; rp
         ;k
         rp-node
         )
      ((< 31 k 127) ;; ascii range char
         k)
      ((eq? k 127)
         (hex-node k))
      ((< k 32)
         (hex-node k))
      (else
         k)))

(define (encode-node k tl)
   (cond 
      ((eq? (type k) type-fix+)
         (encode-point k tl))
      (else
         (foldr encode-node tl (node-screen-representation k)))))

(define (insert-handle-key buff k)
   (lets ((u d l r x y w h off meta buff))
      (lets ((node (key-node k))
             (nw (node-width node)))
         (log "adding node " node)
         (if (< (+ x nw) w)
            (begin
               (log "insert of key " k " at " (cons x y) " = " node)
               (cond
                  (else
                     (values
                        (buffer u d (cons node l) r (+ x nw) y w h off meta)
                        (encode-node node
                           (if (null? r)
                              null
                              (tio
                                 (clear-line-right)
                                 (cursor-save)
                                 (raw (take-printable r (- w (+ x nw))))
                                 (cursor-restore))))))))
            (lets
               ((buff scroll-tio (scroll-right buff))
                (buff insert-tio (insert-handle-key buff k)))
               (values buff
                  (append scroll-tio insert-tio)))))))

(define (insert-backspace buff)
   (lets ((u d l r x y w h off meta buff))
      (if (null? l)
         ;; no-op (could also backspace to line above)
         (if (null? u)
            (values buff null)
            (if (eq? y 1)
               (values buff null)
               (lets
                  ((line u (uncons u null))
                   (line-len (printable-length line))
                   (q x (quotrem (+ line-len 1) w))
                   (xp (* q w))
                   (buff
                     (buffer u d (reverse line) r x (- y 1) w h (cons xp (cdr off)) meta)))
                  (log "backspace")
                  (values buff
                     (update-screen buff)))))
         (let ((cw (node-width (car l))))
           (if (> x cw)
             (values
                (buffer u d (cdr l) r (- x cw) y w h off meta)
                (tio
                   (cursor-left cw)
                   (clear-line-right)
                   (cursor-save)
                   (raw (take-printable r (- w x)))
                   (cursor-restore)))
            (lets 
               ((buff scroll-tio (scroll-left buff))
                (buff bs-tio     (insert-backspace buff)))
               (values buff
                  (append scroll-tio bs-tio))))))))


;; (a b c d ... n) 3 → (c b a) (d... n) delta, because some chars require more space
(define (seek-in-line line pos)
  (let loop ((line line) (pos pos) (l null))
    (cond
      ((eq? pos 0)
        (values l line 0))
      ((null? line)
         (log "warning: empty line at seek-in-line")
         (values l line pos))
      (else
        (lets 
          ((w (node-width (car line)))
           (pos (- pos w)))
          (if (> pos 0)
            (loop (cdr line) pos (cons (car line) l))
            (values (cons (car line) l) (cdr line) pos)))))))

;; lines → u line d y
(define (seek-line lines end)
   (let loop ((lines lines) (pos end) (u null))
      (cond
         ((null? lines)
            (values u null lines (- end pos)))
         ((null? (cdr lines))
            (values u (car lines) (cdr lines) (- end pos)))
         ((eq? pos 0)
            (values u (car lines) (cdr lines) end))
         (else
            (loop (cdr lines) (- pos 1) (cons (car lines) u))))))

;; compute place to show the buffer horizontally to get buffer position pos to screen
(define (set-line-pos buff pos)
   (log "set-line-pos, pos " pos)
   (lets ((u d l r x y w h off meta buff)
          (dx dy off)
          (line (append (reverse l) r))
          (pos (min pos (printable-length line))))
      (if (< pos w)
         ;; can be show without scrolling, so do that
         (lets ((l r off (seek-in-line line pos))
                (x (+ 1 (- pos off))) ;; bug, but doesn't matter for now
                (buff (buffer u d l r x y w h (cons 0 dy) meta)))
            buff)
         (begin
            (log "not moving here yet")
            buff))))

(define (seek-line-end buff)
   (lets ((u d l r x y w h off meta buff)
          (step (>> w 1))
          (dx dy off))
      (if (null? r)
         (values buff null)
         (let loop ((l l) (r r) (x x) (dx dx) (moved? #false))
            (cond
               ((null? (cdr r))
                  (let ((buff (buffer u d l r x y w h (cons dx dy) meta)))
                     (values buff
                        (if moved? (update-screen buff) (tio (set-cursor x y))))))
               ((eq? x w)
                  (loop l r (- x step) (+ dx step) #true))
               (else
                  (loop (cons (car r) l) (cdr r) (+ x (node-width (car r))) dx moved?)))))))

(define (left-paren? x)
   (or (eq? x 40)
       (equal? x lp-node)))

(define (right-paren? x)
   (or (eq? x 41)
       (equal? x rp-node)))

;; buff → (x . y) | #false
(define (seek-matching-paren-back buff)
   (lets ((u d l r x y w h off meta buff))
      (let loop 
         ((x (length l)) (y (+ (cdr off) (- y 1))) (l l) (u u) (depth 1))
         (cond
            ((null? l)
               (if (null? u)
                  #false
                  (loop (length (car u)) (- y 1) (reverse (car u)) (cdr u) depth)))
            ((left-paren? (car l))
               (if (eq? depth 1)
                  (cons (- x 1) y)
                  (loop (- x 1) y (cdr l) u (- depth 1))))
            ((right-paren? (car l))
               (loop (- x 1) y (cdr l) u (+ depth 1)))
            (else
               (loop (- x 1) y (cdr l) u depth))))))

(define (seek-matching-paren-forward buff)
   (lets ((u d l r x y w h off meta buff))
      (let loop 
         ((x (length l)) (y (+ (cdr off) (- y 1))) (r r) (d d) (depth 0))
         (cond
            ((null? r)
               (if (null? d)
                  #false
                  (loop 0 (+ y 1) (car d) (cdr d) depth)))
            ((right-paren? (car r))
               (if (eq? depth 1)
                  (cons x y)
                  (loop (+ x 1) y (cdr r) d (- depth 1))))
            ((left-paren? (car r))
               (loop (+ x 1) y (cdr r) d (+ depth 1)))
            (else
               (loop (+ x 1) y (cdr r) d depth))))))

(define (seek-line-start buff)
   (lets ((u d l r x y w h off meta buff)
          (dx dy off)
          (buff 
            (buffer u d null (append (reverse l) r) 1 y w h (cons 0 dy) meta)))
         (values buff
            (if (eq? dy 0)
               (tio (set-cursor 1 y))
               (update-screen buff)))))

;; row+1 = y + dy, dy = row + 1 - y
(define (buffer-seek buff x y screen-y)
   (log "buffer seek" x "," y ", y row at " screen-y)
   (lets ((u d l r old-x old-y w h off meta buff)
          (lines (append (reverse u) (list (append (reverse l) r)) d))
          (u line d y (seek-line lines y))
          (step (>> w 1))
          (yp (or screen-y (if (< y h) (+ y 1) (>> h 1)))) ;; real or middle of screen
          (dy (- (+ y 1) yp))
          (buff (buffer u d null line 1 yp w h off meta)))
         ;; seek right
         (let loop ((xp 1) (pos x) (l null) (r line) (dx 0))
            (cond
               ((>= xp w)
                  (loop (- xp step) pos l r (+ dx step)))
               ((eq? pos 0)
                  (buffer u d l r xp yp w h (cons dx dy) meta))
               ((null? r)
                  (loop xp 0 l r dx))
               (else
                  (loop (+ xp (node-width (car r))) (- pos 1) (cons (car r) l) (cdr r) dx))))))

;; move line down within the same screen preserving cursor position if possible
(define (line-down buff)
   (lets ((u d l r x y w h off meta buff)
          (dx dy off)
          (line (append (reverse l) r))
          (u (cons line u))
          (y (+ y 1))
          (line d (uncons d null))
          (x (min x (+ 1 (- (printable-length line) (car off)))))
          (line-pos (+ (- x 1) (car off)))
          (l r offset (seek-in-line line line-pos)))
        ;(log "next line length is " (printable-length line) ", x=" x ", dx=" (car off) ", l='" (list->string l) "', r='" (list->string r) "', offset " offset) 
        (values
          (buffer u d l r (- x offset) y w h off meta)
          (- x offset) y)))

;; move line up within the same screen preserving cursor position if possible
(define (line-up buff)
   (lets ((u d l r x y w h off meta buff)
          (dx dy off)
          (line (append (reverse l) r))
          (d (cons line d))
          (y (- y 1))
          (line u (uncons u null))
          (x (min x (+ 1 (- (printable-length line) (car off)))))
          (line-pos (+ (- x 1) (car off)))
          (l r offset (seek-in-line line line-pos)))
        ;(log "line-up went to (x . y) " (cons x y))
        ;(log "next line length is " (printable-length line) ", x=" x ", dx=" (car off) ", l='" (list->string l) "', r='" (list->string r) "'")
        (values
          (buffer u d l r (- x offset) y w h off meta)
          (- x offset) y)))

;; regex is a ^... one
(define (search-from l ls regex x y)
   (cond
      ((null? l)
         (if (null? ls)
            (values #f #f)
            (search-from (car ls) (cdr ls) regex 0 (+ y 1))))
      ((regex l)
         (values x y))
      (else
         (search-from (cdr l) ls regex (+ x 1) y))))
         
(define (find-next buff)
   (lets ((u d l r x y w h off meta buff)
          (dx dy off)
          (regex (get meta 'search-regex (λ (x) #false)))
          (row (+ (length u) 1))
          (_ rt (uncons r #false)) ;; move one char forward
          (mx my 
            (search-from rt d regex (+ x dx) (+ (- y 1) dy))))
         (if mx
            (values (buffer-seek buff mx my #false) #false)
            (lets
               ((this (append (reverse l) r))
                (up (reverse (cons this u)))
                (mx my (search-from (car up) (cdr up) regex 0 0)))
               (if mx
                  (values (buffer-seek buff mx my #false) "search wrapped")
                  (values buff "no matches"))))))

(define (move-arrow buff dir)
   (lets ((u d l r x y w h off meta buff))
      (log "arrow " dir " from " (cons x y) ", dim " (cons w h))
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
                (let ((next-len (printable-length (car u))))
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
              ((>= y h)
                (lets
                  ((buff tio-scroll (scroll-down buff))
                   (buff tio-move (move-arrow buff dir)))
                  (values buff
                    (append tio-scroll tio-move))))
              ((not (eq? 0 (car off))) ;; there is x-offset
                (let ((next-len (printable-length (car d))))
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
            (if (null? l)
               (values buff null)
               (let ((step (node-width (car l))))
                  (if (<= x step)
                     (lets
                       ((buff scroll-tio (scroll-left buff))
                        (buff move-tio (move-arrow buff dir)))
                       (values buff (append scroll-tio move-tio)))
                     (values
                        (buffer u d (cdr l) (cons (car l) r) (- x step) y w h off meta)
                        (tio
                           (cursor-left step)))))))
         ((eq? dir 'right)
            (if (null? r)
               (values buff null)
               (let ((step (node-width (car r))))
                  (if (< (+ x step) w)
                     (values 
                        (buffer u d (cons (car r) l) (cdr r) (+ x step) y w h off meta)
                        (tio (cursor-right step)))
                     (lets
                       ((buff scroll-tio (scroll-right buff))
                        (buff move-tio (move-arrow buff dir)))
                       (values buff (append scroll-tio move-tio)))))))
         ((eq? dir 'command-right) ;; command mode
            (if (or (null? r) (null? (cdr r)))
               (values buff null)
               (let ((step (node-width (car r))))
                  (if (< (+ x step) w)
                     (values 
                        (buffer u d (cons (car r) l) (cdr r) (+ x step) y w h off meta)
                        (tio (cursor-right step)))
                     (lets
                       ((buff scroll-tio (scroll-right buff))
                        (buff move-tio (move-arrow buff dir)))
                       (values buff (append scroll-tio move-tio)))))))
         (else
            (log "odd line move: " dir)
            (values buff null)))))

;;; Undo

(define (initial-undo buff)
   (cons (list buff) null))

;; fixme - should have current at head of undo for dirtiness, or keep track otherwise
(define (push-undo undo buff)
   (log "pushing new version")
   (lets ((prev new undo))
      ;; no way to return to future after changing the past
      (cons (cons buff prev) null)))

(define (pop-undo undo buff)
   (log "popping undo")
   (lets ((prev new undo))
      (if (null? prev)
         (values undo buff)
         (values
            (cons (cdr prev) (cons buff new))
            (car prev)))))

(define (unpop-undo undo buff)
   (log "unpopping undo")
   (lets ((prev new undo))
      (if (null? new)
         (values undo buff)
         (values (cons (cons buff prev) (cdr new)) (car new)))))

;; special case of input not having a terminal newline can be handled 
;; in buffer metadata if necessary
(define (buffer->bytes buff)
   (lets ((u d l r x y w h off meta buff))
      (foldr render-node null
         (foldr 
            (λ (line tl)
               (append line (cons #\newline tl)))
            null
            (append (reverse u) (list (append (reverse l) r)) d)))))

(define (delete-line buff)
   (lets ((u d l r x y w h off meta buff)
          (next d (uncons d #false)))
      (cond
         (next
            (values
               (buffer u d null next 1 y w h off meta)
               (append (reverse l) r)))
         ((null? u)
            (values 
               (buffer u d null null 1 y w h off meta)
               (append (reverse l) r)))
         (else
            (values
               (buffer (cdr u) d null (car u) 1 
                  (min y (length u)) w h off meta)
               (append (reverse l) r))))))

(define (paste-lines-below buff lines)
   (lets ((u d l r x y w h off meta buff))
      (buffer u (append lines d) l r x y w h off meta)))

(define (paste-sequence buff lst)
   (lets ((u d l r x y w h off meta buff))
      (if (null? r)
         (buffer u d l lst x y w h off meta)
         (lets ((this r r))
            ;; paste after cursor
            (buffer u d l (cons this (append lst r)) x y w h off meta)))))

;;
;; Data structures
;;   yank = #(lines <buffer lines>)
;;        | #(sequence <sequence of line data>)
;;        | #(range <end of line>|#false <buffer lines> <line start>|#false)
;;

;; line lines depth -> sexp-lines lines'
(define (cut-sexp line lines depth)
   (let loop ((l line) (ls lines) (cl null) (cls null) (d depth))
      (cond
         ((null? l)
            (if (null? ls)
               (values #false #false)
               (loop (car ls) (cdr ls) null (cons cl cls) d)))
         ((right-paren? (car l))
            (if (eq? d 1)
               (values
                  (reverse (map reverse (cons (cons (car l) cl) cls)))
                  (cons (cdr l) ls))
               (loop (cdr l) ls (cons (car l) cl) cls
                  (- d 1))))
         (else
            (loop (cdr l) ls (cons (car l) cl) cls
               (+ d (if (left-paren? (car l)) 1 0)))))))

;; fixme: placeholds
(define (lines->yank lines)
   (cond
      ((null? (cdr lines))
          (tuple 'sequence (car lines)))
       (else
          (tuple 'lines lines))))
   
   
;; cut forward, for backward move to corresponding open paren and use this
;; buff -> buff' msg
(define (buffer-cut-sexp buff)
   (lets ((u d l r x y w h off meta buff)
          (sexp lines (cut-sexp r d 0)))
      (if sexp
         (lets 
            ((r d (uncons lines null))
             (buff (buffer u d l r x y w h off meta)))
            (values
               (put-buffer-meta buff 'yank (lines->yank sexp))
               "Copied to yank"))
         (values buff "Bad range"))))

(define (paste-yank buff)
   (lets ((u d l r x y w h off meta buff)
          (data (getf meta 'yank)))
      (cond
         ((not data)
            buff)
         ((eq? 'lines (ref data 1))
            (log "appending lines from buffer")
            (lets ((buff (paste-lines-below buff (ref data 2)))
                   (buff tio (move-arrow buff 'down)))
               buff))
         ((eq? 'sequence (ref data 1))
            (log "appending sequence from buffer")
            (lets ((buff (paste-sequence buff (ref data 2))))
               buff))
         (else
            (error "how do i paste " data)))))
 
(define (mark-position buff char)
   (lets 
      ((u d l r x y w h off meta buff)
       (dx dy off)
       (mark-x (+ (- x 1) dx))
       (mark-y (+ (- y 1) dy))
       (marks (get meta 'marks #empty)))
      (log "marked" (list->string (list char)) " as " (cons mark-x mark-y))
      (put-buffer-meta buff 'marks
         (put marks char (cons mark-x mark-y)))))

(define (write-buffer buff path)
   (log "writing to " path)
   (if path
      (lets
         ((port (open-output-file path))
          (lst (buffer->bytes buff))
          (n (length lst))
          (res (byte-stream->port lst port)))
         (if res
            (values #true
               (foldr render null 
                  (list "Wrote " n " bytes to '" path "'")))
            (values #false
               (foldr render null
                  (list "Failed to write to '" path "'")))))
      (begin
         (log "no path")
         (values #false
            (foldr render null
               (list "Give me a name for this"))))))

(define (maybe-keep-y old-y old-y-pos new-y h)
   (lets ((delta (- new-y old-y))
          (rel-pos (+ old-y-pos delta)))
      (cond
         ((< rel-pos 1) #false)  ;; above screen, center on it
         ((< rel-pos h) rel-pos) ;; on screen, ask y to be there
         (else #false))))        ;; below screen
   
(define (maybe-seek-matching-paren buff)
   (lets 
      ((u d l r x y w h off meta buff)
       (yp (+ (cdr off) (- y 1))))      ;; yp is at row y on screen currently
      (cond
         ((null? r)
            (values buff null))
         ((right-paren? (car r))
            (lets ((match (seek-matching-paren-back buff)))
               (log "matching open paren result " match)
               (if match
                  (lets ((mx my match)
                         (buff (buffer-seek buff mx my (maybe-keep-y yp y my h))))
                     (values buff 
                        (update-screen buff)))
                  (values buff null))))
         ((left-paren? (car r))
            (lets ((match (seek-matching-paren-forward buff)))
               (log "matching close paren result " match)
               (if match
                  (lets ((mx my match)
                         (buff (buffer-seek buff mx my (maybe-keep-y yp y my h))))
                     (values buff 
                        (update-screen buff)))
                  (values buff null))))
         (else
            (log "seek-matching-paren: current is " (car r))
            (values buff null)))))


;;;
;;; Command mode actions
;;;

(define (command-regex-search ll buff undo mode r cont)
   (output (tio* (set-cursor 1 (+ 1 (screen-height buff))) (clear-line) (list #\/)))
   (lets ((search-history 
            (get (buffer-meta buff) 'search-history null))
          (ll res (readline ll search-history
                     2 (+ 1 (screen-height buff)) (screen-width buff))))
         (if res
            (lets
               ((buff 
                  (if (equal? res "") buff
                     (put-buffer-meta buff 'search-history 
                        (cons res search-history))))
                (regex ;; note: ^ and $ need special handling later
                  (string->regex (str "m/^" res "/")))
                (buff 
                  (if (equal? res "")
                     buff
                     (-> buff (put-buffer-meta 'search-regex regex))))
                (buff msg
                  (find-next buff)))
               (output (update-screen buff))
               (if msg (notify buff msg))
               (cont ll buff undo mode))
            (begin
               (notify buff "canceled")
               (cont ll buff undo mode)))))

(define (command-find-next ll buff undo mode r cont)
   (lets ((buff msg (find-next buff)))
      (output (update-screen buff))
      (if msg
         (notify buff msg))
      (cont ll buff undo mode)))

(define (command-mark-position ll buff undo mode r cont)
   (lets ((msg ll (uncons ll #false)))
      (if (eq? (ref msg 1) 'key)
         (let ((char (ref msg 2)))
            (cont ll (mark-position buff char) undo mode))
         (cont ll buff undo mode))))

(define (command-line-end ll buff undo mode r cont)
   (lets ((buff out (seek-line-end buff)))
      (output out)
      (cont ll buff undo mode)))

(define (command-line-start ll buff undo mode r cont)
   (lets ((buff out (seek-line-start buff)))
      (output out)
      (cont ll buff undo mode)))

(define (command-move-down ll buff undo mode r cont)
   (lets ((buff out (move-arrow buff 'down))) 
      (output out) 
      (cont ll buff undo mode)))

(define (command-move-up ll buff undo mode r cont)
   (lets ((buff out (move-arrow buff 'up))) 
      (output out) 
      (cont ll buff undo mode)))

(define (command-move-right ll buff undo mode r cont)
   (lets ((buff out (move-arrow buff 'command-right))) 
      (output out) 
      (cont ll buff undo mode)))

(define (command-move-left ll buff undo mode r cont)
   (lets ((buff out (move-arrow buff 'left))) 
      (output out) 
      (cont ll buff undo mode)))

(define (command-seek-matching-paren ll buff undo mode r cont)
   (lets ((buff out (maybe-seek-matching-paren buff))) 
      (output out) 
      (cont ll buff undo mode)))

(define (command-paste ll buff undo mode r cont)
   (lets ((undo (push-undo undo buff))
          (buff (paste-yank buff)))
      (output (update-screen buff))
      (cont ll buff undo mode)))

(define (command-add-line-below ll buff undo mode r cont)
   (lets
      ((undo (push-undo undo buff))
       (u d l r x y w h off meta buff)
       (buff (buffer u (cons null d) l r x y w h off meta))
       (buff tio (move-arrow buff 'down)))
      (output (update-screen buff))
      (cont ll buff undo 'insert)))

(define (map-n op n lst)
   (cond
      ((null? lst) lst)
      ((eq? n 0) lst)
      (else
         (cons (op (car lst))
            (map-n op (- n 1) (cdr lst))))))

;; todo: put to meta later (based on shiftwidth)
(define shift-lst (list #\space #\space #\space))

(define (keys ll key n)
   (if (= n 0)
      ll
      (cons (tuple 'key key)
         (keys ll key (- n 1)))))

(define (line-left l r n)
   (cond
      ((eq? l null) (values l r))
      ((eq? n 0) (values l r))
      (else (line-left (cdr l) (cons (car l) r) (- n 1)))))

(define (line-right l r n)
   (cond
      ((eq? r null) (values l r))
      ((eq? n 0) (values l r))
      (else (line-right (cons (car r) l) (cdr r) (- n 1)))))

(define (command-indent ll buff undo mode n cont)
   (lets ((range ll (uncons ll #false)))
      (if (equal? range (tuple 'key #\>)) ;; only line-based indenting for now
         (lets
            ((undo (push-undo undo buff))
             (u d l r x y w h off meta buff)
             (l (append l shift-lst))
             (l r (line-left l r 3))
             (n (if (number? n) n 1))
             (d 
               (map-n    
                  (lambda (x) (append shift-lst x))
                  (- n 1) d))
             (buff (buffer u d l r x y w h off meta)))
            (output (update-screen buff))
            (cont (keys ll #\l 3) buff undo mode)) ;; move with shift
         (begin
            (log "No such shift range: " range)
            (cont ll buff undo mode)))))

;; drop prefix of lst, if there
(define (drop-prefix lst prefix)
   (cond
      ((null? prefix) lst)
      ((null? lst) #false)
      ((eq? (car lst) (car prefix))
         (drop-prefix (cdr lst) (cdr prefix)))
      (else #false)))

(define (unindent lst)
   (or (drop-prefix lst shift-lst) lst))

(define (command-unindent ll buff undo mode n cont)
   (lets ((range ll (uncons ll #false)))
      (if (equal? range (tuple 'key #\<)) ;; only line-based indenting for now
         (lets
            ((undo (push-undo undo buff))
             (u d l r x y w h off meta buff)
             (rlp (drop-prefix (reverse l) shift-lst)))
            (if rlp
               (lets ((l (reverse rlp))
                      (l r (line-right l r 3))
                      (n (if (number? n) n 1))
                      (d (map-n unindent (- n 1) d))
                      (buff (buffer u d l r x y w h off meta)))
                  (output (update-screen buff))
                  (cont (keys ll #\h 3) buff undo mode))
               (cont ll buff undo mode)))
         (begin
            (log "No such shift range: " range)
            (cont ll buff undo mode)))))

(define (command-delete-char ll buff undo mode r cont)
   (lets
      ((undo (push-undo undo buff))
       (u d l r x y w h off meta buff))
      (if (null? r)
         (if (null? l)
            (cont ll buff undo mode)
            (lets ((buff out (insert-backspace buff)))
               (output out)
               (cont ll buff undo mode)))
         (lets ((buff (buffer u d l (cdr r) x y w h off meta)))
            (output (update-screen buff)) ;; todo: can refresh just current line
            (cont ll buff undo mode)))))

(define (command-join-lines ll buff undo mode n cont)
   (lets
      ((undo (push-undo undo buff))
       (n (if (number? n) n 1)) ;; fixme: no interval handling
       (u d l r x y w h off meta buff))
      (let loop ((r r) (d d) (n n))
         (cond
            ((or (null? d) (eq? n 0))
               (let ((buff (buffer u d l r x y w h off meta)))
                  (output (update-screen buff))
                  (cont ll buff undo mode)))
            (else   
               (lets
                  ((tail (drop-leading-whitespace (car d)))
                   (tail (if (whitespace? (last r #\a)) tail (cons #\space tail))))
                  (loop (append r tail) (cdr d) (- n 1))))))))

(define (command-maybe-save-and-close ll buff undo mode n cont)
   (notify buff "press Z again to save and close")
   (lets ((chr ll (uncons ll #false)))
      (if (equal? chr (tuple 'key #\Z))
         (lets ((path (getf (buffer-meta buff) 'path))
                (ok? msg (write-buffer buff path)))
            (if ok?
               ;; exit to led-buffers
               (values ll buff undo mode 'close)
               (begin
                  (notify buff msg)
                  (cont ll buff undo mode))))
         (begin
            (notify buff "close aborted")
            (cont ll buff undo mode)))))
       
(define (command-go-to-line ll buff undo mode n cont)
   (lets ((buff (buffer-seek buff 0 (if (number? n) (- n 1) 0) #false)))
      (output (update-screen buff))
      (cont ll buff undo mode)))

(define (command-go-to-mark ll buff undo mode r cont)
   (log "marks is " (get-buffer-meta buff 'marks #empty))
   (lets ((msg ll (uncons ll #false)))
      (if (eq? (ref msg 1) 'key)
         (lets
            ((char (ref msg 2))
             (pos (getf (get-buffer-meta buff 'marks #empty) char)))
            (log "going back to position " pos)
            (if pos
               (lets 
                  ((x y pos)
                   (buff (buffer-seek buff x y #f)))
                  (output (update-screen buff))
                  (cont ll buff undo mode))
               (cont ll buff undo mode))))))

(define (command-go-to-last-line ll buff undo mode r cont)
   (lets 
      ((u d l r x y w h off meta buff)
       (last (+ 1 (+ (length u) (length d))))
       (buff (buffer-seek buff 0 last #false)))
      (output (update-screen buff))
      (cont ll buff undo mode)))

(define (command-enter-command ll buff undo mode r cont)
   (output (tio* (set-cursor 1 (+ 1 (screen-height buff))) (clear-line) (list #\:)))
   (lets
      ((metadata (buffer-meta buff))
       (ll res 
       (readline ll 
         (get (buffer-meta buff) 'command-history null) 
         2 (+ 1 (screen-height buff) 1) (screen-width buff)))
       (buff
         (set-buffer-meta buff
           (put metadata 'command-history
             (cons res (get metadata 'command-history null))))))
      (log (str "readline returned '" res "'"))
      (output
         (tio 
            (set-cursor 1 (+ 1 (screen-height buff))) 
            (clear-line)
            (set-cursor (buffer-x buff) (buffer-y buff))))
      (cond
       ((not res)
          (cont ll buff undo mode))
       ((equal? res "")
          (notify buff "canceled")
          (cont ll buff undo mode))
       ((equal? res "q")
         (values ll buff undo mode 'close))
       ((equal? res "q!")
         (values ll buff undo mode 'close))
       ((equal? res "n")
         (values ll buff undo mode 'new))
       ((m/^n [^ ]+$/ res)
         (values ll buff undo mode (tuple 'open (s/n +// res))))
       ((equal? res "vi")
         (cont ll buff undo 'insert))
       ((m/^w / res)
         (let ((path (s/^w +// res)))
            (lets ((ok? write-msg (write-buffer buff path)))
               (notify buff write-msg)
               (cont ll (put-buffer-meta buff 'path path) undo mode))))
       ((m/^r +[^ ]/ res)
          (lets ((path (s/^r +// res))
                 (lines (path->lines path metadata)))
             (log "read" lines "from" path)
             (if lines
                (lets ((undo (push-undo undo buff))
                       (buff (paste-lines-below buff lines)))
                   (output (update-screen buff))
                   (notify buff (str "Read " (length lines) " lines from '" path "'"))
                   (cont ll buff undo mode))
                (begin
                   (notify buff (str "Failed to read '" path "'"))
                   (cont ll buff undo mode)))))
       ((m/^w$/ res)
         (let ((path (getf (buffer-meta buff) 'path)))
            (if path
               (lets ((ok? write-tio (write-buffer buff path)))
                  (notify buff write-tio)
                  (cont ll buff undo mode))
               (cont ll buff undo mode))))
       ((m/^x$/ res)
          (lets ((path (getf (buffer-meta buff) 'path))
                 (ok? msg (write-buffer buff path)))
             (if ok?
                ;; exit to led-buffers
                (values ll buff undo mode 'close)
                (begin
                   (notify buff msg)
                   (cont ll buff undo mode)))))
       ((m/^[0-9]+$/ res)
         (lets ((line (max 0 (- (string->number res 10) 1)))
                (buff (buffer-seek buff 0 line #false)))
            (output (update-screen buff))
            (cont ll buff undo mode)))
       ((equal? res "$")
         (command-go-to-last-line ll buff undo mode r cont))
       (else
         (cont ll buff undo mode)))))

(define (command-redo ll buff undo mode r cont)
  (lets ((undo buffp (unpop-undo undo buff)))
    (if (eq? buff buffp)
       (notify buffp "nothing left to redo")
       (output (update-screen buffp)))
    (cont ll buffp undo mode)))

(define (command-undo ll buff undo mode r cont)
   (lets ((undo buffp (pop-undo undo buff)))
      (if (eq? buff buffp)
         (notify buff "nothing left to undo")
         (output (update-screen buffp)))
      (cont ll buffp undo mode)))

(define (command-insert-before ll buff undo mode r cont)
   (cont ll buff undo 'insert))

(define (command-insert-after ll buff undo mode r cont)
   (cont (cons (tuple 'arrow 'right) ll) buff undo 'insert))

(define (command-insert-after-line ll buff undo mode r cont)
   (cont (ilist (tuple 'key #\$) (tuple 'key #\a) ll) buff undo mode))

(define (command-delete ll buff undo mode r cont)
   (log "would delete " r " of something")
   (lets
      ((what ll (uncons ll #\d))
       (r (if (number? r) r 1))) ;; fixme, also supports ranges
      (cond
         ((equal? what (tuple 'key #\d))
            (let loop ((new buff) (lines null) (r r))
               (if (= r 0)
                  (begin
                     (output (update-screen new))
                     (cont ll
                        (put-buffer-meta new 'yank (tuple 'lines lines))
                        (push-undo undo buff) mode))
                  (lets ((new this (delete-line new)))
                     (loop new (append lines (list this)) (- r 1))))))
         ((equal? what (tuple 'key #\%))
            (lets ((buff msg (buffer-cut-sexp buff)))
               (output (update-screen buff))
               (notify buff msg)
               (cont ll buff undo mode)))
         (else
            (log "cannot delete " what " yet")
            (cont ll buff undo mode)))))

(define (command-no-op ll buff undo mode r cont)
   (cont ll buff undo mode))

(define (command-change-rest-of-line ll buff undo mode r cont)
   (lets ((u d l r x y w h off meta buff)
          (undo (push-undo undo buff))
          (buff (buffer u d l null x y w h off meta)))
         (output
            (tio 
               (cursor-save)
               (font-dim)
               (raw   
                  (repeat #\-
                     (min (+ 1 (- w x)) (printable-length r))))
               (font-normal)
               (cursor-restore)))
         (cont ll buff undo 'insert)))

(define (command-step-forward ll buff undo mode r cont)
   (lets ((u d l r x y w h off meta buff)
          (y (+ (cdr off) (- y 1)))
          (buff (buffer-seek buff (- x 1) (+ y (max 1 (- h 3))) 1)))
      (log "buffer seeking to " (cons x (+ y (max 1 (- h 3)))) " with y at " y)
      (output (update-screen buff))
      (cont ll buff undo mode)))

(define (command-step-backward ll buff undo mode r cont)
   (lets ((u d l r x y w h off meta buff)
          (y (+ (cdr off) (- y 1)))
          (buff (buffer-seek buff (- x 1) (- y (min (- h 3) y)) 1)))
      (output (update-screen buff))
      (cont ll buff undo mode)))

(define (command-update-screen ll buff undo mode r cont)
   (lets ((u d l r x y old-w old-h off meta buff)
          (y (+ (cdr off) (- y 1)))
          (x (+ (car off) (- x 1)))
          (w h ll (get-terminal-size ll))
          (buff (buffer u d l r x y w (max 1 (- h 1)) off meta))
          (buff (buffer-seek buff x y #false)))
      ;(log "updated screen from size " (cons old-w old-h) " to " (cons w h))
      ;(log "current left " l)
      ;(log "current left len " (printable-length l))
      ;(log "current right " r)
      ;(log "current right len " (printable-length r))
      (output (update-screen buff))
      (cont ll buff undo mode)))

(define (key-value event)
   (if (and (tuple? event) (eq? (ref event 1) 'key))
      (ref event 2)
      #false))
         
(define (command-replace-char ll buff undo mode ran cont)
   (lets ((val ll (uncons ll #false))
          (k (key-value val)))
      (if k
         (lets ((u d l r x y w h off meta buff))
            (if (null? r)
               (cont ll buff undo mode)
               (lets 
                  ((ran (if (number? ran) ran 1))
                   (r (map-n (lambda (x) k) ran r))
                   (undo (push-undo undo buff))
                   (buff 
                      (buffer u d l r x y w h off meta)))
                  (output (update-screen buff))
                  (cont ll buff undo mode))))
         (cont ll buff undo mode))))

(define (command-previous-buffer ll buff undo mode r cont)
   (values ll buff undo mode 'left))

(define (command-next-buffer ll buff undo mode r cont)
   (values ll buff undo mode 'right))

;;; Command mode key mapping

(define *command-mode-control-actions*
   (-> #empty
      (put #\f command-step-forward)
      (put #\b command-step-backward)
      (put #\r command-redo)
      (put 'arrow-left command-previous-buffer)
      (put 'arrow-right command-next-buffer)
      (put #\l command-update-screen)))

;; key → (ll buff undo mode range cont → (cont ll' buff' undo' mode'))
(define *command-mode-actions*
   (-> #empty
      (put #\/ command-regex-search)
      (put #\n command-find-next)
      (put #\m command-mark-position)
      (put #\$ command-line-end)
      (put #\0 command-line-start)
      (put #\j command-move-down)
      (put #\k command-move-up)
      (put #\l command-move-right)
      (put #\r command-replace-char)
      (put #\h command-move-left)
      (put #\p command-paste)
      (put #\o command-add-line-below)
      (put #\' command-go-to-mark)
      (put #\x command-delete-char)
      (put #\: command-enter-command)
      (put #\u command-undo)
      (put #\i command-insert-before)
      (put #\a command-insert-after)
      (put #\A command-insert-after-line)
      (put #\d command-delete)
      (put #\J command-join-lines)
      (put #\G command-go-to-line)
      (put #\> command-indent)
      (put #\< command-unindent)
      (put #\Q command-previous-buffer)
      (put #\W command-next-buffer)
      (put #\C command-change-rest-of-line)
      (put #\Z command-maybe-save-and-close)
      (put #\% command-seek-matching-paren)))


;;;
;;; Insert mode actions
;;;

(define (paren-balance lst)
   (fold + 0
      (map 
         (lambda (x) (if (left-paren? x) 1 (if (right-paren? x) -1 0)))
         lst)))

(define (unclosed-parens? lst)
   (> (paren-balance lst) 0))

(define (leading-whitespaces lst)
   (cond
      ((null? lst) null)
      ((whitespace? (car lst))
         (cons (car lst) (leading-whitespaces (cdr lst))))
      (else null)))

(define (drop-spaces lst n)
   (cond
      ((null? lst) lst)
      ((eq? n 0) lst)
      ((eq? (car lst) #\space)
         (drop-spaces (cdr lst) (- n 1)))
      (else lst))) ;; not touching other indents for now

;; up -> l
(define (artificial-intelligence u)
   (if (null? u)
      null
      (lets
         ((bal (paren-balance (car u)))
          (lead (leading-whitespaces (car u))))
         (log "lead is " lead ", paren balance " bal)
         (cond
            ((eq? bal 0)
               lead)
            ((> bal 0)
               (ilist #\space #\space #\space lead))
            (else
               (drop-spaces lead (* bal -3)))))))

;; ai hardcoded for now
(define (insert-enter buff)
   (lets 
      ((u d l r x y w h off meta buff)
       (ind (artificial-intelligence (cons (reverse l) u)))
       (buff 
         (buffer u (cons (append ind r) d) l null x y w h  off meta))
       (draw-tio 
         (update-screen buff))
       (buff move-tio (move-arrow buff 'down)))
      (values
         buff
         (append draw-tio move-tio))))

;;;
;;; Buffer handling loop
;;;

(define (key->digit k)
   (if (eq? (ref k 1) 'key)
      (let ((n (- (ref k 2) #\0)))
         (cond
            ((< n 0) #false)
            ((> n 9) #false)
            (else n)))
      #false))
         
(define space-node (tuple 'key #\space))

(define (maybe-get-range ll)
   (lets ((k ll (uncons ll space-node))
          (n (key->digit k)))
      (log "maybe-get-range: " k " -> " n)
      (cond
         ((not n) (values #false (cons k ll)))
         ((eq? n 0) (values #false (cons k ll)))
         (else
            (let loop ((n n) (ll ll))
               (lets ((x ll (uncons ll 0))
                      (m (key->digit x)))
                  (if m
                     (loop (+ (* 10 n) m) ll)
                     (values n (cons x ll)))))))))

;; ll buff undo mode -> ll' buff' undo' mode' action
(define (led-buffer ll buff undo mode)
   (log-buff buff mode)
   (if (eq? mode 'insert)
      (lets 
         ((msg ll (uncons ll #false))
          (u d l r x y w h off meta buff))
         (log "cursor " (cons x y) ", offset " off ", event " msg)
            (tuple-case msg
               ((key x)
                  (lets ((buff out (insert-handle-key buff x)))
                     (output out)
                     (if (eq? x 41) ;; close paren, highlight the match for a while (hack)
                        (led-buffer 
                           (ilist (tuple 'esc) 
                                  (tuple 'key #\%)
                                  (lambda ()
                                     (sleep 150)
                                     (ilist
                                        (tuple 'key #\%)
                                        (tuple 'key #\a)
                                        ll)))
                             buff undo mode)
                        (led-buffer ll buff undo mode))))
               ((tab)
                  (led-buffer (ilist space-node space-node space-node ll) buff undo mode))
               ((enter)
                  (lets ((buff out (insert-enter buff)))
                     (output out)
                     (led-buffer ll buff undo mode)))
               ((backspace)
                  (lets ((buff out (insert-backspace buff)))
                     (output out)
                     (led-buffer ll buff undo mode)))
               ((arrow dir)
                  (lets ((buff out (move-arrow buff dir)))
                     (output out)
                     (led-buffer ll buff undo mode)))
               ((end-of-text) 
                  (output (update-screen buff))
                  (led-buffer (cons (tuple 'key #\h) ll) buff (push-undo undo buff) 'command))
               ((esc)
                  (log "switching out of insert mode on esc + moving cursor")
                  (output (update-screen buff))
                  (led-buffer (cons (tuple 'key #\h) ll) buff (push-undo undo buff) 'command))
               ((end-of-transmission)
                  ;; ^D = 3 backspace (remove indent)
                  ;; fixme: switch to unindent
                  (led-buffer 
                     (ilist (tuple 'backspace) (tuple 'backspace) (tuple 'backspace) ll)
                     buff undo mode))
               ((ctrl key)
                  (cond
                     ((eq? key 'arrow-left)
                        (values ll buff undo mode 'left))
                     ((eq? key 'arrow-right)
                        (values ll buff undo mode 'right))
                     (else
                        (log "ignoring control " key " in insert mode")
                        (led-buffer ll buff undo mode))))
               (else
                  (led-buffer ll buff undo mode))))
      ;; command mode
      ;; [number] [command] [text object]
      (lets ((range ll (maybe-get-range ll))
             (msg ll (uncons ll space-node)))
         (tuple-case msg
           ((key k)
               ((get *command-mode-actions* k command-no-op)
                  ll buff undo mode range led-buffer))
           ((ctrl key)
               ((get *command-mode-control-actions* key command-no-op)
                  ll buff undo mode range led-buffer))
           (else
               (log "not handling command " msg)
               (led-buffer ll buff undo mode))))))


;;; Program startup 

(define (splash w h)
   (lets
      ((mw (>> w 1))
       (mh (>> h 1)))
      (output
         (tio
           (clear-screen)
           (set-cursor (- mw (>> (string-length version-str) 1)) mh)
           (raw (font-bright (render version-str null)))
           (font-normal)
           (set-cursor (max 1 (- mw 6)) (+ mh 2))
           (raw (render "esc + :q quits" null))
           (set-cursor 1 1)))))

(define (make-new-state ll)
   (lets ((w h ll (get-terminal-size ll))
          (buff (make-empty-buffer w (- h 1) #empty)))
      (values ll
         (tuple buff (initial-undo buff) 'command))))

(define (path->buffer-state ll path)
   (lets ((w h ll (get-terminal-size ll))
          (buff (make-file-state w h path #empty)))
      (values ll
         (if buff
            (tuple buff (initial-undo buff) 'command)
            #false))))
                     
(define (led-buffers ll left state right)
   (lets ((buff undo mode state)
          (_ (output (update-screen buff)))
          (_ (notify buff (or (get-buffer-meta buff 'path #false) "*scratch*")))
          (ll buff undo mode action
            (led-buffer ll buff undo mode))
          (state (tuple buff undo mode)))
      (log "led-buffers: action " action)
      (cond
         ((eq? action 'close)
            (cond
               ((pair? left)
                  (led-buffers ll (cdr left) (car left) right))
               ((pair? right)
                  (led-buffers ll left (car right) (cdr right)))
               (else
                  (log "all buffers closed")
                  (output
                     (tio
                        (clear-screen)
                        (set-cursor 1 1)))
                  0)))
         ((eq? action 'left)
            (if (null? left)
               (led-buffers ll left state right)
               (led-buffers ll (cdr left) (car left)
                  (cons state right))))
         ((eq? action 'right)
            (if (null? right)
               (led-buffers ll left state right)
               (led-buffers ll (cons state left)
                  (car right) (cdr right))))
         ((eq? action 'new)
            (log "making new buffer")
            (lets ((ll new-state (make-new-state ll)))
               (led-buffers ll (cons state left) new-state right)))
         ((tuple? action)
            (tuple-case action
               ((open path)
                  (lets ((ll new (path->buffer-state ll path)))
                     (if new
                        (led-buffers ll (cons state left) new right)
                        (begin
                           (log "failed to open " path)
                           (led-buffers ll left state right)))))
               (else is unknown
                  (log"unknown buffer action " action)
                  (led-buffers ll left state right))))
         (else
            (log "unknown buffer action " action)
            (led-buffers ll left state right)))))

(define (open-all-files paths w h shared-meta)
   (fold
      (lambda (states path)
         (log "loading " path)
         (and states
            (let ((buff (make-file-state w h path shared-meta)))
               (if buff
                  (cons (tuple buff (initial-undo buff) 'command)
                     states)
                  (begin
                     (print-to stderr "Failed to open '" path "'")
                     #false)))))
      null
      paths))

(define (start-led dict args ll)
  (log "start-led " dict ", " args)
  (lets ((w h ll (get-terminal-size ll))
         (h (max (- h 1) 1)))
    (log "dimensions " (cons w h))
    (lets 
      ((meta 
         (-> #empty
            (put 'tab tab-node)
            (put 'tab-width 3)))
       (states
         (if (null? args)
            (let ((buff (make-empty-buffer w h meta)))
               (list (tuple buff (initial-undo buff) 'command)))
            (open-all-files args w h meta))))
      (if states
         (led-buffers ll null (car states) (cdr states))
         1))))

(define usage-text 
  "Usage: led [flags] [file]")

(define command-line-rules
  (cl-rules
    `((help "-h" "--help" comment "show this thing")
      (version "-V" "--version" comment "show program version")
      (log "-L" "--log" has-arg comment "debug log file")
      (faketerm "-I" "--input" has-arg comment "fake terminal input stream source"))))

(define (trampoline)
  (let ((env (wait-mail)))
    (log "main: " env)
    (set-terminal-rawness #false)
    (if (and (eq? (ref env 1) 'led) (eq? (ref (ref env 2) 1) 'finished))
      (begin
         (log "trampoline: finished")
         (wait 10) ;; let other threads run if necessary (mainly send debug info before halt)
         0)
      (begin
        (print "error: " env)
        (mail 'logger (list 'ERROR env))
        (wait 100)
        (halt 1)))))

(define (sink arg)
   (sink (wait-mail)))

(define (log-to port)
   (print-to port (ref (wait-mail) 2))
   (log-to port))

(define (logger meta)
   (let ((log-path (getf meta 'log)))
      (if log-path
         (let ((port (open-output-file log-path)))
            (if port
               (begin
                  (print-to port "Started logging")
                  (log-to port))
               (begin
                  (print-to stderr "could not open log file " log-path)
                  (halt 1))))
         (sink #f))))

(define (led-input-stream dict)
   (let ((path (getf dict 'faketerm)))
      (if path
         (let ((port (open-input-file path)))
            (if port   
               (terminal-input port)
               null))
         (begin
            (set-terminal-rawness #true)
            (terminal-input stdin)))))

(define (start-led-threads dict args)
  (cond
    ((getf dict 'help)
      (print usage-text)
      (print (format-rules command-line-rules))
      0)
    ((getf dict 'version)
      (print version-str)
      0)
    (else
      (log "started " dict ", " args)
      (fork-linked-server 'logger (λ () (logger dict)))
      (fork-linked-server 'led 
         (λ () (start-led dict args (led-input-stream dict))))
      (log "started")
      (trampoline))))

(define (main args)
  (process-arguments (cdr args) command-line-rules usage-text start-led-threads))

main







