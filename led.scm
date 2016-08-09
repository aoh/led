#!/usr/bin/ol --run

; id€ntitéèttı = λä.(ä ä)

(import
  (owl terminal)
  (only (owl unicode) encode-point)
  (owl args))

(define version-str "led v0.1a")

;;; Temporary logging

(define logfd 
   (open-output-file "led.log"))

(define (log . what)
   (print-to logfd what))


;;; Movement and insert mode edit operation

(define (buffer up down left right x y w h off meta)
   (tuple up down left right x y w h off meta))

(define (make-empty-state w h meta)
	(buffer null null null null 1 1 w h (cons 0 0) meta))

(define (buffer-meta buff)
  (ref buff 10))

(define (set-buffer-meta buff meta)
  (set buff 10 meta))

(define (buffer-x buff) (ref buff 5))
(define (buffer-y buff) (ref buff 6))

(define (make-file-state w h path meta)
  (let ((data (map string->list (force-ll (lines (open-input-file path))))))
    (if (pair? data)
      (buffer null (cdr data) null (car data) 1 1 w h (cons 0 0) 
        (put meta 'path path))
      (error "could not open " path))))

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
                    (take-printable (append output line) n))
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
               (drop-printable (append output line) n)))
          (else
            (error "drop-printable: what is " x)))))
    (else
      null)))

(define (printable-length line)
   (fold (λ (n x) (+ n (node-width x))) 0 line))

(define tab-node 
  (tuple 'replace (list #\tab) 4 (list #\| #\- #\- #\.)))

(define empty-buffer-line
	(tio
		(font-bold)
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

(define (log-buff buff)
  (lets
    ((u d l r x y w h off meta buff))
    ;(log "left " l ", right " r)
    (log "log: cursor at " (cons x y) " at offset " off ", line pos " (+ (car off) (- x 1)))))
       
(define (key-node k)
  (cond
    ((eq? k #\tab)
      tab-node)
    (else k)))

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
         (if (< (+ x nw) w)
            (begin
               (log "insert of key " k " at " (cons x y) " = " node)
               (values
                  (buffer u d (cons node l) r (+ x nw) y w h off meta)
                  (encode-node node
                     (if (null? r)
                        null
                        (tio
                           (clear-line-right)
                           (cursor-save)
                           (raw (take-printable r (- w (+ x nw))))
                           (cursor-restore))))))
            (lets
               ((buff scroll-tio (scroll-right buff))
                (buff insert-tio (insert-handle-key buff k)))
               (values buff
                  (append scroll-tio insert-tio)))))))

(define (insert-backspace buff)
   (lets ((u d l r x y w h off meta buff))
      (if (null? l)
         ;; no-op (could also backspace to line above)
         (values buff null)
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


;; (a b c d ... n) 3 → (c b a) (d... n)
(define (line-seek line pos)
  (let loop ((line line) (pos pos) (l null))
    (cond
      ((eq? pos 0)
        (values l line 0))
      ((null? line)
        (error "empty line at line-seek: " pos))
      (else
        (lets 
          ((w (node-width (car line)))
           (pos (- pos w)))
          (if (> pos 0)
            (loop (cdr line) pos (cons (car line) l))
            (values (cons (car line) l) (cdr line) pos)))))))

;; move line down within the same screen preserving cursor position if possible
(define (line-down buff)
   (lets ((u d l r x y w h off meta buff)
          (dx dy off)
          (_ (log "line-down starting from " (cons x y)))
          (line (append (reverse l) r))
          (u (cons line u))
          (y (+ y 1))
          (line d (uncons d null))
          (x (min x (+ 1 (- (printable-length line) (car off)))))
          (line-pos (+ (- x 1) (car off)))
          (l r offset (line-seek line line-pos)))
        (log "line-down went to (x . y) " (cons x y))
        (log "next line length is " (printable-length line) ", x=" x ", dx=" (car off) ", l='" (list->string l) "', r='" (list->string r) "', offset " offset) 
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
          (l r offset (line-seek line line-pos)))
        (log "line-up went to (x . y) " (cons x y))
        (log "next line length is " (printable-length line) ", x=" x ", dx=" (car off) ", l='" (list->string l) "', r='" (list->string r) "'")
        (values
          (buffer u d l r (- x offset) y w h off meta)
          (- x offset) y)))

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

(define (write-buffer buff path)
	(lets
		((port (open-output-file path))
		 (lst (buffer->bytes buff))
		 (n (length lst))
		 (res (byte-stream->port lst port)))
		(if res
			(foldr render null 
				(list "Wrote " n " bytes to '" path "'"))
			(foldr render null
				(list "Failed to write to '" path "'")))))
		
;;; Event dispatcher

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
                      ;(u (cons (reverse l) u))
                      ;(buff (buffer u d null r 1 (+ y 1) w h off meta))
							 (buff 
							 	(buffer u (cons r d) null (reverse l) 1 y w h (cons 0 (cdr off)) meta))
							 (draw-tio (if (eq? (car off) 1) (tio (clear-line-right)) (update-screen buff)))
							 (buff move-tio (move-arrow buff 'down)))
							(mail 'terminal 
								(append draw-tio move-tio))
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
                  (led-buffer (push-undo undo buff)  undo 'command))
               ((esc)         
                  (log "switching out of insert mode on esc")
                  (led-buffer buff (push-undo undo buff) 'command))
               (else
                  (led-buffer buff undo mode)))
            (tuple-case msg
              ((key k)
                  (cond
                     ((eq? k #\:)
                       (mail 'terminal (tio* (set-cursor 1 (screen-height buff)) (clear-line) (list #\:)))
                       (lets
                          ((ll (interact 'terminal 'get-input))
                           (metadata (buffer-meta buff))
                           (ll res 
                            (readline ll 
                              (get (buffer-meta buff) 'command-history null) 
                              2 (screen-height buff) (screen-width buff)))
                           (buff
                              (set-buffer-meta buff
                                (put metadata 'command-history
                                  (cons res (get metadata 'command-history null))))))
                          (log "restoring input stream " ll " to terminal")
                          (mail 'terminal ll) ;; restore input stream
                          (log (str "readline returned '" res "'"))
                          (mail 'terminal 
                           (tio 
                              (set-cursor 1 (screen-height buff)) 
                              (clear-line)
                              (set-cursor (buffer-x buff) (buffer-y buff))
                              ))
                          (cond
                            ((equal? res "q")
                              (begin
                                (mail 'terminal
                                  (tio
                                    (raw (list #\newline))
                                    (set-cursor 1 (screen-height buff))))
                                (mail 'terminal 'stop)
                                0))
                            ((equal? res "vi")
                              (led-buffer buff undo 'insert))
									 ((m/^w / res)
										(let ((path (s/^w +// res)))
											(log "saving buffer to " path)
											(lets ((write-tio (write-buffer buff path)))
												(mail 'terminal
													(tio
														(cursor-save)
														(set-cursor 1 (screen-height buff))
														(raw write-tio)
														(cursor-restore)))
												(led-buffer buff undo mode))))
                            (else
                              (led-buffer buff undo mode)))))
                     ((eq? k #\u)
                        (lets ((undo buff (pop-undo undo buff)))
                           (mail 'terminal (update-screen buff))
                           (led-buffer buff undo mode)))
							((eq? k #\i)
								(led-buffer buff undo 'insert))
                     (else
                        (log "not handling command " msg)
                        (led-buffer buff undo mode))))
              ((ctrl key)
                (cond
                  ((eq? key #\r)
                    (lets ((undo buff (unpop-undo undo buff))) ;; does not keep track of dirtiness
                      (mail 'terminal (update-screen buff))
                      (led-buffer buff undo 'command)))
                  (else
                    (led-buffer buff undo mode))))
              (else
                  (log "not handling command " msg)
                  (led-buffer buff undo mode)))))))


;;; Program startup 

(define (splash w h)
   (lets
      ((mw (>> w 1))
       (mh (>> h 1)))
      (mail 'terminal
         (tio
           (clear-screen)
           (set-cursor (- mw (>> (string-length version-str) 1)) mh)
           (raw (font-bold (render version-str null)))
           (font-normal)
           (set-cursor (max 1 (- mw 6)) (+ mh 2))
           (raw (render "esc + :q quits" null))
           (set-cursor 1 1)))))

(define (start-led dict args)
  (log "start-led " dict ", " args)
  (lets ((dimensions (interact 'terminal 'get-terminal-size))
         (w h dimensions))
    (log "dimensions " dimensions)
    (lets 
      ((buff 
        (if (= (length args) 1)
          (make-file-state w h (car args) #empty)
          (make-empty-state w h #empty))))
	   (mail 'terminal (update-screen buff))
      (led-buffer buff (initial-undo buff) 'command))))

(define usage-text 
  "Usage: led [flags] [file]")

(define command-line-rules
  (cl-rules
    `((help "-h" "--help" comment "show this thing")
      (version "-V" "--version" comment "show program version"))))

(define (trampoline)
  (let ((env (wait-mail)))
    (log "main: " env)
    (if (and (eq? (ref env 1) 'led) (eq? (ref (ref env 2) 1) 'finished))
      (halt 0)
      (begin
        (print "error: " env)
        (halt 1)))))

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
      (fork-linked-server 'terminal (λ () (terminal-server stdin 'led)))
      (fork-linked-server 'led (λ () (start-led dict args)))
      (trampoline))))

(define (main args)
  (process-arguments (cdr args) command-line-rules usage-text start-led-threads))

main
