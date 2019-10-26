(define-library (led buffer)

   (import
      (owl base)
      (owl sys)
      (led log)
      (led node))

   (export
      buffer
      make-buffer-having
      make-empty-buffer
      buffer-meta
      set-buffer-meta
      put-buffer-meta
      get-buffer-meta
      get-global-meta
      put-global-meta
      buffer-path
      buffer-path-str
      buffer-x
      put-copy-buffer
      get-copy-buffer
      screen-width
      screen-height
      buffer-screen-size
      buffer-y
      buffer->lines

      buffer-current-line ;; get value of .
      buffer-line-count   ;; get value of $

      write-buffer
      buffer-range->bytes

      movement
      next-words
      )

   (begin

      (define null '())

      (define (buffer up down left right x y off meta)
         (tuple up down left right x y off meta))

      (define (make-buffer-having w h meta data)
         (lets ((r d (uncons data null)))
            (buffer null d null r  1 1 (cons 0 0) meta)))

      (define (make-empty-buffer w h meta)
         (make-buffer-having w h meta null))

      (define (buffer-meta buff) (ref buff 8))
      (define (set-buffer-meta buff meta) (set buff 8 meta))

      (define (put-buffer-meta buff key val)
         (set-buffer-meta buff (put (buffer-meta buff) key val)))

      (define (get-buffer-meta buff key def)
         (get (buffer-meta buff) key def))

      (define (get-global-meta buff key def)
         (get (get-buffer-meta buff 'global empty) key def))

      (define (screen-width buff) (get-global-meta buff 'width 10))
      (define (screen-height buff) (get-global-meta buff 'height 10))

      (define (buffer-screen-size buff)
         (values
            (get-global-meta buff 'width 20)
            (get-global-meta buff 'height 10)))

      (define (put-global-meta buff key val)
         (put-buffer-meta buff 'global
            (put (get-buffer-meta buff 'global empty) key val)))

      (define (put-copy-buffer buff key val)
         (put-global-meta buff 'copy
            (put (get-global-meta buff 'copy empty) key val)))

      (define (get-copy-buffer buff key def)
         (get (get-global-meta buff 'copy empty) key def))

      (define (buffer-path buff default)
         (get-buffer-meta buff 'path default))

      (define (buffer-path-str buff)
         (get-buffer-meta buff 'path "*scratch*"))

      (define (buffer-x buff) (ref buff 5))
      (define (buffer-y buff) (ref buff 6))

      (define (buffer->lines buff)
         (lets ((u d l r x y off meta buff))
            (log "buffer->lines bound")
            (map (λ (line) (list->string (foldr render-code-point null line)))
               (append (reverse u)
                  (cons (append (reverse l) r) d)))))




      ;; buffer writing

      (define (nodes->bytes nodes)
         (foldr render-node null nodes))

      (define (lines->bytes ls)
         (nodes->bytes
            (foldr
               (λ (line tl)
                  (append line (cons #\newline tl)))
               null ls)))

      (define (buffer->bytes buff)
         (lets ((u d l r x y off meta buff))
            (lines->bytes
               (append (reverse u) (list (append (reverse l) r)) d))))

      (define (pick-lines ls start end)
         (let ((off (- start 1)))
            (take (drop ls off) (- end off))))

      ;; buffer → line-number
      (define (buffer-current-line buff)
         (lets ((u d l r x y off meta buff)
                (dx dy off))
            (+ y dy)))

      (define (buffer-line-count buff)
         (lets ((u d l r x y off meta buff))
            ; = (+ 1 (length u) (length d))
            (+ (buffer-current-line buff)
               (length d))))

      ;; buff start end → (byte ...)
      (define (buffer-range->bytes buff start end)
         (lets ((u d l r x y off meta buff))
            (if (and start end (<= start end))
               (lines->bytes
                  (pick-lines
                     (append (reverse u) (list (append (reverse l) r)) d)
                     start end))
               #false)))

      (define (blank-line? l)
         (cond
            ((null? l) #true)
            ((whitespace? (car l))
               (blank-line? (cdr l)))
            (else #false)))

      ;; l r d -> n lines down (n >= 1)
      (define (next-paragraphs l r d n)
         (define (next ls state dy) ; → ls' dy'
            (cond
               ((null? ls)
                  (values ls dy))
               ((blank-line? (car ls))
                  (if (eq? state 'init)
                     (next (cdr ls) state (+ dy 1))
                     (values ls dy)))
               ((eq? state 'body)
                  (next (cdr ls) state (+ dy 1)))
               (else
                  (next ls 'body dy))))
         (let loop ((ls (cons (append l r) d)) (dy 0) (n n))
            (if (= n 0)
               (values ls dy)
               (lets ((ls dy (next ls 'init dy)))
                  (loop ls dy (- n 1))))))


      ;; r d -> r' d' n-down n-from-left
      (define (next-word start d n-left)
         (let loop ((r start) (d d) (y 0) (x 0) (space? #false))
            (log "loop " r "," y "," x "," space?)
            (cond
               ((null? r)
                  (cond
                     ((null? d)
                        (values null null y x))
                     ((eq? n-left 1)
                        ;; do not eat the final newline after last word
                        (values r d y x))
                     ;(space?
                     ;   (loop (car d) (cdr d) (+ y 1) 0 #t))
                     (else
                        (loop (car d) (cdr d) (+ y 1) 0 #t))))
               ((space-char? (car r))
                  (loop (cdr r) d y (+ x 1) #t))
               ((word-delim-char? (car r))
                  (if (eq? r start)
                     ;; consume the one delimiter char and potential whitespace
                     (loop (cdr r) d y (+ x 1) #true)
                     ;; otherwise stop here
                     (values r d y x)))
               (space?
                  (values r d y x))
               (else
                  (loop (cdr r) d y (+ x 1) #f)))))

      ;; r d n -> dy dx r' d'
      (define (next-words r d n)
         (let loop((y 0) (x 0) (r r) (d d) (n n))
            (if (eq? n 0)
               (values y x r d)
               (lets ((r d dy dx (next-word r d n)))
                  (if (eq? dy 0)
                     (loop y (+ x dx) r d (- n 1))
                     (loop (+ y dy) dx r d (- n 1)))))))

      ;; buff movement-exp -> n | dy dx
      (define (movement buff n type)
         (lets ((u d l r x y off meta buff))
            (cond
               ((eq? type 'end-of-line)
                  (values (- n 1) (length r)))
               ((eq? type 'beginning-of-line)
                  (values (- n 1) (- 0 (length l))))
               ((eq? type 'word)
                  (lets ((y x r d (next-words r d n)))
                     (values y x)))
               ((eq? type 'paragraph)
                  (lets ((d dy (next-paragraphs l r d n)))
                     (values dy 0)))
               ((eq? type 'paragraph-back)
                  (lets ((d dy (next-paragraphs l r u n)))
                     (values (- 0 dy) 0)))
               (else
                  (log "unknown movement type " type)
                  (values #f #f)))))

            (define (write-buffer buff path)
               (log "writing to " path)
               (cond
                  ((not path)
                     (values #false
                        (foldr render null
                           (list "Give me a name for this"))))
                  ((directory? path)
                     (values #false
                        (foldr render null
                           (list "'" path "' is a directory"))))
                  (else
                     (lets
                        ((port (open-output-file path))
                         (lst (buffer->bytes buff))
                         (n (length lst))
                         (res (if port (byte-stream->port lst port) #f)))
                        (if port
                           (close-port port))
                        (if res
                           (values #true
                              (foldr render null
                                 (list "Wrote " n " bytes to '" path "'")))
                           (values #false
                              (foldr render null
                                 (list "Failed to write to '" path "'"))))))))


))
