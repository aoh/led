(define-library (led buffer)

   (import
      (owl base)
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
      screen-width
      screen-height
      buffer-screen-size
      buffer-y
      buffer->lines
      )

   (begin      

      (define (buffer up down left right x y w h off meta)
         (tuple up down left right x y w h off meta))
   
      (define (make-buffer-having w h meta data)
         (lets ((r d (uncons data null)))
            (buffer null d null r  1 1 w h (cons 0 0) meta)))
      
      (define (make-empty-buffer w h meta)
         (make-buffer-having w h meta null))
      
      (define (buffer-screen-size buff)
         (lets ((u d l r x y w h off meta buff))
            (values w h)))
      
      (define (buffer-meta buff) (ref buff 10))
      (define (set-buffer-meta buff meta) (set buff 10 meta))
      (define (screen-width buff) (ref buff 7))
      (define (screen-height buff) (ref buff 8))
   
      (define (put-buffer-meta buff key val)
         (set-buffer-meta buff (put (buffer-meta buff) key val)))
   
      (define (get-buffer-meta buff key def)
         (get (buffer-meta buff) key def))
   
      (define (get-global-meta buff key def)
         (get (get-buffer-meta buff 'global #empty) key def))
      
      (define (put-global-meta buff key val)
         (put-buffer-meta buff 'global
            (put (get-buffer-meta buff 'global #empty) key val)))
   
      (define (buffer-path buff default)
         (get-buffer-meta buff 'path default))
   
      (define (buffer-path-str buff)
         (get-buffer-meta buff 'path "*scratch*"))
   
      (define (buffer-x buff) (ref buff 5))
      (define (buffer-y buff) (ref buff 6))
      
      (define (buffer->lines buff) 
         (lets ((u d l r x y w h off meta buff))
            (log "buffer->lines bound")
            (map (lambda (line) (list->string (foldr render-code-point null line)))
               (append (reverse u)
                  (cons (append (reverse l) r) d)))))
      ))
