(define-library (led buffer)

   (import
      (owl base))
   
   (export
      buffer
      make-empty-buffer
      buffer-meta
      set-buffer-meta
      put-buffer-meta
      get-buffer-meta
      buffer-path
      buffer-path-str
      buffer-x
      screen-width
      screen-height
      buffer-y)

   (begin      

      (define (buffer up down left right x y w h off meta)
         (tuple up down left right x y w h off meta))
   
      (define (make-empty-buffer w h meta)
         (buffer null null null null 1 1 w h (cons 0 0) meta))
   
      (define (buffer-meta buff) (ref buff 10))
      (define (set-buffer-meta buff meta) (set buff 10 meta))
      (define (screen-width buff) (ref buff 7))
      (define (screen-height buff) (ref buff 8))
   
      (define (put-buffer-meta buff key val)
         (set-buffer-meta buff (put (buffer-meta buff) key val)))
   
      (define (get-buffer-meta buff key def)
         (get (buffer-meta buff) key def))
   
      (define (buffer-path buff default)
         (get-buffer-meta buff 'path default))
   
      (define (buffer-path-str buff)
         (get-buffer-meta buff 'path "*scratch*"))
   
      (define (buffer-x buff) (ref buff 5))
      (define (buffer-y buff) (ref buff 6))))
