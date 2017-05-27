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
      screen-width
      screen-height
      buffer-screen-size
      buffer-y
      buffer->lines
      
      write-buffer
      )

   (begin      

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
         (get (get-buffer-meta buff 'global #empty) key def))
      
      (define (screen-width buff) (get-global-meta buff 'width 10))
      (define (screen-height buff) (get-global-meta buff 'height 10))
      
      (define (buffer-screen-size buff)
         (values
            (get-global-meta buff 'width 20)
            (get-global-meta buff 'height 10)))
      
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
         (lets ((u d l r x y off meta buff))
            (log "buffer->lines bound")
            (map (λ (line) (list->string (foldr render-code-point null line)))
               (append (reverse u)
                  (cons (append (reverse l) r) d)))))


      ;; buffer writing
            
      (define (nodes->bytes nodes)       (foldr render-node null nodes))


      (define (buffer->bytes buff)
         (lets ((u d l r x y off meta buff))
            (nodes->bytes
               (foldr 
                  (λ (line tl)
                     (append line (cons #\newline tl)))
                  null
                  (append (reverse u) (list (append (reverse l) r)) d)))))
      
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
