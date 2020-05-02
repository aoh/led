(define-library (led env)
   (import 
      (owl base))

   (export 
      empty-env 
      empty-led-env
      set-status-text
      clear-status-text
      )

   (begin
   
      (define empty-env
         (-> empty
            (put 'autoindent #true) ;; for now
            (put 'undo null)
            (put 'redo null)))
            
      (define (empty-led-env id path) 
         (if path
            (put empty-env 'path path)
            empty-env))

      (define (set-status-text env string)
         (put env 'status-message (string->runes string)))
                
      (define (clear-status-text env)
         (del env 'status-message))
                               
))
                     
                              