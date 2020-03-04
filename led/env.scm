(define-library (led env)
   (import (owl base))
   (export empty-env empty-led-env)
   (begin
      (define empty-env
         (-> empty
            (put 'autoindent #true) ;; for now
            (put 'undo null)
            (put 'redo null)))
      (define (empty-led-env id path) 
         (if path
               (put empty-env 'path path)
                     empty-env))))
                     
                              