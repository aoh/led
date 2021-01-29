(define-library (led env)

   (import
      (owl toplevel))

   (export
      empty-led-env
      set-status-text
      clear-status-text)

   (begin

      (define empty-env
         (pipe empty
            (put 'autoindent #true) ;; for now
            (put 'undo null)
            (put 'redo null)
            (put 'subprocess #false) ;; each buffer can have own (ones)
            ))

      (define (empty-led-env base-env id path)
         (let ((env (ff-union empty-env base-env (lambda (a b) a))))
            (if path
               (put env 'path path)
               env)))

      (define (set-status-text env string)
         (put env 'status-message (string->runes string)))

      (define (clear-status-text env)
         (del env 'status-message))))
