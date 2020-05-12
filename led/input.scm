(define-library (led input)

   (import
      (owl base)
      (led log)
      (owl terminal))

   (export
      start-input-terminal)

   (begin
      (define (input-terminal input target)
         (lfold
            (λ (_ thing)
               (log "input terminal: sending " thing " to " target)
               (mail target thing))
            'unused
            input))

      (define (start-input-terminal target ll)
         (let ((name 'input-terminal))
            (thread name
               (input-terminal ll target))
            (link name)
            name))))
