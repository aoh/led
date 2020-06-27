(define-library (led subprocess)

   (import
      (owl base)
      (only (owl sys) exec fork sigkill kill)
      (led log))

   (export
      start-repl
      communicate
      close-pipe)

   (begin
      (define (pipe)
         (let ((pair (sys-prim 31 0 0 0)))
            (values
               (car pair)    ;; read port
               (cdr pair)))) ;; write port

      (define (dup2 old new)
         (let ((res (sys-prim 30 old new #f)))
            res))

      (define (start-repl call)
         (lets ((stdin-read  stdin-write  (pipe))
                (stdout-read stdout-write (pipe))
                (pid (fork)))
            (cond
               ((eq? pid #true) ;; child proces
                  (dup2 stdin-read stdin)
                  (close-port stdin-write)
                  (dup2 stdout-write stdout)
                  (close-port stdout-read)
                  (exec (car call) call))
               (pid
                  (close-port stdin-read)
                  (close-port stdout-write)
                  (tuple pid call stdin-write stdout-read))
               (else #false))))

      (define (wait-response fd timeout)
         (lets ((req (tuple 'read-timeout fd timeout))
                (resp (interact 'iomux req)))
            (if (eq? req resp)
               ;; can read now
               (lets
                  ((resp (try-get-block fd (* 16 1024) #f))
                   (more (wait-response fd 100)))
                  (if more
                     (append (vector->list resp) more)
                     (vector->list resp)))
               #false)))


      ;; pipe = (send-fd . read-fd)
      (define (communicate pipe data)
         (cond
            ((string? data)
               (communicate pipe (string->bytes data)))
            ((write-bytes (ref pipe 3) data)
               (wait-response (ref pipe 4) 2000))
            (else
               #false)))

      (define (close-pipe pipe)
         (log "closing subprocess " pipe)
         (kill (ref pipe 2) sigkill)
         )

      ))

