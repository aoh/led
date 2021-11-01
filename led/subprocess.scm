(define-library (led subprocess)

   (import
      (owl toplevel)
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

      (define (wait-data fd)
         (lets ((req (tuple 'read-timeout fd 30000))
                (resp (interact 'iomux req)))
            (if (eq? req resp)
               ;; can read now
               (lets
                  ((resp (try-get-block fd (* 16 1024) #f)))
                  resp)
               (wait-data fd))))

      (define (fd-pusher fd)
         (let ((my-id (interact 'ui (tuple 'whoami))))
            (log "starting fd pusher from fd " fd " to thread " my-id)
            (thread
               (let loop ()
                  (let ((data (wait-data fd)))
                     (if (or (not data) (eof-object? data))
                        (mail my-id (tuple 'push "<eof>"))
                        (begin
                           (mail my-id (tuple 'push (vector->list data))) ;; todo, utf8
                           (loop))))))))

      (define (start-repl call)
         (lets ((stdin-read  stdin-write  (pipe))
                (stdout-read stdout-write (pipe))
                (pid (fork)))
            (cond
               ((eq? pid #true) ;; child proces
                  ;; remap stdio and call the command
                  (dup2 stdin-read stdin)
                  (close-port stdin-write)
                  (dup2 stdout-write stdout)
                  (close-port stdout-read)
                  (exec (car call) call))
               (pid
                  ;; leave information on how to talk to the process
                  (close-port stdin-read)
                  (close-port stdout-write)
                  (fd-pusher stdout-read) ;; for use in append mode
                  (tuple pid call stdin-write stdout-read)
                  )
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
         (log "sending " data)
         (cond
            ((string? data)
               (communicate pipe (string->bytes data)))
            ((write-bytes (ref pipe 3) data)
               ;(wait-response (ref pipe 4) 2000)
               data ;; leave as is, append result
               )
            (else
               #false)))

      (define (close-pipe pipe)
         (log "closing subprocess " pipe)
         (kill (ref pipe 2) sigkill)
         )

      ))

