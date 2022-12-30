(define-library (led subprocess)

   (import
      (owl toplevel)
      (only (owl sys) exec fork sigkill kill waitpid getenv file?)
      (led log))

   (export
      start-repl
      communicate
      close-pipe
      fd-pusher      ;; <- maybe relocate
      subprocess-eval
      )

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

      (define (verbose-exit rval)
         (cond
            ((equal? rval '(1 . 0))
               ;; regular exit, success due to 0
               (str ";; subprocess exited successfully\n"))
            ((eq? (car rval) 1)
               (str ";; subprocess failed, exit value " (cdr rval) "\n"))
            (else
               (str ";; abnormal subprocess termination: " rval "\n"))))

      (define (fd-pusher fd pid)
         (let ((my-id (interact 'ui (tuple 'whoami))))
            (log "starting fd pusher from fd " fd " to thread " my-id)
            (thread
               (let loop ()
                  (let ((data (wait-data fd)))
                     (if (or (not data) (eof-object? data))
                        (let ((rval (waitpid pid)))
                           (mail my-id
                              (tuple 'push (verbose-exit rval))))
                        (begin
                           (mail my-id (tuple 'push (vector->list data))) ;; todo, utf8
                           (loop))))))))

      (define (find-binary path)
         (log "looking for binary " path)
         (if (file? path)
            path
            (fold
               (lambda (found dir)
                  (or found
                     (let ((this (str dir "/" path)))
                        (if (file? this)
                           this
                           #f))))
               #f
               (map list->string
                  (split
                     (partial eq? #\:)
                     (string->list
                        (or
                           (getenv "PATH")
                           "")))))))

      ;; call -> ##(pid call stdin-write stdout-read)
      (define (start-repl call)
         (cond
            ((null? call) #f)
            ((find-binary (car call)) =>
               (lambda (bin)
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
                           (exec bin call))
                        (pid
                           ;; leave information on how to talk to the process
                           (close-port stdin-read)
                           (close-port stdout-write)
                           (prod pid call stdin-write stdout-read))
                        (else #false)))))
            (else
               (log "cannot find binary")
               #f)))

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

      ;; pipe = #[pid (cmd ..) to-fd from-fd]
      (define (communicate pipe data)
         (log "sending " data)
         (lets ((pid call in out <- pipe))
            (cond
               ((string? data)
                  (communicate pipe (string->bytes data)))
               ((write-bytes in data)
                  ;(wait-response out 2000)
                  #t
                  )
               (else
                  #false))))

      (define (close-pipe pipe)
         (lets ((pid call in out <- pipe))
            (log "closing subprocess " pid)
            (kill pid sigkill)))

      ;;;
      ;;; one-shot input -> output conversion
      ;;;

      ;; warning: assume potential UTF-8 encoding/decoding will be done outside
      ;; todo: waitpid + check subprocess exit value
      ;; → ok? byte-list|error-string-byte-list
      (define (communicate-io pid sub-in sub-out in-data out-rread tend)
         (cond
            ((readable? sub-out)
               (let ((block (try-get-block sub-out 1024 #f)))
                  (cond
                     ((eof-object? block)
                        ;; ok exit
                        (values #t
                           (foldr
                              (lambda (block tail)
                                 (vec-foldr
                                    (lambda (byte tail)
                                       (cons byte tail))
                                    tail block))
                              '() (reverse out-rread))))
                     ((vector? block)
                        (communicate-io pid sub-in sub-out in-data (cons block out-rread) tend))
                     (else
                        (kill pid sigkill)
                        (values #f
                           (string->list "read error"))
                        ))))
            ((and (writeable? sub-in) (pair? in-data))
               (lets
                  ((next in-data (lsplit in-data 1024))
                   (block (list->bytevector next))
                   (res (try-write-block sub-in block (sizeb block))))
                  (cond
                     ((eq? res (sizeb block)) ;; block write success
                        ;; if no more data, close port
                        (if (null? in-data)
                           (close-port sub-in))
                        (communicate-io pid sub-in sub-out in-data out-rread tend))
                     ((number? res) ;; partial write
                        (lets
                           ((in-data (append (ldrop next res) in-data)))
                           (communicate-io pid sub-in sub-out in-data out-rread tend)))
                     (else
                        ;; write error / unexpected
                        (kill pid sigkill)
                        (values #f (string->list "write error"))))))
            ((> (time-ms) tend)
               ;; timeout
               (kill pid sigkill)
               (values #f (string->list "timeout")))
            (else
               (sleep 100) ;; ms
               (communicate-io pid sub-in sub-out in-data out-rread tend))))

      ;; call (byte ...) timeout → success? data-bytes|error-string-bytes
      (define (subprocess-eval call input-data timeout-s)
         (let ((proc (start-repl call)))
            (if proc
               (lets ((pid call in out <- proc))
                  (communicate-io pid in out input-data '() (+ (time-ms) (* 1000 timeout-s))))
               (values #f
                  (string->list "failed to start subprocess")))))
))

