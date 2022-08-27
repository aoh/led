
(define-library (led screen)

   (import
      (owl toplevel)
      (owl unicode)
      (owl terminal)
      (led log))

   (export
      start-screen
      start-no-screen
      clear-screen
      print-to)

   (begin

      (define (compare new old p)
         (cond
            ((null? old)
               (values p new))
            ((null? new)
               (values p (list #\space))) ;; clear remaining line
            ((eq? (car new) (car old))
               (if (eq? (car new) 27)
                 (lets ((dp dl (compare (cdr new) (cdr old) (+ p 1))))
                   (if (null? dl)
                      (values dp dl)
                      (values p new)))
                 (compare (cdr new) (cdr old) (+ p 1))))
            (else
               (values p new))))


      (define (screen w h old)
         (lets
            ((msg (wait-mail))
             (from msg msg))
            ;(log "got " (ref msg 1) "-message from " from)
            (tuple-case msg
               ((update-screen new-rows)
                  (let loop ((row 1) (rows new-rows) (old old) (out (cursor-show null)) (shared 0))
                     (cond
                        ((null? rows)
                           (write-bytes stdout (cursor-hide out))
                           ;(write-bytes stdout (set-cursor (render (str (list "shared " shared)) null) 10 10))
                           (screen w h new-rows))
                        ((null? old)
                           (loop row rows '(()) out shared))
                        ;((equal? (car rows) (car old))
                        ;   (loop (+ row 1) (cdr rows) (cdr old) out (+ shared 1)))
                        (else
                           (lets
                              ((offset row-left (compare (car rows) (car old) 1)))
                              (if (null? row-left)
                                 (loop (+ row 1) (cdr rows) (cdr old) out (+ shared offset))
                                 (loop
                                    (+ row 1)
                                    (cdr rows)
                                    (cdr old)
                                    (set-cursor
                                       (clear-line-right
                                          (append (utf8-encode row-left) out))
                                       offset row)
                                    (+ shared offset))))))))
               ((clear)
                  (write-bytes stdout
                     (clear-screen null))
                  (screen w h null))
               ((print-to x y thing)
                  (write-bytes stdout
                     (set-cursor
                        (render thing null)
                        (max x 1) (max y 1)))
                  (screen w h null))
               ((print-bytes-to x y thing)
                  (write-bytes stdout
                     (set-cursor thing x y))
                  (screen w h null))
               ((set-cursor x y)
                  (write-bytes stdout
                     (set-cursor null (max x 1) (max y 1)))
                  (screen w h old))
               ((clear-line-right)
                  (write-bytes stdout
                     (clear-line-right null))
                  (screen w h null))
               ((output-raw bytes)
                  ;; output arbitrary data
                  (write-bytes stdout bytes)
                  (log "outputting raw " bytes)
                  (screen w h null))
               ((ping)
                  ;; used for synchronization
                  (mail from (tuple 'pong))
                  (screen w h null))
               (else
                  (log "ERROR: screen wat " msg " from " from)
                  (screen w h old)))))

      (define (start-screen w h)
         (let ((name 'screen))
            (thread name (screen w h null))
            (link name)
            name))

      (define (message-sink)
         (wait-mail)
         (message-sink))

      (define (start-no-screen)
         (thread 'screen (message-sink))
         'screen)

      ;;
      ;; Functions for talkin to the screen from buffers, via UI
      ;;

      (define (clear-screen)
         (mail 'ui (tuple 'clear)))

      (define (print-to x y . stuff)
         (mail 'ui (tuple 'print-to x y (apply str stuff))))

      ))


