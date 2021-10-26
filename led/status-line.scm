;;;
;;; Status Line
;;;


(define-library (led status-line)

   (import
      (owl toplevel)
      (led log)
      (led buffer))

   (export
      start-status-line)

   (begin

      (define (pad-to len lst)
         (let loop ((lst lst) (n (length lst)))
            (if (< n len)
               (loop (cons #\space lst) (+ n 1))
               lst)))

      ;; -> runes
      (define (render-info buff env time width)
         (lets
           ((line (buffer-line buff))
            (p  (buffer-pos buff))
            (l  (buffer-selection-length buff))
            (info
                (str
                   (get env 'path "*scratch*")
                   ":"
                   (if (eq? l 0) "" (str "[" l "] "))
                   line
                   " "
                   time)))
            (pad-to width (string->list info))))

      (define (status-line env buff id info w keys c)
         (lets ((envelope (wait-mail))
                (from msg envelope))
            ;(log "status-line got " msg " from " from ", keys " keys)
            (tuple-case msg
               ((update env buff)
                  (if (null? keys)
                     (lets ((info2 (render-info buff env c w)))
                        (if (not (equal? info info2))
                           (mail id
                              (tuple 'status-line info2 1)))
                        (status-line env buff id info2 w keys c))
                     (status-line env buff id info w keys c)))
               ((terminal-size w h)
                  (status-line env buff id info w keys c))
               ((start-command key)
                  (mail id (tuple 'status-line (list key) 1))
                  (status-line env buff id info w (list key) c))
               ((key x)
                  (mail id (tuple 'status-line (reverse (cons x keys)) (+ 1 (length keys))))
                  (mail id (tuple 'command-updated (reverse (cons x keys))))
                  (status-line env buff id info w (cons x keys) c))
               ((clock c)
                  (if (pair? keys)
                     (status-line env buff id info w keys c)
                     (let ((info (render-info buff env c w)))
                        (mail id (tuple 'status-line info 1))
                        (status-line env buff id info w keys c))))
               ((backspace)
                  (if (null? (cdr keys))
                     (begin
                        (mail id (tuple 'command-aborted))
                        (mail id (tuple 'status-line null 1))
                        (status-line env buff id info w null c))
                     (let ((keys (cdr keys)))
                        (mail id (tuple 'status-line (reverse keys) (+ 1 (length keys))))
                        (status-line env buff id info w keys c))))
               ((esc)
                  (mail id (tuple 'command-aborted))
                  (status-line env buff id info w null c))
               ((enter) ;; remove afted owl 0.2.1
                  (mail id (tuple 'command-entered (reverse keys)))
                  (mail id (tuple 'status-line null 1))
                  (status-line env buff id info w null c))
               ((ctrl key)
                  (if (eq? key 'm)
                     (begin
                        (mail id (tuple 'command-entered (reverse keys)))
                        (mail id (tuple 'status-line null 1))
                        (status-line env buff id info w null c))
                     (status-line env buff id info w keys c)))
               (else
                  (status-line env buff id info w keys c)))))

      (define (start-status-line id w)
         (mail id (tuple 'keep-me-posted))
         (mail 'clock 'subscribe)
         (status-line empty empty-buffer id 0 w null null))

))
