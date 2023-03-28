;;;
;;; Status Line
;;;


(define-library (led status-line)

   (import
      (owl toplevel)
      (led log)
      (owl date)
      (led buffer))

   (export
      start-status-line
      now)

   (begin

      (define (pad-to len lst)
         (let loop ((lst lst) (n (length lst)))
            (if (< n len)
               (loop (cons #\space lst) (+ n 1))
               lst)))

      (define (repeat elem n tl)
         (if (eq? n 0)
            tl
            (repeat elem (- n 1) (cons elem tl))))

      (define (pad-time x)
         (if (< x 10) (str "0" x) x))

      (define (now env)
         (lets
            ((tz-offset (get env 'timezone-offset 0))
             (d m y H M S (date (+ (* tz-offset 3600) (time)))))
            (str d "." m "." y " " (pad-time H) ":" (pad-time M))))

      (define (maybe-drop-space l)
         (if (and (pair? l) (eq? (car l) #\space))
            (cdr l)
            l))

      (define (format-status env buff template width)
         ;(log "Formatting status line " template)
         (lets
            ((data
               (str-foldr
                  (lambda (c tl)
                     (if (eq? c #\%)
                        (cond
                           ((null? tl)
                              (cons c tl))
                           ((eq? (car tl) #\l)
                              (render (buffer-line buff) (cdr tl)))
                           ((eq? (car tl) #\s)
                              (render (buffer-selection-length buff) (cdr tl)))
                           ((eq? (car tl) #\f)
                              (render (get env 'path "*scratch*") (cdr tl)))
                           ((eq? (car tl) #\b) ;; subprocess-binary
                              (lets
                                 ((subprocess (get env 'subprocess #f)))
                                 (if subprocess
                                    (lets ((pid call in out <- subprocess))
                                       (render (car call) (cdr tl)))
                                    (cdr tl))))
                           ((eq? (car tl) #\m)
                              (append (get env 'status-message '()) (cdr tl)))
                           ((eq? (car tl) #\P)
                              (cons 'pad (cdr tl))) ;; <- padding depending on size
                           ((eq? (car tl) #\D) ;; date + time
                              (render (now env) (cdr tl)))
                           ((eq? (car tl) 40)
                              (if (and (pair? (cdr tl)) (eq? (cadr tl) 41))
                                 (maybe-drop-space (cddr tl))
                                 tl))
                           ((eq? (car tl) 91)
                              (if (and (pair? (cdr tl)) (eq? (cadr tl) 93))
                                 (maybe-drop-space (cddr tl))
                                 tl))
                           (else
                              (cons c tl)))
                        (cons c tl)))
                  '()
                  template))
             ;; length without pad
             (len (fold (lambda (n x) (if (eq? x 'pad) n (+ n 1))) 0 data))
             (pad-width (- width len))
             (data
                (foldr
                   (lambda (x tl)
                      (if (eq? x 'pad)
                         (repeat #\space (max pad-width 0) tl)
                         (cons x tl)))
                   '() data))
             (data (take data width)))
            data))

      ;; -> runes
      (define (render-info buff env time width)
         (format-status env buff
            (get env 'status-line-template "?")
            width))


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
                        (mail id (tuple 'status-line (reverse keys) (length keys)))
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
