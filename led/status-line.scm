;;;
;;; Status Line
;;;


(define-library (led status-line)

   (import
      (owl toplevel)
      (led log)
      (owl date)
      (owl lcd)
      (only (owl unicode) utf8-encode)
      (led buffer))

   (export
      start-status-line
      now)

   (begin

      ;;;
      ;;; Custom Readline (librarize later)
      ;;;

      (define (history-push hist line)
         (lets ((up down <- hist)
                (up (append (reverse down) up)))
            (if (and (pair? up) (equal? line (car up)))
               (prod up null)
               (prod (cons line up) null))))

      ;; id is used to track which readline history is being used
      ;; it's the character used for prompt. e.g. : or /, and likely later |
      ;; readline state: [id rleft right hist]
      ;; hist = [up down]

      (define (readline id rleft right history)
         (prod id rleft right history))

      (define (readline-empty key)
         (readline key null null
            (prod null null)))

      (define *readline-empty*
         (readline-empty #\x))

      ;; rl char → rl'
      (define (readline-put rl char)
         (lets ((id rleft right hist <- rl))
            (readline id (cons char rleft) right hist)))

      ;; rl → n, cursor offset starting from 0
      (define (readline-cursor rl)
         (lets ((id rleft right hist <- rl))
            (length rleft)))

      (define (readline-up rl)
         (lets
            ((id rleft right hist <- rl)
             (this (append (reverse rleft) right))
             (up down <- hist))
            (if (null? up)
               rl
               (readline id
                  (reverse (car up))
                  null
                  (prod
                     (cdr up)
                     (cons this down))))))

      (define (readline-down rl)
         (lets
            ((id rleft right hist <- rl)
             (this (append (reverse rleft) right))
             (up down <- hist))
            (if (null? down)
               rl
               (readline id
                  (reverse (car down))
                  null
                  (prod
                     (cons this up)
                     (cdr down))))))

      (define (readline-left rl)
         (lets ((id rleft right hist <- rl))
            (if (null? rleft)
               rl
               (readline id (cdr rleft) (cons (car rleft) right) hist))))

      (define (readline-right rl)
         (lets ((id rleft right hist <- rl))
            (if (null? right)
               rl
               (readline id (cons (car right) rleft) (cdr right) hist))))

      ;; rl → id
      (define (readline-id rl)
         (lets ((id rleft right hist <- rl))
            id))

      ;; rl → rl' | #false if was empty
      (define (readline-backspace rl)
         (lets ((id rleft right hist <- rl))
            (cond
               ((null? rleft)
                  (if (null? right)
                     #false
                     rl))
               (else
                  (readline id (cdr rleft) right hist)))))

      ;; rl → line
      (define (readline-line rl)
         (lets ((id rleft right hist <- rl)
                (line (append (reverse rleft) right)))
            line))

      ;; rl → rl' line, stores history, clears state
      ;; possibly remove this
      (define (readline-flush rl)
         (lets ((id rleft right hist <- rl)
                (line (append (reverse rleft) right)))
            (values
               (readline id null null
                  (history-push hist line))
               line)))

      (define (readline-render rl width)
         ;; no scroll yet
         (lets ((id rleft right hist <- rl))
            (values
               (append (reverse rleft) right)
               (length rleft))))

      ;; add line to end of history, if new
      (define (readline-history-push rl line)
         (lets ((id rleft right hist <- rl))
            (readline id rleft right
               (history-push hist line))))

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

      ;; expand the message (rule list, if any) in data by taking at most max-message-size
      ;; runes of the prefix
      (define (expand-message data max-message-size)
         (cond
            ((null? data) data)
            ((pair? (car data))
               (append
                  (take (car data) max-message-size)
                  (cdr data)))
            (else
               (cons (car data)
                  (expand-message (cdr data) max-message-size)))))


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
                              (append
                                 (string->list (get env 'path "*scratch*"))
                                 (cdr tl)))
                           ((eq? (car tl) #\b) ;; subprocess-binary
                              (lets
                                 ((subprocess (get env 'subprocess #f)))
                                 (if subprocess
                                    (lets ((pid call in out <- subprocess))
                                       (render (car call) (cdr tl)))
                                    (cdr tl))))
                           ((eq? (car tl) #\m)
                              ;; leave status message as list (if any) for expansion
                              (let ((msg (get env 'status-message '())))
                                 (if (null? msg)
                                    (cdr tl)
                                    (cons msg (cdr tl)))))
                           ((eq? (car tl) #\P)
                              (cons 'pad (cdr tl))) ;; <- padding depending on size
                           ((eq? (car tl) #\D) ;; date + time
                              (render (now env) (cdr tl)))
                           ((eq? (car tl) 40)
                              ;; parenthesis, which autoremoves if empty
                              (if (and (pair? (cdr tl)) (eq? (cadr tl) 41))
                                 (maybe-drop-space (cddr tl))
                                 tl))
                           ((eq? (car tl) 91)
                              ;; brackets, autoremoved if empty
                              (if (and (pair? (cdr tl)) (eq? (cadr tl) 93))
                                 (maybe-drop-space (cddr tl))
                                 tl))
                           (else
                              (cons c tl)))
                        (cons c tl)))
                  '()
                  template))
             ;; length without pad or message
             (len (fold (lambda (n x) (if (or (eq? x 'pad) (pair? x))  n (+ n 1))) 0 data))
             (max-message-size (- width len))
             (data (expand-message data max-message-size))
             (len (fold (lambda (n x) (if (eq? x 'pad)  n (+ n 1))) 0 data))
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

      (define (find-history his key)
         (let ((val (get his key)))
            (if val
               val
               (readline-empty key))))

      (define (update-status-line id rl)
         (let ((line (readline-line rl)))
            (mail id
               (tuple 'command-updated
                  (cons (readline-id rl) line)))
            (mail id
               (tuple 'status-line
                  (cons (readline-id rl) line)
                  (+ 1 (readline-cursor rl))))))

      ; rl = current readline state
      ; his = ff of key → readline state, for different prompts
      (define (status-line env buff id info w keys c rl his)
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
                        (status-line env buff id info2 w keys c rl his))
                     (status-line env buff id info w keys c rl his)))
               ((terminal-size w h)
                  (status-line env buff id info w keys c rl his))
               ((start-command key)
                  (mail id (tuple 'status-line (list key) 1))
                  (status-line env buff id info w (list key) c
                     (find-history his key)
                     his))
               ((key x)
                  (lets
                     ((rl (readline-put rl x))
                      (line (readline-line rl)))
                     (update-status-line id rl)
                     (status-line env buff id info w (cons x keys) c
                        rl his)))
               ((clock c)
                  (if (pair? keys)
                     (status-line env buff id info w keys c rl his)
                     (let ((info (render-info buff env c w)))
                        (mail id (tuple 'status-line info 1))
                        (status-line env buff id info w keys c rl his))))
               ((backspace)
                  (let ((rl (readline-backspace rl)))
                     (if rl
                        (begin
                           (update-status-line id rl)
                           (status-line env buff id info w keys c rl his))
                        (begin
                           (mail id (tuple 'command-aborted))
                           (mail id (tuple 'status-line null 1))
                           (status-line env buff id info w null c #f his)))))
               ((esc)
                  (mail id (tuple 'command-aborted))
                  (mail id (tuple 'status-line null 1))
                  (status-line env buff id info w null c #f his))
               ((enter)
                  (lets ((rl line (readline-flush rl))
                         (rid (readline-id rl))
                         (full (cons rid line)))
                     (mail id (tuple 'command-entered full))
                     (mail id (tuple 'status-line null 1))
                     (status-line env buff id info w null c #f
                        ; alternative: push just the line and keep unmodified history
                        (put his (readline-id rl)
                           (readline-history-push (get his (readline-id rl) rl) line)))))
               ((ctrl key)
                  (log "control key " key)
                  (status-line env buff id info w keys c rl his))
               ((arrow dir)
                  (cond
                     ((eq? dir 'up)
                        (lets ((rl (readline-up rl)))
                           (update-status-line id rl)
                           (status-line env buff id info w keys c rl his)))
                     ((eq? dir 'down)
                        (lets ((rl (readline-down rl)))
                           (update-status-line id rl)
                           (status-line env buff id info w keys c rl his)))
                     ((eq? dir 'left)
                        (lets ((rl (readline-left rl)))
                           (update-status-line id rl)
                           (status-line env buff id info w keys c rl his)))
                     ((eq? dir 'right)
                        (lets ((rl (readline-right rl)))
                           (update-status-line id rl)
                           (status-line env buff id info w keys c rl his)))
                     (else
                        (status-line env buff id info w keys c rl his))))
               (else
                  (log "unhandled msg " msg)
                  (status-line env buff id info w keys c rl his)))))

      (define (start-status-line id w)
         ;; request buffer changes from parent
         (mail id (tuple 'keep-me-posted))
         ;; request time
         (mail 'clock 'subscribe)
         (status-line empty empty-buffer id 0 w null null #f empty))

))
