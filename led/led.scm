#!/usr/bin/ol --run

;; led-eval : buff env exp -> buff' env'

(define *expected-owl-version* "0.1.23")

(if (not (equal? *owl-version* *expected-owl-version*))
   (begin
      (print-to stderr "Warning: expected owl version " *expected-owl-version* ", but running in " *owl-version* ". Expect turbulence.")
      (sleep 1000)))

(import
   (prefix (owl parse) get-)
   (only (owl parse) byte-stream->exp-stream fd->exp-stream)
   (only (owl readline) port->readline-byte-stream)
   (owl unicode)
   (only (owl sys) file? directory? kill sigkill)
   (owl terminal)
   (owl proof)
   (owl unicode)
   (only (led system) led-dir->list)
   (owl args)
   (only (led clock) clock-server)
   (only (led log) start-logger log)
   (only (led subprocess) start-repl communicate)
   (only (led parse) parse-runes get-command led-syntax-error-handler)
   (only (led screen) start-screen print-to clear-screen)
   (led buffer)
   (led env)
   (led eval)
   (led input)
   (only (owl syscall) link kill)
   (only (led ui) start-ui ui-yank)
   (led render)
)

(define (bound lo x hi)
  (cond
    ((< x lo) lo)
    ((< hi x) hi)
    (else x)))

;; discard sender
(define (wait-message)
   (let ((envelope (wait-mail)))
      (ref envelope 2)))

;;; Visual operation

(define (next env b w h cx cy)
   (let ((m (check-mail)))
      (if m
         (begin
            ;(update-buffer-view b w h cx cy)
            (values (ref m 1) (ref m 2)))
         (let ((clients (get env 'clients null))
               (update (tuple 'update env b)))
            ;; send buffer and environment update to threads who requested for them
            (fold (λ (_ id) (mail id update)) 0 clients)
            (update-buffer-view env b w h cx cy)
            (let ((m (wait-mail)))
               (values
                  (ref m 1) (ref m 2)))))))

(define (lines-down-offset buff n)
   (let loop ((r (buffer-right buff)) (n n) (steps 0))
      (cond
         ((null? r)
            steps)
         ((eq? (car r) #\newline)
            (if (= n 0)
               (+ steps 1)
               (loop (cdr r) (- n 1) (+ steps 1))))
         (else
            (loop (cdr r) n (+ steps 1))))))

(define (lines-up-offset buff n)
   (let loop ((r (buffer-left buff)) (n n) (steps 0))
      (cond
         ((null? r)
            (* -1 steps))
         ((eq? (car r) #\newline)
            (if (= n 0)
               (* -1 steps)
               (loop (cdr r) (- n 1) (+ steps 1))))
         (else
            (loop (cdr r) n (+ steps 1))))))

(define (write-buffer! env b)
   (let ((bs (buffer->bytes b))
         (p  (get env 'path)))
      (if p
         (if (vector->file (list->vector bs) p)
            (put env 'message "written")
            (put env 'message "write failed"))
         (put env 'message "no path"))))

(define (closing-paren c)
   (cond
      ((eq? c #\() #\))
      ((eq? c #\[) #\])
      ((eq? c #\{) #\})
      (else #f)))

(define (opening-paren c)
   (cond
      ((eq? c #\)) #\()
      ((eq? c #\]) #\[)
      ((eq? c #\}) #\{)
      (else #f)))

(define (paren-hunt l closes len inc dec)
   (cond
      ((null? closes)
         len)
      ((null? l)
         #false)
      ((eq? (car l) (car closes))
         (paren-hunt (cdr l) (cdr closes) (+ len 1) inc dec))
      ((closing-paren (car l)) =>
         (lambda (cp)
            (paren-hunt (cdr l) (cons cp closes) (+ len 1) inc dec)))
      ((opening-paren (car l))
         #false)
      (else
         (paren-hunt (cdr l) closes (+ len 1) inc dec))))

(define (paren-hunter b)
   (b (λ (pos l r len line)
      (if (pair? r)
         (let ((cp (closing-paren (car r))))
            (if cp
               (paren-hunt (cdr r) (list cp) 1 40 41)
               #false))
         #false))))

(define (parent-expression b)
   (b (lambda (pos l r len line)
      (if (null? l)
         (values #false #false)
         (let loop ((l (cdr l)) (r (cons (car l) r)) (d 1))
            (cond
               ((null? r) (values #f #f))
               ((closing-paren (car r)) =>
                  (lambda (cp)
                     (let ((len (paren-hunt (cdr r) (list cp) 1 40 41)))
                        (if (and len (> len d))
                           (values (* -1 d) len)
                           (if (null? l)
                              (values #f #f)
                              (loop (cdr l) (cons (car l) r) (+ d 1)))))))
               ((null? l) (values #false #false))
               (else (loop (cdr l) (cons (car l) r) (+ d 1)))))))))

;; might make sense to require \n.
(define indent-after-newlines
   (string->regex "s/\\n/\\n   /g"))

(define (indent-selection env)
   (lambda (data)
      (ilist #\space #\space #\space
         (indent-after-newlines data))))

(define unindent-after-newlines
   (string->regex "s/\\n   /\n/g"))

(define unindent-start
   (string->regex "s/^   //"))

(define (unindent-selection env)
   (lambda (data)
      (unindent-start (unindent-after-newlines data))))

(define (add-mark env key pos len)
   (log "marking " (list->string (list key)) " as " pos " + " len)
   (let ((marks (get env 'marks)))
      (put env 'marks
         (put marks key (cons pos len)))))

(define (find-mark env key)
   (get (get env 'marks empty) key))

(define (maybe-car x)
   (if (pair? x)
      (car x)
      #f))

(define (next-line-same-pos b)
   (b
      (λ (pos l r len line)
         (lets ((lpos (or (distance-to l #\newline) (length l))) ;; maybe first line
                (rlen (distance-to r #\newline)))
            (if rlen ;; lines ahead
               (lets ((r (drop r (+ rlen 1))) ;; also newline
                      (rlen-next (or (distance-to r #\newline) (length r)))) ;; maybe last line
                  (cond
                     ((eq? rlen-next 0)
                        ;; next line is empty
                        (values (+ rlen 1) lpos))
                     ((<= rlen-next lpos)
                        ;; next line is short, need to move left
                        (values (+ rlen rlen-next)
                           (- lpos rlen-next -1)))
                     (else
                        (values (+ rlen 1 lpos) 0))))
               (values #f #f))))))

(define (prev-line-same-pos b)
   (b
      (λ (pos l r len line)
         (lets ((lpos (distance-to l #\newline)))
            (if lpos
               (lets
                  ((l (drop l (+ lpos 1)))
                   (next-len (or (distance-to l #\newline) (length l))))
                  (cond
                     ((eq? next-len 0)
                        ;; prev line is empty
                        (values (* -1 (+ lpos 1)) lpos))
                     ((<= next-len lpos)
                        (values (* -1 (+ lpos 2)) (- lpos next-len -1)))
                     (else
                        (values (* -1 (+ lpos 1 (- next-len lpos))) 0))))
               (values #f #f))))))

;; choose a nice vertical position for cursor given buffer
(define (nice-cx b w)
   (bound 1
      (+ 1 (buffer-line-offset b))
      w))

(define (first-line lst)
   (foldr
      (lambda (x tl)
         (if (eq? x #\newline)
            null
            (cons x tl)))
      '()
      lst))

(define (show-matching-paren env b)
   (lets
      ((b (seek-delta b -1)) ;; move back inside expression
       (back len (parent-expression b)))
      (if back
         (lets
            ((b (seek-delta b back))
             (b (buffer-selection-delta b len))
             (seln (get-selection b)))
            (set-status-text env
               (list->string
                  (take (first-line seln) 20))))
         (set-status-text env "?"))))

(define (led env mode b cx cy w h)
   ;(print (list 'buffer-window b cx cy w h))
   (lets ((from msg (next env b w h cx cy))
          (op (ref msg 1)))
      (log "led: " mode " <- " msg " from " from)
      (cond
         ((eq? op 'terminal-size)
            (lets ((_ w h msg))
               (for-each
                  (λ (cli) (mail cli msg))
                  (get env 'clients null))
               (clear-screen)
               (update-buffer-view env b w h (min cx w) (min cy h))
               (led env mode b (min cx w) (min cy h) w h)))
         ((eq? op 'status-line)
            (led
               (put env 'status-line msg) ;; #(status-line <bytes> <cursor-x>)
               mode b cx cy w h))
         ((eq? op 'keep-me-posted)
            (led (put env 'clients (cons from (get env 'clients null)))
               mode b cx cy w h))
         ((eq? op 'yank)
            ;; yank message from elsewhere (or me). overwrite yank buffer
            (led (put env 'yank (ref msg 2))
               mode b cx cy w h))
         ((eq? op 'command-entered)
            (lets
               ((runes (ref msg 2)))
               (cond
                  ((eq? (maybe-car runes) #\:)
                     (lets ((buff env (led-eval-runes b env (cdr runes))))
                        (led env 'command   ;; env always there, may have error message
                           (or buff b)      ;; in case command fails
                           (nice-cx buff w) ;; buffer may change from underneath
                           (min cy (buffer-line buff)) ;; ditto
                           w h)))
                  ((eq? (maybe-car runes) #\/)
                     (log "saving last search " (cdr runes))
                     (let ((env (put env 'last-search (cdr runes))))
                        (led env 'command b cx cy w h)))
                  (else
                     (log "wat command " (runes->string runes))
                     (led env 'command b cx cy w h)))))
         ((eq? op 'command-aborted)
            ;; search or colon command was aborted, resume command mode
            (led env 'command b cx cy w h))
         ((eq? op 'command-updated)
            (let ((runes (ref msg 2)))
               (if (eq? (car runes) #\/) ;; this is a search
                  (let ((pos (first-match b (cdr runes))))
                     (if pos
                         (lets ((b (seek-select b pos (length (cdr runes))))
                                (lp (buffer-line-pos b)))
                            (led env mode b (if (>= lp w) 1 (+ lp 1)) 1 w h))
                         (led env mode b cx cy w h)))
                   (led env mode b cx cy w h))))
         ((eq? mode 'command)
            (tuple-case msg
               ((ctrl k)
                  (cond
                     ((eq? k 'f)
                        (led env mode
                           (seek-delta b (lines-down-offset b (max 1 (- h 2))))
                           1 cy w h))
                     ((eq? k 'b)
                        (let ((b (seek-delta b (lines-up-offset b (max 1 (- h 1))))))
                           (led env mode b 1 (min cy (buffer-line b)) w h)))
                     ((eq? k 'l)
                        (mail 'ui (tuple 'clear)) ;; clear screen
                        (led
                           (del env 'status-message)
                           mode b cx cy w h))
                     ((eq? k 'e)
                        (log "cleanup!")
                        (lets ((exp
                                 (tuple 'seq
                                    (tuple 'select 'everything)
                                    (tuple 'call "clean")))
                               (bp ep (led-eval b env exp)))
                           (if bp
                              (led ep mode bp 1 1 w h)
                              (led (set-status-text env "Nothing to clean") mode b cx cy w h))))
                     ((eq? k 'j)
                        (log "formatting")
                        (lets ((exp (tuple 'call "fmt"))
                               (bp ep (led-eval b env exp)))
                           (if bp
                              (led ep mode bp cx cy w h)
                              (led (set-status-text env "nothing happened") mode b cx cy w h))))
                     ((eq? k 'w)
                        (let ((pathp (get env 'path)))
                           (if pathp
                              (lets ((buffp envp (led-eval b env (tuple 'write-buffer pathp))))
                                 (if buffp
                                    (led envp mode buffp cx cy w h)
                                    (led env mode b cx cy w h)))
                              (led
                                 (set-status-text env
                                    "No path yet.")
                                 mode b cx cy w h))))
                     ((eq? k 'x)
                        (let ((proc (get env 'subprocess)))
                           (log " => sending to " proc)
                           (if proc
                              (lets ((resp (communicate proc (get-selection b)))
                                     (b (buffer-after-dot b))
                                     (data (or (utf8-decode (or resp null)) null))
                                     (delta (tuple (buffer-pos b) null data)))
                                 (log " => " data)
                                 (led
                                    (if (null? data)
                                       (set-status-text env "No data received from subprocess.")
                                       (push-undo env delta))
                                    mode
                                    (buffer-append b data)
                                    cx cy w h))
                              (begin
                                 (log " => no subprocess")
                                 (led env mode b cx cy w h)))))
                     (else
                        (led env mode b cx cy w h))))
               ((enter) ;; would treating this as C-m be more or less intuitive?
                  (lets
                     ((bp (if (= 0 (buffer-selection-length b)) (buffer-select-current-word b) b)) ;; fixme - cursor move
                      (cx (nice-cx bp w))
                      (s (list->string (get-selection bp))))
                     (cond
                        ((file? s)
                           (mail 'ui (tuple 'open s))
                           (led env mode bp cx cy w h))
                        ((directory? s)
                           (lets
                              ((fs (or (led-dir->list s) null))
                               (contents
                                  (foldr
                                      (lambda (path tail) (render path (if (null? tail) tail (cons 10 tail))))
                                      null fs))
                               (buff env (led-eval bp env (tuple 'replace contents))))
                              (led env mode buff cx cy w h)))
                        (else
                           (led env mode bp cx cy w h)))))
               ((key x)
                  (cond
                     ((eq? x #\i)
                        (lets
                           ((old (get-selection b)) ;; data to be replaced by insert
                            (env (put env 'insert-start (buffer-pos b)))
                            (env (put env 'insert-original old))
                            (b (buffer-delete b))) ;; remove old selection
                           (led
                              env
                              'insert b cx cy w h)))
                     ((eq? x #\y)
                        (lets ((seln (get-selection b))
                               (env (put env 'yank seln)))
                           (ui-yank seln)
                           (led env mode  b cx cy w h)))
                     ((eq? x #\$)
                        (lets ((nforw (buffer-line-end-pos b))
                               (b (seek-delta b nforw)))
                           (led env mode b
                              (bound 1 (+ cx nforw) w)
                              cy w h)))
                     ((eq? x #\w)
                        (lets ((word-length (buffer-next-word-length b)))
                           (led env mode
                              (buffer-selection-delta b word-length)
                              cx cy w h)))
                     ((eq? x #\n)
                        (let ((s (get env 'last-search)))
                           (log "running last search " s)
                           (if s
                              (lets ((p len (next-match b s)))
                                 (log "next search match is " p)
                                 (if p
                                    (lets ((b (seek-select b p len))
                                           (lp (buffer-line-pos b)))
                                       (led env mode b (if (>= lp w) 1 (+ lp 1)) 1 w h))
                                    (led env mode b cx cy w h)))
                              (led env mode b cx cy w h))))
                     ((eq? x #\m)
                        (lets ((envelope (accept-mail (lambda (x) (eq? (ref (ref x 2) 1) 'key)))))
                           (led
                              (add-mark env (ref (ref envelope 2) 2) (buffer-pos b) (buffer-selection-length b))
                              mode b cx cy w h)))
                     ((eq? x #\')
                        (lets
                           ((envelope (accept-mail (lambda (x) (eq? (ref (ref x 2) 1) 'key))))
                            (from msg envelope)
                            (_ key msg)
                            (location (find-mark env key)))
                           (if location
                              (lets
                                 ((bp (seek-select b (car location) (cdr location))))
                                 (if bp
                                    (led env mode bp
                                       (nice-cx bp w)
                                       1 w h)
                                    (led env mode b cx cy w h)))
                              (led env mode b cx cy w h))))
                     ;((eq? x #\c)
                     ;   (lets ((seln (get-selection b))
                     ;          (buff env (led-eval b env (tuple 'delete))) ;; <- should be merged in delta with insert result
                     ;          (env (put env 'yank seln)))
                     ;      (led
                     ;         env
                     ;         'insert
                     ;         buff
                     ;         cx cy w h)))
                     ((eq? x #\.)
                        (if (= 0 (buffer-selection-length b))
                           (led env mode (select-line b (buffer-line b)) 1 cy w h)
                           (led env mode b cx cy w h)))
                     ((eq? x #\L)
                        (led env mode (buffer-selection-delta b +1) cx cy w h))
                     ((eq? x #\H)
                        (led env mode (buffer-selection-delta b -1) cx cy w h))
                     ((eq? x #\0)
                        (led env mode
                           (seek-start-of-line b)
                           1 cy w h))
                     ((eq? x #\d)
                        (lets ((seln (get-selection b))
                               (env (put env 'yank seln))
                               (action (tuple 'delete))
                               (buff env (led-eval b env (tuple 'delete))))
                           (led env mode buff cx cy w h)))
                     ((eq? x #\p)
                        (lets ((buff env (led-eval b env (tuple 'replace (get env 'yank null)))))
                           (led
                              env
                              mode
                              buff cx cy w h)))
                     ((eq? x #\u)
                        (lets ((b env (led-eval b env (tuple 'undo))))
                           (led env mode b
                              (nice-cx b w)
                              1
                              w h)))
                     ((eq? x #\r)
                        (lets ((b env (led-eval b env (tuple 'redo))))
                           (led env mode b
                              (nice-cx b w)
                              1
                              w h)))
                     ((eq? x #\h) ;; left
                        (let ((bp (seek-delta b -1)))
                           (if (or (not bp) (eq? (buffer-char bp) #\newline))
                              (led env mode b cx cy w h)
                              (led env mode bp (max 1 (- cx (char-width (buffer-char bp)))) cy w h))))
                     ((eq? x #\l) ;; right
                        (lets
                           ((delta (max 1 (buffer-selection-length b)))
                            (bp (seek-delta b delta)))
                           (if (or (not bp)
                                   (eq? (buffer-char b)  #\newline)
                                   )
                              (led env mode b cx cy w h)
                              (led env mode bp (nice-cx bp w)
                                 ;; could also move cy when jumping over selection
                                 cy w h))))
                     ((eq? x #\>) ;; indent, move to led-eval
                        (lets ((buff env (led-eval b env (tuple 'replace ((indent-selection env) (get-selection b))))))
                           (led env mode buff cx cy w h)))
                     ((eq? x #\<) ;; unindent
                        (lets ((buff env (led-eval b env (tuple 'replace ((unindent-selection env) (get-selection b))))))
                           (led env mode buff cx cy w h)))
                     ((eq? x #\j) ;; down
                        (lets ((delta nleft (next-line-same-pos b)))
                           (if delta
                              (led env mode (seek-delta b delta)
                                 (max 1 (- cx nleft))
                                 (min (- h 1) (+ cy 1)) w h)
                              (led env mode b cx cy w h))))
                     ((eq? x #\J) ;; select down
                        (lets
                           ((pos (buffer-pos b))
                            (len (buffer-selection-length b))
                            (bx  (seek b (+ pos len)))
                            (delta nleft (next-line-same-pos bx)))
                           (if delta
                              (led env mode (buffer-selection-delta b delta) cx cy w h)
                              (led env mode b cx cy w h))))
                     ((eq? x #\k) ;; up
                        (lets ((delta nleft (prev-line-same-pos b)))
                           (if delta
                              (led env mode (seek-delta b delta) (- cx nleft) (max 1 (- cy 1)) w h)
                              (led env mode b cx cy w h))))
                     ((eq? x #\%)
                        (lets ((delta (paren-hunter b)))
                           (if (and delta (> delta 0))
                              (led env mode
                                 (buffer-selection-delta (buffer-unselect b) delta)
                                 cx cy w h)
                              (led env mode b cx cy w h))))
                     ((eq? x #\e) ;; parent expression
                        (lets ((back len (parent-expression b))
                               (old-line (buffer-line b)))
                           (if back
                              (lets
                                 ((b (seek-delta b back))
                                  (new-line (buffer-line b)))
                                 (led env mode
                                    (buffer-selection-delta (buffer-unselect b) len)
                                    (nice-cx b w)
                                    (bound 1 (- cy (- old-line new-line)) h)
                                    w h))
                              (led env mode b cx cy w h))))
                     ((eq? x #\N) ;; numbers
                        (led (put env 'line-numbers (not (get env 'line-numbers #false)))
                           mode b cx cy w h))
                     ((eq? x #\Q)
                        (lets ((bp ep (led-eval b env (tuple 'quit #f))))
                           ;; only exits on failure
                           (led
                              (set-status-text env "Buffer has unsaved content.")
                              mode b cx cy w h)))
                     ((eq? x #\W)
                        (lets ((b (buffer-select-current-word b))
                               (seln (get-selection b))
                               (lp (buffer-line-pos b)))
                           (led env mode b
                              (min w (max 1 (+ 1 lp))) cy w h)))
                     ((or (eq? x #\:) (eq? x #\/) (eq? x #\?) (eq? x #\|))
                        (mail (get env 'status-thread-id) (tuple 'start-command x))
                        (led (clear-status-text env) 'enter-command b cx cy w h))
                     (else
                        (led env mode b cx cy w h))))
               ((esc)
                  (led env mode (buffer-unselect b) cx cy w h))
               (else
                  (led env mode b cx cy w h))))
         ((eq? mode 'insert)
            (tuple-case msg
               ((enter)
                  (lets
                     ((i (if (get env 'autoindent) (buffer-line-indent b) null))
                      (b (buffer-append-noselect b (cons #\newline i))))
                     (led env 'insert b
                        (bound 1 (+ (length i) 1) w)
                        (min (- h 1) (+ cy 1)) w h))) ;; -1 for status line
               ((key x)
                  (lets
                     ((b (buffer-append-noselect b (list x))))
                     (if (eq? x 41) ;; closing paren
                        (show-matching-paren env b))
                     (led
                        (if (eq? x 41)
                           (show-matching-paren env b)
                           env)
                        'insert b (min w (+ cx (char-width x))) cy w h)))
               ((refresh)
                  (led env 'insert b cx cy w h))
               ((esc)
                  (lets ((start (get env 'insert-start 0))
                         (end (buffer-pos b))
                         (delta
                            (tuple start
                               (get env 'insert-original null)
                               (buffer-get-range b start end))))
                  (led
                     (push-undo env delta)
                     'command b cx cy w h)))
               ((tab)
                  (lets ((b (buffer-append-noselect b (list #\space #\space #\space))))
                     (led env mode b (min w (+ cx 3)) cy w h)))
               ((ctrl k)
                  (cond
                     ;((eq? k 'c)
                     ;   (led env 'command b cx cy w h))
                     ((eq? k 'w)
                        (let ((pathp (get env 'path)))
                           (if pathp
                              (lets ((buffp envp (led-eval b env (tuple 'write-buffer pathp))))
                                 (if buffp
                                    (led envp mode buffp cx cy w h)
                                    (led env mode b cx cy w h)))
                              (led
                                 (set-status-text env
                                    "No path yet.")
                                 mode b cx cy w h))))
                     (else
                        (led env mode b cx cy w h))))
               ((arrow dir)
                  (cond
                     ((eq? dir 'up)
                        (led env 'insert b cx (max 1 (- cy 1)) w h))
                     ((eq? dir 'down)
                        (led env 'insert b cx (min (+ cy 1) h) w h))
                     ((eq? dir 'left)
                        (led env 'insert b (max 1 (- cx 1)) cy w h))
                     (else
                        (led env 'insert b (min w (+ cx 1)) cy w h))))
               ((backspace)
                  (if (> (buffer-pos b) (get env 'insert-start 0)) ;; no backspacing out of area to be changed
                     (lets
                        ((p (buffer-pos b))
                         (lp (buffer-line-pos b))
                         (b (select b (- p 1) p))
                         (b (buffer-delete b)))
                        (if (eq? lp 0)
                           (led env mode b
                              (min w (+ 1 (buffer-line-pos b)))
                              (max (- cy 1) 1) w h)
                           (led env mode b (max 1 (- cx 1)) cy w h)))
                     (led env mode b cx cy w h)))
               (else is foo
                  (mail 'ui
                     (tuple 'print-to 1 (+ h 1) (str "watx " foo)))
                  (led env 'insert b cx cy w h))))
         ((eq? mode 'enter-command)
            ; colon prefixed command
            ; send keys to the status bar
            (log "Forwarding command " msg " to status thread " (get env 'status-thread-id))
            (mail (get env 'status-thread-id) msg)
            (led env mode b cx cy w h))
         (else
            (led env 'command b cx cy w h)))))

(define (maybe-put ff k v)
   (if v (put ff k v) ff))

(define default-led-opener
   (lambda (path)
      (log "default led opener working on " path)
      (lets
         ((id (or path (list '*scratch*)))
          (status-thread-id (cons id 'status-line)))
         (thread id
            (led
               (put (empty-led-env id path)
                  'status-thread-id status-thread-id)
               'command
               (if (string? path)
                  (or
                     (file-buffer path)
                     (dir-buffer path)
                     (string-buffer (str "")))
                  (string-buffer ""))
               1 1 10 10)) ;; <- ui sends terminal size as first message
         (link id)
         (link
            (thread status-thread-id
               (start-status-line id 80)))
         id)))

(define version-str "led v0.2a")

(define usage-text "led [args] [file-or-directory] ...")

(define command-line-rules
  (cl-rules
    `((help "-h" "--help" comment "show this thing")
      (version "-v" "--version" comment "show program version")
      (log "-L" "--log" has-arg comment "debug log file")
      (repl "-r" "--repl" comment "line-based repl")
      ;(config "-c" "--config" has-arg comment "config file (default $HOME/.ledrc)")
      )))

(define (start-led-threads dict args)
   (cond
      ((get dict 'help)
         (print usage-text)
         (print (format-rules command-line-rules))
         0)
      ((get dict 'version)
         (print version-str)
         0)
      ((get dict 'repl)
         (link (start-logger (get dict 'log)))
         (led-repl (string-buffer "") empty-env))
      (else
         (lets ((input (terminal-input (put empty 'eof-exit? #f)))
                (x y ll (get-terminal-size input)))
            (link (start-logger (get dict 'log)))
            (log "Terminal dimensions " (cons x y))
            (start-screen x y)
            (log "Screen running")
            ;(clear-screen)
            (start-input-terminal (start-ui) ll)
            (log "Input terminal and UI running")
            (thread 'clock (clock-server))
            (mail 'ui (tuple 'add-opener default-led-opener))
            (mail 'ui (tuple 'terminal-size x y))
            (for-each
               (lambda (path)
                  (mail 'ui (tuple 'open path)))
               (if (null? args)
                  (list #false)
                  args))
            (let loop ()
               (let ((mail (wait-mail)))
                  ;(print mail)
                  (log "CRASH " mail)
                  ;(write-bytes stderr (string->bytes (str mail "\n")))
                  ;(halt 1)
                  ;(loop)
                  ))))))

(define (main args)
   (process-arguments (cdr args)
      command-line-rules
      usage-text
      start-led-threads))

main



