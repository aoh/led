#!/usr/bin/ol --run

(define version-str "led v0.2a")

(define usage-text "led [args] [file-or-directory] ...")

(define *expected-owl-version* "0.2.1a")

(if (not (equal? *owl-version* *expected-owl-version*))
   (begin
      (print-to stderr "Warning: expected owl version " *expected-owl-version* ", but running in " *owl-version* ". Expect turbulence.")
      (sleep 1000)))

(import
   (prefix (owl parse) get-)
   (only (owl parse) byte-stream->exp-stream fd->exp-stream)
   (only (owl readline) port->readline-byte-stream)
   (owl unicode)
   (only (owl sys) file? directory? kill sigkill getenv)
   (owl terminal)
   (owl proof)
   (owl unicode)
   (only (led system) led-dir->list)
   (owl args)
   (only (led clock) clock-server)
   (only (led log) start-logger log)
   (only (led subprocess) start-repl communicate)
   (only (led parse) parse-runes get-command led-syntax-error-handler)
   (only (led screen) start-screen start-no-screen print-to clear-screen)
   (only (led find) start-find-thread)
   (led buffer)
   (led env)
   (led eval)
   (led input)
   (led documentation)
   (only (owl syscall) link kill)
   (only (led ui) start-ui ui-put-yank ui-get-yank)
   (led render)
   (led status-line))

(define (bound lo x hi)
  (cond
    ((< x lo) lo)
    ((< hi x) hi)
    (else x)))

;;;
;;; Visual mode actions and utilities
;;;

; movement are to be converted to take old position into account

(define (nice-cx b w)
   (bound 1
      (+ 1 (buffer-line-offset (lambda (x) 1) b))
      w))

(define (env-nice-cx env b w)
   (bound 1
      (+ 1 (buffer-line-offset (lambda (x) (env-char-width env x)) b))
      w))

(define (env-nicer-cx env b cx bp w)
   (lets
      ((wfn (lambda (x) (env-char-width env x)))
       (offset (- (buffer-line-offset wfn b) cx))
       (next (- (buffer-line-offset wfn bp) offset)))
   (bound 1 next w)))

(define (nice-cy b cy h)
   (min cy (buffer-line b)))

;; buffer b at cy changed to bp, screen height h
(define (nicer-cy b cy bp h jump?)
   (lets ((l1 (buffer-line b))
          (l2 (buffer-line bp))
          (delta (- l2 l1))
          (cy (+ cy delta)))
      ;(bound 1 cy (- h 1))
      (cond
         ((< cy 1)
            (if jump?
               (min cy l2)
               1))
         ((< cy h) cy)
         (jump? (max 1 (>> h 1)))
         (else (- h 1)))))


;; visual mode operations that are just wrappers to commands run via led-eval

; just eval and resume with current position
(define (eval-op command)
   (lambda (env mode b cx cy w h led)
      (lets ((buff env (led-eval b env command)))
         (led env mode (or buff b) cx cy w h))))

; eval and likely move cursor, y to top row
(define (moving-eval-op command)
   (lambda (env mode b cx cy w h led)
      (lets ((bp env (led-eval b env command)))
         (if bp
            (led env mode bp
               (env-nicer-cx env b cx bp w)
               (nicer-cy b cy bp h #t)
               w h)
            (led env mode b cx cy w h)))))

;; as above, set text to (say buff env)
(define (moving-verbose-eval-op command say)
   (lambda (env mode b cx cy w h led)
      (lets ((bp env (led-eval b env command)))
         (if bp
            (led
               (set-status-text env (say bp env))
               mode bp
               (env-nicer-cx env b cx bp w)
               (nicer-cy b cy bp h #t) w h)
            (led env mode b cx cy w h)))))

;; ui ops directly corresponding to evaluatable commands

(define ui-indent  (eval-op (tuple 'call "indent")))
(define ui-unindent (eval-op (tuple 'call "unindent")))
(define ui-yank (eval-op (tuple 'copy)))
(define ui-next-match (moving-eval-op (tuple 'next-match #f)))
(define ui-paste (eval-op (tuple 'paste)))
(define ui-undo
   (moving-verbose-eval-op (tuple 'undo)
      (lambda (b e)
            (str "At undo " (length (get e 'undo '())) " / redo " (length (get e 'redo '()))))))

(define ui-redo
   (moving-verbose-eval-op (tuple 'redo)
      (lambda (b e)
            (str "At undo " (length (get e 'undo)) " / redo " (length (get e 'redo))))))

(define ui-clean-buffer
   (moving-eval-op
      (tuple 'seq
         (tuple 'select 'everything)
         (tuple 'call "clean"))))

;; movement based on screen size, so it's not a led-eval op

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

(define (ui-page-up env mode b cx cy w h led)
   (let ((b (seek-delta b (lines-up-offset b (max 1 (- h 1))))))
      (led env mode b 1 (min cy (buffer-line b)) w h)))

(define (ui-page-down env mode b cx cy w h led)
   (led env mode
      (seek-delta b (lines-down-offset b (max 1 (- h 2))))
      1 cy w h))

;; choose a nice vertical position for cursor given buffer

(define (first-line lst)
   (foldr
      (lambda (x tl)
         (if (eq? x #\newline)
            null
            (cons x tl)))
      '()
      lst))

;; show match in status line. alternatively could show the parent expression highlighted
(define (show-matching-paren env b)
   (lets
      ((b (seek-delta b -1)) ;; move back inside expression
       (back len (parent-expression-area b)))
      (if back
         (lets
            ((b (seek-delta b back))
             (b (buffer-selection-delta b len))
             (seln (get-selection b)))
            (set-status-text env
               (list->string
                  (take (first-line seln) 20))))
         (set-status-text env "?"))))



;; UI actions, to be bound to key bindings in env


(define (ui-unbound-key env mode b cx cy w h led)
   (led (set-status-text env "unbound key")
      mode b cx cy w h))

(define (ui-left env mode b cx cy w h led)
   (if (eq? (buffer-selection-length b) 0)
      ;; nothing selected, move
      (lets ((bp env (led-eval b env (tuple 'left))))
         (if (or (not bp) (eq? (buffer-char bp) #\newline))
            (led env mode b cx cy w h)
            (led env mode bp (max 1 (- cx (env-char-width env (buffer-char bp)))) cy w h)))
      ;; something selected - unselect
      (led env mode (buffer-unselect b) cx cy w h)))


;; move down, visual version only because it depends on visual representation of characters

;; -> characters on screen
(define (visual-distance-to-newline env l)
   (let loop ((l l) (n 0))
      (cond
         ((null? l) n)
         ((eq? (car l) #\newline)
            n)
         (else
            (lets ((n (+ n (env-char-width env (car l)))))
               (loop (cdr l) n))))))

;; find position preceding next newline, or null at end of buffer, and count characters (now
;; rendered)

(define (seek-newline l)
   (let loop ((l l) (n 0))
      (cond
         ((null? l)
            (values l n))
         ((eq? (car l) #\newline)
            (values l n))
         (else
            (loop (cdr l) (+ n 1))))))

(define (line-visual-pos env l n)
   (let loop ((l l) (n n) (p 0))
      (cond
         ((null? l) p)
         ((eq? (car l) #\newline) p)
         ((< n 1) p)
         (else
            (loop (cdr l)
               (- n (env-char-width env (car l)))
               (+ p 1))))))

(define (grab-line-rev l)
   (let loop ((l l) (o '()))
      (cond
         ((null? l) o)
         ((eq? (car l) #\newline) o)
         (else (loop (cdr l) (cons (car l) o))))))

(define (reverse-line-visual-pos env l n)
   (lets
      ((l (grab-line-rev l))
       (from-left (line-visual-pos env l n)))
      (- (length l) from-left)))

(define (ui-down-move env b)
   (lets
      ((l (buffer-left b))
       (r (buffer-right b))
       (lvlen (visual-distance-to-newline env l))
       (rp rlen (seek-newline r)))
      (if (null? rp)
         rlen    ;; partial line, go to end
         (+ rlen
            (+ 1  ;; newline
               (line-visual-pos env (cdr rp) lvlen))))))

(define (ui-up-move env b)
   (lets
      ((l (buffer-left b))
       (r (buffer-right b))
       (lvlen (visual-distance-to-newline env l))
       (lp llen (seek-newline l)))
      (if (null? lp) ;; beginning of buffer
         (- 0 llen)
         (* -1
            (+ llen
               (+ 1 (reverse-line-visual-pos env (cdr lp) lvlen)))))))

(define (ui-down env mode b cx cy w h led)
   (let ((bp (seek-delta b (ui-down-move env b))))
      (led env mode bp
         (env-nicer-cx env b cx bp w)
         (nicer-cy b cy bp h #f) w h)))

(define (ui-up env mode b cx cy w h led)
   (let ((bp (seek-delta b (ui-up-move env b))))
      (led env mode bp
         (env-nicer-cx env b cx bp w)
         (nicer-cy b cy bp h #f) w h)))

(define (ui-right-one-char env mode b cx cy w h led)
   (lets ((delta-cx (or (maybe (lambda (x) (env-char-width env x))  (buffer-char b)) 0))
          (bp (seek-delta b 1)))
      (if (or (not bp)
            (eq? (buffer-char b) #\newline))
         ;; no-op if out of line or data
         (led env mode b cx cy w h)
         (led env mode bp
            (if (< (+ cx delta-cx) w)
               (+ cx delta-cx)
               (nice-cx bp w))
            cy w h))))

(define (count-newlines lst)
   (fold
      (lambda (n x)
         (if (eq? x #\newline)
            (+ n 1)
            n))
      0 lst))

;; move by one char if nothing selected, otherwise to end of selection
(define (ui-right env mode b cx cy w h led)
   (let ((n (buffer-selection-length b)))
      (if (eq? n 0)
         ;; move forward regardless of selection
         (ui-right-one-char env mode b cx cy w h led)
         (lets
            ((seln (get-selection b))
             (b (seek-delta b n))
             (cx (nice-cx b w))
             (cy (min (- h 1) (+ cy (count-newlines seln)))))
            (led env mode b cx cy w h)))))

(define (ui-enter-insert-mode env mode b cx cy w h led)
   (lets
      ((old (get-selection b)) ;; data to be replaced by insert
       (env (put env 'insert-start (buffer-pos b)))
       (env (put env 'insert-original old))
       (b (buffer-delete b))) ;; remove old selection
      (led (clear-status-text env) 'insert b cx cy w h)))


(define (ui-select-rest-of-line env mode b cx cy w h led)
   (led env mode
      (select-rest-of-line b #f)
      cx cy w h))

(define (ui-line-end env mode b cx cy w h led)
   (lets ((nforw (buffer-line-end-pos b))
          (b (seek-delta b nforw)))
      (led env mode b
         (bound 1 (+ cx nforw) w)
         cy w h)))

(define (ui-select-word env mode b cx cy w h led)
   (lets ((word-length (buffer-next-word-length b)))
      (led env mode
         (buffer-selection-delta b word-length)
         cx cy w h)))

(define (ui-add-mark env mode b cx cy w h led)
   (lets ((envelope (accept-mail (lambda (x) (eq? (ref (ref x 2) 1) 'key)))))
      (lets
         ((char (ref (ref envelope 2) 2))
          (bp env (led-eval b env (tuple 'add-mark char))))
         (led
            (set-status-text env "marked")
            mode bp cx cy w h))))

(define (ui-go-to-mark env mode b cx cy w h led)
   (lets
      ((envelope (accept-mail (lambda (x) (eq? (ref (ref x 2) 1) 'key))))
       (from msg envelope)
       (_ key msg)
       (bp env (led-eval b env (tuple 'select-mark key))))
      (if bp
         (led env mode bp (nice-cx bp w) 1 w h)
         (led env mode b       cx       cy w h))))

(define (ui-select-current-line env mode b cx cy w h led)
   (if (= 0 (buffer-selection-length b))
      (led env mode (select-line b (buffer-line b)) 1 cy w h)
      (led env mode b cx cy w h)))

(define (ui-select-next-char env mode b cx cy w h led)
   (led env mode (buffer-selection-delta b +1) cx cy w h))

(define (ui-unselect-last-char env mode b cx cy w h led)
   (led env mode (buffer-selection-delta b -1) cx cy w h))

(define (ui-select-everything env mode b cx cy w h led)
   (log "selecting everything")
   (led env mode (select-everything b) 1 1 w h))

;; as with end of line, maybe instead select?
(define (ui-go-to-start-of-line env mode b cx cy w h led)
   (led env mode
      (seek-start-of-line b)
      1 cy w h))

(define (ui-select-start-of-line env mode b cx cy w h led)
   (lets ((pos (buffer-pos b))
          (b (seek-start-of-line b))
          (len (- pos (buffer-pos b))))
      (led env mode
         (set-selection-length b len)
         1 cy w h)))

(define (ui-delete env mode b cx cy w h led)
   (lets ((buff env (led-eval b env (tuple 'delete))))
      (led env mode buff cx cy w h)))

(define (ui-select-down env mode b cx cy w h led)
   (lets
      ((pos (buffer-pos b))
       (len (buffer-selection-length b))
       (bx  (seek b (+ pos len)))
       (move (ui-down-move env bx)))
      (led env mode (buffer-selection-delta b move) cx cy w h)))

(define (ui-select-up env mode b cx cy w h led)
   (lets
      ((len (buffer-selection-length b))
       (move (ui-up-move env (seek-delta b len))))
      (led env mode
         (buffer-selection-delta b move)
         cx cy w h)))

(define (ui-select-parent-expression env mode b cx cy w h led)
   (lets ((old-line (buffer-line b))
          (bp (select-parent-expression b)))
      (if bp
         (let ((new-line (buffer-line bp)))
            (led env mode bp
               (env-nicer-cx env b cx bp w)
               (bound 1 (- cy (- old-line new-line)) h)
               w h))
         (led
            (set-status-text env "cannot select parent")
            mode b cx cy w h))))

(define (ui-toggle-line-numbers env mode b cx cy w h led)
   (led (put env 'line-numbers (not (get env 'line-numbers #false)))
      mode b cx cy w h))

(define (ui-close-buffer-if-saved env mode b cx cy w h led)
   (lets ((bp ep (led-eval b env (tuple 'quit #f))))
      ;; only exits on failure
      (led
         (set-status-text env "Buffer has unsaved content. Force close with :q!")
         mode b cx cy w h)))

(define (ui-start-lex-command env mode b cx cy w h led)
   (mail (get env 'status-thread-id) (tuple 'start-command #\:))
   (led (clear-status-text env) 'enter-command b cx cy w h))

(define (ui-start-search env mode b cx cy w h led)
   (mail (get env 'status-thread-id) (tuple 'start-command #\/))
   (led (clear-status-text env) 'enter-command b cx cy w h))

(define (ui-spell env mode b cx cy w h led)
   ;; select rest of buffer if nothing is selected
   (let ((b (if (= (buffer-selection-length b) 0)
                (set-selection-length b (length (buffer-right b)))
                b)))
      (lets ((exp (tuple 'call "spell"))
             (bp ep (led-eval b env exp)))
         (if bp
            (led ep mode bp
               (nice-cx bp w)
               cy w h)
            (led (set-status-text env "nothing happened") mode b cx cy w h)))))

(define (after-newline lst)
   (cond
      ((null? lst) lst)
      ((eq? (car lst) #\newline) (cdr lst))
      (else (after-newline (cdr lst)))))

;; map format designed mainly to work easily with grep output
(define (find-definition-from data pattern pcs num)
   (cond
      ((null? data)
         (values #f "not found"))
      ((eq? (car data) #\newline)
         (values #f "invalid source map"))
      ((eq? (car data) #\:) ;; between path and number, or number and name
         (if num ;; number read, try to read pattern
            (let loop ((data (cdr data)) (pat pattern))
               (cond
                  ((null? data) ;; fail
                     (find-definition-from null pattern '() #f))
                  ((eq? (car data) #\newline)
                     (if (null? pat) ;; match
                        (let ((path (list->string (reverse pcs))))
                           (log "found match from '" path "', row " num)
                           (values path num))
                        (find-definition-from (cdr data) pattern '() #f)))
                  ((null? pat) ;; fail
                     (find-definition-from (after-newline data) pattern '() #f))
                  ((eq? (car data) (car pat))
                     (loop (cdr data) (cdr pat)))
                  (else
                     (find-definition-from (after-newline data) pattern '() #f))))
           ;; end of patch chars, read number
           (find-definition-from (cdr data) pattern pcs 0)))
     (num ;; reading row number
        (cond
           ((< (car data) 48)
              (values #f "invalid row number"))
           ((> (car data) 57)
              (values #f "invalid row number"))
           (else
              (find-definition-from (cdr data) pattern pcs (+ (- (car data) 48) (* num 10))))))
     (else
        (find-definition-from (cdr data) pattern (cons (car data) pcs) num))))


; -> path-string line-number | #f error-string
(define (find-definition map-path chars)
   (let ((data (file->list map-path)))
      (if data
         (find-definition-from data chars null #f)
         (values #f "no source map"))))

;; read .source.map, or whatever is set via env, end jump to corresponding file+line,
;; usually being the definition of something
(define (ui-jump-via-map env mode b cx cy w h led)
   (lets
      ((bp (if (= 0 (buffer-selection-length b)) (buffer-select-current-word b) b))
       (chars (get-selection bp)))
      (lets ((path line (find-definition (get env 'source-map ".source.map") chars)))
         (if path
            ;; is it me maybe?
            (let ((me (interact 'ui (tuple 'whoami))))
               (if (equal? me path)
                  (lets ((b env (led-eval b env (tuple 'add-mark #\q))))
                     (led
                        (set-status-text env "Found in same file. Return via 'q")
                        mode
                        (select-line b line)
                        1 1 w h))
                  (begin
                     (mail 'ui (tuple 'open path env (list (tuple 'select-line line))))
                     (led
                        (set-status-text env (list->string (get-selection bp)))
                         mode b cx cy w h))))
            (led
               (set-status-text env line)
               mode b cx cy w h)))))

(define (ui-find-occurrences env mode b cx cy w h led)
   (lets
      ((bp (if (= 0 (buffer-selection-length b))
               (buffer-select-current-word b)
               b))
       (chars (get-selection bp)))
      (mail 'ui
         (tuple 'open (cons 'find chars) env '()))
      (led env mode b cx cy w h)))

(define *default-command-mode-key-bindings*
   (ff
      ;#\q ui-memory-info
      #\N ui-toggle-line-numbers
      #\Q ui-close-buffer-if-saved
      #\h ui-left
      #\l ui-right
      #\j ui-down
      #\k ui-up
      #\i ui-enter-insert-mode
      #\y ui-yank
      #\n ui-next-match
      #\$ ui-select-rest-of-line
      #\w ui-select-word
      #\m ui-add-mark
      #\' ui-go-to-mark
      #\. ui-select-current-line
      #\L ui-select-next-char
      #\H ui-unselect-last-char
      #\0 ui-select-start-of-line
      #\d ui-delete
      #\p ui-paste
      #\u ui-undo
      #\r ui-redo
      #\J ui-select-down
      #\K ui-select-up
      #\> ui-indent
      #\< ui-unindent
      ;#\P ui-find-matching-paren
      #\e ui-select-parent-expression
      #\: ui-start-lex-command
      #\/ ui-start-search
      #\% ui-select-everything
      #\s ui-spell
      #\? ui-jump-via-map
      #\F ui-find-occurrences
      ))

(define (ui-repaint env mode b cx cy w h led)
   (mail 'ui (tuple 'clear)) ;; clear screen
   ;(mail 'ui (tuple 'terminal-size 64 12)) ;; <- temporary
   (mail 'ui (tuple 'request-update-terminal-size)) ;; request for an update on terminal size
   (led
      (clear-status-text env)
      mode b cx cy w h))


(define (ui-format-paragraphs env mode b cx cy w h led)
   (lets ((exp (tuple 'call "fmt"))
          (bp ep (led-eval b env exp)))
      (if bp
         (led ep mode bp cx cy w h)
         (led (set-status-text env "nothing happened") mode b cx cy w h))))

(define (ui-save-buffer env mode b cx cy w h led)
   (lets ((buffp envp (led-eval b env (tuple 'write-buffer #f))))
      (if buffp
         (led envp mode buffp cx cy w h)
         (led env mode b cx cy w h))))

(define (ui-send-to-subprocess env mode b cx cy w h led)
   (log "UI send to subprocess")
   (let ((proc (get env 'subprocess)))
      (log " => sending to " proc)
      (if proc
         (lets ((resp (communicate proc (get-selection b)))
                ;(b (buffer-after-dot b))
                ;(data (or (utf8-decode (or resp null)) null))
                ;(delta (tuple (buffer-pos b) null data))
                )
            (log " => " resp)
            (led
               env
               mode
               b cx cy w h))
         (begin
            (log " => no subprocess")
            (led env mode b cx cy w h)))))

(define (part->command cs)
   (cond
      ((null? cs)
         'no-op)
      ((string->number (list->string cs)) =>
         (lambda (n)
            (if (integer? n)
               (tuple 'select-line n)
               #f)))
      (else #f)))

(define (parts->commands lst)
   (foldr
      (lambda (x tl)
         (and tl
            (if (eq? x 'no-op)
               tl
               (cons x tl))))
      null
      (map part->command lst)))

(define (ui-do env mode b cx cy w h led)
   (lets
      ((bp (if (= 0 (buffer-selection-length b)) (buffer-select-current-word b) b)) ;; fixme - cursor move
       (cx (nice-cx bp w))
       (parts (split (partial eq? #\:) (get-selection bp)))
       (s (list->string (car parts)))
       (cmds (parts->commands (cdr parts))))
      (cond
         ((not cmds)
            (led
               (set-status-text env "invalid suffix")
               mode bp cx cy w h))
         ((file? s)
            (mail 'ui (tuple 'open s env cmds))
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


(define *default-command-mode-control-key-bindings*
   (ff
      'f ui-page-down
      'b ui-page-up
      'l ui-repaint
      'e ui-clean-buffer
      'j ui-format-paragraphs
      'w ui-save-buffer
      'x ui-send-to-subprocess
      ;'c ui-send-break
      'm ui-do))

(define (next-event env b w h cx cy)
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

;; convert all actions to (led eval)ed commands later
(define (led env mode b cx cy w h)
   (lets ((from msg (next-event env b w h cx cy))
          (op (ref msg 1)))
      ; (log "led: " mode " <- " msg " from " from)
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
         ((eq? op 'set-status-text)
            (led (set-status-text env (ref msg 2)) mode b cx cy w h))
         ((eq? op 'push)
            ;(log "led got push of " (ref msg 2))
            (lets ((b env (led-eval b env msg)))
               (led env mode b cx cy w h)))
         ((eq? op 'eval)
            (lets ((bp envp (led-eval b env (ref msg 2))))
               (if bp
                  (led envp mode bp
                     (nice-cx bp w)
                     (nicer-cy b cy bp h #t)
                     w h)
                  (led
                     ;(set-status-text env "eval failed")
                     env
                     mode b cx cy w h))))
         ((eq? op 'command-entered)
            (let ((runes (or (ref msg 2) '(#\?))))
               (cond
                  ((eq? (car runes) #\:)
                     (lets ((buff env (led-eval-runes b env (cdr runes))))
                        (led env 'command   ;; env always there, may have error message
                           (or buff b)      ;; in case command fails
                           (nice-cx buff w) ;; buffer may change from underneath
                           (min cy (buffer-line buff)) ;; ditto
                           w h)))
                  ((eq? (car runes) #\/)
                     (lets ((buff env (led-eval b env (tuple 'search-buffer (cdr runes)))))
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
                  (let ((handler (get (get env 'command-mode-control-key-bindings empty) k ui-unbound-key)))
                     (handler env mode b cx cy w h led)))
               ((key x)
                  ((get (get env 'command-mode-key-bindings empty) x ui-unbound-key)
                     env mode b cx cy w h led))
               ((esc)
                  (led
                     (clear-status-text env)
                     mode
                     (buffer-unselect b) cx cy w h))
               ((enter) ;; remove after owl 0.2.1
                  (ui-do env mode b cx cy w h led))
               ((arrow dir)
                  (case dir
                     ((left) (ui-left env mode b cx cy w h led))
                     ((right) (ui-right env mode b cx cy w h led))
                     ((up) (ui-up env mode b cx cy w h led))
                     ((down) (ui-down env mode b cx cy w h led))
                     (else
                        (ui-unbound-key env mode b cx cy w h led))))
               (else
                  (log "Command unhandled key " msg)
                  (led env mode b cx cy w h))))
         ((eq? mode 'insert)
            (tuple-case msg
               ((key x)
                  (lets ((b (buffer-append-noselect b (list x))))
                     (led
                        (if (eq? x 41)
                           (show-matching-paren env b)
                           env)
                        'insert b (min w (+ cx (env-char-width env x))) cy w h)))
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
               ((ctrl k)
                  (cond
                     ;((eq? k 'c)
                     ;   (led env 'command b cx cy w h))
                     ((eq? k 'm) ;; enter
                        (lets
                           ((i (if (get env 'autoindent) (buffer-line-indent b) null))
                            (b (buffer-append-noselect b (cons #\newline i))))
                           (led env 'insert b
                              (env-nice-cx env b w)
                              (min (- h 1) (+ cy 1)) w h))) ;; -1 for status line
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
               ((tab) ;; remove after owl 0.2.1
                  (let ((n (tab-width env)))
                     (log "tab. width is " n ", expand is " (get env 'expand-tabs #t))
                     (if (get env 'expand-tabs #t)
                        (lets ((b (buffer-append-noselect b (repeat-char #\space n '()))))
                           (led env mode b (min w (+ cx n)) cy w h))
                        (lets ((b (buffer-append-noselect b (list #\tab))))
                           (led env mode b (min w (+ cx n)) cy w h)))))
               ((enter)
                  (lets
                     ((i (if (get env 'autoindent) (buffer-line-indent b) null))
                      (b (buffer-append-noselect b (cons #\newline i))))
                     (led env 'insert b
                        (env-nice-cx env b w)
                        (min (- h 1) (+ cy 1)) w h))) ;; -1 for status line
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
                         (bp b)
                         (b (buffer-delete b)))
                        (if (eq? lp 0)
                           (led env mode b
                              (min w (+ 1 (buffer-line-pos b)))
                              (max (- cy 1) 1) w h)
                           (led env mode b (max 1 (- cx (env-char-width env (buffer-char bp)))) cy w h)))
                     (led env mode b cx cy w h)))
               (else is foo
                  (mail 'ui
                     (tuple 'print-to 1 (+ h 1) (str "watx " foo)))
                  (led env 'insert b cx cy w h))))
         ((eq? mode 'enter-command)
            ;; third mode: command is being typed and handled by the status bar thread
            ;(log "Forwarding command " msg " to status thread " (get env 'status-thread-id))
            (mail (get env 'status-thread-id) msg)
            (led env mode b cx cy w h))
         (else
            (led env 'command b cx cy w h)))))


(define default-led-opener
   (lambda (path env)
      (log "UI: opening buffer " path)
      (lets
         ((id (or path (list '*scratch*)))
          (status-thread-id (cons id 'status-line))
          (env buffer
             (cond
                  ((string? path)
                     (lets ((e b (file-buffer env path)))
                        (if b
                           (values e b)
                           (lets ((e b (dir-buffer env path)))
                              (if b
                                 (values e b)
                                 (string-buffer env ""))))))
                  ((pair? path)
                     (cond
                        ((eq? (car path) 'help)
                           (log "Opening help buffer")
                           (help-buffer env path))
                        ((eq? (car path) 'find)
                           (string-buffer env (str "Finding: " (list->string (cdr path)) "\n")))
                        (else
                           (log "Unknown list")
                           (values #f #f))))
                  (else
                     (string-buffer env "")))))
         (thread id
            (led
               (pipe env
                  (empty-led-env id (if (string? path) path #f))
                  (put 'status-thread-id status-thread-id))
               'command
               buffer 1 1 10 10)) ;; <- ui sends terminal size as first message
         (link id)
         (link status-thread-id)
         (thread status-thread-id
            (start-status-line id 8))
         (if (and (list? path) (eq? (car path) 'find))
            (link (start-find-thread env (cdr path) id)))
         id)))




(define command-line-rules
  (cl-rules
    `((help "-h" "--help" comment "show this thing")
      (version "-v" "--version" comment "show program version")
      (log "-L" "--log" has-arg comment "debug log file")
      (repl "-r" "--repl" comment "line-based repl")
      ;(config "-c" "--config" has-arg comment "config file (default $HOME/.ledrc)")
      )))


(define *default-environment*
   (pipe empty
      (put 'command-mode-key-bindings *default-command-mode-key-bindings*)
      (put 'command-mode-control-key-bindings
         *default-command-mode-control-key-bindings*)
      ))

(import (only (led parse) parse-runes))

(define (load-settings-from initial-env data)
   (let ((cmds (parse-runes data)))
      (log "PARSED " cmds)
      (if cmds
         (lets
             ((b env (string-buffer initial-env ""))
              (b env (led-eval b initial-env cmds)))
            (if b
               (clear-status-text env)
               #false))
         #false)))

(define (load-user-settings env)
   (let ((home (getenv "HOME")))
      (if home
         (lets
            ((settings-path (str home "/.ledrc"))
             (data (file->list settings-path)))
            (log "loading user settings from " settings-path)
            (log "data is " data)
            (if data
               (load-settings-from env data)
               env))
         env)))

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
         (start-no-screen)
         (start-ui)
         (lets
            ((env buffer
               (string-buffer
                  (empty-led-env *default-environment* #f #f)
                  "")))
            (led-repl buffer env)))
      (else
         (lets ((input (terminal-input (put empty 'eof-exit? #f)))
                (x y ll (get-terminal-size input))
                (_ (link (start-logger (get dict 'log))))
                (env (load-user-settings *default-environment*)))
            (if env
               (begin
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
                        (log "opening " path)
                        (mail 'ui
                           (tuple 'open path env null)))
                     (if (null? args)
                        (list #false)
                        args))
                  (if (> (length args) 1)
                     (mail 'ui (tuple 'home))) ;; move to first buffer
                  (let loop ()
                     (let ((mail (wait-mail)))
                        ;(print mail)
                        (log "CRASH " mail)
                        ;(write-bytes stderr (string->bytes (str mail "\n")))
                        ;(halt 1)
                        ;(loop)
                        )))
               (begin
                  (print-to stderr "Failed to load $HOME/.ledrc")
                  1))))))

(import (only (owl sys) catch-signals sigpipe))
(import (only (owl thread) set-signal-action signal-handler/ignore))

(define (main args)
   ;; catch SIGPIPE from writing to closed/crashed subprocesses
   (catch-signals (list sigpipe)) ;; don't fail via a signal if subprocess is gone
   ;; ignore it
   (set-signal-action signal-handler/ignore)
   (process-arguments (cdr args)
      command-line-rules
      usage-text
      start-led-threads))

main

