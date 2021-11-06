;;;
;;; EVAL env buffer expression -> env' buffer'
;;;

(define-library (led eval)

   (import
      (owl toplevel)
      (owl parse)
      (owl readline)
      (only (owl sys) isatty)
      (led log)
      (led buffer)
      (led extra)
      (led parse)
      (led env)
      (only (led system) led-path->runes)
      (only (led subprocess) start-repl)
      (only (led render) render-content)
      (only (led ui) ui-get-yank ui-put-yank))

   (export
      led-eval   ;; buff env exp -> buff' env' | #f env' (with error message)
      led-eval-runes
      led-repl   ;; maybe move elsewhere later

      push-undo
      pop-undo

      dirty?    ;; env -> bool, is the buffer dirty (has unsaved changes)
      )

   (begin

      ;; env name -> (data -> data)
      (define (led-eval-rator env name)
         (find-extra name))

      (define (led-eval-position buff env exp)
         (if (number? exp)
            exp
            (begin
               (log "unknown position " exp)
               #f)))

      (define (push-undo env delta)
         (pipe env
            (put 'redo null) ;; destroy future of alternative past
            (put 'undo (cons delta (get env 'undo null)))))

      ;; use the undo stack as a marker of last saved status
      (define (mark-saved env)
         (put env 'saved (get env 'undo null)))

      (define (dirty? env)
         (not (equal?
            (get env 'undo null)
            (get env 'saved null))))

      (define (pop-undo env)
         (let ((stack (get env 'undo null)))
            (if (null? stack)
               (values env #false)
               (values
                  (pipe env
                     (put 'undo (cdr stack))
                     (put 'redo (cons (car stack) (get env 'redo null))))
                  (car stack)))))

      (define (pop-redo env)
         (let ((stack (get env 'redo null)))
            (if (null? stack)
               (values env #false)
               (values
                  (pipe env
                     (put 'redo (cdr stack))
                     (put 'undo (cons (car stack) (get env 'undo null))))
                  (car stack)))))

      (define (string->boolean s)
         (cond
            ((equal? s "true") #t)
            ((equal? s "false") #f)
            (else 'no)))

      (define (any->list x)
         (cond
            ((string? x) (string->list x))
            ((list? x) x)
            (else
               (any->list (str x)))))

      (define (led-eval buff env exp)
         (log "led-eval " exp) ;; can be large
         (tuple-case exp
            ((left) ;; usually keyboard h, move left by one character on current line
               ;; convert to match ui-left
               (lets ((bp (seek-delta buff -1)))
                  (if (and bp (not (eq? #\newline (buffer-char bp))))
                     (values bp env)
                     (values buff env))))
            ((write-buffer target)
               (cond
                  ((and (not target) (not (get env 'path)))
                     (values buff
                        (set-status-text env
                           "No path associated with buffer yet. use :w <path>")))
                  ((and (not target)
                        (> (file-modification-time (get env 'path))
                           (disk-modification-time env)))
                     (log "buffer modification " (file-modification-time (get env 'path)))
                     (log "disk modification   " (disk-modification-time env))
                     (values buff
                        (set-status-text env
                           (str
                              "File on disk is newer. Overwrite with :w " (get env 'path)))))
                  (else
                     (lets ((path (or target (get env 'path)))
                            (fd (and path (open-output-file path))))
                        (log "Writing buffer to " path)
                        (cond
                           (fd
                              (let ((data (buffer->bytes buff)))
                                 (if (write-bytes fd data)
                                    (begin
                                       (close-port fd)
                                       (values buff
                                          (set-status-text
                                             (pipe env
                                                (mark-saved)
                                                (put 'path path)
                                                (update-disk-modification-time))
                                             (str "Wrote " (length data) "b to " path "."))))
                                    (values #f
                                       (set-status-text env (str "Failed to write to " path "."))))))
                           (else
                              (values #f (set-status-text env "Failed to open file for writing"))))))))
            ((read path)
               (let ((data (led-path->runes path)))
                  (if data
                     (led-eval buff env (tuple 'replace data))
                     (values
                        #f
                        (set-status-text env (str "Cannot read '" path "'"))))))
            ((new-buffer path)
               (mail 'ui (tuple 'open path (del env 'path) null))
               (values buff env))
            ((append text)
               (lets
                  ((buff (seek-delta buff (buffer-selection-length buff)))
                   (buff (buffer-unselect buff)))
                  (led-eval buff env (tuple 'insert text))))
            ((select-line n)
               (values
                  (select-line buff n)
                  env))
            ((paste)
               ;; note: does not currently work in lex mode, since there is nowhere to yank
               (let ((data (ui-get-yank)))
                  (if data
                     (led-eval buff env (tuple 'replace data))
                     (values #f
                        (set-status-text env "Nothing copied")))))
            ((copy)
               (ui-put-yank (get-selection buff))
               (values buff env))
            ((print)
               (print (list->string (render-content env (get-selection buff))))
               (values buff env))
            ((subprocess call)
               (cond
                  ((get env 'subprocess)
                     (values buff
                        (set-status-text env
                           (str (get env 'subprocess)))))
                  ((null? call)
                     (values buff
                        (set-status-text env "no subprocess")))
                  (else
                     (let ((info (start-repl call)))
                        (log " => call " call)
                        (log " => subprocess " info)
                        (if info
                           (values buff
                              (set-status-text
                                 (put env 'subprocess info)
                                 (str "Started " (ref info 2) " with PID " (ref info 1) ".")))
                           (values buff
                              (set-status-text env "no")))))))
            ((extend-selection movement)
               (lets ((buffp envp (led-eval buff env movement)))
                  (if buffp
                     (values
                        (merge-selections buff buffp)
                        envp)
                     (values #f #f))))
            ((replace new)
               (lets ((delta (tuple (buffer-pos buff) (get-selection buff) new)))
                  (values
                     (apply-delta buff delta)
                     (push-undo env delta))))
            ((resize w h)
               (mail 'ui (tuple 'terminal-size w h))
               (values buff env))
            ((delete)
               (ui-put-yank (get-selection buff))
               (led-eval buff env (tuple 'replace null)))
            ((undo)
               (lets ((env delta (pop-undo env)))
                  (if delta
                     (values (unapply-delta buff delta) env)
                     (values buff
                        (set-status-text env "nothing to undo")))))
            ((redo)
               (lets ((env delta (pop-redo env)))
                  (if delta
                     (values (apply-delta buff delta) env)
                     (values buff
                        (set-status-text env "nothing to redo")))))
            ((print)
               (let ((data (get-selection buff)))
                  (print (runes->string data))
                  (values buff env)))
            ((select what)
               (cond
                  ((eq? what 'everything)
                     (values (select-everything buff) env))
                  ((eq? what 'current-line)
                     (values (select-line buff (buffer-line buff)) env))
                  ((eq? what 'end-of-file) ;; select empty string at end of file
                     (values (select-end-of-file buff) env))
                  (else
                     (values #f #f))))
            ((subsection from len)
               (if (and (>= from 0)
                        (>= len 0)
                        (<= (+ from len)
                            (buffer-selection-length buff)))
                  (let ((pos (buffer-pos buff)))
                     (values
                        (select buff (+ pos from) (+ pos (+ from len)))
                        env))
                  (values buff env)))
            ((seq a b)
               (lets ((bp ep (led-eval buff env a)))
                  (if bp
                     (led-eval bp ep b)
                     (values #f #f))))
            ((quit force?)
               (if (and (dirty? env) (not force?))
                  (values #f #f)
                  (begin
                      (kill (get env 'status-thread 'id))
                      (mail 'ui (tuple 'buffer-closed))
                      (exit-thread 42))))
            ((apply func)
               (lets ((old (get-selection buff))
                      (new (func env old)))
                  ;(log "apply returned " new) ;; can be large
                  (if (equal? old new)
                     (values #f env) ;; nothing to do
                     (if (tuple? new)
                        ;; it's an action already
                        (led-eval buff env new)
                        (led-eval buff env (tuple 'replace new))))))
            ((call name)
               (let ((func (led-eval-rator env name)))
                  (if func
                     (led-eval buff env (tuple 'apply func))
                     (values #f
                        (set-status-text env "no such extension")))))
            ((search-buffer str) ;; str or char list
               (lets
                  ((cs (if (string? str) (string->list str) str))
                   (env (put env 'last-search cs)))
                  (if (equal? (get-selection buff) cs)
                     (values buff env)
                     (led-eval buff env (tuple 'next-match #t)))))
            ((next-match match-start?)
               (let ((s (get env 'last-search)))
                  (if s
                     (lets ((p len (next-match buff s match-start?)))
                        (if p
                           (values (seek-select buff p len) env)
                           (values #f
                              (set-status-text env "no more matches"))))
                     (values #f
                        (set-status-text env "no search")))))
            ((help subject)
               (mail 'ui (tuple 'open (list 'help subject) (del env 'path) null))
               (values buff env))
            ((add-mark key)
               (let ((pos (buffer-pos buff))
                     (len (buffer-selection-length buff))
                     (marks (get env 'marks)))
                  (values buff
                     (put env 'marks
                        (put marks key (cons pos len))))))
            ((select-mark key)
               (lets ((poss (get (get env 'marks empty) key '(-1 . 0))))
                  (log "poss is " poss)
                  (values
                     (seek-select buff (car poss) (cdr poss))
                     env)))
            ((select-parent)
               (values
                  (select-parent-expression buff)
                  env))
            ((indent)
               (led-eval buff env (tuple 'call "indent")))
            ((unindent)
               (led-eval buff env (tuple 'call "unindent")))
            ((set str-var str-val)
               (cond
                  ((equal? str-var "tab-width")
                     (let ((n (string->number str-val)))
                        (if (and n (> n 0) (integer? n))
                           (values buff
                              (put env 'tab-width n))
                           (values #f
                              (set-status-text env "invalid number")))))
                  ((equal? str-var "expand-tabs?")
                     (let ((v (string->boolean str-val)))
                        (if (boolean? v)
                           (values buff
                              (put env 'expand-tabs? v))
                           (values #f
                              (set-status-text env "invalid boolean value")))))
                  ((equal? str-var "timezone-offset")
                     (values buff (put env 'timezone-offset str-var)))
                  ((equal? str-var "status-line-template")
                     (values buff (put env 'status-line-template str-val)))
                  (else
                     (values #f
                        (set-status-text env "Unknown variable. See :help")))))
            ((push data)
               (lets
                  ((p (buffer-pos buff))
                   (slen (buffer-selection-length buff))
                   (move (length (buffer-right buff)))
                   (buff (buffer-unselect buff))
                   (buff (seek-delta buff move))
                   (buff env (led-eval buff env (tuple 'replace (any->list data))))
                   (buff (seek-delta buff (- 0 move)))
                   (buff (set-selection-length buff slen)))
                  (values buff env)))
            (else
               (log (list 'wat-eval exp))
               (values #f env))))

      ;;; Line-based operation, move elsewhere later

      (define (led-eval-runes buff env s)
         ;(log "eval: parse " s)
         (let ((exp (parse-runes s)))
            ;(log "eval " s " -> " exp)
            (if exp
               (lets ((buffp envp (led-eval buff env exp)))
                  (if buffp
                     (values buffp envp)
                     (begin
                        (set-status-text env "no")
                        (log "eval: no")
                        (values buff env))))
               (values buff
                  (set-status-text env "syntax error")))))

      (define (prompt env)
         (let ((p (get env 'prompt ": ")))
            (if p
               (display p))))

      (define (led-repl buff env)
         (prompt env)
         (lfold
            (λ (state exp)
               (print exp)
               (lets ((buff env <- state)
                      (buff env (led-eval buff env exp)))
                  (if buff
                     (begin
                        ;(print (buffer->string buff))
                        ;(print-buffer buff)
                        (prompt env)
                        (prod buff env))
                     (begin
                        (print "?")
                        (prompt env)
                        state))))
            (prod buff env)
            (byte-stream->exp-stream
               (if (isatty stdin)
                  (port->readline-byte-stream stdin)
                  (port->byte-stream stdin))
               get-command
               led-syntax-error-handler)))

))

