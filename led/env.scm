(define-library (led env)

   (import
      (owl toplevel)
      (led system))

   (export
      empty-led-env
      set-status-text
      clear-status-text
      disk-modification-time        ;; what was file modification time when opened / last saved
      update-disk-modification-time
      env-char-width                ;; env rune -> n
      update-env-value              ;; env svar sval -> ok? env'
      tab-width
      *editor-variables*            ;; '((symbol   type    default-value) ...)

      ;; move later
      file-modification-time)

   (begin

      (define empty-env
         (pipe empty
            ; not settable
            (put 'undo null)             ;; undo stack
            (put 'redo null)             ;; redo stack
            (put 'saved null)            ;; undo stack at the point of save
            (put 'subprocess #false)     ;; each buffer can have own (ones)
            (put 'clients null)          ;; threads requesting updates
            ))

      ;; variables settable via :set <name> <value> along with defaults
      (define *editor-variables*
         `((expand-tabs          boolean  #false)
           (tab-width            number   3)
           (timezone-offset      number   1)
           (status-line-template string   "%(%b) %f:%l+%s %[%m] %P%D")
           (autoindent           boolean  #false)
           (encoding             encoding 'utf8)
           (find-regex           regex    ,(string->regex "m/./"))
           (find-path            string   ".")
           (close-if-dirty       boolean  #false)
           (syntax               boolean  #false)
           (status-prelude       string   "[32m")
           ))

      (define (env-cook type string)
         (cond
            ((eq? type 'number)
               (let ((n (string->number string)))
                  (if n
                     (values #t n)
                     (values #f #f))))
            ((eq? type 'string)
               (values #t string))
            ((eq? type 'boolean)
               (cond
                  ((mem equal? '("true" "yes" "on" "1") string)
                     (values #t #t))
                  ((mem equal? '("false" "no" "off" "0") string)
                     (values #t #f))
                  (else
                     (values #f #f))))
            ((eq? type 'encoding)
               (cond
                  ((equal? string "none") (values #t 'none))
                  ((equal? string "utf8") (values #t 'utf8))
                  (else (values #f #f))))
            ((eq? type 'regex)
               (let ((r (string->regex (str "m/" string "/"))))
                  (if r
                     (values #t r)
                     (values #f #f))))
            (else
               (values #f #f))))

      ;; env s s -> env' | #t env' | #f 0 (nonexistent variable) | #f 1 (bad value)
      (define (update-env-value env svar sval)
         (let loop ((nodes *editor-variables*))
            (if (null? nodes)
               (values #f 0)
               (let ((node (car nodes)))
                  (if (equal? (symbol->string (car node)) svar)
                     (lets ((ok? val (env-cook (cadr node) sval)))
                        (if ok?
                           (values #t (put env (car node) val))
                           (values #f 1)))
                     (loop (cdr nodes)))))))

      (define defaults-env
         (fold
            (lambda (env node)
               (put env (car node) (caddr node)))
            empty-env
            *editor-variables*))

      ;; when was the last file modification time, when the contents of
      ;; the file was read to buffer or buffer was written to file?
      ;; 0 if file does not exist
      (define (disk-modification-time env)
         (get env 'disk-modification-time 0))

      (define (update-disk-modification-time env)
         (let ((p (get env 'path #f)))
            (if (string? p)
               (let ((n (file-modification-time p)))
                  (if n
                     (put env 'disk-modification-time n)
                     env)))))

      (define (empty-led-env base-env id path)
         (lets
            ((base-env (del base-env 'path)) ;; forget path, if any
             (env (ff-union empty-env base-env (lambda (a b) a)))
             (env (ff-union env   defaults-env (lambda (a b) a))))
            (if path
               ;; small chance of race between reading modification time and
               ;; reading contents to buffer
               (update-disk-modification-time (put env 'path path))
               env)))

      (define (set-status-text env string)
         (put env 'status-message (string->runes string)))

      (define (clear-status-text env)
         (del env 'status-message))

      (define (tab-width env)
         (get env 'tab-width 3))

      (define (env-char-width env n)
         (cond
            ((lesser? n 32)
               (cond
                  ((eq? (type n) type-fix-)
                     (env-char-width env (- (* n -1) 1)))
                  (else
                     (if (eq? n #\tab)
                        (tab-width env)
                        4)))) ; 0x__
            ((eq? n 127)
               4)
            (else
               1)))
))
