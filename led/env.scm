(define-library (led env)

   (import
      (owl toplevel)
      (led system))

   (export
      empty-led-env
      set-status-text
      clear-status-text
      disk-modification-time     ;; what was file modification time when opened / last saved
      update-disk-modification-time
      env-char-width  ;; env rune -> n
      update-env-value        ;; env svar sval -> env' | #f
      tab-width
      *editor-variables*        ;; '((symbol   type    default-value) ...)

      ;; move later
      file-modification-time)

   (begin

      (define empty-env
         (pipe empty
            ; not settable
            (put 'undo null)
            (put 'redo null)
            (put 'subprocess #false) ;; each buffer can have own (ones)
            ))

      ;; variables settable via :set <name> <value> along with defaults
      (define *editor-variables*
         '((expand-tabs          boolean  #false)
           (tab-width            number   3)
           (timezone-offset      number   1)
           (status-line-template string   "%(%b) %f:%l+%s %[%m] %P%D")
           (autoindent           boolean  #false)))

      (define (string->boolean s)
         (cond
            ((equal? s "true") #t)
            ((equal? s "false") #f)
            ((equal? s "on") #t)
            ((equal? s "off") #f)
            ((equal? s "1") #t)
            ((equal? s "0") #f)
            (else 'no)))

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
            (else
               (values #f #f))))

      ;; env s s -> env' | #false (cannot be empty)
      (define (update-env-value env svar sval)
         (fold
            (lambda (env node)
               (if (equal? (symbol->string (car node)) svar)
                  (lets ((ok? val (env-cook (cadr node) sval)))
                     (if ok?
                        (put env (car node) val)
                        #f))
                  env))
            env *editor-variables*))

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
            ((env (ff-union empty-env base-env (lambda (a b) a)))
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
