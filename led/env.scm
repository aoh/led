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

      ;; move later
      file-modification-time)

   (begin

      (define empty-env
         (pipe empty

            ; settable
            (put 'tabstop 3)         ;; :set tabstop <n>

            ; soon settable
            (put 'autoindent #true)  ;; will be :set autoindent <strategy>

            ; not settable
            (put 'undo null)
            (put 'redo null)
            (put 'subprocess #false) ;; each buffer can have own (ones)
            ))

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
         (let ((env (ff-union empty-env base-env (lambda (a b) a))))
            (if path
               ;; small chance of race between reading modification time and
               ;; reading contents to buffer
               (update-disk-modification-time (put env 'path path))
               env)))

      (define (set-status-text env string)
         (put env 'status-message (string->runes string)))

      (define (clear-status-text env)
         (del env 'status-message))

      (define (env-char-width env n)
         (cond
            ((lesser? n 32)
               (cond
                  ((eq? (type n) type-fix-)
                     (env-char-width env (- (* n -1) 1)))
                  (else
                     (if (eq? n #\tab)
                        (get env 'tabstop 3)
                        4)))) ; 0x__
            ((eq? n 127)
               4)
            (else
               1)))


      ))
