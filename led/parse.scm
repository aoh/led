(define-library (led parse)

   (import
      (owl toplevel)
      (only (owl regex) get-replace-regex)
      (prefix (owl parse) get-))

   (export
      parse-runes
      get-command
      led-syntax-error-handler)

   (begin

      (define get-natural
         (get-parses
            ((first (get-byte-if (λ (x) (and (< #\0 x) (<= x #\9)))))
             (rest (get-star! (get-byte-if (λ (x) (and (<= #\0 x) (<= x #\9)))))))
            (fold (λ (n x) (+ (* n 10) (- x #\0))) 0 (cons first rest))))

      (define get-integer
         (get-parses
            ((sign (get-one-of (get-word "-" -1) (get-word "+" +1) (get-epsilon +1)))
             (value get-natural))
            (* sign value)))

      (define get-movement
         (get-one-of
            (get-word "^" (tuple 'select 'beginning-of-line))
            (get-word "0" (tuple 'select 'beginning-of-file))
            (get-word "$" (tuple 'select 'end-of-file))
            (get-word "%" (tuple 'select 'everything))
            (get-word "." (tuple 'select 'current-line))
            (get-word "@" (tuple 'select 'selection)) ; yo dawg
            (get-parses
               ((n get-integer))
               (tuple 'select-line n))))

      (define (upto-line delim)
         (get-either
            (get-parses
               ((skip (get-imm #\newline))
                (skip (get-word delim null))
                (skip (get-imm #\newline)))
               '(#\newline))
            (get-parses
               ((r get-rune)
                (rs (upto-line delim)))
               (cons r rs))))

      (define (whitespace-char? x)
         (or (eq? x #\newline)
             (eq? x #\space)))

      (define get-whitespace
         (get-byte-if whitespace-char?))

      (define get-non-whitespace
         (get-byte-if (lambda (x) (not (whitespace-char? x)))))

      (define get-whitespaces
         (get-plus get-whitespace))

      (define get-assignment
         (get-parses
            ((skip (get-word "set" 'set))
             (skip get-whitespace)
             (var  (get-plus (get-rune-if (lambda (x) (not (eq? x #\space))))))
             (skip get-whitespaces)
             ;; this can also be a string
             (val
                (get-either
                   (get-parses
                      ((skip (get-imm #\"))
                       (cs (get-star (get-rune-if (lambda (x) (not (eq? x #\"))))))
                       (skip (get-imm #\")))
                      cs)
                   (get-plus get-non-whitespace))))
            (tuple 'set (list->string var) (list->string val))))

      (define get-spaced-word
         (get-parses
             ((skip (get-plus get-whitespace))
              (path (get-plus (get-rune-if (lambda (x) (not (eq? x #\space)))))))
            (list->string path)))

      (define get-digit
         (get-rune-if
            (lambda (x) (and (<= #\0 x) (<= x #\9)))))

      (define get-spaced-number
         (get-parses
             ((skip (get-plus get-whitespace))
              (digits (get-plus get-digit)))
            (string->number (list->string digits))))

      ;; up to dot, replace selection
      ;; alternatively i/foo/
      (define get-insert
         (get-parses
            ((skip (get-word "i\n" 'insert))
             (content (upto-line ".")))
            (tuple 'replace content)))

      (define (get-non-rune x)
         (get-parses
            ((y get-rune)
             (verify (not (= x y)) #f))
            y))

      (define get-file-command
         (get-parses
            ((op
               (get-one-of
                  (get-word "write" 'write-buffer)
                  (get-word "w" 'write-buffer)     ;; the whole buffer + mark saved, not just selection
                  ;(get-word "r" 'read)
                  (get-word "read" 'read)
                  ;(get-word "n" 'new-buffer)
                  (get-word "new" 'new-buffer)
                  (get-word "search-buffer" 'search-buffer)
                  ))
             (path
                (get-either
                   (get-parses
                      ((skip (get-plus (get-imm #\space)))
                       (path (get-plus (get-non-rune #\newline))))
                      (list->string path))
                   (get-epsilon #false))))
            (tuple op path)))

      (define get-mark-command
         (get-parses
            ((op
               (get-one-of
                  (get-word "add-mark" 'add-mark)
                  (get-word "select-mark" 'select-mark)))
             (skip get-whitespace)
             (key get-rune))
            (tuple op key)))

      (define get-call
         (get-parses
            ((skip
               (get-either
                  (get-word "call" 'call)
                  (get-word "l" 'call))) ;; old [l]isp command
             (op get-spaced-word)) ; <- could be converted to a list
            (tuple 'call op)))

      (define get-subprocess
         (get-parses
            ((skip (get-word "subprocess" 'foo))
             (cmd  (get-plus get-spaced-word)))
            (tuple 'subprocess cmd)))

      (define get-resize
         (get-parses
            ((skip (get-word "resize" 'foo))
             (w get-spaced-number)
             (h get-spaced-number))
            (tuple 'resize w h)))

      (define get-command
         (get-parses
            ((skip (get-star! get-whitespace))
             (val
               (get-one-of
                  get-movement
                  get-file-command
                  get-subprocess
                  get-call
                  get-assignment
                  (get-parses
                     ((op (get-imm #\,))
                      (next get-movement))
                     (tuple 'extend-selection next))
                  (get-word "help" (tuple 'help ""))
                  (get-word "delete" (tuple 'delete))
                  (get-word "d" (tuple 'delete))
                  (get-word "undo" (tuple 'undo))
                  ;(get-word "u" (tuple 'undo))
                  (get-word "paste" (tuple 'paste)) ;; no buffer naming yet
                  (get-word "copy" (tuple 'copy)) ;; no buffer naming yet
                  (get-word "p" (tuple 'print))
                  (get-word "redo" (tuple 'redo))
                  (get-word "select-parent" (tuple 'select-parent))
                  ;(get-word "r" (tuple 'redo)) ;; is parsed as read #f
                  (get-word "q!" (tuple 'quit #t))
                  (get-word "q" (tuple 'quit #f))
                  (get-word "quit!" (tuple 'quit #t))
                  (get-word "quit" (tuple 'quit #f))
                  (get-word "indent" (tuple 'indent))
                  (get-word "unindent" (tuple 'unindent))
                  get-resize
                  (get-word "next-match" (tuple 'next-match #f))
                  get-insert
                  get-mark-command
                  (get-parses
                     ((val get-replace-regex))
                     (begin
                        (tuple 'apply
                           (lambda (env data)
                              (val data)))))
                     )))
            val))

      (define (sequence cmds)
         (cond
            ((null? cmds)
               (tuple 'nop))
            ((null? (cdr cmds))
               (car cmds))
            (else
               (tuple 'seq (car cmds)
                  (sequence (cdr cmds))))))

      (define get-commands
         (get-parses
            ((cmds (get-star! get-command))
             (skip (get-star get-whitespace)))
            (sequence cmds)))

      (define (forward-read ll)
         (if (pair? ll)
            (forward-read (cdr ll))
            ll))

      (define (led-syntax-error-handler recurse ll message)
         (display "? ")
         (recurse (forward-read ll)))


      ;; -> tuple | #false
      (define (parse-runes s)
         (get-parse get-commands s #false))

))


