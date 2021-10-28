;;;
;;; Extra processing functions
;;;

(define-library (led extra)

   ;; there are intentionally no led-specific imports, other than log

   (import
      (owl toplevel)
      (owl date)
      (owl proof)
      (only (led log) log)
      )

   (export
      extra-functions
      find-extra

      ;; below mainly for testing in repl
      format-merged
      format-lines
      split-lines
      unsplit-lines
      split-paragraphs
      paragraph-break
      )

   (begin

      ;;;
      ;;; Generic operations
      ;;;

      ;;; conversion from runes to line list, has no newlines

      (define (split-lines lst)
         (let loop ((rout null) (rthis null) (lst lst))
            (cond
               ((null? lst)
                  (if (null? rthis)
                     (reverse rout)
                     (loop (cons (reverse rthis) rout) null null)))
               ((eq? (car lst) #\newline)
                  (if (null? (cdr lst))
                     (loop (cons null (cons (reverse rthis) rout)) null null)
                     (loop (cons (reverse rthis) rout) null (cdr lst))))
               (else
                  (loop rout (cons (car lst) rthis) (cdr lst))))))

      (define (unsplit-lines lst)
         (foldr append null
            (interleave '(#\newline) lst)))

      (define test-line-1 '(a b #\newline c #\newline #\newline d e f #\newline))
      (define test-line-2 '(#\newline #\newline a b #\newline c #\newline #\newline d e))

      (example
         (unsplit-lines (split-lines '())) = '()
         (unsplit-lines (split-lines '(a))) = '(a)
         (unsplit-lines (split-lines '(a #\newline))) = '(a #\newline)
         (unsplit-lines (split-lines '(#\newline a))) = '(#\newline a)
         (unsplit-lines (split-lines test-line-1)) = test-line-1
         (unsplit-lines (split-lines test-line-2)) = test-line-2)

      ;;; Editing content between newlines

      (define (newline? x) (eq? x #\newline))

      (define (edit-preserving-surrounding-newlines lst op)
         (lets
            ((prelude lst (take-while newline? lst))
             (rfinale rlst (take-while newline? (reverse lst))))
            (append prelude
               (append
                  (op (reverse rlst))
                  rfinale))))

      (define (test x) (list 'x))

      (example
         (edit-preserving-surrounding-newlines '() test) = '(x)
         (edit-preserving-surrounding-newlines '(#\newline a b #\newline c #\newline #\newline) test)
            = '(#\newline x #\newline #\newline))


      ;;;
      ;;; Paragraph formatting
      ;;;

      (define (pick-line data len)
         (cond
            ((null? data)
               (values null null))
            ((eq? len 0)
               (values #f #f))
            ((eq? (car data) #\space)
               (lets ((this rest (pick-line (cdr data) (- len 1))))
                  (if this
                     (values
                        (cons #\space this)
                        rest)
                     (values null (cdr data)))))
            (else
               (lets ((this rest (pick-line (cdr data) (- len 1))))
                  (if this
                     (values (cons (car data) this) rest)
                     (values #f #f))))))

      (define (drop-leading-space data)
         (if (and (pair? data) (eq? (car data) #\space))
            (drop-leading-space (cdr data))
            data))

      (define (format-lines data)
         (if (null? data)
            null
            (lets
               ((data (drop-leading-space data))
                (this rest (pick-line data 80)))
               (if this
                  (cons this (format-lines rest))
                  (list data)))))

      (define remove-duplicate-spaces (string->regex "s/  */ /g"))
      (define newlines-to-spaces (string->regex "s/\n/ /g"))

      (define (format-paragraph settings data)
         (unsplit-lines
            (format-lines
               (remove-duplicate-spaces
                  (newlines-to-spaces data)))))

      ;; split by (trailing) blank lines to paragraphs
      ;; run format-merged on each paragraph
      ;; later preserve shared prefixes of each paragraph

      (define (paragraph-break lst)
         (if (and (pair? lst)
                  (eq? (car lst) #\newline)
                  (pair? (cdr lst))
                  (eq? (cadr lst) #\newline))
            2
            #f))

      (define (split-paragraphs lst)
         (let loop ((lst lst) (rthis null) (rpars null))
            (cond
               ((null? lst)
                  (if (null? rthis)
                     (reverse rpars)
                     (loop lst null (cons (reverse rthis) rpars))))
               ((paragraph-break lst) =>
                  (lambda (n)
                     (loop
                        (drop lst n)
                        null
                        (cons (reverse rthis) rpars))))
               (else
                  (loop (cdr lst) (cons (car lst) rthis) rpars)))))

      (define (format-merged settings data)
         (edit-preserving-surrounding-newlines data
            (lambda (data)
               (foldr
                  (lambda (x out)
                     (append x
                        (if (null? out)
                           null
                           (ilist #\newline #\newline out))))
                  null
                  (map (lambda (par) (format-paragraph settings par))
                     (split-paragraphs data))))))

      (define (current-date settings data)
         (string->list
            (date-str (time)
               ; (get settings 'utc-offset 0)
               0)))

      (define (lex-less? a b)
         (cond
            ((null? a) #true)
            ((null? b) #false)
            ((< (car a) (car b)) #true)
            ((= (car a) (car b))
               (lex-less? (cdr a) (cdr b)))
            (else #false)))

      (define (lex-sort settings data)
         (unsplit-lines
            (sort lex-less?
               (split-lines data))))

      (define (reverse-line-order settings lines)
         (reverse lines))

      (define (reverse-lines settings data)
         (unsplit-lines
            (map reverse
               (split-lines data))))


      ;;;
      ;;; Spelling and error checking
      ;;;

      (define spell-regexen
         (list
            (string->regex "g/^[A-Z][A-Z][a-z]/")    ;; double capital
            (string->regex "g/^([aeiouyAEIOUY])\\1\\1/") ;; triple vowel
            (string->regex "g/^[aeiouyAEIOUY]{4,}/") ;; quad vowel
            (string->regex "g/^[qwrtpsdfghjklzxcvbnmQWRTPSDFGHJKLZXCVBNM]{5,}/") ;; five consonants
            (string->regex "g/^[^a-zA-Z]([a-zA-Z]+)[ \\n]+\\1[^a-zA-Z]/") ;; double word
            (string->regex "g/^[.,?!][a-zA-Z]/") ;; no space after dot or comma
            (string->regex "g/^[.!?][ \\n]+[a-z]/") ;; lower case sentence start
            (string->regex "g/^[A-Z][^.!?]{200,}[.!?]/")
            ))

      (define (spell settings data)
         (let loop ((data data) (pos 0))
            (if (null? data)
               ;; all good
               (tuple 'subsection 0 0)
               (let ((match (fold (lambda (found rex) (or found (rex data))) #f spell-regexen)))
                  (if match
                     (tuple 'subsection pos (length match))
                     (loop (cdr data) (+ pos 1)))))))


      (define remove-trailing-spaces
         (string->regex "s/  *\\n/\\n/g"))

      (define (cleanup settings data)
         (remove-trailing-spaces data))


      ;;;
      ;;; Words
      ;;;

      (define split-words (string->regex "c/[ \n\t\r]+/"))
      (define words-cleanup (string->regex "s/[.,\"“”(){}[\\]?!-_]/ /g"))


      (define (lex-less? a b)
         (cond
            ((null? a) #t)
            ((null? b) #f)
            ((lesser? (car a) (car b))
               #t)
            ((eq? (car a) (car b))
               (lex-less? (cdr a) (cdr b)))
            (else #f)))

      (define (enlist x)
         (if (list? x)
            x
            (string->list x)))

      (define (counter lst)
         (let loop ((lst lst) (out null) (n 0) (x #f))
            (cond
               ((null? lst)
                  (if x
                     (cons (cons n x) out)
                     out))
               ((equal? (car lst) x)
                  (loop (cdr lst) out (+ n 1) x))
               (x
                  (loop (cdr lst) (cons (cons n x) out) 1 (car lst)))
               (else
                  (loop (cdr lst) out 1 (car lst))))))

      (define (car> a b) (> (car a) (car b)))

      (define (words settings data)
         (lets ((ws (split-words (words-cleanup data)))
                (ws (map enlist ws))
                (ws (sort lex-less? ws))
                (ws (counter ws))          ;; ((count rune ...) ...)
                (ws (sort car> ws)))
            (foldr
               (lambda (x tail)
                  (render
                     ;(number->string (car x) 4) ;; yields interesting results
                     (car x)
                     (cons 32
                        (append (cdr x)
                           (cons #\newline tail)))))
               '() ws)))


      ;;;
      ;;; Indentation
      ;;;

      ; these could be 'indent-regex later in env
      (define indent-after-newlines
         (string->regex "s/\\n/\\n   /g"))

      (define (indent-selection env data)
         (ilist #\space #\space #\space
            (indent-after-newlines data)))

      (define unindent-after-newlines
         (string->regex "s/\\n   /\n/g"))

      (define unindent-start
         (string->regex "s/^   //"))

      (define (unindent-selection env data)
         (log "unindent makes " (unindent-start (unindent-after-newlines data)))
         (unindent-start (unindent-after-newlines data)))


      (define extra-functions
         (list
            (cons "sort"    lex-sort)
            ; (cons "reverse" reverse-line-order)
            (cons "rev"     reverse-lines)
            (cons "date"    current-date)
            (cons "fmt"     format-merged)
            (cons "crash"   (λ args (car 42)))
            (cons "clean"   cleanup)
            (cons "del"     (lambda (env x) null))
            (cons "spell"   spell)
            (cons "words"   words)
            (cons "indent"  indent-selection)
            (cons "unindent"  unindent-selection)
            ))

      (define (find-extra name)
         (let ((res (assoc name extra-functions)))
            (if (pair? res)
               (cdr res)
               #false)))))



