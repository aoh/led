
(define-library (led render)

   (import
      (owl toplevel)
      (only (owl terminal) font-normal font-reverse font-dim)
      (only (led env) env-char-width)
      (led log))

   (export
      update-buffer-view
      distance-to         ;; lst x -> offset | #f, a shared utility function
      render-content      ;; runes -> printable-char-list
      repeat-char)

   (begin

      ;; hack warning: buffer contains bytes or unicode code points depending on
      ;; the encoding used. for rendering purposes we convert these to -(n+1) and
      ;; back to mark which parts are currently selected

      (define (toggle-selected x) (* -1 (+ x 1)))

      (define (i x) x)

      (define font-normal-color
         (let ((bs (ilist 27 91 (string->list "0m"))))
            (λ (x)
               (append x bs))))

      ;; n first values to -(x+1) to denote selected code points
      (define (mark-selected lst n)
         (if (eq? n 0)
            lst
            (cons (toggle-selected (car lst))
               (mark-selected (cdr lst) (- n 1)))))

      (define hex
         (let ((cs (vector #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                           #\a #\b #\c #\d #\e #\f)))
            (λ (c)
               (vector-ref cs c))))

      (define (render-hex x tail op)
         (if (< x 0)
            (render-hex (toggle-selected x) tail toggle-selected)
            (ilist
               (op (hex (band x #b1111)))
               (op (hex (band (>> x 4) #b1111)))
               (op #\x)
               (op #\0)
               tail)))

      (define (repeat-char x n tl)
         (if (eq? n 0)
            tl
            (repeat-char x (- n 1) (cons x tl))))

      ;; convert possibly selected chars to a renderable sequence
      ;; char tail -> tail' len
      (define (represent env char tail)
         (cond
            ((lesser? char 32)
               (cond
                  ((eq? char #\tab) ;; tab
                     (let ((n (get env 'tab-width 3)))
                        (values
                           (repeat-char #\_ n tail)
                           n)))
                  ((eq? char -10) ;; selected tab
                     (let ((n (get env 'tab-width 3)))
                        (values
                           (repeat-char (toggle-selected #\_) n tail)
                           n)))
                  (else
                     (values
                        (render-hex char tail i)
                        4))))
            ((eq? char 127)
               (values
                  (render-hex char tail i)
                  4))
            (else
               (values (cons char tail) 1))))

      (define (render-content env cs)
         (foldr
            (lambda (char tail)
               (if (eq? char #\newline)
                  ;; for repl use
                  (cons char tail)
                  (lets ((bs len
                           (represent env char tail)))
                     bs)))
            null cs))

      ;; go to beginning, or after next newline, count down visible steps from i
      (define (find-line-start env l r i)
         (cond
            ((null? l)
               (values l r i))
            ((eq? (car l) #\newline)
               (values l r i))
            (else
               (find-line-start env (cdr l)
                  (cons (car l) r)
                  (- i
                     (env-char-width env (car l)))))))

      ;; number of things up to next newline or end

      ;; ugly, number of steps to get from next newline up to same position or end of next line
      (define (distance-past-newline l n)
         (let loop ((l l) (steps 0))
            (cond
               ((null? l) steps)
               ((eq? (car l) #\newline)
                  (let loop2 ((l (cdr l)) (steps (+ steps 1)) (n n))
                     (cond
                        ((null? l) steps)
                        ((eq? n 0) steps)
                        ((eq? (car l) #\newline) steps)
                        (else
                           (loop2 (cdr l) (+ steps 1) (- n 1))))))
               (else
                  (loop (cdr l) (+ steps 1))))))

      ;; -> false | offset
      (define (distance-to l x)
         (let loop ((l l) (n 0))
            (cond
               ((null? l) #f)
               ((eq? (car l) x) n)
               (else (loop (cdr l) (+ n 1))))))

      (define (lines-back env l r n)
         (cond
            ((null? l)
               r)
            ((eq? n 0)
               r)
            (else
               (lets ((l r _ (find-line-start env (cdr l) (cons (car l) r) 0)))
                  (lines-back env l r (- n 1))))))

      (define (drop-upto-newline lst)
         (cond
            ((null? lst) null)
            ((eq? (car lst) #\newline) (cdr lst))
            ((eq? (car lst) -11) (cdr lst)) ;; selected
            (else (drop-upto-newline (cdr lst)))))


      ;; take at most max-len values from lst, possibly drop the rest, cut at newline (removing it) if any
      (define (take-line env lst max-len)
         ;(print "Taking line from " lst)
         (let loop ((lst lst) (taken null) (n max-len))
            (cond
               ((eq? n 0)
                  (values (reverse taken) (drop-upto-newline lst)))
               ((null? lst)
                  (loop lst (cons #\~ taken) 0))
               ((or (eq? (car lst) #\newline) (eq? (car lst) -11))
                  ;(print "Took line " (reverse taken))
                  (values (reverse taken) (cdr lst)))
               (else
                  (lets ((taken width (represent env (car lst) taken)))
                     (loop (cdr lst) taken (max 0 (- n width))))))))

      (define (handle-padding env lst pad)
         (cond
            ((eq? pad 0)
               lst)
            ((null? lst)
               null)
            ((eq? (car lst) #\newline)
               lst)
            ((eq? (car lst) -11)
               lst)
            ((< pad 0)
               (lets ((lst width (represent env (car lst) (cdr lst))))
                  (handle-padding env (cdr lst) (+ pad 1))))
            (else
               (cons #\~ (handle-padding env lst (- pad 1))))))

      (define (ansi-unselection lst)
         (cond
            ((null? lst)
               (font-normal lst))
            ((< (car lst) 0)
               (cons (toggle-selected (car lst))
                  (ansi-unselection (cdr lst))))
            (else
               (font-normal lst))))

      (define (ansi-selection lst)
         (cond
            ((null? lst) lst)
            ((< (car lst) 0)
               (font-reverse (ansi-unselection lst)))
            (else
               (cons (car lst)
                  (ansi-selection (cdr lst))))))

      (define (render-line env lst pad width)
         (lets ((lst (handle-padding env lst pad))
                (row lst (take-line env lst width)))
            ;(print "Selected line " row)
            (values (ansi-selection row) lst)))

      (define (render-lines env r h n w ln)
         (if (eq? h 0)
            null
            (lets ((l r (render-line env r n w)))
               (cons l
                  (render-lines env r (- h 1) n w (+ ln 1))))))

      (define (pad-to-length n lst)
         (if (< (length lst) n)
            (pad-to-length n (cons #\space lst))
            lst))

      (define (render-buffer env buff w h cx cy)
         (buff
            (λ (pos l r len line)
               (lets
                  ((rows (max 1 (- h 1)))
                   (line-col-width (+ 3 (string-length (str (+ (- line cy) rows)))))
                   (cols ;; character width on screen
                      (if (get env 'line-numbers)
                         (max 1 (- w line-col-width))
                         w))
                   (r (mark-selected r len))
                   (l r n (find-line-start env l r (- cx 1)))
                   (r (lines-back env l r (- cy 1)))
                   (lines
                     (render-lines env r rows n cols (- line (- cy 1)))))
                  (if (get env 'line-numbers)
                     (lets
                        ((line-numbers (iota (- (+ line 1) cy) 1 (+ line rows)))
                         (lines (map (λ (x) (cons #\space (cons #\space x))) lines))
                         (lines
                            (zip append
                               (map
                                  (λ (x)
                                    (pad-to-length (- line-col-width 2)
                                       (render x null)))
                                  line-numbers)
                               lines)))
                        (values lines line-col-width))
                     (values lines 0))))))

      (define (paren? x)
         (or (eq? x #\() (eq? x #\))))

      (define esc 27)

      (define (dim-string lst cont)
         (cond
            ((null? lst)
               (cont (font-normal lst)))
            ((eq? (car lst) #\")
               (cons (car lst)
                  (font-normal
                     (cont (cdr lst)))))
            (else
               (cons (car lst)
                  (dim-string (cdr lst) cont)))))

      (define (syntax-highlight lst)
         (let loop ((lst lst))
            (if (null? lst)
               lst
               (let ((x (car lst)))
                  (cond
                     ((eq? x esc) ;; already coloured. don't touch this.
                        lst)
                     ((paren? x)
                        (font-dim
                           (cons x
                              (font-normal
                                 (loop (cdr lst))))))
                     ((eq? x #\;)
                        (append
                           (font-dim lst)
                           (font-normal '())))
                     ((eq? x #\")
                        (font-dim
                           (cons x
                              (dim-string (cdr lst) syntax-highlight))))
                     (else
                        (cons x (loop (cdr lst)))))))))


      (define (update-buffer-view env b w h cx cy)
         (lets
            ((lsts dcx (render-buffer env b w h cx cy))
             (lsts (if (get env 'syntax #f) (map syntax-highlight lsts) lsts))
             (status-bytes (ref (get env 'status-line #f) 2))
             (status-message (get env 'status-message null))
             (status-prelude (get env 'status-prelude ""))
             (lsts
                (append lsts
                   (list
                      (str-foldr
                         cons
                         (append
                            (or status-bytes '())
                            (font-normal '()))
                         status-prelude)))))
            (mail 'ui
               (tuple 'update-screen lsts))
            (mail 'ui ;; may choose to use status line instead later
               (tuple 'set-cursor (+ dcx cx) cy))
            ;(log "status bytes are " status-bytes)
            ))



      ))
