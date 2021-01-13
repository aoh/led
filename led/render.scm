(define-library (led render)

   (import
      (owl toplevel)
      (only (owl terminal) font-normal font-reverse)
      (led log))

   (export
      update-buffer-view
      char-width          ;; rune -> n
      distance-to         ;; lst x -> offset | #f, a shared utility function
      )

   (begin

      ;; flip sign in leading n values
      (define (mark-selected lst n)
         (if (eq? n 0)
            lst
            (cons (* -1 (car lst))
               (mark-selected (cdr lst) (- n 1)))))

      (define (char-width n)
         (if (eq? n #\tab)
            3
            1))

      ;; go to beginning, or after next newline, count down steps from i
      (define (find-line-start l r i)
         (cond
            ((null? l)
               (values l r i))
            ((eq? (car l) #\newline)
               (values l r i))
            (else
               (find-line-start (cdr l)
                  (cons (car l) r)
                  (- i
                     (char-width (car l)))))))

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

      (define (lines-back l r n)
         (cond
            ((null? l)
               r)
            ((eq? n 0)
               r)
            (else
               (lets ((l r _ (find-line-start (cdr l) (cons (car l) r) 0)))
                  (lines-back l r (- n 1))))))

      (define (drop-upto-newline lst)
         (cond
            ((null? lst) null)
            ((eq? (car lst) #\newline) (cdr lst))
            ((eq? (car lst) -10) (cdr lst)) ;; selected
            (else (drop-upto-newline (cdr lst)))))



      ;; take at most max-len values from lst, possibly drop the rest, cut at newline (removing it) if any
      (define (take-line lst max-len)
         ;(print "Taking line from " lst)
         (let loop ((lst lst) (taken null) (n max-len))
            (cond
               ((eq? n 0)
                  (values (reverse taken) (drop-upto-newline lst)))
               ((null? lst)
                  (loop lst (cons #\~ taken) 0))
               ((or (eq? (car lst) #\newline) (eq? (car lst) -10))
                  ;(print "Took line " (reverse taken))
                  (values (reverse taken) (cdr lst)))
               ((eq? (car lst) #\tab)
                  ;;
                  (loop (cdr lst) (ilist #\_ #\_ #\_ taken) (max 0 (- n 3))))
               (else
                  (loop (cdr lst) (cons (car lst) taken)  (- n 1))))))

      (define (handle-padding lst pad)
         (cond
            ((eq? pad 0)
               lst)
            ((null? lst)
               null)
            ((eq? (car lst) #\newline)
               lst)
            ((eq? (car lst) -10)
               lst)
            ((< pad 0)
               (handle-padding (cdr lst) (+ pad 1)))
            (else
               (cons #\~ (handle-padding lst (- pad 1))))))

      (define (ansi-unselection lst)
         (cond
            ((null? lst)
               (font-normal lst))
            ((< (car lst) 0)
               (cons (* (car lst) -1)
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

      (define (render-line lst pad width)
         (lets ((lst (handle-padding lst pad))
                (row lst (take-line lst width)))
            ;(print "Selected line " row)
            (values (ansi-selection row) lst)))

      (define (render-lines r h n w ln)
         (if (eq? h 0)
            null
            (lets ((l r (render-line r n w)))
               ;(print h " line is " l " = " (list->string l))
               (cons
                  ;(append (string->list (str ln ": ")) l)
                  l
                  (render-lines r (- h 1) n w (+ ln 1))))))

      (define (pad-to-length n lst)
         (if (< (length lst) n)
            (pad-to-length n (cons #\space lst))
            lst))

      (define (render-buffer env buff w h cx cy)
         (buff
            (λ (pos l r len line)
               (lets
                  ((rows (max 1 (- h 1)))
                   (line-col-width (+ 3 (string-length (str (+ (- (+ line 0) cy) rows)))))
                   (cols
                      (if (get env 'line-numbers)
                         (max 1 (- w line-col-width))
                         w))
                   (r (mark-selected r len))
                   (l r n (find-line-start l r (- cx 1)))
                   (r (lines-back l r (- cy 1)))
                   (lines
                     (render-lines r rows n cols (- line (- cy 1)))))
                  ;(print "Rendering lines starting from " r)
                  (if (get env 'line-numbers)
                     (lets
                        ((line-numbers (iota (- (+ line 1) cy) 1 (+ line rows)))
                         (lines (map (λ (x) (cons #\space (cons #\| (cons #\space x)))) lines))
                         ;(lines (zip append (map (λ (x) (pad-to-length (- line-col-width 1) (render x null))) line-numbers) lines))
                         (lines
                            (zip append
                               (map
                                  (λ (x)
                                    (pad-to-length (- line-col-width 3)
                                       (let ((r (remainder x 10)))
                                          (render
                                            (cond
                                               ((eq? r 0) x)
                                               ;((eq? r 5) "-")
                                               (else ""))
                                            null))))
                                  line-numbers)
                               lines))
                         )
                        (values lines line-col-width))
                     (values lines 0))))))

      (define (overlay a b)
         (cond
            ((null? a)
               b)
            ((null? b)
               a)
            (else
               (cons (car a)
                  (overlay (cdr a) (cdr b))))))

      (define (update-buffer-view env b w h cx cy)
         (lets
            ((lsts dcx (render-buffer env b w h cx cy))
             (status-bytes (ref (get env 'status-line #f) 2))
             (status-message (get env 'status-message null))
             (lsts
                (append lsts
                   (list
                      (overlay
                         status-message
                        (or status-bytes (list 32))))))
             )
            (mail 'ui
               (tuple 'update-screen lsts))
            (mail 'ui ;; may choose to use status line instead later
               (tuple 'set-cursor (+ dcx cx) cy))
            ;(log "status bytes are " status-bytes)
            ))



      ))
