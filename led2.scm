(import
   (prefix (owl parse) get-)
   (only (owl parse) byte-stream->exp-stream fd->exp-stream)
   (only (owl readline) port->readline-byte-stream)
   (owl unicode)
   (only (owl sys) file? directory?)
   (owl terminal)
   (owl sys)
   (owl proof)
   (owl unicode)
   (owl date)
   (only (led system) led-dir->list)
   (owl args))

;; fixme: undo buffer two node types, crashes

; 'screen <- #(update-screen lsts)
;         <- #(set-cursor x y)
; 'ui
; 'buff-<n> + '(buff-<n> . status-line)
;

(define (log . x)
   ;; just dropped if logger is not running
   (mail 'log x))

(define (logger port)
   (lets ((envelope (wait-mail))
          (from msg envelope))
      (print-to port from ": " msg)
      (logger port)))

(define (start-logger path)
   (if path
      (let ((port (open-output-file path)))
         (if port
            (begin
               (thread 'log (logger port))
               log)
            (error "Cannot open log file " path)))))

;; discard sender
(define (wait-message)
   (let ((envelope (wait-mail)))
      (ref envelope 2)))

;;; Buffers --------------------------------------------------

(define (buffer pos l r len line)
   (λ (op) (op pos l r len line)))

(define (buffer-char b)
   (b (λ (pos l r len line) (if (pair? r) (car r) #false))))

(define (buffer-selection-delta b d)
   (b (λ (pos l r len line)
      (buffer pos l r (max (+ len d) 0) line))))

;; read current selection
(define (get-selection b)
   (b (λ (pos l r len line) (take r len))))

(define (buffer-pos b)
   (b (λ (pos l r len line) pos)))

(define (buffer-selection-length b)
   (b (λ (pos l r len line) len)))

(define (buffer-unselect b)
   (b (λ (pos l r len line) (buffer pos l r 0 line))))

(import (only (owl syscall) link kill))

;; current line (at start of dot)
(define (buffer-line b)
   (b (λ (pos l r len line) line)))

(define (buffer-right b)
   (b (λ (pos l r len line) r)))

(define (buffer-left b)
   (b (λ (pos l r len line) l)))

(define empty-buffer
   (buffer 0 null null 0 1))

(define (string-buffer str)
   (buffer 0 null (string->list str) 0 1))

;; -> buffer | #false
(define (file-buffer path)
   (log (str "trying to open " path " as file"))
   (if (file? path)
      (buffer 0 null (utf8-decode (file->list path)) 0 1)
      #false))

;; -> buffer | #false
(define (dir-buffer path)
   (log (str "trying to open " path " as directory"))
   (let ((paths (led-dir->list path)))
      (if paths
         (buffer 0 null
            (foldr
               (λ (x tail) (append (string->list x) (cons #\newline tail)))
               null paths)
             0 1)
          #false)))

(define (buffer-append-to-end b data)
   (b (λ (pos l r len line)
         (buffer pos l (append r data) len line))))

;;; do-command 
(define word-chars
   (fold (λ (ff x) (put ff x x))
      empty
      (string->list
         "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-/_!?<>.:+-*")))

(define (word-char? x) 
   (get word-chars x))

(define (word-chars l)
   (cond
      ((null? l) l)
      ((word-char? (car l))
         (cons (car l) (word-chars (cdr l))))
      (else null)))

(define (add-nl line char delta)
   (if (eq? char #\newline)
      (+ line delta)
      line))

(define (match-prefix? lst pat)
   (cond
      ((null? pat) #t)
      ((null? lst) #f)
      ((eq? (car lst) (car pat))
         (match-prefix? (cdr lst) (cdr pat)))
      (else #f)))

(define (first-match b runes)
   (b (λ (pos l r len line)
      (let ((data (append (reverse l) r)))
         (let loop ((data data) (pos 0))
            (cond
               ((null? data) #false)
               ((match-prefix? data runes)
                  pos)
               (else
                  (loop (cdr data) (+ pos 1)))))))))

(define (maybe-car x)
   (if (pair? x)
      (car x)
      #f))

(define (maybe-cdr x)
   (if (pair? x)
      (cdr x)
      x))

(define (next-match b runes)
   (b (λ (pos l r len line)
      (let loop ((data (maybe-cdr r)) (pos (+ pos 1)))
         (cond
            ((null? data) #false)
            ((match-prefix? data runes)
               pos)
            (else
               (loop (cdr data) (+ pos 1))))))))

;; seek position
(define (seek b to)
   (define (find pos l r len line)
      (cond
         ((= pos to)
            (buffer pos l r len line))
         ((< pos to)
            (if (pair? r)
               (find (+ pos 1) (cons (car r) l) (cdr r)
                  0 (add-nl line (car r) +1))
               #false))
         (else
            (if (pair? l)
               (find (- pos 1) (cdr l) (cons (car l) r)
                  0 (add-nl line (car l) -1))
               #false))))
   (b find))

(define (buffer-select-current-word b)
   (b (λ (pos l r len line)
      (lets ((dl (length (word-chars l)))
             (dr (length (word-chars r))))
         (buffer-selection-delta
            (buffer-unselect (seek b (- pos dl)))
            (+ dl dr))))))

(define (seek-delta b n)
   (b (λ (pos l r len line) (seek b (+ pos n)))))

(define (buffer-char b)
   (b (λ (pos l r len line) (if (null? r) #false (car r)))))

(define (nth-offset lst nth elem)
   (if (< nth 1)
      (begin
         (print-to stderr "bug: nth-offset < 1")
         #false) ;; bug in call
      (let loop ((lst lst) (nth nth) (n 0))
         (cond
            ((null? lst) #f)
            ((eq? (car lst) elem)
               (if (eq? nth 1)
                  n
                  (loop (cdr lst) (- nth 1) (+ n 1))))
            (else
               (loop (cdr lst) nth (+ n 1)))))))

(define (buffer->bytes b)
   (b (λ (pos l r len line) (utf8-encode (append (reverse l) r)))))

(example
   (nth-offset '(a a x a x a a x a) 1 'x) = 2
   (nth-offset '(a a x a x a a x a) 2 'x) = 4
   (nth-offset '(a a x a x a a x a) 3 'x) = 7
   (nth-offset '(a a x a x a a x a) 4 'x) = #false)

(define (select b from to)
   (let ((b (seek b from)))
      (cond
         ((not b) #false)     ;; not in buffer
         ((< to from) #false) ;; invalid range
         (else
            (b (λ (pos l r len line)
                  (buffer pos l r (- to from) line)))))))

;; select rest of line including newline, if there
(define (select-rest-of-line b)
   (b
      (λ (pos l r len line)
         (let ((end (nth-offset r 1 #\newline)))
            (if end
               (buffer pos l r (+ end 1) line)
               (buffer pos l r (length r) line)))))) ;; partial line

;; select empty string at beginning of current line
(define (seek-start-of-line b)
   (b
      (λ (pos l r len line)
         (let loop ((pos pos) (l l) (r r))
            (cond
               ((null? l)
                  (buffer pos l r 0 line))
               ((eq? (car l) #\newline)
                  (buffer pos l r 0 line))
               (else
                  (loop (- pos 1) (cdr l) (cons (car l) r))))))))

(define (merge-selections ba bb)
   (ba (λ (pos l r len line)
         (bb (λ (posb lb rb lenb lineb)
               (if (<= pos posb) ;; start from ba
                  (buffer pos l r (- (max (+ pos len) (+ posb lenb)) pos) line)
                  (merge-selections bb ba)))))))

(define (select-line b n)
   (b
      (λ (pos l r len line)
         (cond
            ((eq? n 1) ;; select first line (which has no preceding #\newline to look for)
               (select-rest-of-line
                  (buffer 0 null (append (reverse l) r) 0 1)))
            ((< n line) ;; select a complete preceding or current line
               (let ((start (nth-offset l (- line (- n 1)) #\newline)))
                  (if start
                     (select-rest-of-line
                        (seek b (- pos start)))
                     #false)))
            ((= n line)
               (select-rest-of-line
                  (seek-start-of-line b)))
            (else
               (let ((start (nth-offset r (- n line) #\newline)))
                  (if start
                     (select-rest-of-line
                        (seek b (+ pos (+ start 1)))) ;; after newline
                     #false)))))))

(define (distance-to-newline l)
   (let loop ((l l) (n 0))
      (cond
         ((null? l) n)
         ((eq? (car l) #\newline) n)
         (else
            (loop (cdr l) (+ n 1))))))

(define (buffer-line-pos b)
   (b (λ (pos l r len line)
      (distance-to-newline l))))

(define (select-next-line b)
   (b
      (λ (pos l r len line)
         (lets ((r (drop r len))
                (more (distance-to-newline r)))
            (buffer pos l r (+ len more) line)))))

(define (select-lines b from to)
   (let
      ((bf (select-line b from))
       (bt (select-line b to)))
      (if (and bf bt)
         (merge-selections bf bt)
         #false)))

(define (walk l r len line)
   (if (eq? len 0)
      (values l r line)
      (let ((this (car r)))
         (walk (cons this l)
            (cdr r)
            (- len 1)
            (+ line (if (eq? this #\newline) 1 0))))))

;; append after current dot, select added content
(define (buffer-append b val)
   (b
      (λ (pos l r len line)
         (lets
            ((new-pos (+ pos len))
             (l r line (walk l r len line)))
            (buffer
               new-pos
               l (append val r)
               (length val)
               line)))))

(define (buffer-after-dot b)
   (b
      (λ (pos l r len line)
         (lets
            ((new-pos (+ pos len))
             (l r line (walk l r len line)))
            (buffer new-pos l r 0 line)))))

(define (buffer-append-noselect b v)
   (buffer-after-dot
      (buffer-append b v)))

;; replace selection with value
(define (buffer-replace b val)
   (b
      (λ (pos l r len line)
         (buffer pos l
            (append val (drop r len))
            (length val)
            line))))

;; delete selection
(define (buffer-delete b)
   (b
      (λ (pos l r len line)
         (buffer pos l (drop r len) 0 line))))

(define (buffer-apply b func)
   (buffer-replace b (func (get-selection b))))

;;; Screen rendering -----------------------------------------------------------------------

;; flip sign in leading n values
(define (mark-selected lst n)
   (if (eq? n 0)
      lst
      (cons (* -1 (car lst))
         (mark-selected (cdr lst) (- n 1)))))

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
            (- i 1)))))

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

;; buffer -> positive-offset missing-length
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
               (values #f #f)))))) ;; last line

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
               (values #f #f)))))) ;; first line

(define (lines-back l r n)
   ;(print "lines-back " n " from " r)
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
                                         ((eq? r 5) "")
                                         (else ""))
                                      null))))
                            line-numbers)
                         lines))
                   )
                  (values lines line-col-width))
               (values lines 0))))))


(define (buffer->string buff)
   (buff
      (λ (pos l r len line)
         (lets
            ((title (str "--(" pos "+" len ", line " line ")-------------------------------------------------------\n"))
             (pre (reverse l))
             (dot (take r len))
             (post (drop r len)))
            (str
               title
               (runes->string pre)
               (bytes->string (font-reverse null))
               (runes->string dot)
               (bytes->string (font-normal null))
               (runes->string post)
               "\n----------------------------------------------------------------------\n")))))


;;; LED evaluation ------------------------------------------

(define (led-eval-position buff env exp)
   ;(print "evaling position " exp)
   (if (number? exp)
      exp
      (begin
         (print-to stderr "unknown position " exp)
         #f)))

(define (led-apply buff op)
   (tuple-case op
      ((append val)
         (buffer-append buff val))
      (else
         (print-to stderr (list 'waat op))
         #false)))

(define (push-undo env action)
   (-> env
      (put 'redo null) ;; destroy future of alternative past
      (put 'undo (cons action (get env 'undo null)))))

(define (pop-undo env)
   (let ((stack (get env 'undo null)))
      (if (null? stack)
         (values env #false)
         (values
            (-> env
               (put 'undo (cdr stack))
               (put 'redo (cons (car stack) (get env 'redo null))))
            (car stack)))))

(define (set-status-text env string)
   (put env 'status-message (string->runes string)))

(define (clear-status-text env)
   (del env 'status-message))

(define (led-eval buff env exp)
   (log "led-eval " exp)
   (tuple-case exp
      ((write-buffer target)
         (lets ((path (or target (get env 'path)))
                (fd (and path (open-output-file path))))
            (log "Writing buffer to " path)
            (if fd
               (let ((data (buffer->bytes buff)))
                  (if (write-bytes fd data)
                     (values buff
                        (set-status-text
                           (put env 'path path)
                           (str "Wrote " (length data) "b to " path ".")))
                     (values buff
                        (set-status-text env (str "Failed to write to " path ".")))))
               (begin
                  (log "Failed to open " path " for writing.")
                  (values #f #f)))))
      ((new-buffer path)
         (mail 'ui exp)
         (values buff env))
      ((append text)
         (lets
            ((action exp)
             (b (led-apply buff action)))
            (if b
               (values b
                  (push-undo env buff))
               (values #f #f))))
      ((select-line n)
         (values
            (select-line buff n)
            env))
      ((extend-selection movement)
         (lets ((buffp envp (led-eval buff env movement)))
            (if buffp
               (values
                  (merge-selections buff buffp)
                  envp)
               (values #f #f))))
      ((delete)
         (values
            (buffer-delete buff)
            (push-undo env buff)))
      ((undo)
         (lets ((env buff (pop-undo env)))
            (values buff env)))
      ((print)
         (let ((data (get-selection buff)))
            (print (runes->string data))
            (values buff env)))
      (else
         (print-to stderr (list 'wat-eval exp))
         (values #f #f))))


;;; Edit language, assumes local state, creates actions

(define empty-env
   (-> empty
      (put 'undo null)
      (put 'redo null)))

;;; PARSING ---------------------------------------------------

(define get-integer
   (get-parses
      ((sign (get-one-of (get-word "-" -1) (get-word "+" +1) (get-epsilon +1)))
       (first (get-byte-if (λ (x) (and (< #\0 x) (<= x #\9)))))
       (rest (get-star! (get-byte-if (λ (x) (and (<= #\0 x) (<= x #\9)))))))
      (* sign (fold (λ (n x) (+ (* n 10) (- x #\0))) 0 (cons first rest)))))

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
         null)
      (get-parses
         ((r get-rune)
          (rs (upto-line delim)))
         (cons r rs))))

(define (get-action get-movement)
   (get-one-of
      (get-parses
         ((skip (get-imm #\d)))
         (tuple 'delete))
      (get-parses
         ((op (get-word "a" 'append))
          (nl (get-imm #\newline))
          (data (upto-line ".")))
         (tuple 'append data))
      (get-parses ((op (get-word "u" 'undo))) (tuple op))
      (get-parses ((op (get-word "R" 'redo))) (tuple op))
      (get-parses ((op (get-word "q" 'quit))) (tuple op))
      (get-parses
         ((op (get-imm #\,))
          (next get-movement))
         (tuple 'extend-selection next))
      (get-parses
         ((skip (get-imm #\p)))
         (tuple 'print))))

(define get-whitespace
   (get-byte-if
      (λ (x) (or (eq? x #\newline) (eq? x #\space)))))

(define get-file-command
   (get-parses
      ((op
         (get-one-of
            (get-word "w" 'write-buffer)     ;; the whole buffer + mark saved, not just selection
            (get-word "write" 'write-buffer)
            (get-word "r" 'read)
            (get-word "read" 'read)
            (get-word "n" 'new-buffer)
            (get-word "new" 'new-buffer)))
       (path
          (get-either
             (get-parses
                ((skip (get-plus get-whitespace))
                 (path (get-plus get-rune)))
                (list->string path))
             (get-epsilon #false))))
      (tuple op path)))

(define get-command
   (get-parses
      ((skip (get-star! get-whitespace))
       (val
         (get-one-of
            ;get-movement
            ;(get-action get-movement)
            get-file-command
            (get-word "delete" (tuple 'delete))
            (get-word "d" (tuple 'delete)))))
      val))

(define (forward-read ll)
   (if (pair? ll)
      (forward-read (cdr ll))
      ll))

(define (syntax-error-handler recurse ll message)
   (display "? ")
   (recurse (forward-read ll)))


;; -> tuple | #false
(define (led-parse-runes s)
   (get-parse get-command s #false))

(define (led-eval-runes buff env s)
   (let ((exp (led-parse-runes s)))
      (log "eval " s " -> " exp)
      (if exp
         (lets ((buffp envp (led-eval buff env exp)))
            (if buffp
               (values buffp envp)
               (begin
                  (log "command " exp " failed")
                  (values buff env))))
         (values buff
            (put env 'status-line
               (tuple 'status-line
                  (string->list "syntax error")
                  #f))))))

(define (led-repl buff env)
   (display "> ")
   (lfold
      (λ (buff-env exp)
         (lets ((buff env buff-env)
                (buff env (led-eval buff env exp)))
            (if buff
               (begin
                  ;(print (buffer->string buff))
                  ;(print-buffer buff)
                  (display "> ")
                  (cons buff env))
               (begin
                  (print "?")
                  buff-env))))
      (cons buff env)
      (byte-stream->exp-stream
         (port->readline-byte-stream stdin)
         get-command
         syntax-error-handler)))

(define (compare new old p)
   (cond
      ((null? old)
         (values p new))
      ((null? new)
         (values p (list #\space))) ;; clear remaining line
      ((eq? (car new) (car old))
         (if (eq? (car new) 27)
           (lets ((dp dl (compare (cdr new) (cdr old) (+ p 1))))
             (if (null? dl)
                (values dp dl)
                (values p new)))
           (compare (cdr new) (cdr old) (+ p 1))))
      (else
         (values p new))))



;;;
;;; ( SCREEN SERVER )-----------------------------------------------------------------------
;;;

(define (screen w h old)
   ;; optimized update belongs here later
   ;(print (list 'screen w h))
   (lets
      ((msg (wait-mail))
       (from msg msg))
      ;(print "screen -> " msg)
      (log "screen: op " (ref msg 1) " from " from)
      (tuple-case msg
         ((update-screen new-rows)
            (let loop ((row 1) (rows new-rows) (old old) (out (cursor-show null)) (shared 0))
               (cond
                  ((null? rows)
                     (write-bytes stdout (cursor-hide out))
                     ;(write-bytes stdout (set-cursor (render (str (list "shared " shared)) null) 10 10))
                     (screen w h new-rows))
                  ((null? old)
                     (loop row rows '(()) out shared))
                  ; jos halutaan vain kokonaiset jaetut rivit (osittaisissa on riski ansi hajoamisille)
                  ;((equal? (car rows) (car old))
                  ;   (loop (+ row 1) (cdr rows) (cdr old) out (+ shared 1)))
                  (else
                     (lets
                        ((offset row-left (compare (car rows) (car old) 1)))
                        (if (null? row-left)
                           (loop (+ row 1) (cdr rows) (cdr old) out (+ shared offset))
                           (loop
                              (+ row 1)
                              (cdr rows)
                              (cdr old)
                              (set-cursor
                                 (clear-line-right
                                    (append (utf8-encode row-left) out))
                                 offset row)
                              (+ shared offset))))))))
         ((clear)
            (write-bytes stdout
               (clear-screen null))
            (screen w h null))
         ((print-to x y thing)
            (write-bytes stdout
               (set-cursor
                  (render thing null)
                  (max x 1) (max y 1)))
            (screen w h null))
         ((print-bytes-to x y thing)
            (write-bytes stdout
               (set-cursor thing x y))
            (screen w h null))
         ((set-cursor x y)
            (write-bytes stdout
               (set-cursor null (max x 1) (max y 1)))
            (screen w h old))
         ((clear-line-right)
            (write-bytes stdout
               (clear-line-right null))
            (screen w h null))
         (else
            (print "screen: wat " msg " from " from)
            (screen w h old)))))

(define (clear-screen)
   (mail 'screen (tuple 'clear))
   )

(define (print-to x y . stuff)
   (mail 'screen (tuple 'print-to x y (apply str stuff))))

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
      (log "status bytes are " status-bytes)
      ))

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

(define (paren-hunt l depth len inc dec)
   (cond
      ((eq? depth 0)
         len)
      ((null? depth)
         #false)
      ((null? l)
         #false)
      ((eq? (car l) inc)
         (paren-hunt (cdr l) (+ depth 1) (+ len 1) inc dec))
      ((eq? (car l) dec)
         (paren-hunt (cdr l) (- depth 1) (+ len 1) inc dec))
      (else
         (paren-hunt (cdr l) depth (+ len 1) inc dec))))

(define (paren-hunter b)
   (b (λ (pos l r len line)
      (if (and (pair? r) (eq? (car r) 40))
         (paren-hunt (cdr r) 1 1 40 41)
         #false))))

;;; Content Operations

(define (indent-selection env)
   (lambda (data)
      (ilist #\space #\space #\space (s/(\n)/\1   /g data))))

(define (unindent-selection env)
   (lambda (data)
      (s/^   // (s/(\n)   /\1/g data))))

(define (led env mode b cx cy w h)
   ;(print (list 'buffer-window b cx cy w h))
   (lets ((from msg (next env b w h cx cy))
          (op (ref msg 1)))
      (log "led: " mode " <- " msg " from " from)
      (cond
         ((eq? op 'push)
            (led env mode
               (buffer-append-to-end b (ref msg 2))
               cx cy w h))
         ((eq? op 'terminal-size)
            (lets ((_ w h msg))
               (for-each
                  (λ (cli) (mail cli msg))
                  (get env 'clients null))
               (led env mode b (min cx w) (min cy h) w h)))
         ((eq? op 'status-line)
            (led
               (put env 'status-line msg) ;; #(status-line <bytes> <cursor-x>)
               mode b cx cy w h))
         ((eq? op 'keep-me-posted)
            (led (put env 'clients (cons from (get env 'clients null)))
               mode b cx cy w h))
         ((eq? op 'command-entered)
            (lets
               ((runes (ref msg 2)))
               (cond
                  ((eq? (maybe-car runes) #\:)
                     (lets ((buff env (led-eval-runes b env (cdr runes))))
                        (led env 'command buff cx cy w h)))
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
                         (lets ((b (seek b pos))
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
                        (led
                           (del env 'status-message)
                           mode b cx cy w h))
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
               ((enter)
                  (let ((s (list->string (get-selection b))))
                     (cond
                        ((file? s)
                           (mail 'ui (tuple 'new-buffer s))
                           (led env mode b cx cy w h))
                        ((directory? s)
                           (log "Would have opened " s)
                           (led env mode b cx cy w h))
                        (else
                           (led env mode b cx cy w h)))))
               ((key x)
                  (cond
                     ((eq? x #\i)
                        (led env 'insert b cx cy w h))
                     ((eq? x #\y)
                        (lets ((seln (get-selection b))
                               (env (put env 'yank seln)))
                           (led env mode  b cx cy w h)))
                     ((eq? x #\n)
                        (let ((s (get env 'last-search)))
                           (log "running last search " s)
                           (if s
                              (let ((p (next-match b s)))
                                 (log "next search match is " p)
                                 (if p
                                    (lets ((b (seek b p))
                                           (lp (buffer-line-pos b)))
                                       (led env mode b (if (>= lp w) 1 (+ lp 1)) 1 w h))
                                    (led env mode b cx cy w h)))
                              (led env mode b cx cy w h))))
                     ((eq? x #\c)
                        (lets ((seln (get-selection b))
                               (env (put env 'yank seln)))
                           (led
                              (push-undo env (tuple b cx cy))
                              'insert
                              (buffer-delete b)
                              cx cy w h)))
                     ((eq? x #\.)
                        (if (= 0 (buffer-selection-length b))
                           (led env mode (select-line b (buffer-line b)) 1 cy w h)
                           (led env mode (select-next-line b) 1 cy w h)))
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
                               (env (put env 'yank seln)))
                           (led
                              (push-undo env (tuple b cx cy))
                              mode
                              (buffer-delete b)
                              cx cy w h)))
                     ((eq? x #\p)
                        (led
                           (push-undo env (tuple b cx cy))
                           mode
                           (buffer-replace b (get env 'yank null))
                           cx cy w h))
                     ((eq? x #\u)
                        (lets ((env node (pop-undo env)))
                           (if node
                              (lets ((b cx cy node))
                                 (led env mode b cx cy w h))
                              (led env mode b cx cy w h))))
                     ((eq? x #\h) ;; left
                        (let ((bp (seek-delta b -1)))
                           (if (or (not bp) (eq? (buffer-char bp) #\newline))
                              (led env mode b cx cy w h)
                              (led env mode bp (max 1 (- cx 1)) cy w h))))
                     ((eq? x #\l) ;; right
                        (let ((bp (seek-delta b +1)))
                           (if (or (not bp)
                                   ;(eq? (buffer-char bp) #\newline)
                                   (eq? (buffer-char b)  #\newline)
                                   )
                              (led env mode b cx cy w h)
                              (led env mode bp (min w (+ cx 1)) cy w h))))
                     ((eq? x #\>) ;; indent
                        (led env mode
                           (buffer-apply b (indent-selection env))
                           cx cy w h))
                     ((eq? x #\<) ;; unindent
                        (led env mode
                           (buffer-apply b (unindent-selection env))
                           cx cy w h))
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
                     ((eq? x #\N) ;; numbers
                        (led (put env 'line-numbers (not (get env 'line-numbers #false)))
                           mode b cx cy w h))
                     ((eq? x #\Q)
                        ;; clean up, notify UI we are done and finish
                        (kill (get env 'status-thread'id))
                        (mail 'ui (tuple 'buffer-closed))
                        42)
                     ((eq? x #\W)
                        (lets ((b (buffer-select-current-word b))
                               (seln (get-selection b))
                               (lp (buffer-line-pos b)))
                           (led env mode
                              (buffer-select-current-word b)
                              (max 1 (+ 1 lp)) cy w h)))
                     ((or (eq? x #\:) (eq? x #\/) (eq? x #\?) (eq? x #\|))
                        (mail (get env 'status-thread-id) (tuple 'start-command x))
                        (led (clear-status-text env) 'enter-command b cx cy w h))
                     (else
                        (led env mode b cx cy w h))))
               (else
                  (led env mode b cx cy w h))))
         ((eq? mode 'insert)
            ;; insert mode
            (tuple-case msg
               ((enter)
                  (lets
                     ((b (buffer-append-noselect b (list #\newline))))
                     (led env 'insert b 1 (min h (+ cy 1)) w h)))
               ((key x)
                  (lets
                     ((b (buffer-append-noselect b (list x))))
                     (led env 'insert b (min w (+ cx 1)) cy w h)))
               ((refresh)
                  (led env 'insert b cx cy w h))
               ((esc)
                  (led env 'command b cx cy w h))
               ((tab)
                  (lets ((b (buffer-append-noselect b (list #\space #\space #\space))))
                     (led env mode b (min w (+ cx 3)) cy w h)))
               ((ctrl k)
                  (cond
                     ((eq? k 'c)
                        (led env 'command b cx cy w h))
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
                  (if (> (buffer-pos b) 0)
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

(define (empty-led-env id path)
   (if path
      (put empty 'path path)
      empty))

(define (pad-to len lst)
   (let loop ((lst lst) (n (length lst)))
      (if (< n len)
         (loop (cons #\space lst) (+ n 1))
         lst)))

(define (status-line id info w keys)
   (lets ((envelope (wait-mail))
          (from msg envelope))
      (log "status-line got " msg " from " from ", keys " keys)
      (tuple-case msg
         ((update env buff)
            (lets ((line (buffer-line buff))
                   (p  (buffer-pos buff))
                   (l  (buffer-selection-length buff))
                   (info2 (str (if (eq? l 0) "" (str "[" l "] ")) line " ")))
               (if (not (equal? info info2))
                  (mail id
                     (tuple 'status-line
                        (pad-to w (string->list info2))
                        1)))
               (status-line id info2 w keys)))
         ((terminal-size w h)
            (status-line id info w keys))
         ((start-command key)
            (mail id (tuple 'status-line (list key) 1))
            (status-line id info w (list key)))
         ((key x)
            (mail id (tuple 'status-line (reverse (cons x keys)) (+ 1 (length keys))))
            (mail id (tuple 'command-updated (reverse (cons x keys))))
            (status-line id info w (cons x keys)))
         ((backspace)
            (if (null? (cdr keys))
               (begin
                  (mail id (tuple 'command-aborted))
                  (mail id (tuple 'status-line null 1))
                  (status-line id info w null))
               (let ((keys (cdr keys)))
                  (mail id (tuple 'status-line (reverse keys) (+ 1 (length keys))))
                  (status-line id info w keys))))
         ((esc)
            (mail id (tuple 'command-aborted))
            (status-line id info w null))
         ((enter)
            (mail id (tuple 'command-entered (reverse keys)))
            (mail id (tuple 'status-line null 1))
            (status-line id info w null))
         (else
            (status-line id info w keys)))))

(define (start-status-line id w)
   (mail id (tuple 'keep-me-posted))
   (status-line id 0 w null))

(define (status-pusher to)
   (mail to
      (tuple 'status-line (string->list (str "it's " (time-ms) " o-clock")) 3))
   (sleep 30000)
   (status-pusher to))

(define (maybe-put ff k v)
   (if v (put ff k v) ff))

(define (new-buffer-window id path w h)
   ;(print "new-buffer-window " id)
   (let ((status-thread-id (cons id 'status-line)))
      (thread id
         (led
            (put (empty-led-env id path)
               'status-thread-id status-thread-id)
             'command
            (if path
               (or
                  (file-buffer path)
                  (dir-buffer path))
               (string-buffer ""))
            1 1 w h))
      (link id)
      (link
         (thread status-thread-id
            (start-status-line id w)))
      id))

(define (input-terminal input target)
   (lfold
      (λ (_ thing)
         (log "input terminal: sending " thing " to " target)
         (mail target thing))
      'unused
      input))

(define (start-input-terminal target ll)
   (let ((name 'input-terminal))
      (thread name
         (input-terminal ll target))
      (link name)
      name))

(define (start-screen w h)
   ;(print "starting screen")
   (let ((name 'screen))
      (thread name (screen w h null))
      (link name)
      ;(clear-screen)
      name))

(define (refresh window)
   ;(clear-screen)
   (mail window (tuple 'refresh)))

;; manage threads and their views
;; decide which ones get to draw on screen

(define (close-buffer l r)
   (if (null? (cdr l))
      (if (null? r)
         (values #f #f)
         (values (list (car r)) (cdr r)))
      (values (cdr l) r)))

(define (ui l r i)
   (lets ((msg (wait-mail))
          (from msg msg))
      (log "ui: " msg " from " from)
      (cond
         ((eq? from 'input-terminal)
            (tuple-case msg
               ((ctrl x)
                  (cond
                     ((eq? x 'n)
                        (if (null? r)
                           (ui l r i)
                           (begin
                              (refresh (car r))
                              (ui (cons (car r) l) (cdr r) i))))
                     ((eq? x 'p)
                        (if (null? (cdr l))
                           (ui l r i)
                           (begin
                              (refresh (cadr l))
                              (ui (cdr l) (cons (car l) r) i))))
                     ((eq? x 'q) ;; close current buffer (from outside), may leave zombies for now
                        (lets ((l r (close-buffer l r)))
                           (if l
                              (begin
                                 (refresh (car l))
                                 (ui l r i))
                              0)))
                     ;((eq? x 'l)
                     ;   (refresh (car l))
                     ;   (ui l r i))
                     (else
                        (mail (car l) msg)
                        (ui l r i))))
               (else
                  (mail (car l) msg)
                  (ui l r i))))
         ((eq? (ref msg 1) 'new-buffer)
            (let ((new
                  (new-buffer-window
                     (list (if (ref msg 2) (ref msg 2) '*scratch*))
                     (ref msg 2)
                     (get i 'width 80)
                     (get i 'height 30))))
               (ui (cons new l) r i)))
         ((eq? (ref msg 1) 'buffer-closed)
            (lets ((l (keep (lambda (x) (not (eq? x from))) l))
                   (r (keep (lambda (x) (not (eq? x from))) r)))
               (if (null? l)
                  (if (null? r)
                     (halt 0)
                     (begin
                        (refresh (car r))
                        (ui (list (car r)) (cdr r) i)))
                  (begin
                     (refresh (car l))
                     (ui l r i)))))
         ((eq? from (car l))
            ;; forward print message from current window
            (mail 'screen msg)
            ;(print " - forwarding to screen")
            (ui l r i))
         ((eq? 'error (ref msg 1))
            (log "UI received ERROR: " msg " from " from)
            (ui l r i))
         ((eq? (ref msg 1) 'terminal-size)
            (lets ((_ w h msg))
               (map
                  (λ (id)
                     (log "ui: forwarding terminal-size to " id)
                     (mail id msg))
                  (append l r))
               (ui l r
                  (-> i
                     (put 'width w)
                     (put 'height h)))))
         (else
            ;(print-to 3 3 "waty" from)
            ;(print-to 3 4 "watz " msg)
            (ui l r i)))))

(define (pusher to)
   (sleep 30000)
   (mail to
      (tuple 'push
         (string->list
            (str "pushing to " to " on " (date-str (time)) "\n"))))
   (pusher to))

(define (start-ui w h paths)
   (print "Starting ui")
   (lets ((name 'ui)
          (buffers
             (if (null? paths)
                (list (new-buffer-window (list 'scratch) #f w h))
                (map (λ (x) (new-buffer-window x x w h)) paths))))
      (thread name
         (ui
            (list
               (car buffers))
            (cdr buffers)
            empty))
      (link name)
      ;; ui tells the size to client threads
      (mail 'ui (tuple 'terminal-size w h))
      name))

(define version-str "led v0.2a")

(define usage-text "led [args] [file-or-directory] ...")

(define command-line-rules
  (cl-rules
    `((help "-h" "--help" comment "show this thing")
      (version "-v" "--version" comment "show program version")
      (log "-L" "--log" has-arg comment "debug log file")
      ;(config "-c" "--config" has-arg comment "config file (default $HOME/.ledrc)")
      ;(faketerm #f "--terminal" has-arg comment "fake terminal input stream source")
      ;(record #f "--record" has-arg comment "record all terminal input to file")
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
      (else
         (lets ((input (terminal-input empty))
                (x y ll (get-terminal-size input)))
            (log "Terminal dimensions " (cons x y))
            (start-screen x y)
            (clear-screen)
            (start-input-terminal (start-ui x y args) ll)
            (log "Input terminal and UI running")
            (log "Screen running")
            (link (start-logger (get dict 'log)))
            ;(link (thread (pusher 'buff-2)))
            ;(link (thread (pusher 'buff-4)))
            ;(link (thread (status-pusher 'buff-1)))
            ;(link (thread (status-pusher 'buff-3)))
            (let loop ()
               (let ((mail (wait-mail)))
                  ;(print mail)
                  (log "CRASH " mail)
                  ;(write-bytes stderr (string->bytes (str mail "\n")))
                  ;(halt 1)
                  ;(loop)
                  ))))))

(define (main args)
   (process-arguments (cdr args) command-line-rules usage-text start-led-threads))

main




