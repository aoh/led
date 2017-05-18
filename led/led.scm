#!/usr/bin/ol --run

(import
  (led terminal)
  (led log)
  (led buffer)
  (led undo)
  (led node)
  (led system)
  (led search)
  (owl sys)
  (owl args))


;; temporary workaround until owl upgrade
(define (file? x) (let ((p (open-input-file x))) (if p (begin (close-port p) #true) #false)))

(define version-str "led v0.1a")

(define (output lst)
   (write-bytes stdout lst))

;;; Movement and insert mode edit operation

(define empty-buffer-line
   (tio
      (font-dim)
      (raw (render "~" null))
      (font-normal)))

(define (key x) (tuple 'key x))

;;;
;;; Screen update
;;;

(define (draw-lines-at-offset tl w dx y dy end lines)
   (cond
      ((null? lines) 
         (draw-lines-at-offset tl w dx y dy end
            (list empty-buffer-line)))
      ((eq? y end) tl)
      ((not (car lines)) ;; not drawn, leave on screen
         ;(log "not drawing screen line " y  ", buffer line " (+ y dy))
         (draw-lines-at-offset tl w dx (+ y dy) dy end (cdr lines)))
      (else
         ;(log "drawing screen line " y  ", buffer line " (+ y dy))
         (let ((these (drop-printable (car lines) dx)))
            ;(log "printable after " dx " of " (car lines) " is " (drop-printable (car lines) dx))
            (tio*
               (set-cursor 1 y)
               (clear-line-right)
               (raw (take-printable these w))
               (draw-lines-at-offset w dx (+ y dy) dy end (cdr lines))
               tl)))))

(define (meta-dimensions meta)
   (let ((glob (get meta 'global #empty)))
      (values 
         (get glob 'width 20)
         (get glob 'height 10))))

(define (update-screen buff)
   ;(log "FULL UPDATE SCREEN")
   (lets 
      ((u d l r x y off meta buff)
       (w h (meta-dimensions meta))
       (this (append (reverse l) r)))
      (tio
          (clear-screen)
          (draw-lines-at-offset w (car off) y -1 0 (cons this u))
          (draw-lines-at-offset w (car off) (+ y 1) +1 (+ h 1) d)
          (set-cursor x y))))

(define (maybe-car lst def) (if (pair? lst) (car lst) def))
(define (maybe-cdr lst) (if (pair? lst) (cdr lst) null))

(define (maybe-draw-lines-at-offset tl w dx y dy end lines old-lines)
   (cond
      ((eq? y end) 
         tl)
      ((null? lines) 
         (maybe-draw-lines-at-offset tl w dx y dy end
            (list empty-buffer-line) old-lines))
      ((equal? (car lines) (maybe-car old-lines empty-buffer-line))
         ;(log "not redrawing shared line at " y)
         (maybe-draw-lines-at-offset tl w dx (+ y dy) dy end (cdr lines) (maybe-cdr old-lines)))
      (else
         ;(log " - DIFFERENT from before " (car lines) ", old " (maybe-car old-lines empty-buffer-line))
         (let ((these (drop-printable (car lines) dx)))
            (tio*
               (set-cursor 1 y)
               (clear-line-right)
               (raw (take-printable these w))
               (maybe-draw-lines-at-offset w dx (+ y dy) dy end (cdr lines) (maybe-cdr old-lines))
               tl)))))

(define (same-line u d dy)
   (cond
      ((eq? dy 0) (values u d))
      ((> dy 0)
         (lets ((x u (uncons u empty-buffer-line)))
            (same-line u (cons x d) (- dy 1))))
      (else
         (lets ((x d (uncons d empty-buffer-line)))
            (same-line (cons x u) d (+ dy 1))))))

;; note: delta updates could be used to work around lack of clear-line-upto and allow

(define (delta-update-screen old new)
   (if (eq? old new)
      (begin
         (log " - full share")
         null)
      (lets 
         ((ou od ol or ox oy ooff meta old)
          (nu nd nl nr nx ny noff meta new)
          (w h (meta-dimensions meta))
          (old-this (append (reverse ol) or))
          (new-this (append (reverse nl) nr))
          (nu (cons new-this nu))
          (ou (cons old-this ou))
          (ou od (same-line ou od (- oy ny))))
         (if (equal? ooff noff)
            ;; no offset changes, so old lines may be intact
            (begin
               (tio
                  (maybe-draw-lines-at-offset w (car noff)    ny    -1      0  nu ou)
                  (maybe-draw-lines-at-offset w (car noff) (+ ny 1) +1 (+ h 1) nd od)
                  (set-cursor nx ny)))
            (update-screen new)))))


;;;
;;; Screen movement
;;;
    
(define (scroll-right buff)
   (lets 
      ((u d l r x y off meta buff)
       (w h (meta-dimensions meta))
       (dx dy off)
       (step (* 2 (div w 3)))
       (buff (buffer u d l r (- x step) y (cons (+ dx step) dy) meta)))
      buff))

(define (scroll-left buff)
   (lets 
      ((u d l r x y off meta buff)
       (w h (meta-dimensions meta))
       (dx dy off))
      (if (eq? dx 1)
        buff
        (lets
          ((step (min dx (* 2 (div w 3))))
           (buff (buffer u d l r (+ x step) y (cons (- dx step) dy) meta)))
          buff))))

(define (scroll-down buff)
   (lets 
    ((u d l r x y off meta buff)
     (w h (meta-dimensions meta))
     ;(step (+ 1 (* 2 (div h 3))))
     (step 1)
     (dx dy off)
     (buff 
      (buffer u d l r x (- y step) (cons dx (+ dy step)) meta)))
    buff))

(define (scroll-up buff)
   (lets 
    ((u d l r x y off meta buff)
     (w h (meta-dimensions meta))
     (dx dy off)
     (step (min dy 1))
     (buff 
      (buffer u d l r x (+ y step) (cons dx (- dy step)) meta)))
    buff))

(define (log-buff buff undo mode)
  (lets
    ((u d l r x y off meta buff)
     (w h (meta-dimensions meta))
     (dx dy off)
     (x (+ dx x))
     (y (+ dy y))
     (status (str "       " x "," y " " (if (eq? mode 'insert) "i" "c") 
                 (if (dirty-buffer? buff undo)
                    "*"
                    " ")))
     (poss (render status null)))
    ;(log "left " l ", right " r)
    ;(log "log: cursor at " (cons x y) " at offset " off ", line pos " (+ (car off) (- x 1)))
    (output
      (tio
         (cursor-save)
         (set-cursor (max 1 (- w (+ 1 (length poss)))) (+ h 1))
         (font-dim)
         (raw poss)
         (font-normal)
         (cursor-restore)))))

(define (notify buff txt)
   (lets 
      ((u d l r x y off meta buff)
       (w h (meta-dimensions meta)))
      (output
         (tio
            (set-cursor 1 (+ h 1))
            (clear-line)
            (font-dim)
            (raw (if (string? txt) (render txt null) txt))
            (font-normal)
            (cursor-restore)
            (set-cursor x y)))))

(define test-abbrs 
   (-> empty
      (put #\l
         (put empty #\z (reverse (string->list "λ"))))
      (put #\e
         (put empty #\z (list #x2205)))))

(define (add-abbreviation abbrs sfrom sto)
   (define (push tree chars val)
      (if (null? chars)
         (put tree 'abbreviation val)
         (put tree (car chars)
            (push (get tree (car chars) empty)
               (cdr chars) val))))
   (push abbrs (reverse (string->list sfrom))
      (reverse (string->list sto))))
            
(define (buff-add-abbreviation buff from to)
   (notify buff (str "abbreviating '" from "' -> '" to "'"))
   (let ((abbs (get-global-meta buff 'abbreviations #empty)))
      (put-global-meta buff 'abbreviations
         (add-abbreviation 
            (get-global-meta buff 'abbreviations #empty)
            from to))))
      
(define (abbreviate l abs)
   (log "abbreviate at " l " vs " abs)
   (cond
      ((eq? abs empty) #false)
      ((or (null? l) (not (word-char? (car l))))
         (let ((val (getf abs 'abbreviation)))
            (log "abbreviation led to " val)
            (if val
               (append val l)
               #false)))
      ((getf abs (car l)) =>
         (λ (val)
            (abbreviate (cdr l) val)))
      (else
         #false)))
         
(define (maybe-unabbreviate buff l)
   (or (abbreviate l (get-global-meta buff 'abbreviations #empty))
       l))
      
(define (insert-handle-key buff k)
   (lets 
      ((u d l r x y off meta buff)
       (w h (meta-dimensions meta)))
      (lets ((node (key-node k meta))
             (nw (node-width node)))
         (log "adding node " node)
         (if (< (+ x nw) w)
            (if (not (word-char? node))
               (lets ((lp (maybe-unabbreviate buff l))
                      (x (if (eq? l lp) x (+ 1 (text-width lp)))))
                  ;(log "insert of key " k " at " (cons x y) " = " node)
                  (buffer u d (cons node lp) r (+ x nw) y off meta))
               (buffer u d (cons node l) r (+ x nw) y off meta))
            (lets
               ((buff (scroll-right buff))
                (buff (insert-handle-key buff k)))
               buff)))))

(define (insert-backspace buff)
   (lets 
      ((u d l r x y off meta buff)
       (w h (meta-dimensions meta)))
      (if (null? l)
         ;; no-op (could also backspace to line above)
         (if (null? u)
            buff
            (if (eq? y 1)
               buff
               (lets
                  ((line u (uncons u null))
                   (line-len (printable-length line))
                   (q x (quotrem (+ line-len 1) w))
                   (xp (* q w))
                   (buffp
                     (buffer u d (reverse line) r x (- y 1) (cons xp (cdr off)) meta)))
                  (log "backspace")
                  buffp)))
         (let ((cw (node-width (car l))))
           (if (> x cw)
            (buffer u d (cdr l) r (- x cw) y off meta)
            (lets 
               ((buff (scroll-left buff))
                (buff (insert-backspace buff)))
               buff))))))


;; (a b c d ... n) 3 → (c b a) (d... n) delta, because some chars require more space
(define (seek-in-line line pos)
  (let loop ((line line) (pos pos) (l null))
    (cond
      ((eq? pos 0)
        (values l line 0))
      ((null? line)
         (log "warning: empty line at seek-in-line")
         (values l line pos))
      (else
        (lets 
          ((w (node-width (car line)))
           (pos (- pos w)))
          (if (> pos 0)
            (loop (cdr line) pos (cons (car line) l))
            (values (cons (car line) l) (cdr line) pos)))))))

(define (seek-in-list line pos)
   (let loop ((line line) (pos pos) (l null))
      (cond
         ((eq? pos 0)
            (values l line))
         ((null? line)
            (values l line))
         (else
            (loop (cdr line) (- pos 1) (cons (car line) l))))))

;; lines → u line d y
(define (seek-line lines end)
   (let loop ((lines lines) (pos end) (u null))
      (cond
         ((null? lines)
            (values u null lines (- end pos)))
         ((null? (cdr lines))
            (values u (car lines) (cdr lines) (- end pos)))
         ((eq? pos 0)
            (values u (car lines) (cdr lines) end))
         (else
            (loop (cdr lines) (- pos 1) (cons (car lines) u))))))

(define (step-zipper l r n)
   (cond
      ((eq? n 0) (values l r))
      ((> n 0)
         (if (null? r)
            (values l r)
            (step-zipper (cons (car r) l) (cdr r) (- n 1))))
      (else
         (if (null? l)
            (values l r)
            (step-zipper (cdr l) (cons (car l) r) (+ n 1))))))
            
(define (seek-line-end buff)
   (lets ((u d l r x y off meta buff)
          (w h (meta-dimensions meta))
          (step (>> w 1))
          (dx dy off))
      (if (null? r)
         buff
         (let loop ((l l) (r r) (x x) (dx dx))
            (cond
               ((null? (cdr r))
                  (buffer u d l r x y (cons dx dy) meta))
               ((eq? x w)
                  (loop l r (- x step) (+ dx step)))
               (else
                  (loop (cons (car r) l) (cdr r) (+ x (node-width (car r))) dx)))))))



;;;
;;; Text range operations
;;;

;; r d -> r' d' n-down n-from-left
(define (next-word start d n-left)
   (let loop ((r start) (d d) (y 0) (x 0) (space? #false))
      (log "loop " r "," y "," x "," space?)
      (cond
         ((null? r)
            (cond
               ((null? d)
                  (values null null y x))
               ((eq? n-left 1)
                  ;; do not eat the final newline after last word
                  (values r d y x))
               ;(space?
               ;   (loop (car d) (cdr d) (+ y 1) 0 #t))
               (else
                  (loop (car d) (cdr d) (+ y 1) 0 #t))))
         ((space-char? (car r))
            (loop (cdr r) d y (+ x 1) #t))
         ((word-delim-char? (car r))
            (if (eq? r start)
               ;; consume the one delimiter char and potential whitespace
               (loop (cdr r) d y (+ x 1) #true)
               ;; otherwise stop here
               (values r d y x)))
         (space?
            (values r d y x))
         (else
            (loop (cdr r) d y (+ x 1) #f)))))

;; r d n -> dy dx r' d'
(define (next-words r d n)
   (let loop((y 0) (x 0) (r r) (d d) (n n))
      (if (eq? n 0)
         (values y x r d)
         (lets ((r d dy dx (next-word r d n)))
            (if (eq? dy 0)
               (loop y (+ x dx) r d (- n 1))
               (loop (+ y dy) dx r d (- n 1)))))))

;; buff movement-exp -> n | dy dx
(define (movement buff n type)
   (lets ((u d l r x y off meta buff))
      (cond
         ((eq? type 'end-of-line)
            (values (- n 1) (length r)))
         ((eq? type 'beginning-of-line)
            (values (- n 1) (- 0 (length l))))
         ((eq? type 'word)
            (lets ((y x r d (next-words r d n)))
               (values y x)))
         (else
            (log "unknown movement type " type)
            (values #f #f)))))
      
(define space-key (tuple 'key #\space))
     
(define (key->digit k)
   (if (eq? (ref k 1) 'key)
      (let ((n (- (ref k 2) #\0)))
         (cond
            ((< n 0) #false)
            ((> n 9) #false)
            (else n)))
      #false))

;; ll def -> n ll' | def ll
(define (maybe-get-count ll def)
   (lets ((k ll (uncons ll space-key))
          (n (key->digit k)))
      ;(log "maybe-get-count: " k " -> " n)
      (cond
         ((not n) (values def (cons k ll)))
         ((eq? k space-key) (values def ll))
         ((eq? n 0) (values def (cons k ll)))
         (else
            (let loop ((n n) (ll ll))
               (lets ((x ll (uncons ll 0))
                      (m (key->digit x)))
                  (if m
                     (loop (+ (* 10 n) m) ll)
                     (values n (cons x ll)))))))))

;; buff -> dy dx | #f #f, use new movement deltas
(define (movement-matching-paren-forward buff)
   (lets ((u d l r x y off meta buff))
      (let loop ((x 0) (y 0) (r r) (d d) (depth 0))
         (cond
            ((null? r)
               (if (null? d)
                  (values #f #f)
                  (loop 0 (+ y 1) (car d) (cdr d) depth)))
            ((right-paren? (car r))
               (if (eq? depth 1)
                  (values y (+ x 1))
                  (loop (+ x 1) y (cdr r) d (- depth 1))))
            ((left-paren? (car r))
               (loop (+ x 1) y (cdr r) d (+ depth 1)))
            (else
               (loop (+ x 1) y (cdr r) d depth))))))

(define sexp-key #\s)

(define eof (tuple 'eof))

(define (get-key ll)
   (lets ((x ll (uncons ll eof)))
      (values ll
         (if (eq? (ref x 1) 'key)
            (ref x 2)
            #false))))

(define (increase n)
   (+ n (if (> n 0) +1 -1)))

(define (get-relative-movement ll buff r self)
   (lets ((np ll (maybe-get-count ll 1))
          (n (* np r)) ;; 6dw = d6w = 3d2w
          (op ll (uncons ll eof)))
      (tuple-case op
         ((key k)
            (cond
               ;((eq? k #\k) 
               ;   (values ll (- 0 n) 0))
               ;((eq? k #\h) 
               ;   (values ll 0 (- 0 n)))
               ((eq? k #\j) 
                  (values ll n 0))
               ((eq? k #\l) 
                  (values ll 0 n))
               ((eq? k #\$)
                  (lets ((u d l r x y off meta buff))
                     (values ll 0 (length r))))
               ((eq? k #\w) 
                  (lets ((u d l r x y off meta buff)
                         (dy dx rp dp (next-words r d n)))
                      (values ll dy dx)))
               ((eq? k #\%) 
                  (lets ((dy dx (movement-matching-paren-forward buff)))
                     (if dy
                        (values ll dy dx)
                        (values ll #f #f))))
               ((eq? k sexp-key) 
                  (lets ((dy dx (movement-matching-paren-forward buff)))
                     (if dy
                        (values ll dy dx)
                        (values ll #f #f))))
               ((eq? k #\') ;; from cursor to mark
                  (lets ((ll k (get-key ll)))
                     (cond
                        ((not k)
                           (log "no key")
                           (values ll #f #f))
                        ((get (get-buffer-meta buff 'marks #empty) k #false) =>
                           (λ (pos)
                              (lets ((u d l r x y off meta buff)
                                     (dx dy off)
                                     (mx my pos)
                                     (tx (+ (- x 1) dx))
                                     (ty (+ (- y 1) dy)))
                                 (log "cursor is at " (cons x y))
                                 (log "offset is " (cons dx dy))
                                 (log "relative movement from mar " pos " to this " (cons tx ty))
                                 (log "relative movement up to pos" pos)
                                 ;; include cursor position at mark
                                 (cond
                                    ((= my ty)
                                       (values ll 0 (increase (- mx tx))))
                                    ((< my ty)
                                       (values ll (- my ty) mx))
                                    (else
                                       (values ll (- my ty) (increase mx)))))))
                        (else
                           (values ll #f #f)))))
               (else
                  (log "get-relative-movement confused: " n ", " k ", op was " k)
                  (values ll #false #false))))
         (else
            (log "get-relative-movement confused: " op)
            (values ll #f #f)))))

(define (cut-backward-multiline u l dy dx)
   (lets ((next-lines u (split u (- dy 1)))
          (new-l u (uncons u null))
          (new-l last-partial (split new-l dx)))
      (values u (reverse new-l)
         (tuple 'line-sequence
            (cons last-partial (reverse (cons (reverse l) next-lines)))))))

(define (cut-forward r d dy dx)
   (if (eq? dy 0)
      (lets ((cutd r (split r dx)))
         (values r d (tuple 'sequence cutd)))
      (lets ((next-lines d (split d (- dy 1)))
             (new-r d (uncons d null))
             (last-partial new-r (split new-r dx)))
         (values new-r d 
            (tuple 'line-sequence 
               (cons r (append next-lines (list last-partial))))))))

;; move buffer left if x is off screen
(define (maybe-scroll-left buff)
   (if (> (buffer-x buff) 0)
      buff
      (lets ((u d l r x y off meta buff)
             (w h (meta-dimensions meta))
             (dx dy off))
         (let loop ((x x) (dx dx))
            (cond
               ((> x 0)
                  (buffer u d l r x y (cons dx dy) meta))
               (else
                  (let ((step (min dx w)))
                     (loop (+ x step) (- dx step)))))))))
      
(define (cut-relative-movement ll buff dy dx)
   (lets ((u d l r x y off meta buff))
      (cond
         ((eq? dy 0)
            (if (< dx 0)
               ;; cut backwards, one line
               (lets ((l r (step-zipper l r +1)) ;; include cursor position
                      (rcut l (split l (* dx -1))))
                  (values ll
                     (maybe-scroll-left 
                        (buffer u d l r (+ 1 (- x (printable-length rcut))) y off meta))
                     (tuple 'sequence (reverse rcut))))
               ;; cut forwards, oneline
               (lets ((r d cut (cut-forward r d dy dx)))
                  (values ll (buffer u d l r x y off meta) cut))))
         ((< dy 0)
            ;; cut backwards, multiple lines
            (lets 
               ((l r (step-zipper l r 1)) ;; include char at cursor if there
                (u l cut (cut-backward-multiline u l (* -1 dy) dx)))
               ;; fixme, scrolling
               (values ll (buffer u d l r (+ 1 (printable-length l)) y off meta) cut)))
         (else
            (lets 
               ((r d cut (cut-forward r d dy dx)))
               (log "cut forwards")
               (values ll (buffer u d l r (+ 1 (printable-length l)) y off meta) cut))))))

(define (seek-line-start buff)
   (lets ((u d l r x y off meta buff)
          (dx dy off)
          (buffp 
            (buffer u d null (append (reverse l) r) 1 y (cons 0 dy) meta)))
         buffp))

;; row+1 = y + dy, dy = row + 1 - y
(define (buffer-seek buff x y screen-y)
   (log "buffer seek" x "," y ", y row at " screen-y)
   (lets ((u d l r old-x old-y off meta buff)
          (w h (meta-dimensions meta))
          (lines (append (reverse u) (list (append (reverse l) r)) d))
          (u line d y (seek-line lines y))
          (step (>> w 1))
          (yp (or screen-y (if (< y h) (+ y 1) (>> h 1)))) ;; real or middle of screen
          (dy (- (+ y 1) yp))
          (buff (buffer u d null line 1 yp off meta)))
         ;; seek right
         (let loop ((xp 1) (pos x) (l null) (r line) (dx 0))
            (cond
               ((>= xp w)
                  (loop (- xp step) pos l r (+ dx step)))
               ((eq? pos 0)
                  (buffer u d l r xp yp (cons dx dy) meta))
               ((null? r)
                  (loop xp 0 l r dx))
               (else
                  (loop (+ xp (node-width (car r))) (- pos 1) (cons (car r) l) (cdr r) dx))))))

;; move line down within the same screen preserving cursor position if possible
(define (line-down buff ip)
   (lets ((u d l r x y off meta buff)
          (w h (meta-dimensions meta))
          (dx dy off)
          (line (append (reverse l) r))
          (u (cons line u))
          (y (+ y 1))
          (line d (uncons d null))
          (x (min x (+ (if ip 1 0) (- (printable-length line) (car off)))))
          (line-pos (+ (- x 1) (car off)))
          (l r offset (seek-in-line line line-pos)))
        ;(log "next line length is " (printable-length line) ", x=" x ", dx=" (car off) ", l='" (list->string l) "', r='" (list->string r) "', offset " offset) 
      (buffer u d l r (- x offset) y off meta)))

;; move line up within the same screen preserving cursor position if possible
(define (line-up buff ip)
   (lets ((u d l r x y off meta buff)
          (w h (meta-dimensions meta))
          (dx dy off)
          (line (append (reverse l) r))
          (d (cons line d))
          (y (- y 1))
          (line u (uncons u null))
          (x (min x (+ (if ip 1 0) (- (printable-length line) (car off)))))
          (line-pos (+ (- x 1) (car off)))
          (l r offset (seek-in-line line line-pos)))
        ;(log "line-up went to (x . y) " (cons x y))
        ;(log "next line length is " (printable-length line) ", x=" x ", dx=" (car off) ", l='" (list->string l) "', r='" (list->string r) "'")
      (buffer u d l r (- x offset) y off meta)))
 
(define (move-arrow buff dir ip)
   (lets ((u d l r x y off meta buff)
          (w h (meta-dimensions meta)))
      ;(log "arrow " dir " from " (cons x y) ", dim " (cons w h))
      (cond
         ((eq? dir 'up)
            (cond
               ((null? u) buff)
               ((eq? y 1)
                  (move-arrow (scroll-up buff) dir 'up))
              ((not (eq? 0 (car off))) ;; there is x-offset
                (let ((next-len (printable-length (car u))))
                  (if (< next-len (car off)) ;; next line start not visible
                    (lets ;; dummy version
                      ((buff (move-arrow buff 'left ip))
                       (buff (move-arrow buff dir ip)))
                      buff)
                    (line-up buff ip))))
               (else
                 (lets ((buff (line-up buff ip)))
                    buff))))
         ((eq? dir 'down)
            (cond
              ((null? d) buff)
              ((>= y h) (move-arrow (scroll-down buff) dir 'down))
              ((not (eq? 0 (car off))) ;; there is x-offset
                (let ((next-len (printable-length (car d))))
                  (if (< next-len (car off)) ;; next line start not visible
                    (lets ;; dummy version
                      ((buff (move-arrow buff 'left ip))
                       (buff (move-arrow buff dir ip)))
                      buff)
                    (line-down buff ip))))
              (else
                (lets ((buff (line-down buff ip)))
                   buff))))
         ((eq? dir 'left)
            (if (null? l)
               buff
               (let ((step (node-width (car l))))
                  (if (<= x step)
                     (lets
                       ((buff (scroll-left buff))
                        (buff (move-arrow buff dir ip)))
                        buff)
                     (buffer u d (cdr l) (cons (car l) r) (- x step) y off meta)))))
         ((eq? dir 'right)
            (if (null? r)
               buff
               (let ((step (node-width (car r))))
                  (if (< (+ x step) w)
                     (buffer u d (cons (car r) l) (cdr r) (+ x step) y off meta)
                     (lets
                       ((buff (scroll-right buff))
                        (buff (move-arrow buff dir ip)))
                       buff)))))
         ((eq? dir 'command-right) ;; command mode
            (if (or (null? r) (null? (cdr r)))
               buff
               (let ((step (node-width (car r))))
                  (if (< (+ x step) w)
                     (buffer u d (cons (car r) l) (cdr r) (+ x step) y off meta)
                     (lets
                       ((buff (scroll-right buff))
                        (buff (move-arrow buff dir ip)))
                       buff)))))
         (else
            (log "odd line move: " dir)
            buff))))

(define (cut-lines ll buff n)
   (lets 
      ((u d l r x y off meta buff)
       (d (cons (append (reverse l) r) d))
       (taken d (split d n))
       (l null))
      (if (null? d)
         ;; need to move up, unless u is null
         (if (null? u)
            (values ll
               (buffer u d null null 1 1 '(0 . 0) meta)
               (tuple 'lines taken))
            (lets ((buff (move-arrow buff 'up #f))
                   (u d l r x y off meta buff))
               (values ll
                  (buffer u (cdr d) l r x y off meta)
                  (tuple 'lines taken))))
         (lets ((r d (uncons d null)))
            (values ll
               (buffer u d l r 1 y (cons 0 (cdr off)) meta)
               (tuple 'lines taken))))))
   
;; ll buff rep self -> ll' buff' tob|#false
(define (cut-movement ll buff r self)
   (lets ((np ll (maybe-get-count ll 1))
          (n (* np r))
          (op ll (uncons ll eof)))
       (tuple-case op
          ((key k)
             (cond
                ((eq? self k)
                   ;; cut lines via shortcut
                   (log "cut-lines")
                   (cut-lines ll buff n))
                (else
                   (lets ((ll dy dx (get-relative-movement (cons op ll) buff n self)))
                      (log "relative movement for cut is " (cons dy dx))
                      (cond
                         ((not dy)
                            (values ll buff #false))
                         (else
                            (log "cutting relative")
                            (cut-relative-movement ll buff dy dx)))))))
          (else
             (values ll buff #false)))))

;; buff → (x . y) | #false
(define (seek-matching-paren-back buff left? right?)
   (lets ((u d l r x y off meta buff))
      (let loop 
         ((x (length l)) (y (+ (cdr off) (- y 1))) (l l) (u u) (depth 1))
         (cond
            ((null? l)
               (if (null? u)
                  #false
                  (loop (length (car u)) (- y 1) (reverse (car u)) (cdr u) depth)))
            ((left? (car l))
               (if (eq? depth 1)
                  (cons (- x 1) y)
                  (loop (- x 1) y (cdr l) u (- depth 1))))
            ((right? (car l))
               (loop (- x 1) y (cdr l) u (+ depth 1)))
            (else
               (loop (- x 1) y (cdr l) u depth))))))

(define (seek-matching-paren-forward buff left? right?)
   (lets ((u d l r x y off meta buff))
      (let loop 
         ((x (length l)) (y (+ (cdr off) (- y 1))) (r r) (d d) (depth 0))
         (cond
            ((null? r)
               (if (null? d)
                  #false
                  (loop 0 (+ y 1) (car d) (cdr d) depth)))
            ((right? (car r))
               (if (eq? depth 1)
                  (cons x y)
                  (loop (+ x 1) y (cdr r) d (- depth 1))))
            ((left? (car r))
               (loop (+ x 1) y (cdr r) d (+ depth 1)))
            (else
               (loop (+ x 1) y (cdr r) d depth))))))


;; (codepoint ...) regex x -> x' | #false

(define (search-from-line code-points regex x)
   ;(log "searching line " (list->string code-points))
   (let loop ((l code-points) (x x))
      ;(log "search at " (list->string l))
      (cond
         ((null? l) #false)
         ((regex l) x)
         (else (loop (cdr l) (+ x 1))))))

;; node-list (node-list ...) regex x y -> x' y' | #f #f

(define (search-from l ls regex x y)
   (cond
      ((search-from-line (line->code-points l) regex x) =>
         (λ (x) (values x y)))
      ((null? ls)
         (values #f #f))
      (else
         (search-from (car ls) (cdr ls) regex 0 (+ y 1)))))
            
(define (find-next buff)
   (lets ((u d l r x y off meta buff)
          (dx dy off)
          (regex (get meta 'search-regex (λ (x) #false)))
          (row (+ (length u) 1))
          (_ rt (uncons r #false)) ;; move one char forward
          (mx my 
            (search-from rt d regex (+ x dx) (+ (- y 1) dy))))
         (if mx
            (values (buffer-seek buff mx my #false) #false)
            (lets
               ((this (append (reverse l) r))
                (up (reverse (cons this u)))
                (mx my (search-from (car up) (cdr up) regex 0 0)))
               (if mx
                  (values (buffer-seek buff mx my #false) "search wrapped")
                  (values buff "no matches"))))))

(define (nodes->bytes nodes)       (foldr render-node null nodes))

(define (nodes->code-points nodes) (foldr render-code-point null nodes))

(define (buffer->bytes buff)
   (lets ((u d l r x y off meta buff))
      (nodes->bytes
         (foldr 
            (λ (line tl)
               (append line (cons #\newline tl)))
            null
            (append (reverse u) (list (append (reverse l) r)) d)))))

(define (paste-lines-below buff lines)
   (lets ((u d l r x y off meta buff))
      (buffer u (append lines d) l r x y off meta)))

(define (paste-sequence-after buff lst)
   (lets ((u d l r x y off meta buff))
      (if (null? r)
         (buffer u d l lst x y off meta)
         (lets ((this r r))
            ;; paste after cursor
            (buffer u d l (cons this (append lst r)) x y off meta)))))

(define (paste-sequence-before buff lst)
   (lets ((u d l r x y off meta buff))
      (buffer u d l (append lst r) x y off meta)))

(define (maybe-join-partials a b d)
   (let ((new (append a b)))
      (if (null? new)
         d
         (cons new d))))
   
;; paste 0-n lines with partial ones at both ends
(define (paste-line-sequence buff lst)
   (lets ((u d l r x y off meta buff)
          (this lst (uncons lst null))
          (fulls lasts (split lst (- (length lst) 1)))
          (last _ (uncons lasts null)))
      (if (and (null? fulls) (null? last))
          ;; special case, no new lines
         (paste-sequence-after buff this)
         (if (null? r)
            (buffer u
               (append fulls (maybe-join-partials last r d))
               l this x y off meta)
            (lets ((current r r)) ;; paste after cursor if content
               (buffer u 
                  (append fulls (maybe-join-partials last r d))
                  l (cons current this) x y off meta))))))
               
;;
;; Data structures
;;   yank = #(lines <buffer lines>)
;;        | #(sequence <sequence of line data>)
;;        | #(range <end of line>|#false <buffer lines> <line start>|#false)
;;

;; line lines depth -> sexp-lines lines'
(define (cut-sexp line lines depth)
   (let loop ((l line) (ls lines) (cl null) (cls null) (d depth))
      (cond
         ((null? l)
            (if (null? ls)
               (values #false #false)
               (loop (car ls) (cdr ls) null (cons cl cls) d)))
         ((right-paren? (car l))
            (if (eq? d 1)
               (values
                  (reverse (map reverse (cons (cons (car l) cl) cls)))
                  (cons (cdr l) ls))
               (loop (cdr l) ls (cons (car l) cl) cls
                  (- d 1))))
         (else
            (loop (cdr l) ls (cons (car l) cl) cls
               (+ d (if (left-paren? (car l)) 1 0)))))))

;; fixme: placeholds
(define (lines->yank lines)
   (cond
      ((null? (cdr lines))
          (tuple 'sequence (car lines)))
       (else
          (tuple 'lines lines))))
   
   
;; cut forward, for backward move to corresponding open paren and use this
;; buff -> buff' msg
(define (buffer-cut-sexp buff)
   (lets ((u d l r x y off meta buff)
          (sexp lines (cut-sexp r d 0)))
      (if sexp
         (lets 
            ((r d (uncons lines null))
             (buff (buffer u d l r x y off meta)))
            (values
               (put-global-meta buff 'yank (lines->yank sexp))
               "Copied to yank"))
         (values buff "Bad range"))))

(define (paste-yank buff)
   (lets ((data (get-global-meta buff 'yank #false)))
      (cond
         ((not data)
            buff)
         ((eq? 'lines (ref data 1))
            (log "appending lines from buffer")
            (lets ((buff (paste-lines-below buff (ref data 2)))
                   (buff (move-arrow buff 'down #f)))
               buff))
         ((eq? 'sequence (ref data 1))
            (log "appending sequence from buffer")
            (lets ((buff (paste-sequence-after buff (ref data 2))))
               buff))
         ((eq? 'line-sequence (ref data 1))
            (log "appending line sequence " (ref data 2) " from buffer")
            (paste-line-sequence buff (ref data 2)))
         (else
            (error "how do i paste " data)))))
 
(define (mark-position buff char)
   (lets 
      ((u d l r x y off meta buff)
       (dx dy off)
       (mark-x (+ (- x 1) dx))
       (mark-y (+ (- y 1) dy))
       (marks (get meta 'marks #empty)))
      (log "marked" (list->string (list char)) " as " (cons mark-x mark-y))
      (put-buffer-meta buff 'marks
         (put marks char (cons mark-x mark-y)))))

(define (write-buffer buff path)
   (log "writing to " path)
   (cond
      ((not path)
         (values #false
            (foldr render null
               (list "Give me a name for this"))))
      ((directory? path)
         (values #false
            (foldr render null
               (list "'" path "' is a directory"))))
      (else
         (lets
            ((port (open-output-file path))
             (lst (buffer->bytes buff))
             (n (length lst))
             (res (if port (byte-stream->port lst port) #f)))
            (if port 
               (close-port port))
            (if res
               (values #true
                  (foldr render null 
                     (list "Wrote " n " bytes to '" path "'")))
               (values #false
                  (foldr render null
                     (list "Failed to write to '" path "'"))))))))

(define (maybe-keep-y old-y old-y-pos new-y h)
   (lets ((delta (- new-y old-y))
          (rel-pos (+ old-y-pos delta)))
      (cond
         ((< rel-pos 1) #false)  ;; above screen, center on it
         ((< rel-pos h) rel-pos) ;; on screen, ask y to be there
         (else #false))))        ;; below screen

(define (paren-hunter buff seeker left? right?)
   (lets 
      ((u d l r x y off meta buff)
       (w h (meta-dimensions meta))
       (yp (+ (cdr off) (- y 1))))      ;; yp is at row y on screen currently
      (lets ((match (seeker buff left? right?)))
         (log "matching open paren result " match)
         (if match
            (lets ((mx my match)
                   (buffp (buffer-seek buff mx my (maybe-keep-y yp y my h))))
               buffp)
            buff))))

(define (is? x) 
   (λ (y) (eq? x y)))

(define (maybe-seek-matching-paren buff)
   (lets ((u d l r x y off meta buff))
      (cond
         ((null? r)
            buff)
         ((right-paren? (car r))
            (paren-hunter buff seek-matching-paren-back left-paren? right-paren?))
         ((left-paren? (car r))
            (paren-hunter buff seek-matching-paren-forward left-paren? right-paren?))
         ((eq? (car r) #\{)
            (paren-hunter buff seek-matching-paren-forward (is? #\{) (is? #\})))
         ((eq? (car r) #\})
            (paren-hunter buff seek-matching-paren-back (is? #\{) (is? #\})))
         ((eq? (car r) #\[)
            (paren-hunter buff seek-matching-paren-forward (is? #\[) (is? #\])))
         ((eq? (car r) #\])
            (paren-hunter buff seek-matching-paren-back (is? #\[) (is? #\])))
         (else
            (log "seek-matching-paren: current is " (car r))
            buff))))

;;;
;;; Command mode actions
;;;



(define (command-regex-search ll buff undo mode r cont)
   (output (tio* (set-cursor 1 (+ 1 (screen-height buff))) (clear-line) (list #\/)))
   (lets ((search-history 
            (get (buffer-meta buff) 'search-history null))
          (ll res (readline ll search-history
                     2 (+ 1 (screen-height buff)) (screen-width buff))))
         (if res
            (lets
               ((buff 
                  (if (equal? res "") buff
                     (put-buffer-meta buff 'search-history 
                        (cons res search-history))))
                (regex ;; note: ^ and $ need special handling later
                  (string->regex (str "m/^" res "/"))))
               (if regex
                  (lets
                     ((buff 
                        (if (equal? res "")
                           buff
                           (-> buff (put-buffer-meta 'search-regex regex))))
                      (buffp msg
                        (find-next buff)))
                     (cont ll buffp undo mode (or msg "")))
                  (cont ll buff undo mode "invalid regexp")))
            (cont ll buff undo mode "canceled"))))

(define (command-find-next ll buff undo mode r cont)
   (lets ((buffp msg (find-next buff)))
      (cont ll buffp undo mode (or msg ""))))

(define (command-mark-position ll buff undo mode r cont)
   (notify buff "mark location")
   (lets ((msg ll (uncons ll #false)))
      (if (eq? (ref msg 1) 'key)
         (let ((char (ref msg 2)))
            (cont ll (mark-position buff char) undo mode 
               (str "marked '" (list->string (list char)) "'")))
         (cont ll buff undo mode))))

(define (command-line-end ll buff undo mode r cont)
   (lets ((buff (seek-line-end buff)))
      (cont ll buff undo mode)))

(define (command-line-start ll buff undo mode r cont)
   (lets ((buff (seek-line-start buff)))
      (cont ll buff undo mode)))

(define (command-move-down ll buff undo mode r cont)
   (lets ((buff (move-arrow buff 'down #f)))
      (cont ll buff undo mode)))

(define (command-move-up ll buff undo mode r cont)
   (lets ((buff (move-arrow buff 'up #f))) 
      (cont ll buff undo mode)))

(define (command-move-right ll buff undo mode r cont)
   (lets ((buff (move-arrow buff 'command-right #f))) 
      (cont ll buff undo mode)))

(define (command-move-left ll buff undo mode r cont)
   (lets ((buff (move-arrow buff 'left #f))) 
      (cont ll buff undo mode)))

(define (command-seek-matching-paren ll buff undo mode r cont)
   (lets ((buff (maybe-seek-matching-paren buff))) 
      (cont ll buff undo mode)))

(define (command-paste-after ll buff undo mode r cont)
   (lets ((undo (push-undo undo buff))
          (buffp (paste-yank buff)))
      (cont ll buffp undo mode "pasted")))

(define blank-yank
   (tuple 'sequence null))

(define (command-paste-before ll buff undo mode r cont)
   (tuple-case (get-global-meta buff 'yank blank-yank)
      ((sequence nodes)
         ;; move left, paste, move right
         (lets
            ((undo (push-undo undo buff))
             (buff (paste-sequence-before buff nodes)))
            (cont ll buff undo mode "pasted")))
      ((lines nodes)
         (command-paste-after ll (move-arrow buff 'up #f) undo mode r 
            (λ (ll buff undo mode msg)
               (cont ll (move-arrow buff 'down #f) undo mode msg))))
      (else
         (log "unknown yank buffer type in paste-before"))))

(define (command-add-line-below ll buff undo mode r cont)
   (cont (ilist (tuple 'key #\A) (tuple 'enter) ll) buff undo mode))

(define (command-add-line-above ll buff undo mode r cont)
   (cont (ilist (tuple 'key #\k) (tuple 'key #\o) ll) buff undo mode))

(define (map-n op n lst)
   (cond
      ((null? lst) lst)
      ((eq? n 0) lst)
      (else
         (cons (op (car lst))
            (map-n op (- n 1) (cdr lst))))))

;; todo: put to meta later (based on shiftwidth)
(define shift-lst (list #\space #\space #\space))

(define (keys ll key n)
   (if (= n 0)
      ll
      (cons (tuple 'key key)
         (keys ll key (- n 1)))))

(define (command-keys lst n)
   (if (null? lst)
      n
      (cons (tuple 'esc)
         (append
            (map (λ (x) (tuple 'key x)) lst)
            (cons (tuple 'enter) n)))))

(define (line-left l r n)
   (cond
      ((eq? l null) (values l r))
      ((eq? n 0) (values l r))
      (else (line-left (cdr l) (cons (car l) r) (- n 1)))))

(define (line-right l r n)
   (cond
      ((eq? r null) (values l r))
      ((eq? n 0) (values l r))
      (else (line-right (cons (car r) l) (cdr r) (- n 1)))))

(define (indented-depth nodes)
   (let loop ((l nodes) (n 0))
      (cond
         ((null? l) n)
         ((eq? (car l) #\space)
            (loop (cdr l) (+ n 1)))
         (else n))))

;; indent to next multiple of 'tabstop
(define (indent-lines buff n)
   (lets
      ((u d l r x y off meta buff)
       (current (indented-depth (append (reverse l) r)))
       (tabstop (get meta 'tabstop 3))
       (shift-n (- tabstop (remainder current tabstop))) ;; move to next multiple of tabstop
       (shift-lst (map (λ (x) #\space) (iota 0 1 shift-n))) ;; content to add
       (l (append l shift-lst))
       (l r (line-left l r shift-n)) ;; move cursor to make x valid again
       (d (map-n (λ (x) (append shift-lst x)) (- n 1) d)))
      (values
         (buffer u d l r x y off meta)
         shift-n)))
   
(define (command-indent ll buff undo mode n cont)
   (lets 
      ((range ll (uncons ll #false))
       (undop (push-undo undo buff)))
      (cond
         ((equal? range (tuple 'key #\>))
            (lets ((buffp indented (indent-lines buff n)))
               (cont (keys ll #\l indented) buffp undop mode)))
         ((equal? range (tuple 'key #\%))
            (lets ((dy dx (movement-matching-paren-forward buff))
                   (buffp indented (indent-lines buff (+ dy 1)))) ;; current line + dy down
               (cont (keys ll #\l indented) buffp undop mode)))
         ((equal? range (tuple 'key sexp-key))
            (lets ((dy dx (movement-matching-paren-forward buff))
                   (buffp indented (indent-lines buff (+ dy 1)))) ;; current line + dy down
               (cont (keys ll #\l indented ) buffp undop mode)))
         (else
            (log "No such shift range: " range)
            (cont ll buff undo mode)))))

;; drop prefix of lst, if there
(define (drop-prefix lst prefix)
   (cond
      ((null? prefix) lst)
      ((null? lst) #false)
      ((eq? (car lst) (car prefix))
         (drop-prefix (cdr lst) (cdr prefix)))
      (else #false)))

(define (unindent lst)
   (or (drop-prefix lst shift-lst) lst))

(define (unindent-lines buff n)
   (lets
      ((u d l r x y off meta buff)
       (rlp (drop-prefix (reverse l) shift-lst)))
      (if rlp
         (lets ((l (reverse rlp))
                (l r (line-right l r 3))
                (d (map-n unindent (- n 1) d))
                (buffp (buffer u d l r x y off meta)))
             buffp)
          buff)))
         
(define (command-unindent ll buff undo mode n cont)
   (lets ((range ll (uncons ll #false)))
      (cond
         ((equal? range (tuple 'key #\<))
            (lets ((buffp (unindent-lines buff n)))
               (log "foo")
               (cont (keys ll #\h 3) buffp (push-undo undo buff) mode)))
         ((equal? range (tuple 'key #\%))
            (lets ((dy dx (movement-matching-paren-forward buff))
                   (buffp (unindent-lines buff (+ dy 1)))) ;; current line + dy down
               (if (eq? buff buffp)
                  (cont ll buff undo mode)
                  (cont (keys ll #\h 3) buffp (push-undo undo buff) mode))))
         ((equal? range (tuple 'key sexp-key))
            (lets ((dy dx (movement-matching-paren-forward buff))
                   (buffp (unindent-lines buff (+ dy 1)))) ;; current line + dy down
               (if (eq? buff buffp)
                  (cont ll buff undo mode)
                  (cont (keys ll #\h 3) buffp (push-undo undo buff) mode))))
         (else
            (cont ll buff undo mode "unsupported range")))))

(define (command-delete-char ll buff undo mode r cont)
   (lets
      ((undo (push-undo undo buff))
       (u d l r x y off meta buff))
      (if (null? r)
         (if (null? l)
            (cont ll buff undo mode)
            (lets ((buffp (insert-backspace buff)))
               (cont ll 
                  (put-global-meta buffp 'yank (tuple 'sequence (list (car l))))
                  undo mode)))
         (lets ((buffp (buffer u d l (cdr r) x y off meta)))
            (cont ll 
               (put-global-meta buffp 'yank (tuple 'sequence (list (car r))))
               undo mode)))))

(define (command-join-lines ll buff undo mode n cont)
   (lets
      ((undo (push-undo undo buff))
       (buff (seek-line-end buff))
       (n (if (number? n) n 1)) ;; fixme: no interval handling
       (u d l r x y off meta buff))
      (let loop ((r r) (d d) (n n))
         (cond
            ((or (null? d) (eq? n 0))
               (let ((buffp (buffer u d l r x y off meta)))
                  (cont ll buffp undo mode)))
            (else   
               (lets
                  ((tail (drop-leading-whitespace (car d)))
                   (tail (if (whitespace? (last r #\a)) tail (cons #\space tail))))
                  (loop (append r tail) (cdr d) (- n 1))))))))

(define (command-maybe-save-and-close ll buff undo mode n cont)
   (notify buff "press Z again to save and close")
   (lets ((chr ll (uncons ll #false)))
      (if (equal? chr (tuple 'key #\Z))
         (lets ((path (buffer-path buff #false))
                (ok? msg (write-buffer buff path)))
            (if ok?
               ;; exit to led-buffers
               (values ll buff undo mode 'close)
               (cont ll buff undo mode msg)))
         (cont ll buff undo mode "close aborted"))))

(define (command-save ll buff undo mode n cont)
   (lets ((path (buffer-path buff #false))
          (ok? msg (write-buffer buff path))
          (msg (list->string msg)))
      (if ok?
         (cont ll buff (mark-saved undo buff (time-ms)) mode msg)
         (cont ll buff undo mode msg))))

(define (command-close-buffer ll buff undo mode n cont)
   ;; todo: add dirtiness check
   (if (dirty-buffer? buff undo)
      (begin
         ;; todo: search the changes to see what is about to be lost
         (notify buff "Unsaved changes. Press Q again to close anyway.")
         (lets ((chr ll (uncons ll #false)))
            (if (equal? chr (tuple 'key #\Q))
               (values ll buff undo mode 'close)
               (cont (cons chr ll) buff undo mode "Close aborted"))))
      (values ll buff undo mode 'close)))
         
(define (command-go-to-line ll buff undo mode n cont)
   (lets ((buff (buffer-seek buff 0 (if (number? n) (- n 1) 0) #false)))
      (cont ll buff undo mode (str "line " n))))

(define (command-go-to-mark ll buff undo mode r cont)
   (log "marks is " (get-buffer-meta buff 'marks #empty))
   (lets ((msg ll (uncons ll #false)))
      (if (eq? (ref msg 1) 'key)
         (lets
            ((char (ref msg 2))
             (pos (getf (get-buffer-meta buff 'marks #empty) char)))
            (log "going back to position " pos)
            (if pos
               (lets 
                  ((x y pos)
                   (buff (buffer-seek buff x y #f)))
                  (cont ll buff undo mode (str "mark '" (list->string (list char))) "'"))
               (cont ll buff undo mode "no such mark")))
         (cont ll buff undo mode))))

(define (command-go-to-last-line ll buff undo mode cont)
   (lets 
      ((u d l r x y off meta buff)
       (last (+ 1 (+ (length u) (length d))))
       (buff (buffer-seek buff 0 last #false)))
      (cont ll buff undo mode "last line")))

(define (convert-paren meta)
   (let ((tab (get meta 'tab tab-node)))
      (λ (line)
         (map (λ (node) (key-node node meta)) line))))
      
(define (path->lines path meta)
   (let ((fd (open-input-file path)))
      ;; todo: allow binary mode operation
      (if fd
         (map (convert-paren meta) (map string->list (force-ll (lines fd))))
         #false)))

;; fixme: led-eval is silly
(define (led-eval ll buff undo mode cont notify exp)
   (cond
      ((equal? exp "")
        (notify buff "canceled")
        (cont ll buff undo mode))
      ((equal? exp "q")
         (if (dirty-buffer? buff undo)
            (begin
               (notify buff "Unsaved changes. q! quits anyway.")
               (cont ll buff undo mode))
            (values ll buff undo mode 'close)))
      ((equal? exp "q!")
        (values ll buff undo mode 'close))
      ((equal? exp "Q!")
        (values ll buff undo mode 'close-all))
      ((equal? exp "n")
        (values ll buff undo mode 'new))
      ((m/^n [^ ]+$/ exp)
         (values ll buff undo mode (tuple 'open (s/n +// exp) null)))
      ((equal? exp "vi")
        (cont ll buff undo 'insert))
      ((m/^w / exp)
        (let ((path (s/^w +// exp)))
           (lets ((ok? write-msg (write-buffer buff path)))
              (cont ll 
                 (put-buffer-meta buff 'path path)
                 (mark-saved undo buff (time-ms))
                 mode (list->string write-msg)))))
      ((m/^r +[^ ]/ exp)
         (lets ((path (s/^r +// exp))
                (lines (path->lines path (buffer-meta buff))))
            (log "read" lines "from" path)
            (if lines
               (lets ((undo (push-undo undo buff))
                      (buff (paste-lines-below buff lines)))
                  (output (update-screen buff))
                  (notify buff (str "Read " (length lines) " lines from '" path "'"))
                  (cont ll buff undo mode))
               (begin
                  (notify buff (str "Failed to read '" path "'"))
                  (cont ll buff undo mode)))))
      ((m/^w$/ exp)
        (let ((path (buffer-path buff #false)))
           (if path
              (lets ((ok? write-tio (write-buffer buff path)))
                 (notify buff write-tio)
                 (cont ll buff 
                    (mark-saved undo buff (time-ms))
                    mode))
              (cont ll buff undo mode))))
      ((m/^x$/ exp)
         (lets ((path (buffer-path buff #false))
                (ok? msg (write-buffer buff path)))
            (if ok?
               ;; exit to led-buffers
               (values ll buff undo mode 'close)
               (begin
                  (notify buff msg)
                  (cont ll buff undo mode)))))
      ((m/^[0-9]+$/ exp)
        (lets ((line (max 0 (- (string->number exp 10) 1)))
               (buff (buffer-seek buff 0 line #false)))
           (output (update-screen buff))
           (cont ll buff undo mode)))
      ((m/^move +[0-9]+$/ exp)
          (let ((n (string->number (s/^move +// exp))))
            (values ll buff undo mode (tuple 'move n))))
      ((equal? exp "$")
         (command-go-to-last-line ll buff undo mode cont))
      ((m/set *ai/ exp)
         (notify buff "AI enabled")
         (log "AI -> paren")
         (cont ll (put-buffer-meta buff 'ai 'paren) undo mode))
      ((m/set *noai/ exp)
         (notify buff "AI disabled")
         (log "AI -> none")
         (cont ll (put-buffer-meta buff 'ai 'none) undo mode))
      ((m/set *noab/ exp)
         (notify buff "abbreviations disabled")
         (log "abbreviations -> off")
         (cont ll (put-buffer-meta buff 'ab #false) undo mode))
      ((m/set *ab/ exp)
         (notify buff "abbreviations enabled")
         (log "abbreviations -> on")
         (cont ll (put-buffer-meta buff 'ab #true) undo mode))
      ((m/ab / exp)
         (lets ((parts (c/ +/ exp)))
            (if (= (length parts) 3)
               (cont ll
                  (buff-add-abbreviation buff (cadr parts) (caddr parts))
                  undo mode)
               (begin
                  (notify "no. ab [what] [replacement]")
                  (cont ll buff undo mode)))))
      ((m/set *expandtab/ exp)
         (notify buff "Expanding tabs")
         (cont ll (put-buffer-meta buff 'expandtab #true) undo mode))
      ((m/set *noexpandtab/ exp)
         (notify buff "Not expanding tabs")
         (cont ll (put-buffer-meta buff 'expandtab #false) undo mode))
      ((m/set *noshowmatch/ exp)
         (notify buff "Not showing matching parens")
         (cont ll (put-buffer-meta buff 'show-match #false) undo mode))
      ((m/set *showmatch/ exp)
         (notify buff "Showing matching parens")
         (cont ll (put-buffer-meta buff 'show-match #true) undo mode))
      ((m/set *tabstop=[1-9][0-9]*/ exp)
         (lets ((n (string->integer (s/set *tabstop=// exp))))
            (notify buff (str "Tabstop = " n))
            (cont ll (put-buffer-meta buff 'tabstop n) undo mode)))
      ((m/^search .*/ exp)
         (values ll buff undo mode (tuple 'search (s/search // exp))))
      ((m/settings/ exp)
         ;; open a settings buffer
         (values ll buff undo mode 'settings))
      (else
         (cont ll buff undo mode))))

(define (command-enter-command ll buff undo mode r cont)
   (output (tio* (set-cursor 1 (+ 1 (screen-height buff))) (clear-line) (list #\:)))
   (lets
      ((metadata (buffer-meta buff))
       (ll res 
          (readline ll 
            (get (buffer-meta buff) 'command-history null) 
            2 (+ 1 (screen-height buff) 1) (screen-width buff)))
       (buff
         (if res
            (set-buffer-meta buff
              (put metadata 'command-history
                (cons res (get metadata 'command-history null))))
            buff)))
      (log (str "readline returned '" res "'"))
      (output
         (tio 
            (set-cursor 1 (+ 1 (screen-height buff))) 
            (clear-line)
            (set-cursor (buffer-x buff) (buffer-y buff))))
      (if res
         (led-eval ll buff undo mode cont notify res)
         (cont ll buff undo mode))))

(define (command-redo ll buff undo mode r cont)
  (lets ((undo buffp (unpop-undo undo buff)))
    (cont ll buffp undo mode
       (if (eq? buff buffp)
          "nothing left to redo"
          "redone. press u to re-undo."))))

(define (command-undo ll buff undo mode r cont)
   (lets ((undo buffp (pop-undo undo buff)))
      (cont ll buffp undo mode
         (if (eq? buff buffp)
            "nothing left to undo"
            "undone. press ^r to redo."))))

(define (command-substitute-line ll buff undo mode r cont)
   (cont (ilist (tuple 'key #\0) (tuple 'key #\C) ll) buff undo mode))

(define (command-substitute-char ll buff undo mode r cont)
   (cont (ilist (tuple 'key #\x) (tuple 'key #\i) ll) buff undo mode))

(define (command-insert-before ll buff undo mode r cont)
   (cont ll buff undo 'insert))

(define (command-insert-after ll buff undo mode r cont)
   (cont (cons (tuple 'arrow 'right) ll) buff undo 'insert))

(define (command-insert-after-line ll buff undo mode r cont)
   (cont (ilist (tuple 'key #\$) (tuple 'key #\a) ll) buff undo mode))

;; should also w to first non-space later
(define (command-insert-at-line-start ll buff undo mode r cont)
   (cont (ilist (tuple 'key #\0) (tuple 'key #\i) ll) buff undo mode))

(define (select-lines ll buff n)
   (lets ((u d l r x y off meta buff))
      (if (eq? n 1)
         (values ll (length l) 0 (length r) 0)
         (values ll (length l) 0 (- n 1) 0))))
   
(define (command-delete ll buff undo mode r cont)
   (lets ((undop (push-undo undo buff))
          (ll buffp tob (cut-movement ll buff r #\d)))
       (log "deleted " tob)
       (if tob
          (cont ll (put-global-meta buffp 'yank tob) undop mode)
          (cont ll buff undo mode))))
 
(define (command-change ll buff undo mode r cont)
   ;; convert possible next c of [c]c to d, 
   (lets 
      ((next ll (uncons ll eof))
       (ll (cons (if (equal? next (tuple 'key #\c)) (tuple 'key #\d) next) ll)))
      (command-delete ll buff undo mode r
         (λ (ll buff undo mode)
            (cont ll buff undo 'insert)))))

(define (command-move-words ll buff undo mode r cont)
   (lets ((dy dx (movement buff (or r 1) 'word)))
      (log "moving" r "words gives dy" dy ", dx" dx)
      (if (eq? dy 0)
         (cont (keys ll #\l dx) buff undo mode) ;; use repetitions later
         (cont (-> ll (keys #\l dx) (keys #\j dy) (keys #\0 1) ) buff undo mode))))

(define (command-yank ll buff undo mode r cont)
   (lets ((undop (push-undo undo buff))
          (ll buffp tob (cut-movement ll buff r #\y)))
       (if tob
          (begin
             (log "cut data " tob)
             (cont ll (put-global-meta buff 'yank tob) undop mode "yanked"))
          (cont ll buff undo mode))))
 
(define (command-no-op ll buff undo mode r cont)
   (cont ll buff undo mode))

(define (command-change-rest-of-line ll buff undo mode r cont)
   (lets ((u d l r x y off meta buff)
          (undo (push-undo undo buff))
          (buff (buffer u d l null x y off meta)))
         ;; not having to repaint over these when switching modes reduces flicker for now
         ;(output
         ;   (tio 
         ;      (cursor-save)
         ;      (font-dim)
         ;      (raw   (repeat #\- (min (+ 1 (- w x)) (printable-length r))))
         ;      (font-normal)
         ;      (cursor-restore)))
         (cont ll buff undo 'insert)))

(define (command-step-forward ll buff undo mode r cont)
   (lets ((u d l r x y off meta buff)
          (w h (meta-dimensions meta))
          (y (+ (cdr off) (- y 1)))
          (buff (buffer-seek buff (- x 1) (+ y (max 1 (- h 3))) 1)))
      (log "buffer seeking to " (cons x (+ y (max 1 (- h 3)))) " with y at " y)
      (cont ll buff undo mode)))

(define (command-step-backward ll buff undo mode r cont)
   (lets ((u d l r x y off meta buff)
          (w h (meta-dimensions meta))
          (y (+ (cdr off) (- y 1)))
          (buff (buffer-seek buff (- x 1) (- y (min (- h 3) y)) 1)))
      (cont ll buff undo mode)))

(define (command-update-screen ll buff undo mode r cont)
   (lets ((u d l r x y off meta buff)
          (y (+ (cdr off) (- y 1)))
          (x (+ (car off) (- x 1)))
          (w h ll (get-terminal-size ll))
          (h (max 1 (- h 1)))
          (buff (buffer u d l r x y off meta))
          (buff (buffer-seek buff x y #false))
          (buff (put-global-meta buff 'width w))
          (buff (put-global-meta buff 'height h)))
      (output (update-screen buff))
      (notify buff (str (get-buffer-meta buff 'type "*scratch*") " " (buffer-path buff "")))
      (cont ll buff undo mode)))

(define (key-value event)
   (if (and (tuple? event) (eq? (ref event 1) 'key))
      (ref event 2)
      #false))

(define (drop-dir-contents slst dir)
   (log "dropping contents of " dir)
   (let ((pre (append (string->list dir) '(#\/))))
      (let loop ((lst slst))
         (if (null? lst)
            null
            (if (drop-prefix (car lst) pre)
               (loop (cdr lst))
               lst)))))

(define (directory-contents prefix contents)
   (map
      (λ (x)
         (string->list
            (if (equal? prefix ".")
               (s/^\.\/// x)
               x)))
      contents))

(define word-chars 
   (fold (λ (ff x) (put ff x x))
      #empty
      (string->list
         "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZåäöÅÄÖ-/_!?<>.")))

(define (word-char? x)
   (getf word-chars x))

(define (current-word l r)
   (lets ((l _ (take-while word-char? (nodes->code-points l)))
          (r _ (take-while word-char? (nodes->code-points r))))
      (list->string (append (reverse l) r))))

;; translate part after file link possibly into a command 
;;   e.g. foo/bar.txt:8 -> suffix = "8" -> '(#\: #\8)
;; arbitrary command are not allowed for security reasons
(define (maybe-string->command s)
   (cond
      ((m/^[0-9]+$/ s)
         (cons #\: (string->list s)))
      ((m/^\/[a-zA-ZäöÄÖ0-9 _.- !]*\/$/ s)
         (reverse (cdr (reverse (string->list s)))))
      (else null)))
   
;; todo: add a recursive-open flag
(define (command-do ll buff undo mode r cont)
   (lets 
      ((u d l r x y off meta buff)
       (line (list->string (foldr render-node null (append (reverse l) r))))
       (parts (c/:/ line)))
      (cond
         ((and (pair? parts) (file? (car parts)))
            (log "do: open file '" line "' with args " (cdr parts))
            (values ll buff undo mode (tuple 'open (car parts) 
               (if (pair? (cdr parts))
                  (maybe-string->command (cadr parts))
                  null))))
         ((directory? line)
            (let ((dp (drop-dir-contents d line)))
               (log "dropping length " (length d) " -> " (length dp))
               (if (eq? dp d) ;; no prefixed lines, open it
                  (let ((contents (led-dir-recursive->list line)))
                     (if contents
                        (cont ll
                           (buffer u 
                              (append (map string->list contents) d)
                              l r x y off meta)
                           (push-undo undo buff) mode)
                        (begin
                           (notify buff (str "Cannot read '" line "'"))
                           (cont ll buff undo mode))))
                  (let ((buffp (buffer u dp l r x y off meta)))
                     (cont ll buffp (push-undo undo buff) mode)))))
         (else
            (let ((word (current-word l r)))
               (log "Current word is " word)
               (values ll buff undo mode (tuple 'search word)))))))

(define (command-go-home ll buff undo mode ran cont)
   (values ll buff undo mode (tuple 'buffer 1)))

(define (command-replace-char ll buff undo mode ran cont)
   (lets ((val ll (uncons ll #false))
          (k (key-value val)))
      (if k
         (lets ((u d l r x y off meta buff))
            (if (null? r)
               (cont ll buff undo mode)
               (lets
                  ((r (map-n (λ (x) k) ran r))
                   (undo (push-undo undo buff))
                   (buffp
                      (buffer u d l r x y off meta)))
                  (cont ll buffp undo mode))))
         (cont ll buff undo mode))))

(define (command-previous-buffer ll buff undo mode r cont)
   (values ll buff undo mode 'left))

(define (command-next-buffer ll buff undo mode r cont)
   (values ll buff undo mode 'right))

;;; Command mode key mapping

(define *command-mode-control-actions*
   (-> #empty
      (put #\f command-step-forward)
      (put #\b command-step-backward)
      (put #\r command-redo)
      (put #\h command-go-home)
      (put 'arrow-left command-previous-buffer)
      (put 'arrow-right command-next-buffer)
      (put '#\p command-previous-buffer) ;; also available in insert mode
      (put '#\n command-next-buffer)     ;; ditto
      (put #\w command-save)
      (put #\l command-update-screen)))

;; key → (ll buff undo mode range cont → (cont ll' buff' undo' mode'))
(define *command-mode-actions*
   (-> #empty
      (put #\/ command-regex-search)
      (put #\n command-find-next)
      (put #\m command-mark-position)
      (put #\w command-move-words)
      (put #\$ command-line-end)
      (put #\0 command-line-start)
      (put #\j command-move-down)
      (put #\k command-move-up)
      (put #\l command-move-right)
      (put #\r command-replace-char)
      (put #\h command-move-left)
      (put #\P command-paste-before)
      (put #\p command-paste-after)
      (put #\o command-add-line-below)
      (put #\O command-add-line-above)
      (put #\' command-go-to-mark)
      (put #\x command-delete-char)
      (put #\: command-enter-command)
      (put #\u command-undo)
      (put #\s command-substitute-char)
      (put #\S command-substitute-line)
      (put #\i command-insert-before)
      (put #\I command-insert-at-line-start)
      (put #\a command-insert-after)
      (put #\A command-insert-after-line)
      (put #\d command-delete)
      (put #\c command-change)
      (put #\y command-yank)
      (put #\J command-join-lines)
      (put #\G command-go-to-line)
      (put #\> command-indent)
      (put #\< command-unindent)
      (put #\C command-change-rest-of-line)
      (put #\W command-save)
      (put #\Q command-close-buffer)
      (put #\% command-seek-matching-paren)
      (put sexp-key command-seek-matching-paren)))


;;;
;;; Insert mode actions
;;;

(define (paren-balance lst)
   (fold + 0
      (map
         (λ (x) (if (left-paren? x) 1 (if (right-paren? x) -1 0)))
         lst)))

(define (unclosed-parens? lst)
   (> (paren-balance lst) 0))

(define (leading-whitespaces lst)
   (cond
      ((null? lst) null)
      ((whitespace? (car lst))
         (cons (car lst) (leading-whitespaces (cdr lst))))
      (else null)))

(define (drop-spaces lst n)
   (cond
      ((null? lst) lst)
      ((eq? n 0) lst)
      ((eq? (car lst) #\space)
         (drop-spaces (cdr lst) (- n 1)))
      (else lst))) ;; not touching other indents for now

(define (repeat n val tail)
   (if (eq? n 0)
      tail
      (cons val (repeat (- n 1) val tail))))

;; up -> l
(define (artificial-intelligence u tabstop)
   (if (null? u)
      null
      (lets
         ((bal (paren-balance (car u)))
          (lead (leading-whitespaces (car u))))
         (log "lead is " lead ", paren balance " bal)
         (cond
            ((eq? bal 0)
               lead)
            ((> bal 0)
               (repeat tabstop #\space lead))
            (else
               (drop-spaces lead (* bal (- 0 tabstop))))))))

(define (key x) 
   (tuple 'key x))

(define (insert-enter buff)
   (lets ((u d l r x y off meta buff)
          (ai (get meta 'ai 'none)))
      (cond
         ((eq? ai 'none)
            (lets
               ((buffp (buffer u (cons r d) l null x y off meta))
                (buffp (move-arrow (seek-line-start buffp) 'down #true)))
               buffp))
         ((eq? ai 'paren)
            (lets
               ((ind (artificial-intelligence (cons (reverse l) u) (get meta 'tabstop 3)))
                (buffp
                  (move-arrow 
                     (seek-line-start 
                        (buffer u (cons (append ind r) d) l null x y off meta))
                     'down #true)))
               (fold
                  (λ (buff node)
                     (lets ((buff (move-arrow buff 'right #true))) buff))
                  buffp ind)))
         (else
            (error "Unknown AI: " ai)))))


;;;
;;; Buffer handling loop
;;;

(define (maybe-get-target ll)
   (values #false ll))

(define (update-cont self buff)
   (λ (ll buffp undo mode . msg)
      (output (delta-update-screen buff buffp))
      (if (pair? msg)
         (let ((what (apply str msg)))
            (notify buffp what)))
      (self ll buffp undo mode)))


(define (highlight-match ll)
   (ilist (tuple 'esc)
          (tuple 'key #\%)
          (λ ()
             (sleep 150)
             (ilist
                (tuple 'key #\%)
                (tuple 'key #\a)
                ll))))

(define (closing-paren? x)
   (or ;(eq? x #\()
       (eq? x #\))
       ;(eq? x #\[)
       (eq? x #\])
       ;(eq? x #\{)
       (eq? x #\})))
 
;; ll buff undo mode -> ll' buff' undo' mode' action
(define (led-buffer ll buff undo mode)
   (log-buff buff undo mode)
   (cond
      ((eq? mode 'insert)
         (lets
            ((msg ll (uncons ll #false))
             (u d l r x y off meta buff))
            (log "cursor " (cons x y) ", offset " off ", event " msg)
            (tuple-case msg
               ((key x)
                  (lets ((buffp (insert-handle-key buff x)))
                     (output (delta-update-screen buff buffp))
                     (if (and (closing-paren? x) (get-buffer-meta buffp 'show-match #false))
                        (led-buffer (highlight-match ll) buffp undo mode)
                        (led-buffer ll buffp undo mode))))
               ((tab)
                  ;(led-buffer (ilist space-key space-key space-key ll) buff undo mode)
                  ;; expandtab = #false or tab width
                  (let ((exp (getf meta 'expandtab)))
                     (log "tab expansion -> " exp)
                     (if exp
                        (led-buffer (keys ll #\space (get meta 'tabstop 8)) buff undo mode)
                        (led-buffer (cons (tuple 'key 9) ll) buff undo mode))))
               ((enter)
                  (lets ((buffp (insert-enter buff)))
                     (output (delta-update-screen buff buffp))
                     (led-buffer ll buffp undo mode)))
               ((backspace)
                  (lets ((buffp (insert-backspace buff)))
                     (output (delta-update-screen buff buffp))
                     (led-buffer ll buffp undo mode)))
               ((arrow dir)
                  (lets ((buffp (move-arrow buff dir #t)))
                     (delta-update-screen buff buffp)
                     (led-buffer ll buffp undo mode)))
               ((end-of-text)
                  ; (output (update-screen buff)) ;; would clear overstrike
                  (led-buffer (cons (tuple 'key #\h) ll) buff (push-undo undo buff) 'command))
               ((esc)
                  (log "switching out of insert mode on esc + moving cursor")
                  ;(output (update-screen buff)) ;; would clear overstrike
                  (led-buffer (cons (tuple 'key #\h) ll) buff (push-undo undo buff) 'command))
               ((end-of-transmission)
                  ;; ^D = 3 backspace (remove indent)
                  ;; fixme: switch to unindent
                  (led-buffer
                     (ilist (tuple 'backspace) (tuple 'backspace) (tuple 'backspace) ll)
                     buff undo mode))
               ((ctrl key)
                  (cond
                     ((eq? key 'arrow-left) (values ll buff undo mode 'left))
                     ((eq? key 'arrow-right) (values ll buff undo mode 'right))
                     ((eq? key #\p) (values ll buff undo mode 'left))
                     ((eq? key #\n) (values ll buff undo mode 'right))
                     ((eq? key #\w) 
                        (command-save ll buff undo mode 1
                           (λ (ll buff undo mode msg)
                              (if msg (notify buff msg))
                              (led-buffer ll buff undo mode))))
                     (else
                        (log "ignoring control " key " in insert mode")
                        (led-buffer ll buff undo mode))))
               (else
                  (led-buffer ll buff undo mode)))))
      ((null? ll)
         (values ll buff undo mode 'close-all))
      (else
         (lets ((count ll (maybe-get-count ll 1)) ;; upgrade to uncomputed range later
                (msg ll (uncons ll space-key)))
            ;; todo: read the possible text object here based on command type, so that a function to recompute the last change can be stored for .
            (tuple-case msg
              ((key k)
                 ((get *command-mode-actions* k command-no-op)
                     ll buff undo mode count (update-cont led-buffer buff)))
              ((ctrl key)
                  ((get *command-mode-control-actions* key command-no-op)
                     ll buff undo mode count (update-cont led-buffer buff)))
              ((enter)
                 (command-do ll buff undo mode count (update-cont led-buffer buff)))
              (else
                  (log "not handling command " msg)
                  (led-buffer ll buff undo mode)))))))


;;; Program startup

(define (splash w h)
   (lets
      ((mw (>> w 1))
       (mh (>> h 1)))
      (output
         (tio
           (clear-screen)
           (set-cursor (- mw (>> (string-length version-str) 1)) mh)
           (raw (font-bright (render version-str null)))
           (font-normal)
           (set-cursor (max 1 (- mw 6)) (+ mh 2))
           (raw (render "esc + :q quits" null))
           (set-cursor 1 1)))))

(define (initial-state buff)
   (tuple buff (initial-undo buff) 'command))

(define (empty-initial-state w h meta)
   (initial-state (make-empty-buffer w h meta)))

(define (make-new-state buff)
   (lets ((w h (buffer-screen-size buff))
          (buff (make-empty-buffer w h #empty)))
      (tuple buff (initial-undo buff) 'command)))

(define (make-new-state-having buff type content)
   (lets ((w h (buffer-screen-size buff))
          (buff 
             (make-buffer-having w h 
                (-> #empty
                   (put 'type type))
                content)))
      (tuple buff (initial-undo buff) 'command)))

(define (make-file-state w h path meta)
   (log "making file state out out of " path)
   (cond
      ((path->lines path meta) =>
         (λ (data)
            (let ((meta (-> meta (put 'path path) (put 'type 'file))))
               (if (pair? data)
                  (buffer null (cdr data) null (car data) 1 1 (cons 0 0) meta)
                  (buffer null null null null 1 1 (cons 0 0) meta)))))
      (else
         (buffer null null null null 1 1 (cons 0 0) (put meta 'path path)))))

(define (path->buffer-state path meta buff)
   (lets ((w h (buffer-screen-size buff))
          (buff (make-file-state w h path meta)))
      (if buff
         (tuple buff (initial-undo buff) 'command)
         #false)))

(define (make-directory-buffer path contents meta w h)
   (lets ((contents (directory-contents path contents))
          (buff (buffer null (cdr contents) null (car contents) 1 1 (cons 0 0) 
                   (-> meta 
                      (put 'type 'directory)
                      (put 'path path)))))
      (tuple buff (initial-undo buff) 'command)))

(define (notify-buffer-source left buff right)
   (lets ((source (buffer-path-str buff))
          (nth (+ 1 (length left)))
          (total (+ nth (length right)))
          (slider
             (map
                (λ (x) (if (eq? x nth) #\x #\space))
                (iota 1 1 (+ total 1))))
          (msg
             (cond
                ((= total 1) "")
                ((< total 6)
                   (list->string (append (cons #\[ slider) (list #\] #\space))))
                (else
                   (str "[" nth "/" total "] ")))))
         (log "total is " total)
         (notify buff
            (string-append msg source))))

(define (exit-led n)
   (output
      (tio
         (clear-screen)
         (set-cursor 1 1)))
   (set-terminal-rawness #false)
   n)

(define (buffer-position l s r path)
   (let loop ((pos 1) (bs (append (reverse l) (cons s r))))
      (cond
         ((null? bs) #false)
         ((equal? path (get-buffer-meta (ref (car bs) 1) 'path #false))
            pos)
         (else
            (loop (+ pos 1) (cdr bs))))))

;; somewhat hardcoded for now
(define scheme-source?  m/\.scm$/)
(define clojure-source? m/\.clj[cxs]?$/)
(define c-source?       m/\.(c|cc|C|h|H|cpp|cxx)$/)
(define web-source?     m/\.(js|x?html?|css)$/)

(define (allowed-search-from buff)
   (let ((path (get-buffer-meta buff 'path "")))
      (cond
         ((scheme-source? path)  scheme-source?)
         ((clojure-source? path) clojure-source?)
         ((c-source? path)       c-source?)
         ((web-source? path)     web-source?)
         (else (λ (x) #true)))))
      
         
(define (led-buffers-action ll left state right action led-buffers)
   (lets ((buff undo mode state))
      (cond
         ((eq? action 'close-all)
            (log "close-all")
            (exit-led 0))
         ((eq? action 'close)
            (cond
               ((pair? left)
                  (led-buffers ll (cdr left) (car left) right 
                     (str "Closed " (buffer-path-str buff))))
               ((pair? right)
                  (led-buffers ll left (car right) (cdr right) 
                     (str "Closed " (buffer-path-str buff))))
               (else
                  (log "all buffers closed")
                  (exit-led 0))))
         ((eq? action 'left)
            (if (null? left)
               (led-buffers ll left state right 
                  (if (null? right)
                     (str "You only have " (buffer-path-str buff) " open.")
                     "already at first buffer"))
               (led-buffers ll (cdr left) (car left)
                  (cons state right) #false)))
         ((eq? action 'right)
            (if (null? right)
               (led-buffers ll left state right 
                  (if (null? left)
                     (str "You only have " (buffer-path-str buff) " open.")
                     "already at last buffer"))
               (led-buffers ll (cons state left)
                  (car right) (cdr right) #false)))
         ((eq? action 'new)
            (log "making new buffer")
            (lets ((new-state (make-new-state buff)))
               (led-buffers ll (cons state left) new-state right "new scratch buffer")))
         ((eq? action 'settings)
            (log "making a new settings buffer")
            (lets ((new-state (make-new-state-having buff 'settings '((97 98 99)))))
               (led-buffers ll (cons state left) new-state right "new settings buffer")))
         ((tuple? action)
            (tuple-case action
               ((open path init)
                  (cond
                     ((buffer-position left state right path) =>
                        (λ (pos)
                           (log "Already open at " pos)
                           (led-buffers-action (command-keys init ll) left state right (tuple 'buffer pos) led-buffers)))
                     ((led-dir->list path) =>
                        (λ (subs)
                           (notify buff (str "Opening directory " path))
                           (lets
                              ((w h (buffer-screen-size buff)) 
                               (new (make-directory-buffer path subs (buffer-meta buff) w h)))
                              (log "made dir buffer")
                              (led-buffers ll (cons state left) new right path))))
                     (else
                        (notify buff (str "opening " path "..."))
                        (lets ((new (path->buffer-state path (buffer-meta buff) buff)))
                           (if new
                              (led-buffers (command-keys init ll) (cons state left) new right "")
                              (begin
                                 (log "failed to open " path)
                                 (led-buffers ll left state right #false)))))))
               ((search what)
                  (if (and what (> (string-length what) 0))
                     (lets
                        ((index (last left #false))
                         (ibuff (if index (ref index 1) buff))
                         (w h (buffer-screen-size buff)))
                        (log "Searching for " what)
                        (led-buffers ll (cons state left)
                           (initial-state
                              (make-buffer-having w h 
                                 (-> (buffer-meta ibuff) 
                                    (put 'type 'search-results))
                                 (run-search what (buffer->lines ibuff)
                                    (λ (msg) (notify ibuff msg))
                                    (allowed-search-from buff))))
                           right (str "Searched for '" what "'")))
                     (led-buffers ll left state right
                        "Move cursor over something to search")))
               ((buffer n)
                  (log "Going to buffer " n)
                  (lets ((buffers (append (reverse left) (cons state right))))
                     (if (and (> n 0) (<= n (length buffers)))
                        (lets ((left right (split buffers (- n 1))))
                           (led-buffers ll (reverse left) (car right) (cdr right) 
                              (str "switched to " n)))
                        (begin
                           (notify buff "No such buffer")
                           (led-buffers ll left state right #false)))))
               ((move n)
                  (log "moving buffer to" n)
                  (lets ((left right (seek-in-list (append (reverse left) right) (max 0 (- n 1)))))
                     (log "lens " (cons (length left) (length right)))
                     (led-buffers ll left state right (str "moved to " (+ 1 (length left))))))
               (else is unknown
                  (log "unknown buffer action " action)
                  (led-buffers ll left state right #false))))
         (else
            (log "unknown buffer action " action)
            (led-buffers ll left state right #false)))))

(define (copy-global-settings old new)
   (if (eq? old new)
      new
      (lets ((ob ou om old)
             (nb nu nm new))
          (tuple 
             (put-buffer-meta nb 'global
                (get-buffer-meta ob 'global #empty))
             nu nm))))

(define (led-buffers ll left state right msg)
   (lets ((buff undo mode state)
          (_ (output (update-screen buff)))
          (_ (if msg (notify buff msg) (notify-buffer-source left buff right)))
          (ll buff undo mode action (led-buffer ll buff undo mode))
          (state (tuple buff undo mode)))
      (log "led-buffers: action " action)
      (led-buffers-action ll left state right action 
         (λ (ll left statep right msg)
            ;; copy global settings from the old buffer to the new one
            (led-buffers ll left
               (copy-global-settings state statep)
               right msg)))))

; (define (led-buffers-action ll left state right action led-buffers)
   
;; todo: use the buffer open command instead
;                                 (led-buffers ll left state right #false)))))))
(define (open-all-files paths w h shared-meta)
   (let loop ((left null) (state (empty-initial-state w h shared-meta)) (right null) (paths paths))
      (log "open all files loop " paths)
      (if (null? paths)
         (append (reverse left) (cons state right))
         (led-buffers-action null left state right (tuple 'open (car paths) null)
            (λ (ll left state right result)
               (log "led-buffers-action response to opening " (car paths) " is " result)
               (if result
                  (loop left state right (cdr paths))
                  (begin
                     (print "Failed to open '" (car paths) "'")
                     #false)))))))

(define (initial-terminal-setup)
   (output
      (tio
         (clear-screen)
         (set-cursor 1 1)))
   '(output
      (fold
         (λ (out bg)
            (fold
               (λ (out fg)
                  (fold
                     (λ (out att)
                        (tio*
                           (font-attrs att fg bg)
                           (raw (string->list (str att ";" fg ";" bg " ")))
                           (font-attrs 0 0 0)
                           out))
                     out 
                        ;(list 0 1 2 3 4 5 6 7)
                        (list 2 0 4 1)
                        ))
               out (list 30 31 32 33 34 35 36 37)))
         null (list 40 41 42 43 44 45 46 47)))
   '(sleep 1000)
   (output
      (tio
         (disable-line-wrap))))

(define sink 
   (λ x x))

;; dict -> meta | #false
(define (load-settings dict terminal-width terminal-height)
   (lets/cc ret
      ((config-path 
         (or (getf dict 'config)
             (str (or (getenv "HOME") ".")
                  "/.ledrc")))
       (empty-config 
          (-> #empty
             (put 'global
                (-> #empty
                   (put 'width terminal-width)
                   (put 'height terminal-height)))))
       (config-port 
         (open-input-file config-path)))
      (log "loading config from " config-path)
      (cond
         ((not config-port)
            empty-config)
         ((force-ll (lines config-port)) =>
            (λ (lines)
               (buffer-meta
                  (fold
                     (λ (buff cmd)
                        (log "led eval: " cmd)
                        (led-eval null buff empty-undo 'command 
                            (λ (ll buff undo mode) buff)
                            sink cmd))
                     (make-empty-buffer 10 10 empty-config)
                     lines))))
         (else
            (print-to stderr "Bad data in config")
            #false))))

(define (start-led dict args ll)
   (log "start-led " dict ", " args)
   (lets ((w h ll (get-terminal-size ll))
          (h (max (- h 1) 1)))
   (log "dimensions " (cons w h))
   (initial-terminal-setup)
   (lets
      ((meta (load-settings dict w h))
       (states
         (open-all-files args w h meta)))
      (cond
         ((null? args)
            (led-buffers ll null (car states) (cdr states) "*scratch*"))
         ((not states)
            1)
         (else
            (led-buffers ll null (cadr states) (cddr states) "ok"))))))

(define usage-text
  "Usage: led [flags] [file] ...")

(define command-line-rules
  (cl-rules
    `((help "-h" "--help" comment "show this thing")
      (version "-V" "--version" comment "show program version")
      (log "-L" "--log" has-arg comment "debug log file")
      (config "-c" "--config" has-arg comment "config file (default $HOME/.ledrc)")
      (faketerm "-I" "--input" has-arg comment "fake terminal input stream source"))))

(define (trampoline)
  (let ((env (wait-mail)))
    (log "main: " env)
    (set-terminal-rawness #false)
    (if (and (eq? (ref env 1) 'led) (eq? (ref (ref env 2) 1) 'finished))
      (begin
         (log "trampoline: finished")
         (wait 10) ;; let other threads run if necessary (mainly send debug info before halt)
         0)
      (begin
        (print "error: " env)
        (mail 'logger (list 'ERROR env))
        (wait 100)
        (halt 1)))))

(define (led-input-stream dict)
   (let ((path (getf dict 'faketerm)))
      (if path
         (let ((port (open-input-file path)))
            (if port
               (terminal-input port)
               null))
         (begin
            (set-terminal-rawness #true)
            (terminal-input stdin)))))

(define (start-led-threads dict args)
   (cond
      ((getf dict 'help)
         (print usage-text)
         (print (format-rules command-line-rules))
         0)
      ((getf dict 'version)
         (print version-str)
         0)
      (else
         (log "started " dict ", " args)
         (fork-linked-server 'logger (λ () (start-log dict)))
         (fork-linked-server 'led (λ () (start-led dict args (led-input-stream dict))))
         (log "started")
         (trampoline))))


(define (main args)
   (process-arguments (cdr args) command-line-rules usage-text start-led-threads))


main

