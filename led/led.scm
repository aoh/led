#!/usr/bin/ol --run

(import
  (led terminal)
  (led log)
  (led buffer)
  (led undo)
  (led node)
  (owl args))

(define version-str "led v0.1a")

(define (output lst)
   (write-bytes stdout lst))

;;; Movement and insert mode edit operation
  
(define empty-buffer-line
   (tio
      (font-dim)
      (raw (render "~" null))
      (font-normal)))


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
         (draw-lines-at-offset tl w dx (+ y dy) dy end (cdr lines)))
      (else
         (let ((these (drop-printable (car lines) dx)))
            ;(log "printable after " dx " of " (car lines) " is " (drop-printable (car lines) dx))
            (tio*
               (set-cursor 1 y)
               (clear-line-right)
               (raw (take-printable these w))
               (draw-lines-at-offset w dx (+ y dy) dy end (cdr lines))
               tl)))))
                  
(define (update-screen buff)
   (log "FULL UPDATE SCREEN")
   (lets 
      ((u d l r x y w h off meta buff)
       (this (append (reverse l) r)))
      (tio
          (clear-screen)
          (draw-lines-at-offset w (car off) y -1 0 (cons this u))
          (draw-lines-at-offset w (car off) (+ y 1) +1 (+ h 1) d)
          (set-cursor x y))))

(define (maybe-car lst) (if (pair? lst) (car lst) #false))
(define (maybe-cdr lst) (if (pair? lst) (cdr lst) null))

(define (maybe-draw-lines-at-offset tl w dx y dy end lines old-lines)
   (cond
      ((null? lines) 
         (maybe-draw-lines-at-offset tl w dx y dy end
            (list empty-buffer-line) old-lines))
      ((eq? y end) tl)
      ((equal? (car lines) (maybe-car old-lines))
         ;(log "not redrawing shared line at " y)
         (maybe-draw-lines-at-offset tl w dx (+ y dy) dy end (cdr lines) (maybe-cdr old-lines)))
      (else
         ;(log " - delta updating line " y " having '" (list->string (car lines)) "' vs '" (list->string (or null (maybe-car old-lines))) "'")
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
         (lets ((x u (uncons u null)))
            (same-line u (cons x d) (- dy 1))))
      (else
         (lets ((x d (uncons d null)))
            (same-line (cons x u) d (+ dy 1))))))

;; note: delta updates could be used to work around lack of clear-line-upto and allow

(define (delta-update-screen old new)
   (if (eq? old new)
      (begin
         (log " - full share")
         null)
      (lets 
         ((ou od ol or ox oy ow oh ooff meta old)
          (nu nd nl nr nx ny w h noff meta new)
          (old-this (append (reverse ol) or))
          (new-this (append (reverse nl) nr))
          (nu (cons new-this nu))
          (ou (cons old-this ou))
          (ou od (same-line ou od (- oy ny))))
         (if (equal? ooff noff)
            ;; no offset changes, so old lines may be intact
            (begin
               (log "doing delta update")
               (tio
                  (maybe-draw-lines-at-offset w (car noff) ny -1 0 nu ou)
                  (maybe-draw-lines-at-offset w (car noff) (+ ny 1) +1 (+ h 1) nd od)
                  (set-cursor nx ny)))
            (update-screen new)))))


;;;
;;; Screen movement
;;;
    
(define (scroll-right buff)
   (lets 
      ((u d l r x y w h off meta buff)
       (dx dy off)
       (step (* 2 (div w 3)))
       (buff (buffer u d l r (- x step) y w h (cons (+ dx step) dy) meta)))
      (values buff (update-screen buff))))

(define (scroll-left buff)
   (lets 
      ((u d l r x y w h off meta buff)
       (dx dy off))
      (if (eq? dx 1)
        (values buff null)
        (lets
          ((step (min dx (* 2 (div w 3))))
           (buff (buffer u d l r (+ x step) y w h (cons (- dx step) dy) meta)))
          (values buff (update-screen buff))))))

(define (scroll-down buff)
   (lets 
    ((u d l r x y w h off meta buff)
     ;(step (+ 1 (* 2 (div h 3))))
     (step 1)
     (dx dy off)
     (buff 
      (buffer u d l r x (- y step) w h (cons dx (+ dy step)) meta)))
    (values buff
      (update-screen buff))))

(define (scroll-up buff)
   (lets 
    ((u d l r x y w h off meta buff)
     (dx dy off)
     ;(step (min dy (+ 1 (* 2 (div h 3)))))
     (step (min dy 1))
     (buff 
      (buffer u d l r x (+ y step) w h (cons dx (- dy step)) meta)))
    (values buff
      (update-screen buff))))

(define (log-buff buff mode)
  (lets
    ((u d l r x y w h off meta buff)
     (dx dy off)
     (x (+ dx x))
     (y (+ dy y))
     (status (str "       " x "," y " " (if (eq? mode 'insert) "i" "c")))
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

(define (insert-handle-key buff k)
   (lets ((u d l r x y w h off meta buff))
      (lets ((node (key-node k))
             (nw (node-width node)))
         (log "adding node " node)
         (if (< (+ x nw) w)
            (begin
               (log "insert of key " k " at " (cons x y) " = " node)
               (cond
                  (else
                     (values
                        (buffer u d (cons node l) r (+ x nw) y w h off meta)
                        (encode-node node
                           (if (null? r)
                              null
                              (tio
                                 (clear-line-right)
                                 (cursor-save)
                                 (raw (take-printable r (- w (+ x nw))))
                                 (cursor-restore))))))))
            (lets
               ((buff scroll-tio (scroll-right buff))
                (buff insert-tio (insert-handle-key buff k)))
               (values buff
                  (append scroll-tio insert-tio)))))))

(define (insert-backspace buff)
   (lets ((u d l r x y w h off meta buff))
      (if (null? l)
         ;; no-op (could also backspace to line above)
         (if (null? u)
            (values buff null)
            (if (eq? y 1)
               (values buff null)
               (lets
                  ((line u (uncons u null))
                   (line-len (printable-length line))
                   (q x (quotrem (+ line-len 1) w))
                   (xp (* q w))
                   (buffp
                     (buffer u d (reverse line) r x (- y 1) w h (cons xp (cdr off)) meta)))
                  (log "backspace")
                  (values buffp
                     (delta-update-screen buff buffp)))))
         (let ((cw (node-width (car l))))
           (if (> x cw)
             (values
                (buffer u d (cdr l) r (- x cw) y w h off meta)
                (tio
                   (cursor-left cw)
                   (clear-line-right)
                   (cursor-save)
                   (raw (take-printable r (- w x)))
                   (cursor-restore)))
            (lets 
               ((buff scroll-tio (scroll-left buff))
                (buff bs-tio     (insert-backspace buff)))
               (values buff
                  (append scroll-tio bs-tio))))))))


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

(define (seek-line-end buff)
   (lets ((u d l r x y w h off meta buff)
          (step (>> w 1))
          (dx dy off))
      (if (null? r)
         buff
         (let loop ((l l) (r r) (x x) (dx dx))
            (cond
               ((null? (cdr r))
                  (buffer u d l r x y w h (cons dx dy) meta))
               ((eq? x w)
                  (loop l r (- x step) (+ dx step)))
               (else
                  (loop (cons (car r) l) (cdr r) (+ x (node-width (car r))) dx)))))))



;;;
;;; Text range operations
;;;

;; r d -> r' d' n-down n-from-left
(define (next-word start d)
   (let loop ((r start) (d d) (y 0) (x 0) (space? #false))
      (log "loop " r "," y "," x "," space?)
      (cond
         ((null? r)
            (cond
               ((null? d)
                  (values null null y x))
               ;(space?
               ;   (loop (car d) (cdr d) (+ y 1) 0 #t))
               (else
                  (loop (car d) (cdr d) (+ y 1) 0 #t))))
         ((space-char? (car r))
            (loop (cdr r) d y (+ x 1) #t))
         (space?
            (values r d y x))
         (else
            (loop (cdr r) d y (+ x 1) #f)))))

;; r d n -> dy dx r' d'
(define (next-words r d n)
   (let loop((y 0) (x 0) (r r) (d d) (n n))
      (if (eq? n 0)
         (values y x r d)
         (lets ((r d dy dx (next-word r d)))
            (if (eq? dy 0)
               (loop y (+ x dx) r d (- n 1))
               (loop (+ y dy) dx r d (- n 1)))))))

;; buff movement-exp -> n | dy dx
(define (movement buff n type)
   (lets ((u d l r x y w h off meta buff))
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

(define (cut-backward u l dy dx)
   (if (eq? dy 0)
      (lets ((all (length l))
             (l cutd (split l (max 0 (- all dx)))))
         (values u l (tuple 'sequence cutd)))
      (lets ((next-lines u (split u (- dy 1))))
         (if (eq? dx 0)
            (lets ((new-l u (uncons u null)))
               (values u new-l (tuple 'lines (append next-lines (list (reverse l))))))
            (lets ((new-l u (uncons u null))
                   (all (length new-l))
                   (new-l last-partial (split new-l (max 0 (- all dx)))))
                (values new-l u 
                   (tuple 'lines 
                      (cons last-partial (append next-lines (list (reverse l)))))))))))
 
(define (cut-forward r d dy dx)
   (if (eq? dy 0)
      (lets ((cutd r (split r dx)))
         (values r d (tuple 'sequence cutd)))
      (lets ((next-lines d (split d (- dy 1))))
         (if (eq? dx 0)
            (lets ((new-r d (uncons d null)))
               (values new-r d (tuple 'lines (cons r next-lines))))
            (lets ((new-r d (uncons d null))
                   (last-partial new-r (split new-r dx)))
                (values new-r d 
                   (tuple 'lines 
                      (cons r (append next-lines (list last-partial))))))))))
      
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
   (lets ((u d l r x y w h off meta buff))
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
                  (lets ((u d l r x y w h off meta buff))
                     (values ll 0 (length r))))
               ((eq? k #\w) 
                  (lets ((u d l r x y w h off meta buff)
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
                           (lambda (pos)
                              (lets ((u d l r x y w h off meta buff)
                                     (dy dx off)
                                     (mx my pos)
                                     (tx (+ (length l) dx))
                                     (ty (+ (- y 1) dy)))
                                 (log "relative movement up to pos" pos)
                                 (values ll (- my ty) (- mx tx)))))
                        (else
                           (values ll #f #f)))))
               (else
                  (log "get-relative-movement confused: " n ", " k ", op was " k)
                  (values ll #false #false))))
         (else
            (log "get-relative-movement confused: " op)
            (values ll #f #f)))))

(define (cut-relative-movement ll buff dy dx)
   (lets ((u d l r x y w h off meta buff))
      (cond
         ((eq? dy 0)
            (if (< dx 0)
               (lets ((rcut l (split l (* dx -1))))
                  (values ll
                     (buffer u d l r (- x (printable-length rcut)) y w h off meta)
                     (tuple 'sequence (reverse rcut))))
               (lets ((r d cut (cut-forward r d dy dx)))
                  (values ll (buffer u d l r x y w h off meta) cut))))
         ((< dy 0)
            (lets ((u l cut (cut-backward u l (* -1 dy) (* -1 dx))))
               (values ll (buffer u d l r x y w h off meta) cut)))
         (else
            (lets ((r d cut (cut-forward r d dy dx)))
               (values ll (buffer u d l r x y w h off meta) cut))))))

(define (cut-lines ll buff n)
   (lets 
      ((u d l r x y w h off meta buff)
       (d (cons (append (reverse l) r) d))
       (taken d (split d n))
       (l null)
       (r d (uncons d null))) ;; bug, but ok for now
      (values ll
         (buffer u d l r 1 y w h (cons 0 (cdr off)) meta)
         (tuple 'lines taken))))
   
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
(define (seek-matching-paren-back buff)
   (lets ((u d l r x y w h off meta buff))
      (let loop 
         ((x (length l)) (y (+ (cdr off) (- y 1))) (l l) (u u) (depth 1))
         (cond
            ((null? l)
               (if (null? u)
                  #false
                  (loop (length (car u)) (- y 1) (reverse (car u)) (cdr u) depth)))
            ((left-paren? (car l))
               (if (eq? depth 1)
                  (cons (- x 1) y)
                  (loop (- x 1) y (cdr l) u (- depth 1))))
            ((right-paren? (car l))
               (loop (- x 1) y (cdr l) u (+ depth 1)))
            (else
               (loop (- x 1) y (cdr l) u depth))))))

(define (seek-matching-paren-forward buff)
   (lets ((u d l r x y w h off meta buff))
      (let loop 
         ((x (length l)) (y (+ (cdr off) (- y 1))) (r r) (d d) (depth 0))
         (cond
            ((null? r)
               (if (null? d)
                  #false
                  (loop 0 (+ y 1) (car d) (cdr d) depth)))
            ((right-paren? (car r))
               (if (eq? depth 1)
                  (cons x y)
                  (loop (+ x 1) y (cdr r) d (- depth 1))))
            ((left-paren? (car r))
               (loop (+ x 1) y (cdr r) d (+ depth 1)))
            (else
               (loop (+ x 1) y (cdr r) d depth))))))



(define (seek-line-start buff)
   (lets ((u d l r x y w h off meta buff)
          (dx dy off)
          (buffp 
            (buffer u d null (append (reverse l) r) 1 y w h (cons 0 dy) meta)))
         buffp))

;; row+1 = y + dy, dy = row + 1 - y
(define (buffer-seek buff x y screen-y)
   (log "buffer seek" x "," y ", y row at " screen-y)
   (lets ((u d l r old-x old-y w h off meta buff)
          (lines (append (reverse u) (list (append (reverse l) r)) d))
          (u line d y (seek-line lines y))
          (step (>> w 1))
          (yp (or screen-y (if (< y h) (+ y 1) (>> h 1)))) ;; real or middle of screen
          (dy (- (+ y 1) yp))
          (buff (buffer u d null line 1 yp w h off meta)))
         ;; seek right
         (let loop ((xp 1) (pos x) (l null) (r line) (dx 0))
            (cond
               ((>= xp w)
                  (loop (- xp step) pos l r (+ dx step)))
               ((eq? pos 0)
                  (buffer u d l r xp yp w h (cons dx dy) meta))
               ((null? r)
                  (loop xp 0 l r dx))
               (else
                  (loop (+ xp (node-width (car r))) (- pos 1) (cons (car r) l) (cdr r) dx))))))

;; move line down within the same screen preserving cursor position if possible
(define (line-down buff ip)
   (lets ((u d l r x y w h off meta buff)
          (dx dy off)
          (line (append (reverse l) r))
          (u (cons line u))
          (y (+ y 1))
          (line d (uncons d null))
          (x (min x (+ (if ip 1 0) (- (printable-length line) (car off)))))
          (line-pos (+ (- x 1) (car off)))
          (l r offset (seek-in-line line line-pos)))
        ;(log "next line length is " (printable-length line) ", x=" x ", dx=" (car off) ", l='" (list->string l) "', r='" (list->string r) "', offset " offset) 
        (values
          (buffer u d l r (- x offset) y w h off meta)
          (- x offset) y)))

;; move line up within the same screen preserving cursor position if possible
(define (line-up buff ip)
   (lets ((u d l r x y w h off meta buff)
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
        (values
          (buffer u d l r (- x offset) y w h off meta)
          (- x offset) y)))

;; regex is a ^... one
(define (search-from l ls regex x y)
   (cond
      ((null? l)
         (if (null? ls)
            (values #f #f)
            (search-from (car ls) (cdr ls) regex 0 (+ y 1))))
      ((regex l)
         (values x y))
      (else
         (search-from (cdr l) ls regex (+ x 1) y))))
         
(define (find-next buff)
   (lets ((u d l r x y w h off meta buff)
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

(define (move-arrow buff dir ip)
   (lets ((u d l r x y w h off meta buff))
      (log "arrow " dir " from " (cons x y) ", dim " (cons w h))
      (cond
         ((eq? dir 'up)
            (cond
               ((null? u)
                  (values buff null))
               ((eq? y 1)
                  (lets
                    ((buff tio-scroll (scroll-up buff))
                     (buff tio-move (move-arrow buff dir ip)))
                    (values buff
                      (append tio-scroll tio-move))))
              ((not (eq? 0 (car off))) ;; there is x-offset
                (let ((next-len (printable-length (car u))))
                  (if (< next-len (car off)) ;; next line start not visible
                    (lets ;; dummy version
                      ((buff tio (move-arrow buff 'left ip))
                       (buff tio-this (move-arrow buff dir ip)))
                      (values buff (append tio tio-this)))
                    (lets ((buff x y (line-up buff ip)))
                      (values buff
                        (tio (set-cursor x y)))))))
               (else
                 (lets ((buff x y (line-up buff ip)))
                   (values buff
                     (tio (set-cursor x y)))))))
         ((eq? dir 'down)
            (cond
              ((null? d)
                (values buff null))
              ((>= y h)
                (lets
                  ((buff tio-scroll (scroll-down buff))
                   (buff tio-move (move-arrow buff dir ip)))
                  (values buff
                    (append tio-scroll tio-move))))
              ((not (eq? 0 (car off))) ;; there is x-offset
                (let ((next-len (printable-length (car d))))
                  (if (< next-len (car off)) ;; next line start not visible
                    (lets ;; dummy version
                      ((buff tio (move-arrow buff 'left ip))
                       (buff tio-this (move-arrow buff dir ip)))
                      (values buff (append tio tio-this)))
                    (lets ((buff x y (line-down buff ip)))
                      (values buff
                        (tio (set-cursor x y)))))))
              (else
                (lets ((buff x y (line-down buff ip)))
                  (values buff (tio (set-cursor x y)))))))
         ((eq? dir 'left)
            (if (null? l)
               (values buff null)
               (let ((step (node-width (car l))))
                  (if (<= x step)
                     (lets
                       ((buff scroll-tio (scroll-left buff))
                        (buff move-tio (move-arrow buff dir ip)))
                       (values buff (append scroll-tio move-tio)))
                     (values
                        (buffer u d (cdr l) (cons (car l) r) (- x step) y w h off meta)
                        (tio
                           (cursor-left step)))))))
         ((eq? dir 'right)
            (if (null? r)
               (values buff null)
               (let ((step (node-width (car r))))
                  (if (< (+ x step) w)
                     (values 
                        (buffer u d (cons (car r) l) (cdr r) (+ x step) y w h off meta)
                        (tio (cursor-right step)))
                     (lets
                       ((buff scroll-tio (scroll-right buff))
                        (buff move-tio (move-arrow buff dir ip)))
                       (values buff (append scroll-tio move-tio)))))))
         ((eq? dir 'command-right) ;; command mode
            (if (or (null? r) (null? (cdr r)))
               (values buff null)
               (let ((step (node-width (car r))))
                  (if (< (+ x step) w)
                     (values 
                        (buffer u d (cons (car r) l) (cdr r) (+ x step) y w h off meta)
                        (tio (cursor-right step)))
                     (lets
                       ((buff scroll-tio (scroll-right buff))
                        (buff move-tio (move-arrow buff dir ip)))
                       (values buff (append scroll-tio move-tio)))))))
         (else
            (log "odd line move: " dir)
            (values buff null)))))

;; special case of input not having a terminal newline can be handled 
;; in buffer metadata if necessary
(define (buffer->bytes buff)
   (lets ((u d l r x y w h off meta buff))
      (foldr render-node null
         (foldr 
            (λ (line tl)
               (append line (cons #\newline tl)))
            null
            (append (reverse u) (list (append (reverse l) r)) d)))))

(define (paste-lines-below buff lines)
   (lets ((u d l r x y w h off meta buff))
      (buffer u (append lines d) l r x y w h off meta)))

(define (paste-sequence buff lst)
   (lets ((u d l r x y w h off meta buff))
      (if (null? r)
         (buffer u d l lst x y w h off meta)
         (lets ((this r r))
            ;; paste after cursor
            (buffer u d l (cons this (append lst r)) x y w h off meta)))))

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
   (lets ((u d l r x y w h off meta buff)
          (sexp lines (cut-sexp r d 0)))
      (if sexp
         (lets 
            ((r d (uncons lines null))
             (buff (buffer u d l r x y w h off meta)))
            (values
               (put-buffer-meta buff 'yank (lines->yank sexp))
               "Copied to yank"))
         (values buff "Bad range"))))

(define (paste-yank buff)
   (lets ((u d l r x y w h off meta buff)
          (data (getf meta 'yank)))
      (cond
         ((not data)
            buff)
         ((eq? 'lines (ref data 1))
            (log "appending lines from buffer")
            (lets ((buff (paste-lines-below buff (ref data 2)))
                   (buff tio (move-arrow buff 'down #f)))
               buff))
         ((eq? 'sequence (ref data 1))
            (log "appending sequence from buffer")
            (lets ((buff (paste-sequence buff (ref data 2))))
               buff))
         (else
            (error "how do i paste " data)))))
 
(define (mark-position buff char)
   (lets 
      ((u d l r x y w h off meta buff)
       (dx dy off)
       (mark-x (+ (- x 1) dx))
       (mark-y (+ (- y 1) dy))
       (marks (get meta 'marks #empty)))
      (log "marked" (list->string (list char)) " as " (cons mark-x mark-y))
      (put-buffer-meta buff 'marks
         (put marks char (cons mark-x mark-y)))))

(define (write-buffer buff path)
   (log "writing to " path)
   (if path
      (lets
         ((port (open-output-file path))
          (lst (buffer->bytes buff))
          (n (length lst))
          (res (byte-stream->port lst port)))
         (if res
            (values #true
               (foldr render null 
                  (list "Wrote " n " bytes to '" path "'")))
            (values #false
               (foldr render null
                  (list "Failed to write to '" path "'")))))
      (begin
         (log "no path")
         (values #false
            (foldr render null
               (list "Give me a name for this"))))))

(define (maybe-keep-y old-y old-y-pos new-y h)
   (lets ((delta (- new-y old-y))
          (rel-pos (+ old-y-pos delta)))
      (cond
         ((< rel-pos 1) #false)  ;; above screen, center on it
         ((< rel-pos h) rel-pos) ;; on screen, ask y to be there
         (else #false))))        ;; below screen
   
(define (maybe-seek-matching-paren buff)
   (lets 
      ((u d l r x y w h off meta buff)
       (yp (+ (cdr off) (- y 1))))      ;; yp is at row y on screen currently
      (cond
         ((null? r)
            buff)
         ((right-paren? (car r))
            (lets ((match (seek-matching-paren-back buff)))
               (log "matching open paren result " match)
               (if match
                  (lets ((mx my match)
                         (buffp (buffer-seek buff mx my (maybe-keep-y yp y my h))))
                     buffp)
                  buff)))
         ((left-paren? (car r))
            (lets ((match (seek-matching-paren-forward buff)))
               (log "matching close paren result " match)
               (if match
                  (lets ((mx my match)
                         (buffp (buffer-seek buff mx my (maybe-keep-y yp y my h))))
                     buffp)
                  buff)))
         (else
            (log "seek-matching-paren: current is " (car r))
            buff))))

;;;
;;; Command mode actions
;;;

(define (notify buff txt)
   (lets ((u d l r x y w h off meta buff))
      (output
         (tio
            (set-cursor 1 (+ h 1))
            (clear-line)
            (font-dim)
            (raw (if (string? txt) (render txt null) txt))
            (font-normal)
            (cursor-restore)
            (set-cursor x y)))))

(define (command-regex-search ll buff undo mode r t cont)
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

(define (command-find-next ll buff undo mode r t cont)
   (lets ((buffp msg (find-next buff)))
      (cont ll buffp undo mode (or msg ""))))

(define (command-mark-position ll buff undo mode r t cont)
   (lets ((msg ll (uncons ll #false)))
      (if (eq? (ref msg 1) 'key)
         (let ((char (ref msg 2)))
            (cont ll (mark-position buff char) undo mode 
               (str "marked '" (list->string (list char)) "'")))
         (cont ll buff undo mode))))

(define (command-line-end ll buff undo mode r t cont)
   (lets ((buff (seek-line-end buff)))
      (cont ll buff undo mode)))

(define (command-line-start ll buff undo mode r t cont)
   (lets ((buff (seek-line-start buff)))
      (cont ll buff undo mode)))

(define (command-move-down ll buff undo mode r t cont)
   (lets ((buff out (move-arrow buff 'down #f)))
      (cont ll buff undo mode)))

(define (command-move-up ll buff undo mode r t cont)
   (lets ((buff out (move-arrow buff 'up #f))) 
      (cont ll buff undo mode)))

(define (command-move-right ll buff undo mode r t cont)
   (lets ((buff out (move-arrow buff 'command-right #f))) 
      (cont ll buff undo mode)))

(define (command-move-left ll buff undo mode r t cont)
   (lets ((buff out (move-arrow buff 'left #f))) 
      (cont ll buff undo mode)))

(define (command-seek-matching-paren ll buff undo mode r t cont)
   (lets ((buff (maybe-seek-matching-paren buff))) 
      (cont ll buff undo mode)))

(define (command-paste ll buff undo mode r t cont)
   (lets ((undo (push-undo undo buff))
          (buffp (paste-yank buff)))
      (cont ll buffp undo mode "pasted")))

(define (command-add-line-below ll buff undo mode r t cont)
   (cont (ilist (tuple 'key #\A) (tuple 'enter) ll) buff undo mode))

(define (command-add-line-above ll buff undo mode r t cont)
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

(define (indent-lines buff n)
   (lets
      ((u d l r x y w h off meta buff)
       (l (append l shift-lst))
       (l r (line-left l r 3))
       (d (map-n (lambda (x)(append shift-lst x)) (- n 1) d)))
      (buffer u d l r x y w h off meta)))
   
(define (command-indent ll buff undo mode n t cont)
   (lets 
      ((range ll (uncons ll #false))
       (undop (push-undo undo buff)))
      (cond
         ((equal? range (tuple 'key #\>)) ;; only line-based indenting for now
            (lets ((buffp (indent-lines buff n)))
               (cont (keys ll #\l 3) buffp undop mode)))
         ((equal? range (tuple 'key #\%))
            (lets ((dy dx (movement-matching-paren-forward buff))
                   (buffp (indent-lines buff (+ dy 1)))) ;; current line + dy down
               (cont (keys ll #\l 3) buffp undop mode)))
         ((equal? range (tuple 'key sexp-key))
            (lets ((dy dx (movement-matching-paren-forward buff))
                   (buffp (indent-lines buff (+ dy 1)))) ;; current line + dy down
               (cont (keys ll #\l 3) buffp undop mode)))
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
      ((u d l r x y w h off meta buff)
       (rlp (drop-prefix (reverse l) shift-lst)))
      (if rlp
         (lets ((l (reverse rlp))
                (l r (line-right l r 3))
                (d (map-n unindent (- n 1) d))
                (buffp (buffer u d l r x y w h off meta)))
             buffp)
          buff)))
         
(define (command-unindent ll buff undo mode n t cont)
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

(define (command-delete-char ll buff undo mode r t cont)
   (lets
      ((undo (push-undo undo buff))
       (u d l r x y w h off meta buff))
      (if (null? r)
         (if (null? l)
            (cont ll buff undo mode)
            (lets ((buff out (insert-backspace buff)))
               (cont ll buff undo mode)))
         (lets ((buffp (buffer u d l (cdr r) x y w h off meta)))
            (cont ll buffp undo mode)))))

(define (command-join-lines ll buff undo mode n t cont)
   (lets
      ((undo (push-undo undo buff))
       (buff (seek-line-end buff))
       (n (if (number? n) n 1)) ;; fixme: no interval handling
       (u d l r x y w h off meta buff))
      (let loop ((r r) (d d) (n n))
         (cond
            ((or (null? d) (eq? n 0))
               (let ((buffp (buffer u d l r x y w h off meta)))
                  (cont ll buffp undo mode)))
            (else   
               (lets
                  ((tail (drop-leading-whitespace (car d)))
                   (tail (if (whitespace? (last r #\a)) tail (cons #\space tail))))
                  (loop (append r tail) (cdr d) (- n 1))))))))

(define (command-maybe-save-and-close ll buff undo mode n t cont)
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
       
(define (command-go-to-line ll buff undo mode n t cont)
   (lets ((buff (buffer-seek buff 0 (if (number? n) (- n 1) 0) #false)))
      (cont ll buff undo mode (str "line " n))))

(define (command-go-to-mark ll buff undo mode r t cont)
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

(define (command-go-to-last-line ll buff undo mode r t cont)
   (lets 
      ((u d l r x y w h off meta buff)
       (last (+ 1 (+ (length u) (length d))))
       (buff (buffer-seek buff 0 last #false)))
      (cont ll buff undo mode "last line")))

(define (untab meta)
   (let ((tab (get meta 'tab tab-node)))
      (λ (line)
         (map 
            (λ (node)   
               (cond
                  ((eq? node #\() lp-node)
                  ((eq? node #\)) rp-node)
                  ((eq? node #\tab) tab)
                  (else node)))
            line))))
      
(define (path->lines path meta)
   (let ((fd (open-input-file path)))
      (if fd
         (map (untab meta) (map string->list (force-ll (lines fd))))
         #false)))

(define (command-enter-command ll buff undo mode r t cont)
   (output (tio* (set-cursor 1 (+ 1 (screen-height buff))) (clear-line) (list #\:)))
   (lets
      ((metadata (buffer-meta buff))
       (ll res 
       (readline ll 
         (get (buffer-meta buff) 'command-history null) 
         2 (+ 1 (screen-height buff) 1) (screen-width buff)))
       (buff
         (set-buffer-meta buff
           (put metadata 'command-history
             (cons res (get metadata 'command-history null))))))
      (log (str "readline returned '" res "'"))
      (output
         (tio 
            (set-cursor 1 (+ 1 (screen-height buff))) 
            (clear-line)
            (set-cursor (buffer-x buff) (buffer-y buff))))
      (cond
       ((not res)
          (cont ll buff undo mode))
       ((equal? res "")
          (notify buff "canceled")
          (cont ll buff undo mode))
       ((equal? res "q")
         (values ll buff undo mode 'close))
       ((equal? res "q!")
         (values ll buff undo mode 'close))
       ((equal? res "Q!")
         (values ll buff undo mode 'close-all))
       ((equal? res "n")
         (values ll buff undo mode 'new))
       ((m/^n [^ ]+$/ res)
         (values ll buff undo mode (tuple 'open (s/n +// res))))
       ((equal? res "vi")
         (cont ll buff undo 'insert))
       ((m/^w / res)
         (let ((path (s/^w +// res)))
            (lets ((ok? write-msg (write-buffer buff path)))
               (cont ll (put-buffer-meta buff 'path path) undo mode (list->string write-msg)))))
       ((m/^r +[^ ]/ res)
          (lets ((path (s/^r +// res))
                 (lines (path->lines path metadata)))
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
       ((m/^w$/ res)
         (let ((path (buffer-path buff #false)))
            (if path
               (lets ((ok? write-tio (write-buffer buff path)))
                  (notify buff write-tio)
                  (cont ll buff undo mode))
               (cont ll buff undo mode))))
       ((m/^x$/ res)
          (lets ((path (buffer-path buff #false))
                 (ok? msg (write-buffer buff path)))
             (if ok?
                ;; exit to led-buffers
                (values ll buff undo mode 'close)
                (begin
                   (notify buff msg)
                   (cont ll buff undo mode)))))
       ((m/^[0-9]+$/ res)
         (lets ((line (max 0 (- (string->number res 10) 1)))
                (buff (buffer-seek buff 0 line #false)))
            (output (update-screen buff))
            (cont ll buff undo mode)))
       ((m/^move +[0-9]+$/ res)
          (let ((n (string->number (s/^move +// res))))
             (values ll buff undo mode (tuple 'move n))))
       ((equal? res "$")
         (command-go-to-last-line ll buff undo mode r #f cont))
       (else
         (cont ll buff undo mode)))))

(define (command-redo ll buff undo mode r t cont)
  (lets ((undo buffp (unpop-undo undo buff)))
    (cont ll buffp undo mode
       (if (eq? buff buffp)
          "nothing left to redo"
          "redone. press u to re-undo."))))

(define (command-undo ll buff undo mode r t cont)
   (lets ((undo buffp (pop-undo undo buff)))
      (cont ll buffp undo mode
         (if (eq? buff buffp)
            "nothing left to undo"
            "undone. press ^r to redo."))))

(define (command-substitute-line ll buff undo mode r t cont)
   (cont (ilist (tuple 'key #\0) (tuple 'key #\C) ll) buff undo mode))

(define (command-substitute-char ll buff undo mode r t cont)
   (cont (ilist (tuple 'key #\x) (tuple 'key #\i) ll) buff undo mode))

(define (command-insert-before ll buff undo mode r t cont)
   (cont ll buff undo 'insert))

(define (command-insert-after ll buff undo mode r t cont)
   (cont (cons (tuple 'arrow 'right) ll) buff undo 'insert))

(define (command-insert-after-line ll buff undo mode r t cont)
   (cont (ilist (tuple 'key #\$) (tuple 'key #\a) ll) buff undo mode))

;; should also w to first non-space later
(define (command-insert-at-line-start ll buff undo mode r t cont)
   (cont (ilist (tuple 'key #\0) (tuple 'key #\i) ll) buff undo mode))

(define (select-lines ll buff n)
   (lets ((u d l r x y w h off meta buff))
      (if (eq? n 1)
         (values ll (length l) 0 (length r) 0)
         (values ll (length l) 0 (- n 1) 0))))
   
(define (command-delete ll buff undo mode r t cont)
   (lets ((undop (push-undo undo buff))
          (ll buffp tob (cut-movement ll buff r #\d)))
       (log "deleted " tob)
       (if tob
          (cont ll (put-buffer-meta buffp 'yank tob) undop mode)
          (cont ll buff undo mode))))
 
(define (command-change ll buff undo mode r t cont)
   ;; convert possible next c of [c]c to d, 
   (lets 
      ((next ll (uncons ll eof))
       (ll (cons (if (equal? next (tuple 'key #\c)) (tuple 'key #\d) next) ll)))
      (command-delete ll buff undo mode r t
         (lambda (ll buff undo mode)
            (cont ll buff undo 'insert)))))

(define (command-move-words ll buff undo mode r t cont)
   (lets ((dy dx (movement buff (or r 1) 'word)))
      (log "moving" r "words gives dy" dy ", dx" dx)
      (if (eq? dy 0)
         (cont (keys ll #\l dx) buff undo mode) ;; use repetitions later
         (cont (-> ll (keys #\l dx) (keys #\j dy) (keys #\0 1) ) buff undo mode))))

(define (command-yank ll buff undo mode r t cont)
   (lets ((undop (push-undo undo buff))
          (ll buffp tob (cut-movement ll buff r #\y)))
       (if tob
          (begin
             (log "cut data " tob)
             (log "target is " t)
             (cont ll (put-buffer-meta buff 'yank tob) undop mode "yanked"))
          (cont ll buff undo mode))))
 
(define (command-no-op ll buff undo mode r t cont)
   (cont ll buff undo mode))

(define (command-change-rest-of-line ll buff undo mode r t cont)
   (lets ((u d l r x y w h off meta buff)
          (undo (push-undo undo buff))
          (buff (buffer u d l null x y w h off meta)))
         ;; not having to repaint over these when switching modes reduces flicker for now
         ;(output
         ;   (tio 
         ;      (cursor-save)
         ;      (font-dim)
         ;      (raw   (repeat #\- (min (+ 1 (- w x)) (printable-length r))))
         ;      (font-normal)
         ;      (cursor-restore)))
         (cont ll buff undo 'insert)))

(define (command-step-forward ll buff undo mode r t cont)
   (lets ((u d l r x y w h off meta buff)
          (y (+ (cdr off) (- y 1)))
          (buff (buffer-seek buff (- x 1) (+ y (max 1 (- h 3))) 1)))
      (log "buffer seeking to " (cons x (+ y (max 1 (- h 3)))) " with y at " y)
      (cont ll buff undo mode)))

(define (command-step-backward ll buff undo mode r t cont)
   (lets ((u d l r x y w h off meta buff)
          (y (+ (cdr off) (- y 1)))
          (buff (buffer-seek buff (- x 1) (- y (min (- h 3) y)) 1)))
      (cont ll buff undo mode)))

(define (command-update-screen ll buff undo mode r t cont)
   (lets ((u d l r x y old-w old-h off meta buff)
          (y (+ (cdr off) (- y 1)))
          (x (+ (car off) (- x 1)))
          (w h ll (get-terminal-size ll))
          (buff (buffer u d l r x y w (max 1 (- h 1)) off meta))
          (buff (buffer-seek buff x y #false)))
      ;(log "updated screen from size " (cons old-w old-h) " to " (cons w h))
      ;(log "current left " l)
      ;(log "current left len " (printable-length l))
      ;(log "current right " r)
      ;(log "current right len " (printable-length r))
      (cont ll buff undo mode)))

(define (key-value event)
   (if (and (tuple? event) (eq? (ref event 1) 'key))
      (ref event 2)
      #false))

(define (command-replace-char ll buff undo mode ran t cont)
   (lets ((val ll (uncons ll #false))
          (k (key-value val)))
      (if k
         (lets ((u d l r x y w h off meta buff))
            (if (null? r)
               (cont ll buff undo mode)
               (lets
                  ((r (map-n (lambda (x) k) ran r))
                   (undo (push-undo undo buff))
                   (buffp
                      (buffer u d l r x y w h off meta)))
                  (cont ll buffp undo mode))))
         (cont ll buff undo mode))))

(define (command-previous-buffer ll buff undo mode r t cont)
   (values ll buff undo mode 'left))

(define (command-next-buffer ll buff undo mode r t cont)
   (values ll buff undo mode 'right))

;;; Command mode key mapping

(define *command-mode-control-actions*
   (-> #empty
      (put #\f command-step-forward)
      (put #\b command-step-backward)
      (put #\r command-redo)
      (put 'arrow-left command-previous-buffer)
      (put 'arrow-right command-next-buffer)
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
      (put #\p command-paste)
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
      (put #\Q command-previous-buffer)
      (put #\W command-next-buffer)
      (put #\C command-change-rest-of-line)
      (put #\Z command-maybe-save-and-close)
      (put #\% command-seek-matching-paren)
      (put sexp-key command-seek-matching-paren)
      ))


;;;
;;; Insert mode actions
;;;

(define (paren-balance lst)
   (fold + 0
      (map
         (lambda (x) (if (left-paren? x) 1 (if (right-paren? x) -1 0)))
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

;; up -> l
(define (artificial-intelligence u)
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
               (ilist #\space #\space #\space lead))
            (else
               (drop-spaces lead (* bal -3)))))))

;; ai hardcoded for now
(define (insert-enter buff)
   (lets
      ((u d l r x y w h off meta buff)
       (ind (artificial-intelligence (cons (reverse l) u)))
       (buffp
         (buffer u (cons (append ind r) d) l null x y w h  off meta))
       (draw-tio
         (delta-update-screen buff buffp))
       (buffp move-tio (move-arrow buffp 'down #t)))
      (values
         buffp
         (append draw-tio move-tio))))

;;;
;;; Buffer handling loop
;;;

(define (maybe-get-target ll)
   (values #false ll))

(define (update-cont self buff)
   (lambda (ll buffp undo mode . msg)
      (output (delta-update-screen buff buffp))
      (if (pair? msg)
         (let ((what (apply str msg)))
            (notify buffp what)))
      (self ll buffp undo mode)))

;; ll buff undo mode -> ll' buff' undo' mode' action
(define (led-buffer ll buff undo mode)
   (log-buff buff mode)
   (if (eq? mode 'insert)
      (lets
         ((msg ll (uncons ll #false))
          (u d l r x y w h off meta buff))
         (log "cursor " (cons x y) ", offset " off ", event " msg)
            (tuple-case msg
               ((key x)
                  (lets ((buff out (insert-handle-key buff x)))
                     (output out)
                     (if (eq? x 41) ;; close paren, highlight the match for a while (hack)
                        (led-buffer
                           (ilist (tuple 'esc)
                                  (tuple 'key #\%)
                                  (lambda ()
                                     (sleep 150)
                                     (ilist
                                        (tuple 'key #\%)
                                        (tuple 'key #\a)
                                        ll)))
                             buff undo mode)
                        (led-buffer ll buff undo mode))))
               ((tab)
                  (led-buffer (ilist space-key space-key space-key ll) buff undo mode))
               ((enter)
                  (lets ((buff out (insert-enter buff)))
                     (output out)
                     (led-buffer ll buff undo mode)))
               ((backspace)
                  (lets ((buff out (insert-backspace buff)))
                     (output out)
                     (led-buffer ll buff undo mode)))
               ((arrow dir)
                  (lets ((buff out (move-arrow buff dir #t)))
                     (output out)
                     (led-buffer ll buff undo mode)))
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
                     ((eq? key 'arrow-left)
                        (values ll buff undo mode 'left))
                     ((eq? key 'arrow-right)
                        (values ll buff undo mode 'right))
                     (else
                        (log "ignoring control " key " in insert mode")
                        (led-buffer ll buff undo mode))))
               (else
                  (led-buffer ll buff undo mode))))
      (lets ((target ll (maybe-get-target ll))
             (count ll (maybe-get-count ll 1))
             (msg ll (uncons ll space-key)))
         ;; todo: read the possible text object here based on command type, so that a function to recompute the last change can be stored for .
         (tuple-case msg
           ((key k)
               ((get *command-mode-actions* k command-no-op)
                  ll buff undo mode count target (update-cont led-buffer buff)))
           ((ctrl key)
               ((get *command-mode-control-actions* key command-no-op)
                  ll buff undo mode count target (update-cont led-buffer buff)))
           (else
               (log "not handling command " msg)
               (led-buffer ll buff undo mode))))))


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

(define (make-new-state ll)
   (lets ((w h ll (get-terminal-size ll))
          (buff (make-empty-buffer w (- h 1) #empty)))
      (values ll
         (tuple buff (initial-undo buff) 'command))))

(define (make-file-state w h path meta)
   (log "making file state out out of " path)
   (cond
      ((path->lines path meta) =>
         (lambda (data)
            (if (pair? data)
               (buffer null (cdr data) null (car data) 1 1 w h (cons 0 0) (put meta 'path path))
               (buffer null null null null 1 1 w h (cons 0 0) (put meta 'path path)))))
      ((open-output-file path) =>
         (lambda (fd)
            (log "opened new fd " fd)
            (close-port fd) ;; now input succeeds
            (log "created " path)
            (make-file-state w h path meta)))
      (else
         (log "Could not open " path)
         #false)))

(define (path->buffer-state ll path)
   (lets ((w h ll (get-terminal-size ll))
          (buff (make-file-state w h path #empty)))
      (values ll
         (if buff
            (tuple buff (initial-undo buff) 'command)
            #false))))

(define (notify-buffer-source left buff right)
   (lets ((source (buffer-path-str buff))
          (nth (+ 1 (length left)))
          (total (+ nth (length right)))
          (slider
             (map
                (lambda (x) (if (eq? x nth) #\x #\space))
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

(define (already-open? l s r path)
   (first
      (lambda (st) (equal? path (get-buffer-meta (ref st 1) 'path #false)))
      (append l (cons s r))
      #false))

(define (led-buffers ll left state right msg)
   (lets ((buff undo mode state)
          (_ (output (update-screen buff)))
          (_ (if msg (notify buff msg) (notify-buffer-source left buff right)))
          (ll buff undo mode action
            (led-buffer ll buff undo mode))
          (state (tuple buff undo mode)))
      (log "led-buffers: action " action)
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
            (lets ((ll new-state (make-new-state ll)))
               (led-buffers ll (cons state left) new-state right "new scratch buffer")))
         ((tuple? action)
            (tuple-case action
               ((open path)
                  (if (already-open? left state right path)
                     (begin
                        (notify (ref state 1) "buffer is already open")
                        (log "buffer is already open")
                        (led-buffers ll left state right 
                           (str "'" path "' is already open")))
                     (lets ((ll new (path->buffer-state ll path)))
                        (if new
                           (led-buffers ll (cons state left) new right #false)
                           (begin
                              (log "failed to open " path)
                              (led-buffers ll left state right #false))))))
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

(define (open-all-files paths w h shared-meta)
   (fold
      (lambda (states path)
         (log "loading " path)
         (and states
            (let ((buff (make-file-state w h path shared-meta)))
               (if buff
                  (cons (tuple buff (initial-undo buff) 'command)
                     states)
                  (begin
                     (print-to stderr "Failed to open '" path "'")
                     #false)))))
      null
      paths))

(define (initial-terminal-setup)
   (output
      (tio
         (clear-screen)
         (set-cursor 1 1)))
   '(output
      (fold
         (lambda (out bg)
            (fold
               (lambda (out fg)
                  (fold
                     (lambda (out att)
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
   '(sleep 10000)
   (output
      (tio
         (disable-line-wrap))))

(define (start-led dict args ll)
  (log "start-led " dict ", " args)
  (lets ((w h ll (get-terminal-size ll))
         (h (max (- h 1) 1)))
    (log "dimensions " (cons w h))
    (initial-terminal-setup)
    (lets
      ((meta
         (-> #empty
            (put 'tab tab-node)
            (put 'tab-width 3)))
       (states
         (reverse
            (if (null? args)
               (let ((buff (make-empty-buffer w h meta)))
                  (list (tuple buff (initial-undo buff) 'command)))
               (open-all-files args w h meta)))))
      (if states
         (led-buffers ll null (car states) (cdr states) #false)
         1))))

(define usage-text
  "Usage: led [flags] [file] ...")

(define command-line-rules
  (cl-rules
    `((help "-h" "--help" comment "show this thing")
      (version "-V" "--version" comment "show program version")
      (log "-L" "--log" has-arg comment "debug log file")
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

