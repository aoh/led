#!/usr/bin/ol --run

(import
  (led terminal)
  (only (owl unicode) encode-point)
  (owl args))

(define version-str "led v0.1a")

(define (log . what)
   (mail 'logger what))

(define (output lst)
   (write-bytes stdout lst))

;;; Movement and insert mode edit operation

(define (buffer up down left right x y w h off meta)
   (tuple up down left right x y w h off meta))

(define (make-empty-state w h meta)
   (buffer null null null null 1 1 w h (cons 0 0) meta))

(define (buffer-meta buff) (ref buff 10))
(define (set-buffer-meta buff meta) (set buff 10 meta))

(define (put-buffer-meta buff key val)
   (set-buffer-meta buff (put (buffer-meta buff) key val)))

(define (get-buffer-meta buff key def)
   (get (buffer-meta buff) key def))

(define (buffer-x buff) (ref buff 5))
(define (buffer-y buff) (ref buff 6))

(define rp-unmatched-node
   (tuple 'replace (list 41) 1 (tio (font-bold) (raw (list 41)) (font-normal))))

(define lp-unmatched-node
   (tuple 'replace (list 40) 1 (tio (font-bold) (raw (list 40)) (font-normal))))

(define tab-node 
  (tuple 'replace 
   (list #\tab) 
   3 
   ;(list #\_ #\_ #\_)
   ;(list #\_ #\_ #\_)
   ;(list #\⇥ #\space #\space)
   (list #\space #\▹ #\space)
   ))

(define (untab meta)
   (let ((tab (get meta 'tab tab-node)))
      (λ (line)
         (map (λ (node) (if (eq? node #\tab) tab node)) line))))

(define (make-file-state w h path meta)
  (let ((data (map (untab meta) (map string->list (force-ll (lines (open-input-file path)))))))
    (if (pair? data)
      (buffer null (cdr data) null (car data) 1 1 w h (cons 0 0) 
        (put meta 'path path))
      (error "could not open " path))))

(define (screen-width buff) (ref buff 7))

(define (screen-height buff) (ref buff 8))

(define (node-screen-width x) (ref x 3))

(define (node-screen-representation x) (ref x 4))

(define (node-width x)
   (cond
      ((eq? (type x) type-fix+) 1)
      ((tuple? x) (node-screen-width x))
      (else (error "node-width: " x))))

(define (take-printable line n)
  (cond
    ((eq? n 0) null)
    ((pair? line)
      (lets ((x line line))
        (cond
          ((eq? (type x) type-fix+)
            ;; a printable unicode code point
            (if (eq? 0 (fxband x #x80))
              ;; a printable ascii range thingie (usual suspect)
              (cons x (take-printable line (- n 1)))
              (encode-point x
                (take-printable line (- n 1)))))
          ((tuple? x)
            ;; #(type actual-codepoints width-on-screen screen-codepoints)
            (lets ((type cps width output x))
              (cond
                ((eq? type 'replace)
                    (take-printable (append output line) n))
                (else
                  (error "take-printable: what is " x)))))
          (else
            (error "take-printable: what is " x)))))
    (else
      null)))

(define (render-node node tl)
   (cond
      ((eq? (type node) type-fix+)
         (encode-point node tl))
      ((and (tuple? node) (eq? (ref node 1) 'replace))
         (foldr render-node tl (ref node 2)))
      (else
         (error "render-node: what is " node))))

(define (drop-printable line n)
  (cond
    ((eq? n 0) line)
    ((pair? line)
      (lets ((x line line))
        (cond
          ((eq? (type x) type-fix+)
            (drop-printable line (- n 1)))
          ((tuple? x)
            ;; #(type actual-codepoints width-on-screen screen-codepoints)
            (lets ((type cps width output x))
               (drop-printable (append output line) n)))
          (else
            (error "drop-printable: what is " x)))))
    (else
      null)))

(define (printable-length line)
   (fold (λ (n x) (+ n (node-width x))) 0 line))

(define empty-buffer-line
   (tio
      (font-bold)
      (raw (render "~" null))
      (font-normal)
      ))

(define (draw-lines-at-offset tl w dx y dy end lines)
   (cond
      ((null? lines) 
         (draw-lines-at-offset tl w dx y dy end
            (list empty-buffer-line)))
      ((eq? y end) tl)
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
   (lets 
      ((u d l r x y w h off meta buff)
       (this (append (reverse l) r)))
      (tio
          (clear-screen)
          (draw-lines-at-offset w (car off) y -1 0 (cons this u))
          (draw-lines-at-offset w (car off) (+ y 1) +1 (+ h 1) d)
          (set-cursor x y))))

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
     (step (+ 1 (* 2 (div h 3))))
     (dx dy off)
     (buff 
      (buffer u d l r x (- y step) w h (cons dx (+ dy step)) meta)))
    (values buff
      (update-screen buff))))

(define (scroll-up buff)
   (lets 
    ((u d l r x y w h off meta buff)
     (dx dy off)
     (step (min dy (+ 1 (* 2 (div h 3)))))
     (buff 
      (buffer u d l r x (+ y step) w h (cons dx (- dy step)) meta)))
    (values buff
      (update-screen buff))))

(define (log-buff buff)
  (lets
    ((u d l r x y w h off meta buff))
    ;(log "left " l ", right " r)
    (log "log: cursor at " (cons x y) " at offset " off ", line pos " (+ (car off) (- x 1)))))
       
(define (key-node k)
   (cond
      ((eq? k #\tab)
         tab-node)
      ((eq? k 40) ;; lp
         lp-unmatched-node)
      (else k)))

(define (encode-node k tl)
   (cond 
      ((eq? (type k) type-fix+)
         (encode-point k tl))
      (else
         (foldr encode-node tl (node-screen-representation k)))))

(define (replace-node lst old new)
   (cond
      ((null? lst)
         #false)
      ((eq? (car lst) old)
         (cons new (cdr lst)))
      ((replace-node (cdr lst) old new) =>
         (λ (tl) (cons (car lst) tl)))
      (else #false)))
         
(define (find-matching-lp l u)
   (cond
      ((replace-node l lp-unmatched-node 40) =>
         (λ (l)
            (values l u #true)))
      (else
         (values l u #false))))

(define (insert-handle-key buff k)
   (lets ((u d l r x y w h off meta buff))
      (lets ((node (key-node k))
             (nw (node-width node)))
         (if (< (+ x nw) w)
            (begin
               (log "insert of key " k " at " (cons x y) " = " node)
               (cond
                  ((eq? node 41) ; rp
                     (lets ((l u found? (find-matching-lp l u)))
                        (if found?
                           (let ((buff (buffer u d (cons node l) r (+ x 1) y w h off meta)))
                              (values buff (update-screen buff)))
                           (let ((buff (buffer u d (cons rp-unmatched-node l) r (+ x 1) y w h off meta)))
                              (values buff (update-screen buff))))))
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
                   (buff
                     (buffer u d (reverse line) r x (- y 1) w h (cons xp (cdr off)) meta)))
                  (log "backspace")
                  (values buff
                     (update-screen buff)))))
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

;; lines → u line d y
(define (seek-line lines end)
   (let loop ((lines lines) (pos end) (u null))
      (cond
         ((null? lines)
            (values u null lines (- end pos)))
         ((eq? pos 0)
            (values u (car lines) (cdr lines) end))
         (else
            (loop (cdr lines) (- pos 1) (cons (car lines) u))))))

;; compute place to show the buffer horizontally to get buffer position pos to screen
(define (set-line-pos buff pos)
   (log "set-line-pos, pos " pos)
   (lets ((u d l r x y w h off meta buff)
          (dx dy off)
          (line (append (reverse l) r))
          (pos (min pos (printable-length line))))
      (if (< pos w)
         ;; can be show without scrolling, so do that
         (lets ((l r off (seek-in-line line pos))
                (x (+ 1 (- pos off))) ;; bug, but doesn't matter for now
                (buff (buffer u d l r x y w h (cons 0 dy) meta)))
            buff)
         (begin
            (log "not moving here yet")
            buff))))

(define (seek-line-end buff)
   (lets ((u d l r x y w h off meta buff)
          (step (>> w 1))
          (dx dy off))
      (let loop ((l l) (r r) (x x) (dx dx) (moved? #false))
         (cond
            ((null? r)
               (let ((buff (buffer u d l r x y w h (cons dx dy) meta)))
                  (values buff
                     (if moved? (update-screen buff) (tio (set-cursor x y))))))
            ((eq? x w)
               (loop l r (- x step) (+ dx step) #true))
            (else
               (loop (cons (car r) l) (cdr r) (+ x (node-width (car r))) dx moved?))))))

(define (left-paren? x)
   (or (eq? x 40)
       (equal? x lp-unmatched-node)))

(define (right-paren? x)
   (or (eq? x 41)
       (equal? x rp-unmatched-node)))

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
          (buff 
            (buffer u d null (append (reverse l) r) 1 y w h (cons 0 dy) meta)))
         (values buff
            (if (eq? dy 0)
               (tio (set-cursor 1 y))
               (update-screen buff)))))

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
(define (line-down buff)
   (lets ((u d l r x y w h off meta buff)
          (dx dy off)
          (line (append (reverse l) r))
          (u (cons line u))
          (y (+ y 1))
          (line d (uncons d null))
          (x (min x (+ 1 (- (printable-length line) (car off)))))
          (line-pos (+ (- x 1) (car off)))
          (l r offset (seek-in-line line line-pos)))
        ;(log "next line length is " (printable-length line) ", x=" x ", dx=" (car off) ", l='" (list->string l) "', r='" (list->string r) "', offset " offset) 
        (values
          (buffer u d l r (- x offset) y w h off meta)
          (- x offset) y)))

;; move line up within the same screen preserving cursor position if possible
(define (line-up buff)
   (lets ((u d l r x y w h off meta buff)
          (dx dy off)
          (line (append (reverse l) r))
          (d (cons line d))
          (y (- y 1))
          (line u (uncons u null))
          (x (min x (+ 1 (- (printable-length line) (car off)))))
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
            (buffer-seek buff mx my #false)
            buff)))

(define (move-arrow buff dir)
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
                     (buff tio-move (move-arrow buff dir)))
                    (values buff
                      (append tio-scroll tio-move))))
              ((not (eq? 0 (car off))) ;; there is x-offset
                (let ((next-len (printable-length (car u))))
                  (if (< next-len (car off)) ;; next line start not visible
                    (lets ;; dummy version
                      ((buff tio (move-arrow buff 'left))
                       (buff tio-this (move-arrow buff dir)))
                      (values buff (append tio tio-this)))
                    (lets ((buff x y (line-up buff)))
                      (values buff
                        (tio (set-cursor x y)))))))
               (else
                 (lets ((buff x y (line-up buff)))
                   (values buff
                     (tio (set-cursor x y)))))))
         ((eq? dir 'down)
            (cond
              ((null? d)
                (values buff null))
              ((>= y h)
                (lets
                  ((buff tio-scroll (scroll-down buff))
                   (buff tio-move (move-arrow buff dir)))
                  (values buff
                    (append tio-scroll tio-move))))
              ((not (eq? 0 (car off))) ;; there is x-offset
                (let ((next-len (printable-length (car d))))
                  (if (< next-len (car off)) ;; next line start not visible
                    (lets ;; dummy version
                      ((buff tio (move-arrow buff 'left))
                       (buff tio-this (move-arrow buff dir)))
                      (values buff (append tio tio-this)))
                    (lets ((buff x y (line-down buff)))
                      (values buff
                        (tio (set-cursor x y)))))))
              (else
                (lets ((buff x y (line-down buff)))
                  (values buff (tio (set-cursor x y)))))))
         ((eq? dir 'left)
            (if (null? l)
               (values buff null)
               (let ((step (node-width (car l))))
                  (if (<= x step)
                     (lets
                       ((buff scroll-tio (scroll-left buff))
                        (buff move-tio (move-arrow buff dir)))
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
                        (buff move-tio (move-arrow buff dir)))
                       (values buff (append scroll-tio move-tio)))))))
         (else
            (log "odd line move: " dir)
            (values buff null)))))

;;; Undo

(define (initial-undo buff)
   (cons (list buff) null))

;; fixme - should have current at head of undo for dirtiness, or keep track otherwise
(define (push-undo undo buff)
   (log "pushing new version")
   (lets ((prev new undo))
      ;; no way to return to future after changing the past
      (cons (cons buff prev) null)))

(define (pop-undo undo buff)
   (log "popping undo")
   (lets ((prev new undo))
      (if (null? prev)
         (values undo buff)
         (values
            (cons (cdr prev) (cons buff new))
            (car prev)))))

(define (unpop-undo undo buff)
   (log "unpopping undo")
   (lets ((prev new undo))
      (if (null? new)
         (values undo buff)
         (values (cons (cons buff prev) (cdr new)) (car new)))))

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

(define (delete-line buff)
   (lets ((u d l r x y w h off meta buff)
          (next d (uncons d null)))
      (values
         (buffer u d null next 1 y w h off meta)
         (append (reverse l) r))))

(define (paste-yank buff)
   (lets ((u d l r x y w h off meta buff)
          (data (getf meta 'yank)))
      (cond
         ((not data)
            buff)
         ((eq? 'lines (ref data 1))
            (log "appending " (ref data 2))
            (lets ((buff (buffer u (append (ref data 2) d) l r x y w h off meta))
                   (buff tio (move-arrow buff 'down)))
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
   (lets
      ((port (open-output-file path))
       (lst (buffer->bytes buff))
       (n (length lst))
       (res (byte-stream->port lst port)))
      (if res
         (foldr render null 
            (list "Wrote " n " bytes to '" path "'"))
         (foldr render null
            (list "Failed to write to '" path "'")))))

(define (maybe-seek-matching-paren buff)
   (lets 
      ((u d l r x y w h off meta buff))
      (cond
         ((null? r)
            (values buff null))
         ((right-paren? (car r))
            (lets ((match (seek-matching-paren-back buff)))
               (log "matching open paren result " match)
               (if match
                  (lets ((x y match)
                         (buff (buffer-seek buff x y #false)))
                     (values buff 
                        (update-screen buff)))
                  (values buff null))))
         ((left-paren? (car r))
            (lets ((match (seek-matching-paren-forward buff)))
               (log "matching close paren result " match)
               (if match
                  (lets ((x y match)
                         (buff (buffer-seek buff x y #false)))
                     (values buff 
                        (update-screen buff)))
                  (values buff null))))
         (else
            (log "seek-matching-paren: current is " (car r))
            (values buff null)))))


;;;
;;; Command mode actions
;;;

(define (command-regex-search ll buff undo mode cont)
   (output (tio* (set-cursor 1 (screen-height buff)) (clear-line) (list #\/)))
   (lets ((search-history 
            (get (buffer-meta buff) 'search-history null))
         (ll res (readline ll search-history
                     2 (screen-height buff) (screen-width buff)))
         (buff 
            (if (equal? res "") buff
               (put-buffer-meta buff 'search-history 
                  (cons res search-history))))
         (regex ;; note: ^ and $ need special handling later
            (string->regex (str "m/^" res "/")))
         (buff 
            (if (equal? res "")
               buff
               (-> buff (put-buffer-meta 'search-regex regex))))
         (buff 
            (find-next buff)))
      (output (update-screen buff))
      (cont ll buff undo mode)))

(define (command-find-next ll buff undo mode cont)
   (lets ((buff (find-next buff)))
      (output (update-screen buff))
      (cont ll buff undo mode)))

(define (command-mark-position ll buff undo mode cont)
   (lets ((msg ll (uncons ll #false)))
      (if (eq? (ref msg 1) 'key)
         (let ((char (ref msg 2)))
            (cont ll (mark-position buff char) undo mode))
         (cont ll buff undo mode))))

(define (command-line-end ll buff undo mode cont)
   (lets ((buff out (seek-line-end buff)))
      (output out)
      (cont ll buff undo mode)))

(define (command-line-start ll buff undo mode cont)
   (lets ((buff out (seek-line-start buff)))
      (output out)
      (cont ll buff undo mode)))

(define (command-move-down ll buff undo mode cont)
   (lets ((buff out (move-arrow buff 'down))) 
      (output out) 
      (cont ll buff undo mode)))

(define (command-move-up ll buff undo mode cont)
   (lets ((buff out (move-arrow buff 'up))) 
      (output out) 
      (cont ll buff undo mode)))

(define (command-move-right ll buff undo mode cont)
   (lets ((buff out (move-arrow buff 'right))) 
      (output out) 
      (cont ll buff undo mode)))

(define (command-move-left ll buff undo mode cont)
   (lets ((buff out (move-arrow buff 'left))) 
      (output out) 
      (cont ll buff undo mode)))

(define (command-seek-matching-paren ll buff undo mode cont)
   (lets ((buff out (maybe-seek-matching-paren buff))) 
      (output out) 
      (cont ll buff undo mode)))

(define (command-paste ll buff undo mode cont)
   (lets ((undo (push-undo undo buff))
          (buff (paste-yank buff)))
      (output (update-screen buff))
      (cont ll buff undo mode)))

(define (command-add-line-below ll buff undo mode cont)
   (lets
      ((undo (push-undo undo buff))
       (u d l r x y w h off meta buff)
       (buff (buffer u (cons null d) l r x y w h off meta))
       (buff tio (move-arrow buff 'down)))
      (output (update-screen buff))
      (cont ll buff undo 'insert)))

;; key → (ll buff undo mode cont → (cont ll' buff' undo' mode'))
(define *command-mode-actions*
   (-> #empty
      (put #\/ command-regex-search)
      (put #\n command-find-next)
      (put #\m command-mark-position)
      (put #\$ command-line-end)
      (put #\0 command-line-start)
      (put #\j command-move-down)
      (put #\k command-move-up)
      (put #\l command-move-right)
      (put #\h command-move-left)
      (put #\p command-paste)
      (put #\o command-add-line-below)
      (put #\% command-seek-matching-paren)))


;;;
;;; Insert mode actions
;;;


;;;
;;; Buffer handling loop
;;;

(define space-node (tuple 'key #\space))

(define (led-buffer ll buff undo mode)
   (log-buff buff)
   (lets 
      ((msg ll (uncons ll #false))
       (u d l r x y w h off meta buff))
      (log "cursor " (cons x y) ", offset " off ", event " msg)
      (if (eq? mode 'insert)
         (tuple-case msg
            ((key x)
               (if (eq? x #\tab)
                  (led-buffer (ilist space-node space-node space-node ll) buff undo mode)
                  (lets ((buff out (insert-handle-key buff x)))
                     (output out)
                     (led-buffer ll buff undo mode))))
            ((enter)
               (lets 
                  ((u d l r x y w h off meta buff)
                   (buff 
                     (buffer u (cons r d) null (reverse l) 1 
                        y w h (cons 0 (cdr off)) meta))
                   (draw-tio 
                     (if (eq? (car off) 1) 
                        (tio (clear-line-right)) 
                        (update-screen buff)))
                   (buff move-tio (move-arrow buff 'down)))
                  (output
                     (append draw-tio move-tio))
                  (led-buffer ll buff undo mode)))
            ((backspace)
               (lets ((buff out (insert-backspace buff)))
                  (output out)
                  (led-buffer ll buff undo mode)))
            ((arrow dir)
               (lets ((buff out (move-arrow buff dir)))
                  (output out)
                  (led-buffer ll buff undo mode)))
            ((end-of-text) 
               (led-buffer ll buff (push-undo undo buff) 'command))
            ((esc)         
               (log "switching out of insert mode on esc")
               (led-buffer ll buff (push-undo undo buff) 'command))
            (else
               (led-buffer ll buff undo mode)))
         ;; command mode
         (tuple-case msg
           ((key k)
               (let ((action (getf *command-mode-actions* k)))
                  (if action
                     (action ll buff undo mode led-buffer)
                     (cond 
                        ((eq? k #\h)
                           (lets ((buff out (move-arrow buff 'left)))
                              (output out)
                              (led-buffer ll buff undo mode)))
                        ((eq? k #\') ;; go to marked position
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
                                          (output (update-screen buff))
                                          (led-buffer ll buff undo mode))
                                       (led-buffer ll buff undo mode))))))
                        ((eq? k #\:) ;; enter command interactively
                          (output (tio* (set-cursor 1 (screen-height buff)) (clear-line) (list #\:)))
                          (lets
                             ((metadata (buffer-meta buff))
                              (ll res 
                               (readline ll 
                                 (get (buffer-meta buff) 'command-history null) 
                                 2 (screen-height buff) (screen-width buff)))
                              (buff
                                 (set-buffer-meta buff
                                   (put metadata 'command-history
                                     (cons res (get metadata 'command-history null))))))
                             (log (str "readline returned '" res "'"))
                             (output
                              (tio 
                                 (set-cursor 1 (screen-height buff)) 
                                 (clear-line)
                                 (set-cursor (buffer-x buff) (buffer-y buff))
                                 ))
                             (cond
                               ((equal? res "q")
                                (output
                                  (tio
                                    (raw (list #\newline))
                                    (set-cursor 1 (screen-height buff))))
                                    0)
                               ((equal? res "vi")
                                 (led-buffer ll buff undo 'insert))
                               ((m/^w / res)
                                 (let ((path (s/^w +// res)))
                                    (log "saving buffer to " path)
                                    (lets ((write-tio (write-buffer buff path)))
                                       (output
                                          (tio
                                             (cursor-save)
                                             (set-cursor 1 (screen-height buff))
                                             (raw write-tio)
                                             (cursor-restore)))
                                       (led-buffer ll 
                                          (put-buffer-meta buff 'path path)
                                          undo mode))))
                               ((m/^w$/ res)
                                 (let ((path (getf (buffer-meta buff) 'path)))
                                    (if path
                                       (lets ((write-tio (write-buffer buff path)))
                                          (output
                                             (tio
                                                (cursor-save)
                                                (set-cursor 1 (screen-height buff))
                                                (raw write-tio)
                                                (cursor-restore)))
                                          (led-buffer ll buff undo mode))
                                       (led-buffer ll buff undo mode))))
                               ((m/^[0-9]+$/ res)
                                 (lets ((line (max 0 (- (string->number res 10) 1)))
                                        (buff (buffer-seek buff 0 line #false)))
                                    (output (update-screen buff))
                                    (led-buffer ll buff undo mode)))
                               (else
                                 (led-buffer ll buff undo mode)))))
                        ((eq? k #\u)
                           (lets ((undo buff (pop-undo undo buff)))
                              (output (update-screen buff))
                              (led-buffer ll buff undo mode)))
                        ((eq? k #\i)
                           (led-buffer ll buff undo 'insert))
                        ((eq? k #\d)
                           (lets ((what ll (uncons ll #\d)))
                              (cond
                                 ((equal? what (tuple 'key #\d))
                                    (log "removing a line")
                                    (lets ((buff this (delete-line buff)))
                                       (output (update-screen buff))
                                       (led-buffer ll
                                          (put-buffer-meta buff 'yank (tuple 'lines (list this)))
                                          (push-undo undo buff) mode)))
                                 (else
                                    (log "cannot delete " what " yet")
                                    (led-buffer ll buff undo mode)))))
                        (else
                           (log "not handling command " msg)
                           (led-buffer ll buff undo mode))))))
           ((ctrl key)
             (cond
               ((eq? key #\r)
                 (lets ((undo buff (unpop-undo undo buff))) ;; does not keep track of dirtiness
                   (output (update-screen buff))
                   (led-buffer ll buff undo 'command)))
               ((eq? key #\f)
                  (lets ((u d l r x y w h off meta buff)
                         (y (+ (cdr off) (- y 1)))
                         (buff (buffer-seek buff (- x 1) (+ y (max 1 (- h 3))) 1)))
                     (log "buffer seeking to " (cons x (+ y (max 1 (- h 3)))) " with y at " y)
                     (output (update-screen buff))
                     (led-buffer ll buff undo mode)))
               ((eq? key #\b)
                  (lets ((u d l r x y w h off meta buff)
                         (y (+ (cdr off) (- y 1)))
                         (buff (buffer-seek buff (- x 1) (- y (min (- h 3) y)) 1)))
                     (output (update-screen buff))
                     (led-buffer ll buff undo mode)))
               (else
                 (led-buffer ll buff undo mode))))
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
           (raw (font-bold (render version-str null)))
           (font-normal)
           (set-cursor (max 1 (- mw 6)) (+ mh 2))
           (raw (render "esc + :q quits" null))
           (set-cursor 1 1)))))

(define (start-led dict args ll)
  (log "start-led " dict ", " args)
  (lets ((w h ll (get-terminal-size ll)))
    (log "dimensions " (cons w h))
    (lets 
      ((state 
         (-> #empty
            (put 'tab tab-node)
            (put 'tab-width 3)
            (put 'path (if (null? args) #false (car args)))))
       (buff 
        (if (= (length args) 1)
          (make-file-state w h (car args) #empty)
          (make-empty-state w h state))))
      (output (update-screen buff))
      (led-buffer ll buff (initial-undo buff) 'command))))

(define usage-text 
  "Usage: led [flags] [file]")

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
      (halt 0)
      (begin
        (print "error: " env)
        (mail 'logger (list 'ERROR env))
        (wait 100)
        (halt 1)))))

(define (sink arg)
   (sink (wait-mail)))

(define (log-to port)
   (print-to port (ref (wait-mail) 2))
   (log-to port))

(define (logger meta)
   (let ((log-path (getf meta 'log)))
      (if log-path
         (let ((port (open-output-file log-path)))
            (if port
               (begin
                  (print-to port "Started logging")
                  (log-to port))
               (begin
                  (print-to stderr "could not open log file " log-path)
                  (halt 1))))
         (sink #f))))

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
      (fork-linked-server 'logger (λ () (logger dict)))
      (fork-linked-server 'led 
         (λ () (start-led dict args (led-input-stream dict))))
      (log "started")
      (trampoline))))

(define (main args)
  (process-arguments (cdr args) command-line-rules usage-text start-led-threads))

main































