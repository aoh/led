;;;
;;; Primitive edit actions (buffer → buffer')
;;;

(define-library (led ops)
   
   (export 
       op-delete-lines   ;; buff from to target → buff'
       op-paste-register ;; buff where reg → buff'
       
       buffer-seek seek-line meta-dimensions paste-register ;; temp exports
       scroll-right scroll-left move-arrow seek-line-start 
       paste-line-sequence-before paste-lines-below cut-lines
       )
    
   (import
      (owl base)
      (led buffer)
      (led log)
      (led node))
  
   (begin 

      (define (meta-dimensions meta)
        (let ((glob (get meta 'global #empty)))
           (values 
              (get glob 'width 20)
              (get glob 'height 10))))
     
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

      (define (seek-line-start buff)
         (lets ((u d l r x y off meta buff)
                (dx dy off)
                (buffp 
                  (buffer u d null (append (reverse l) r) 1 y (cons 0 dy) meta)))
               buffp))


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
 
      ;;;
      ;;; Screen movement
      ;;;
       
      (define (scroll-right buff)
         (lets 
            ((u d l r x y off meta buff)
             (w h (meta-dimensions meta))
             (dx dy off)
             (step (* 2 (quotient w 3)))
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
                ((step (min dx (* 2 (quotient w 3))))
                 (buff (buffer u d l r (+ x step) y (cons (- dx step) dy) meta)))
                buff))))

      (define (scroll-down buff)
         (lets 
          ((u d l r x y off meta buff)
           (w h (meta-dimensions meta))
           ;(step (+ 1 (* 2 (quotient h 3))))
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
     
      ;; todo: ll no longer needed here 
      (define (cut-lines ll buff n)
         (lets 
            ((u d l r x y off meta buff)
             (d (cons (append (reverse l) r) d))
             (taken d (lsplit d n))
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
         
      (define (paste-lines-below buff lines replace-empty?)
         (lets ((u d l r x y off meta buff))
            (if (and replace-empty? (null? u))
               (lets ((first rest (uncons lines null)))
                  (buffer u (append rest d) l (append first r) x y off meta))
               (buffer u (append lines d) l r x y off meta))))

      (define (paste-sequence-after buff lst)
         (lets ((u d l r x y off meta buff))
            (if (null? r)
               (buffer u d l lst x y off meta)
               (lets ((this r r))
                  ;; paste after cursor
                  (buffer u d l (cons this (append lst r)) x y off meta)))))

      (define (paste-line-sequence-before buff lst)
         (if (null? lst)
            (error "empty line sequnce: " lst)
            (lets 
               ((u d l r x y off meta buff)
                (first-tail lst (uncons lst null))
                (last-head lstr (uncons (reverse lst) null)))
               (buffer
                  u 
                  (append (reverse lstr)
                     (cons (append last-head r) d))
                  l
                  first-tail 
                  x y off meta))))
      
      (define (maybe-join-partials a b d)
         (let ((new (append a b)))
            (if (null? new)
               d
               (cons new d))))
                      
      ;; paste 0-n lines with partial ones at both ends
      (define (paste-line-sequence buff lst)
         (lets ((u d l r x y off meta buff)
                (this lst (uncons lst null))
                (fulls lasts (lsplit lst (- (length lst) 1)))
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
 
      (define (paste-register buff reg replace-empty?)
         (lets ((data (get-copy-buffer buff reg #false)))
            (cond
               ((not data)
                  buff)
               ((eq? 'lines (ref data 1))
                  (log "appending lines from buffer")
                  (lets ((buff (paste-lines-below buff (ref data 2) replace-empty?))
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
      
      (define (delete-lines-below buff n target)
         (lets ((ll buff lines (cut-lines null buff n)))
            (put-copy-buffer buff target lines)))
                   
      (define (op-delete-lines buff from to target)
         (lets ((buff (buffer-seek buff 0 (- from 1) #false)))
            (delete-lines-below buff (+ 1 (- to from)) target)))
     
      (define (op-paste-register buff where reg replace-empty?)
         (let ((data (get-copy-buffer buff reg #false)))
            (if data
               (paste-register
                  (buffer-seek buff 0 (- where 1) #false)
                  reg replace-empty?)
               buff)))
))

