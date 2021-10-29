;; Buffers
;;
;; A buffer holds the data being edited. It is a prod-tuple holding the
;; current character position, left and right character lists, current
;; selection length and line number of start of selection.
;;
;; Lines are counted from 1 for compatibility with ed & vi.
;;

(define-library (led buffer)
   (import
      (owl toplevel)
      (owl unicode)
      (only (owl sys) file? directory?)
      (only (led system) led-dir->list)
      (owl proof)
      (led log)
      (only (led render) distance-to) ;; <- likely worth relocating
      )

   (export
      buffer
      apply-delta
      unapply-delta

      seek                    ;; b n -> b' | #f
      seek-delta
      buffer-get-range        ;; b start end -> (rune ...) | #f
      buffer-char             ;; b -> next-rune | #f
      buffer-selection-delta  ;; buff i -> buff'
      get-selection           ;; b -> (rune ...)
      buffer-pos              ;; b -> n
      buffer-selection-length ;; b -> n
      set-selection-length    ;; b n -> b'
      buffer-unselect         ;; b -> b' (select null string at same pos)
      buffer-line             ;; b -> n (current line number)
      buffer-left             ;; b -> (rune ...), reverse
      buffer-right            ;; b -> (rune ...), in order
      buffer-line-pos         ;; b -> n (character position in current line)
      buffer-line-offset      ;; b -> n (visual position in current line)
      buffer-line-end-pos     ;; b -> n
      buffer-delete           ;; b -> b' (delete selection)
      empty-buffer
      string-buffer           ;; string -> b
      file-buffer             ;; path -> b | #f (utf8 decoded)
      dir-buffer
      buffer-append-to-end    ;; buff (rune ...) -> buff'
      buffer-select-current-word ;; buff -> buff'
      buffer-next-word-length ;; buff -> n
      buffer-append
      buffer-after-dot
      next-line-same-pos
      prev-line-same-pos
      first-match
      next-match
      seek-select
      buffer->bytes
      select
      select-rest-of-line
      select-lines           ;; b from to
      select-end-of-file
      select-everything
      select-parent-expression
      seek-start-of-line
      merge-selections
      select-line
      buffer-append-noselect
      buffer-replace
      buffer-line-indent
      buffer-apply           ;; b func -> b', selection replaced
   )

   (begin

      (define (buffer pos l r len line)
         (prod pos l r len line))

      (define (drop-prefix lst prefix)
         (cond
            ((null? prefix) lst)
            ((null? lst) #f)
            ((eq? (car lst) (car prefix))
               (drop-prefix (cdr lst) (cdr prefix)))
            (else #f)))

      (define (buffer-right b)
         (b (λ (pos l r len line) r)))

      (define (buffer-left b)
         (b (λ (pos l r len line) l)))

      (define (add-nl line char delta)
         (if (eq? char #\newline)
            (+ line delta)
            line))

      ;; seek a position
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

      (define (buffer-get-range b from to)
         (let ((b (seek b from)))
            (if b
               (let ((r (buffer-right b)))
                  (if (< to from)
                     #false
                     (take r (- to from))))
               #f)))

      (define (apply-delta buff delta)
         (log "delta " delta)
         (lets ((buff (seek buff (ref delta 1))))
            (if buff
               (buff
                  (lambda (pos l r len line)
                     (let ((r (drop-prefix r (ref delta 2))))
                        (if r
                           (buffer pos l (append (ref delta 3) r) (length (ref delta 3)) line)
                          #false))))
               #false)))

      (define (unapply-delta buff delta)
         (lets ((pos old new delta))
            (apply-delta buff (tuple pos new old))))

      (define (buffer-char b)
         (b (λ (pos l r len line) (if (pair? r) (car r) #false))))

      (define (length>=? l n)
         (cond
            ((eq? n 0) #true)
            ((eq? l null) #false)
            (else (length>=? (cdr l) (- n 1)))))

      (define (buffer-selection-delta b d)
         (b (λ (pos l r len line)
            (let ((lenp (max (+ len d) 0)))
               (if (or (< d 1) (length>=? r lenp))
                  (buffer pos l r lenp line)
                  (buffer pos l r len line))))))

      ;; read current selection
      (define (get-selection b)
         (b (λ (pos l r len line) (take r len))))

      (define (buffer-pos b)
         (b (λ (pos l r len line) pos)))

      ;; current line (at start of dot)
      (define (buffer-line b)
         (b (λ (pos l r len line) line)))

      (define (buffer-selection-length b)
         (b (λ (pos l r len line) len)))

      (define (set-selection-length b n)
         (b (λ (pos l r len line)
            (buffer pos l r n line))))

      (define (buffer-unselect b)
         (b (λ (pos l r len line) (buffer pos l r 0 line))))

      (define empty-buffer
         (buffer 0 null null 0 1))

      (define (string-buffer str)
         (buffer 0 null (string->list str) 0 1))

      (define (drop-seq r ds k)
         (cond
            ((null? ds) (k r))
            ((null? r)  #false)
            ((eq? (car ds) (car r))
               (drop-seq (cdr r) (cdr ds) k))
            (else
               #false)))

      ;; action = #(pos del-content new-content)
      (define (buffer-action buff action)
         (lets ((pos old new action)
                (buff (seek buff pos)))
            (if buff
               (buff
                  (lambda (pos l r len line)
                     (drop-seq r old
                        (lambda (r)
                           (buffer pos l (append new r) (length new) line)))))
               #f)))


      (define (seek-delta b n)
         (b (λ (pos l r len line) (seek b (+ pos n)))))

      (define (buffer->bytes b)
         (b (λ (pos l r len line) (utf8-encode (append (reverse l) r)))))

      ;; -> buffer | #false
      (define (file-buffer path)
         (log (str "trying to open " path " as file"))
         (if (file? path)
            (lets
               ((bytes (file->list path))
                (data (utf8-decode bytes)))
               (cond
                  ((not bytes) #false)
                  ((not data)
                     (buffer 0 null bytes 0 1))
                  (else
                     (buffer 0 null data 0 1))))
            #false))

      ;; -> buffer | #false
      (define (dir-buffer path)
         (log (str "trying to open " path " as directory"))
         (let ((paths (led-dir->list path)))
            (log "paths " paths)
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

      ;;; Word processing
      (define (whitespace? char)
         (cond
            ((eq? char #\tab) #t)
            ((eq? char #\space) #t)
            ((eq? char #\newline) #t)
            (else #f)))

      (define (word-length l)
         (let loop ((l l) (n 0) (word? #true))
            (cond
               ((null? l) n)
               ((whitespace? (car l))
                  (if word?
                     (loop l n #false) ;; stop matching word
                     (loop (cdr l) (+ n 1) #f)))
               (else
                  (if word?
                     (loop (cdr l) (+ n 1) #t)
                     n)))))

      (define (buffer-next-word-length b)
         (b (lambda (pos l r len line)
               (word-length (drop r len)))))

      ;;; do-command, switch to non-word-chars later?
      (define word-chars
         (fold (λ (ff x) (put ff x x))
            empty
            (string->list
               "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-/_!?<>.:+-*åäöÅÄÖ")))

      (define (word-char? x)
         (get word-chars x))

      (define (word-chars l)
         (cond
            ((null? l) l)
            ((word-char? (car l))
               (cons (car l) (word-chars (cdr l))))
            (else null)))

      (define (buffer-select-current-word b)
         (b (λ (pos l r len line)
            (lets ((dl (length (word-chars l)))
                   (dr (length (word-chars r))))
               (buffer-selection-delta
                  (buffer-unselect (seek b (- pos dl)))
                  (+ dl dr))))))

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

      ;; in-buffer search

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

      (define (maybe-cdr x)
         (if (pair? x)
            (cdr x)
            x))

      (define (next-match b runes start?)
         (b (λ (pos l r len line)
            (let loop ((data (if start? r (maybe-cdr r)))
                       (pos (+ pos (if start? 0 1))))
               (cond
                  ((null? data) (values #false #false))
                  ((match-prefix? data runes)
                     (values pos (length runes)))
                  (else
                     (loop (cdr data) (+ pos 1))))))))

      (define (length>=? l n)
         (cond
            ((eq? n 0) #t)
            ((null? n) #f)
            (else (length>=? (cdr l) (- n 1)))))

      (define (seek-select b pos len)
         (let ((b (seek b pos)))
            (if b
               (b (lambda (pos l r _ line)
                     (if (length>=? r len)
                        (buffer pos l r len line)
                        #f)))
               #false)))

      (define (select b from to)
         (let ((b (seek b from)))
            (cond
               ((not b) #false)     ;; not in buffer
               ((< to from) #false) ;; invalid range
               (else
                  (b (λ (pos l r len line)
                        (buffer pos l r (- to from) line)))))))

      (define (nth-offset lst nth elem)
         (if (< nth 1)
            (begin
               (log "bug: nth-offset < 1")
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

      (example
         (nth-offset '(a a x a x a a x a) 1 'x) = 2
         (nth-offset '(a a x a x a a x a) 2 'x) = 4
         (nth-offset '(a a x a x a a x a) 3 'x) = 7
         (nth-offset '(a a x a x a a x a) 4 'x) = #false)


      ;; select rest of line including newline, if there
      (define (select-rest-of-line b newline?)
         (b
            (λ (pos l r len line)
               (let ((end (nth-offset r 1 #\newline)))
                  (if end
                     (buffer pos l r (+ end (if newline? 1 0)) line)
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
                        (buffer 0 null (append (reverse l) r) 0 1)
                        #true))
                  ((< n line) ;; select a complete preceding or current line
                     (let ((start (nth-offset l (- line (- n 1)) #\newline)))
                        (if start
                           (select-rest-of-line
                              (seek b (- pos start))
                              #true)
                           )))
                  ((= n line)
                     (select-rest-of-line
                        (seek-start-of-line b)
                        #true))
                  (else
                     (let ((start (nth-offset r (- n line) #\newline)))
                        (if start
                           (select-rest-of-line
                              (seek b (+ pos (+ start 1)))
                              #true))))))))

      (define (select-everything b)
         (b
            (lambda (pos l r len line)
               (let ((r (append (reverse l) r)))
                  (buffer 0 null r (length r) 1)))))


      (define (select-end-of-file b)
         (b
            (lambda (pos l r len line)
               (lets
                  ((n (length r))
                   (l r line (walk l r n line)))
                  (buffer (+ pos n) l r 0 line)))))

      (define (distance-to-newline l)
         (let loop ((l l) (n 0))
            (cond
               ((null? l) n)
               ((eq? (car l) #\newline) n)
               (else
                  (loop (cdr l) (+ n 1))))))

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
                     (values #f #f))))))

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
                     (values #f #f))))))


      ;; now that there are different representations for characters, we should have a clear
      ;; abstraction barrier between code working on unicode code points and code working on
      ;; representations of them. this is one function where we leak such information.
      (define (offset-to-newline l)
         (let loop ((l l) (n 0))
            (cond
               ((null? l) n)
               ((eq? (car l) #\newline) n)
               ((eq? (car l) #\tab)
                  (loop (cdr l) (+ n 3)))
               (else
                  (loop (cdr l) (+ n 1))))))

      ;; character count
      (define (buffer-line-pos b)
         (b (λ (pos l r len line)
            (distance-to-newline l))))

      ;; visual position
      (define (buffer-line-offset b)
         (b (λ (pos l r len line)
            (offset-to-newline l))))

      (define (line-indent l n)
         (cond
            ((null? l) n)
            ((eq? (car l) #\space)
               (line-indent (cdr l) (cons #\space n)))
            ((eq? (car l) #\tab)
               (line-indent (cdr l) (cons #\tab n)))
            ((eq? (car l) #\newline)
               n)
            (else
               (line-indent (cdr l) null))))

      (define (buffer-line-indent b)
         (b (lambda (pos l r len line)
               (line-indent l null))))

      (define (buffer-line-end-pos b)
         (b (λ (pos l r len line)
            (distance-to-newline r))))

      (define (select-lines b from to)
         (let
            ((bf (select-line b from))
             (bt (select-line b to)))
            (if (and bf bt)
               (merge-selections bf bt)
               #false)))

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

      (define (closing-paren c)
         (cond
            ((eq? c #\() #\))
            ((eq? c #\[) #\])
            ((eq? c #\{) #\})
            (else #f)))

      (define (opening-paren c)
         (cond
            ((eq? c #\)) #\()
            ((eq? c #\]) #\[)
            ((eq? c #\}) #\{)
            (else #f)))

      (define (paren-hunt l closes len inc dec)
         (cond
            ((null? closes)
               len)
            ((null? l)
               #false)
            ((eq? (car l) (car closes))
               (paren-hunt (cdr l) (cdr closes) (+ len 1) inc dec))
            ((closing-paren (car l)) =>
               (lambda (cp)
                  (paren-hunt (cdr l) (cons cp closes) (+ len 1) inc dec)))
            ((opening-paren (car l))
               #false)
            (else
               (paren-hunt (cdr l) closes (+ len 1) inc dec))))

      (define (paren-hunter b)
         (b (λ (pos l r len line)
            (if (pair? r)
               (let ((cp (closing-paren (car r))))
                  (if cp
                     (paren-hunt (cdr r) (list cp) 1 40 41)
                     #false))
               #false))))

      (define (parent-expression-area b)
         (b (lambda (pos l r len line)
            (if (null? l)
               (values #false #false)
               (let loop ((l (cdr l)) (r (cons (car l) r)) (d 1))
                  (cond
                     ((null? r) (values #f #f))
                     ((closing-paren (car r)) =>
                        (lambda (cp)
                           (let ((len (paren-hunt (cdr r) (list cp) 1 40 41)))
                              (if (and len (> len d))
                                 (values (* -1 d) len)
                                 (if (null? l)
                                    (values #f #f)
                                    (loop (cdr l) (cons (car l) r) (+ d 1)))))))
                     ((null? l) (values #false #false))
                     (else (loop (cdr l) (cons (car l) r) (+ d 1)))))))))

      (define (select-parent-expression b)
         (lets ((back len (parent-expression-area b)))
            (and back
               (buffer-selection-delta (buffer-unselect (seek-delta b back)) len))))
))
