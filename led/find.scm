;;;
;;; File Content Search
;;;
;
; Led provides functionality for searching for fixed strings from text
; files in subdirectories of current working directory. The operation
; is essentially equal to `grep -n <searchstring> -R .`, with some
; extra filtering settable via find- -prefixed settings. Search is done
; by a background thread, which pushes results to end of the a specified
; buffer thread.
;

(define-library (led find)

   (import
      (owl toplevel)
      (led system)
      (led log)
      (only (owl unicode) utf8-decode)
      (only (owl sys) file?))

   (export
      start-find-thread)

   (begin

      (define (path-finder full-path tail)
         (cond
            ((file? full-path)
               (cons full-path tail))
            ((led-dir->list full-path) =>
               (lambda (nodes)
                  (lambda ()
                     (foldr path-finder tail nodes))))
            (else tail)))

      (define (match-here? data pat)
         (cond
            ((null? pat) #t)
            ((null? data) #f)
            ((eq? (car data) (car pat))
               (match-here? (cdr data) (cdr pat)))
            (else #f)))

      (define (grab-row lst)
         (if (or (null? lst) (eq? (car lst) #\newline))
            '()
            (cons (car lst)
               (grab-row (cdr lst)))))

      ; -> #(push (byte ...))
      (define (match-finder data row-start path pat id row tl)
         (cond
            ((null? data)
               tl)
            ((match-here? data pat)
               (lets
                  ((row-chars (grab-row row-start))
                   (row-string (str path ":" row ":" (list->string row-chars) "\n")))
                  (pair row-string
                     (match-finder (cdr data) row-start path pat id row tl))))
            ((eq? (car data) #\newline)
               (match-finder (cdr data) (cdr data) path pat id (+ row 1) tl))
            (else
               (match-finder (cdr data) row-start path pat id row tl))))

      (define send-interval-ms 500)

      (define (flush id rlines)
         (mail id
            (tuple 'push
               (fold
                  (lambda (tl s)
                     (append (string->list s) tl))
                  '()
                  rlines)))
         (wait 100)) ; <- this has frequently turned out to be useful. provide as (next-thread)?

      (define (result-sender ress id at rlines)
         (cond
            ((null? ress)
               (flush id rlines))
            ((pair? ress)
               (let ((rlines (if (car ress) (cons (car ress) rlines) rlines))
                     (now (time-ms)))
                  (if (> now at)
                     (begin
                        (if (pair? rlines)
                           (begin
                              (log "FLUSH " (list now at))
                              (flush id rlines)))
                        (result-sender (cdr ress) id (+ (time-ms) send-interval-ms) '()))
                     (result-sender (cdr ress) id at rlines))))
            (else
               (result-sender (ress) id at rlines))))

      (define (ping-or-exit tid exit)
         (mail tid (tuple 'ping))
         (let loop ((n 1000))
            (cond
               ((eq? n 0)
                  ;; setting status text in case it really is bogged down
                  (mail tid
                     (tuple 'set-status-text "no pong, exiting search"))
                  (exit '()))
               ((check-mail) =>
                  (lambda (envelope)
                     ;(log "pong at " n)
                     ;; no other mails are sent here
                     envelope))
               (else
                  (set-ticker 0)
                  (loop (- n 1))))))

      (define something (string->regex "m/./"))

      (define (lfoldn op state ll)
         (cond
            ((null? ll)
               state)
            ((pair? ll)
               (op (car ll)
                  (lambda ()
                     (lfoldn op state (cdr ll)))))
            (else
               (lfoldn op state (ll)))))

      (define (start-find-thread env chars id)
         (thread (list 'finder-of id)
            (begin
               (mail id
                  (tuple 'set-status-text "Search running..."))
               (result-sender
                  (lets/cc exit
                     ((path-ll (path-finder (get env 'find-path ".") '()))
                      (path-ll (lkeep (get env 'find-regex something) path-ll)))
                     (lfoldn
                        (lambda (path tll)
                           ;; check if parent buffer is still there
                           ;(log "find " path)
                           (ping-or-exit id exit)
                           ;; search a specific file
                           (lets
                               ((data (file->list path))            ;; <- streaming would reduce memory load
                                (data (utf8-decode (or data '()))))
                              (if data
                                 (pair #f (match-finder data data path chars id 1 tll))
                                 tll)))
                        '()
                        path-ll))
                  id (+ (time-ms) 100) '())
               (mail id
                  (tuple 'set-status-text "Search finished."))
               )))

))

