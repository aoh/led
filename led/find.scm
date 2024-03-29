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
      (only (owl unicode) utf8-decoder)
      (only (owl sys) file?))

   (export
      start-find-thread)

   (begin

      ;; read paths recursively to a lazy list
      (define (path-finder full-path tail)
         (cond
            ((file? full-path)
               (cons full-path tail))
            ((led-dir->list full-path) =>
               (lambda (nodes)
                  (lambda ()
                     (foldr path-finder tail nodes))))
            (else tail)))

      (define (match-here? ll pat)
         (cond
            ((null? pat)
               (values ll #t))
            ((null? ll)
               (values ll #f))
            ((pair? ll)
               (if (eq? (car ll) (car pat))
                  (lets ((llp res (match-here? (cdr ll) (cdr pat))))
                     (values (cons (car ll) llp) res))
                  (values ll #f)))
            (else
               (match-here? (ll) pat))))

      ;; ll -> (char ...) ll', grab up to newline and remove it
      (define (grab-row ll)
         (cond
            ((null? ll)
               (values ll ll))
            ((pair? ll)
               (if (eq? (car ll) #\newline)
                  (values '() (cdr ll))
                  (lets ((r llp (grab-row (cdr ll))))
                     (values
                        (cons (car ll) r)
                        llp))))
            (else
               (grab-row (ll)))))

      ; -> #(push (byte ...))
      (define (match-finder ll rowr path pat id row tl)
         (cond
            ((null? ll)
               tl)
            ((pair? ll)
               (lets ((ll matched? (match-here? ll pat)))
                  (cond
                     (matched?
                        ;(log "matched at " row " of " path)
                        (lets
                           ((post llp (grab-row ll))
                            (row-chars (append (reverse rowr) post)) ;; with prefix
                            (row-string (str path ":" row ":" (list->string row-chars) "\n"))) ;; just after match
                           (pair row-string
                              (match-finder llp '() path pat id (+ row 1) tl))))
                     ((eq? (car ll) #\newline)
                        (match-finder (cdr ll) '() path pat id (+ row 1) tl))
                     (else
                        (match-finder (cdr ll) (cons (car ll) rowr) path pat id row tl)))))
            (else
               (match-finder (ll) rowr path pat id row tl))))

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

      (define (is? x)
         (lambda (y)
            (equal? x y)))

      (define suffix-of
         (string->regex "s/.*\\.//"))

      (define (suffix-matcher suffix-options)
         (if suffix-options
            (let ((opts (split (is? #\,) (string->list suffix-options))))
               (lambda (path)
                  (first (is? (string->list (suffix-of path))) opts #f)))
            (lambda (x) #t)))

      (define (start-find-thread env chars id)
         (thread (list 'finder-of id)
            (begin
               (mail id
                  (tuple 'set-status-text "Search running..."))
               (result-sender
                  (lets/cc exit
                     ((path-ll (path-finder (get env 'find-path ".") '()))
                      (path-ll (lkeep (get env 'find-regex something) path-ll))
                      (path-ll (lkeep (suffix-matcher (get env 'find-suffixes #f)) path-ll)))
                     (lfoldn
                        (lambda (path tll)
                           (mail id
                              (tuple 'set-status-text (str "Search at " path)))
                           ;; check if parent buffer is still there
                           ;(log "find " path)
                           (ping-or-exit id exit)
                           ;; search a specific file
                           (lets
                               ((fd (open-input-file path)))
                               (if fd
                                  (lets
                                     ((ll (port->byte-stream fd))
                                      (cpll (utf8-decoder ll (lambda (a b c) (close-port fd) '()))))
                                    (pair #f (match-finder cpll '() path chars id 1 tll)))
                                  tll)))
                        '()
                        path-ll))
                  id (+ (time-ms) 100) '())
               (mail id
                  (tuple 'set-status-text "Search finished."))
               )))

))

