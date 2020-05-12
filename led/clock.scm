(define-library (led clock)

   (import
      (owl base)
      (owl date))

   (export clock-server)

   (begin

      (define tz-offset (* 60 60 2))

      (define (pad-time x)
         (if (< x 10) (str "0" x) x))

      (define (now)
         (lets ((d m y H M S (date (+ tz-offset (time)))))
            (values
               (str d "." m "." y " " (pad-time H) ":" (pad-time M))
               S)))

      (define (clock-server)
         (mail 'iomux (tuple 'alarm 1000))
         (let loop ((subscribers null))
            (lets ((envelope (wait-mail))
                   (from msg envelope)
                   (time-str s (now)))
               (cond
                  ((eq? from 'iomux) ;; the bell tolls
                     (mail 'iomux (tuple 'alarm (* 1000 (- 61 s))))
                     (if (pair? subscribers)
                        (for-each
                           (lambda (sub)
                              (mail sub (tuple 'clock time-str)))
                           subscribers))
                     (loop subscribers))
                  (else
                     (mail from (tuple 'clock time-str))
                     (loop (cons from subscribers)))))))

      ))
