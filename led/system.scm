(define-library (led system)

   (export
      directory? ;; temporary
      led-dir->list
      led-path->runes
      led-dir-recursive->list
      file-modification-time)

   (import
      (owl toplevel)
      (owl unicode)
      (owl sys)
      (owl alist))

   (begin

      ;; path -> seconds | #f
      (define (file-modification-time path)
         (if-lets ((us (alget (stat path #t) 'mtim #f)))
            (quotient us 1000000000)))

      (define leading-dot?
         (string->regex "m/^\\./"))

      (define (led-path->runes path)
         (let ((data (file->list path)))
            (and data
               (or
                  (utf8-decode data)
                  data))))

      (define (sort-paths paths)
         (sort string<?
            (remove leading-dot? paths)))

      (define (led-dir-recursive->list path)
         (if (directory? path)
            (let
               ((sorted-subs
                  (map
                     (λ (x) (str path "/" x))
                     (sort-paths (dir->list path)))))
               (foldr append null
                  (map led-dir-recursive->list sorted-subs)))
            (list path)))

      (define strip-trailing-slashes
         (string->regex "s/\\/*$//"))

      ;; path -> ("path/a" "path/b" ...) | #false
      (define (led-dir->list path)
         (lets ((path (strip-trailing-slashes path))
                (contents (dir->list path)))
            (cond
               ((not contents)
                  #false)
               ((equal? path ".")
                  (sort-paths contents))
               (else
                  (map
                     (λ (x) (str path "/" x))
                     (sort-paths contents))))))))

