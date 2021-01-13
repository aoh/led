(define-library (led system)

   (export
      directory? ;; temporary
      led-dir->list
      led-dir-recursive->list)

   (import
      (owl toplevel)
      (owl sys))

   (begin

      (define leading-dot?
         (string->regex "m/^\\./"))

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

