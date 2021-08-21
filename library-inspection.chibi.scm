(define (library-list)
  (map car *modules*))

(define (library-exports library-name)
  (let ((m (cdr (or (assoc library-name *modules*)
                    (error "No such library" library-name)))))
    (env-exports (module-env m))))
