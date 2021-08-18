(define-library (lassik library-inspection)
  (export library-list
          library-exports)
  (import (scheme base))
  (cond-expand
   (chibi
    (import (only (meta)
                  *modules*
                  env-exports
                  module-env)))
   (gauche
    (import (only (gauche base)
                  all-modules
                  find-module
                  module-exports
                  module-name))))
  (cond-expand

   (chibi
    (begin

      (define (library-list)
        (map car *modules*))

      (define (library-exports library-name)
        (let ((m (cdr (or (assoc library-name *modules*)
                          (error "No such library" library-name)))))
          (env-exports (module-env m))))))

   (gauche
    (begin

      (define (module-name->library-name module-name)

        (define (split-at char string)
          (let loop ((a 0) (b 0) (parts '()))
            (cond ((= a b (string-length string))
                   (reverse parts))
                  ((= b (string-length string))
                   (loop b b (cons (substring string a b) parts)))
                  ((char=? char (string-ref string b))
                   (loop (+ b 1) (+ b 1) (cons (substring string a b) parts)))
                  (else
                   (loop a (+ b 1) parts)))))

        (define (string->library-name-part string)
          (or (string->number string)
              (string->symbol string)))

        (define (srfi-number string)
          (let ((srfi- "srfi-"))
            (and (> (string-length string) (string-length srfi-))
                 (string=? srfi- (substring string 0 (string-length srfi-)))
                 (string->number (substring string (string-length srfi-)
                                            (string-length string))))))

        (let* ((string (symbol->string module-name))
               (number (srfi-number string)))
          (if number
              `(srfi ,number)
              (map string->library-name-part (split-at #\. string)))))

      (define (library-name->module-name library-name)

        (define (string-join strings delim)
          (if (null? strings) ""
              (let loop ((acc (car strings)) (strings (cdr strings)))
                (if (null? strings) acc
                    (loop (string-append acc delim (car strings))
                          (cdr strings))))))

        (define (library-name-part->string part)
          (if (number? part) (number->string part) (symbol->string part)))

        (string->symbol
         (if (and (= (length library-name) 2)
                  (eq? 'srfi (car library-name))
                  (number? (cadr library-name)))
             (string-append "srfi-" (number->string (cadr library-name)))
             (string-join (map library-name-part->string library-name)
                          "."))))

      (define (library-list)
        (map (lambda (m) (module-name->library-name (module-name m)))
             (all-modules)))

      (define (library-exports library-name)
        (define (remove match? list)
          (let loop ((acc '()) (list list))
            (if (null? list) (reverse acc)
                (loop (if (match? (car list)) acc (cons (car list) acc))
                      (cdr list)))))
        (let ((m (find-module (library-name->module-name library-name))))
          (if m (remove (lambda (x) (memq x '(*1 *1+ *2 *2+ *3 *3+ *e *history)))
                        (module-exports m))
              (error "No such library" library-name))))))))
