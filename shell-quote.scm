;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC

(define (delimit delimiter strings)
  (if (null? strings) ""
      (let loop ((strings (cdr strings)) (acc (car strings)))
        (if (null? strings) acc
            (loop (cdr strings)
                  (string-append acc delimiter (car strings)))))))

(define (error* . strings) (error (delimit " " strings)))

(define (natural? x) (and (integer? x) (exact? x) (>= x 0)))

(define (the-natural x what)
  (if (natural? x) (number->string x) (error* what "must be an integer")))

(define (the-string x what)
  (if (string? x) x (error* what "must be a string")))

(define (the-natural-or-string x what)
  (cond ((string? x) x)
        ((natural? x) (number->string x))
        (else (error* what "must be an integer or string"))))

(define (bare-char? char)
  (case char ((#\- #\. #\/ #\_) #t) (else (ascii-alphanumeric? char))))

(define (bare-argument? string)
  (and (not (string=? "" string))
       (call-with-port (open-input-string string)
                       (lambda (in)
                         (let loop ()
                           (let ((char (read-char in)))
                             (or (eof-object? char)
                                 (and (bare-char? char) (loop)))))))))

(define (quote-string string)
  (if (bare-argument? string) string
      (call-with-port
       (open-input-string string)
       (lambda (in)
         (call-with-port
          (open-output-string)
          (lambda (out)
            (write-char #\" out)
            (let loop ()
              (let ((char (read-char in)))
                (unless (eof-object? char)
                  (case char ((#\$ #\` #\" #\\) (write-char #\\ out)))
                  (write-char char out)
                  (loop))))
            (write-char #\" out)
            (get-output-string out)))))))

(define (quote-arg arg)
  (cond ((string? arg) (quote-string arg))
        ((and (pair? arg) (eq? 'arg (car arg)))
         (let loop ((parts (cdr arg)) (acc ""))
           (if (null? parts) acc
               (let ((part (car parts)))
                 (cond ((and (pair? part) (eq? 'verbatim (car part)))
                        (unless (= 2 (length part))
                          (error "Bad (verbatim ...) argument"))
                        (loop (cdr parts) (string-append acc (cadr part))))
                       (else
                        (loop (cdr parts) (string-append acc part))))))))
        (else (error "command line argument not a string or glob" arg))))

(define (redir1 operator path)
  (string-append operator (quote-string (the-string path "redirect path"))))

(define (redir2 operator fd path-or-fd)
  (string-append
   (the-natural fd "file descriptor")
   operator
   (the-natural-or-string path-or-fd "redirect path or file descriptor")))

(define (cmdgroup group)
  (match group
    (('redirect group . fd-map)
     (let loop ((fd-map fd-map) (acc (cmdgroup group)))
       (if (null? fd-map) acc
           (loop (cdr fd-map)
                 (string-append
                  acc " "
                  (match (car fd-map)
                    (('in path)       (redir1 "<"  path))
                    (('in fd from)    (redir2 "<"  fd from))
                    (('out path)      (redir1 ">"  path))
                    (('out fd to)     (redir2 ">"  fd to))
                    (('append path)   (redir1 ">>" path))
                    (('append fd to)  (redir2 ">>" fd to))))))))
    (('subshell group) (string-append "(" (cmdgroup group) ")"))
    (('begin . groups)
     (string-append "{ " (delimit "; " (append (map cmdgroup groups)
                                               '("}")))))
    (('and   . groups) (delimit " && " (map cmdgroup groups)))
    (('or    . groups) (delimit " || " (map cmdgroup groups)))
    (('pipe  . groups) (delimit " | "  (map cmdgroup groups)))
    (('cmd c . args)   (delimit " "    (map quote-arg (cons c args))))))

(define shell-quote cmdgroup)
