(define (with-input-from-string string proc)
  (call-with-port
   (open-input-string string)
   (lambda (port)
     (parameterize ((current-input-port port))
       (proc)))))

(define (with-output-to-string proc)
  (call-with-port
   (open-output-string)
   (lambda (port)
     (parameterize ((current-output-port port))
       (proc) (get-output-string port)))))

(define (safe-without-quotes? arg)
  (define (safe-char? char)
    (case char
      ((#\_ #\- #\+ #\/ #\@ #\.) #t)
      (else (or (char<=? #\0 char #\9)
                (char<=? #\A char #\Z)
                (char<=? #\a char #\z)))))
  (and (not (= 0 (string-length arg)))
       (with-input-from-string arg
         (lambda ()
           (let loop ()
             (let ((char (read-char)))
               (or (eof-object? char) (and (safe-char? char) (loop)))))))))

(define (join-command-line double-quote args)
  (if (null? args) ""
      (with-output-to-string
        (lambda ()
          (let loop ((args args))
            (let ((arg (car args)))
              (if (safe-without-quotes? arg)
                  (write-string arg)
                  (begin (write-char #\")
                         (with-input-from-string arg double-quote)
                         (write-char #\")))
              (unless (null? (cdr args))
                (write-char #\space)
                (loop (cdr args)))))))))

(define (double-quote-posix)
  (let loop ()
    (let ((char (read-char)))
      (unless (eof-object? char)
        (case char ((#\\ #\" #\` #\$ #\newline) (write-char #\\)))
        (write-char char)
        (loop)))))

(define (double-quote-windows)
  (define (write-backslashes n) (write-string (make-string n #\\)))
  (let loop ((backslashes 0))
    (let ((char (read-char)))
      (cond ((eqv? #\\ char)
             (loop (+ backslashes 1)))
            ((eof-object? char)
             (write-backslashes (* 2 backslashes)))
            ((char=? #\" char)
             (write-backslashes (+ 1 (* 2 backslashes)))
             (write-char char)
             (loop 0))
            (else
             (write-backslashes backslashes)
             (write-char char)
             (loop 0))))))

(define (join-posix-command-line args)
  (join-command-line double-quote-posix args))

(define (join-windows-command-line args)
  (join-command-line double-quote-windows args))
