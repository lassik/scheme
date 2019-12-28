#!

(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scheme read)
        (scheme write)
        (trivial-tar-writer))

(define (slurp-binary-file filename)
  (call-with-port
   (open-binary-input-file filename)
   (lambda (port)
     (let loop ((whole (make-bytevector 0)))
       (let ((part (read-bytevector 4096 port)))
         (if (eof-object? part)
             whole
             (loop (bytevector-append whole part))))))))

(define (main arguments)
  (for-each (lambda (file) (tar-write-file (string-append "test/" file)
                                           (slurp-binary-file file)))
            (cdr arguments)))

(main (command-line))
