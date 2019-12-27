;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC

(define tar-owner (make-parameter (cons 0 "root")))
(define tar-group (make-parameter (cons 0 "root")))
(define tar-unix-mode (make-parameter #o644))
(define tar-unix-time (make-parameter 0))

(define nulls (make-bytevector 512 0))
(define blank-checksum (make-bytevector 7 (char->integer #\space)))

(define (tar-string nbyte string)
  (let* ((bytes (string->utf8 string))
         (nnull (- nbyte (bytevector-length bytes))))
    (when (< nnull 1) (error "tar: string too long"))
    (bytevector-append bytes (make-bytevector nnull 0))))

(define (tar-octal nbyte number)
  (unless (integer? number) (error "tar: not an integer"))
  (when (< number 0) (error "tar: negative integer"))
  (let* ((bytes (string->utf8 (number->string number 8)))
         (nzero (- nbyte 1 (bytevector-length bytes))))
    (when (< nzero 0) (error "tar: number too big"))
    (bytevector-append (make-bytevector nzero (char->integer #\0))
                       bytes (bytevector 0))))

(define (tar-checksum . vectors)
  (let loop-vectors ((vectors vectors) (sum 0))
    (if (null? vectors) (truncate-remainder sum (expt 8 6))
        (let* ((v (car vectors)) (n (bytevector-length v)))
          (let loop-bytes ((i 0) (sum sum))
            (if (= i n) (loop-vectors (cdr vectors) sum)
                (loop-bytes (+ i 1) (+ sum (bytevector-u8-ref v i)))))))))

(define (tar-write-file fake-path bytes)
  (let* ((unix-time-now 0)
         (nbyte (bytevector-length bytes))
         (nnull (- 512 (truncate-remainder nbyte 512)))
         (header-before-checksum
          (bytevector-append
           (tar-string 100 fake-path)
           (tar-octal 8 (tar-unix-mode))
           (tar-octal 8 (car (tar-owner)))
           (tar-octal 8 (car (tar-group)))
           (tar-octal 12 nbyte)
           (tar-octal 12 (tar-unix-time))))
         (header-after-checksum
          (bytevector-append
           (bytevector (char->integer #\space))
           (bytevector (char->integer #\0))
           (tar-string 100 "")
           (tar-string 8 "ustar  ")
           (tar-string 32 (cdr (tar-owner)))
           (tar-string 32 (cdr (tar-group)))
           (make-bytevector 183 0)))
         (checksum (tar-octal 7 (tar-checksum header-before-checksum
                                              blank-checksum
                                              header-after-checksum))))
    (write-bytevector header-before-checksum)
    (write-bytevector checksum)
    (write-bytevector header-after-checksum)
    (write-bytevector bytes)
    (write-bytevector nulls (current-output-port) 0 nnull)))

(define (tar-write-end)
  (write-bytevector nulls)
  (write-bytevector nulls))
