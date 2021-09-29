;;; Interactively examine binary files from a Scheme REPL.

(define-library (lassik peek)
  (export peek-open
          peek-port
          origin
          peek
          peek*)
  (import (scheme base) (scheme file))
  (cond-expand
    (gambit
     (import (only (gambit)
                   input-port-byte-position
                   bitwise-ior
                   arithmetic-shift))))
  (begin

    (define peek-port (make-parameter #f))

    (define origin (make-parameter 0))

    (define (peek-open filename)
      (let ((old-port (peek-port)))
        (when old-port
          (close-input-port old-port)))
      (origin 0)
      (peek-port #f)
      (let ((new-port (open-binary-input-file filename)))
        (peek-port new-port)
        new-port))

    (define-syntax abs
      (syntax-rules ()
        ((abs n expr)
         (parameterize ((origin n))
           expr))))

    (define-syntax rel
      (syntax-rules ()
        ((rel n expr)
         (parameterize ((origin (+ n (origin))))
           expr))))

    (define (peek-exactly-n-bytes n)
      (let ((port (peek-port)))
        (input-port-byte-position port (origin))
        (let ((bytes (read-bytevector n port)))
          (if (= n (bytevector-length bytes))
              bytes
              (error "How?")))))

    (define (peek-bytevectors count n-byte)
      (let ((vector (make-vector count #f)))
        (let loop ((i 0))
          (if (= i count) (vector->list vector)
              (begin (vector-set! vector i
                                  (parameterize ((origin (+ (origin)
                                                            (* i n-byte))))
                                    (peek-exactly-n-bytes n-byte)))
                     (loop (+ i 1)))))))

    (define (decode-unsigned-word byte-order bytes)
      (let ((n (bytevector-length bytes)))
        (let-values (((initial-shift shift-change)
                      (case byte-order
                        ((big-endian)
                         (values (* 8 (- n 1)) -8))
                        ((little-endian)
                         (values 0 8))
                        (else
                         (error "What?")))))
          (let loop ((value 0) (shift initial-shift) (i 0))
            (if (= i n) value
                (loop (bitwise-ior
                       value
                       (arithmetic-shift (bytevector-u8-ref bytes i)
                                         shift))
                      (+ shift shift-change)
                      (+ i 1)))))))

    (define (peek-ints count n-byte byte-order signedness base)
      (map (lambda (bytes)
             (let* ((unsigned (decode-unsigned-word byte-order bytes))
                    (signed (case signedness
                              ((unsigned)
                               unsigned)
                              ((signed)
                               unsigned) ;TODO
                              (else
                               (error "What?")))))
               (number->string signed base)))
           (peek-bytevectors count n-byte)))

    (define (peek-chars count n-byte byte-order)
      #f)

    (define (peek-floats count n-byte byte-order)
      #f)

    (define (peek* things)
      (define (bad-thing thing)
        (error "What?" thing))
      (let ((count 1)
            (n-byte 1)
            (byte-order 'little-endian)
            (signedness 'unsigned)
            (base 16)
            (decode 'ints))
        (for-each (lambda (thing)
                    (cond ((and (integer? thing)
                                (exact-integer? thing)
                                (positive? thing))
                           (set! count thing))
                          ((symbol? thing)
                           (case thing
                             ((base-2 bin binary)
                              (set! base 2))
                             ((base-8 oct octal)
                              (set! base 8))
                             ((base-10 dec decimal)
                              (set! base 10))
                             ((base-16 hex hexadecimal)
                              (set! base 16))
                             ((big-endian little-endian)
                              (set! byte-order thing))
                             ((signed unsigned)
                              (set! signedness thing))
                             ((ints floats chars)
                              (set! decode thing))
                             ((1-byte) (set! n-byte 1))
                             ((2-byte) (set! n-byte 2))
                             ((3-byte) (set! n-byte 3))
                             ((4-byte) (set! n-byte 4))
                             ((8-byte) (set! n-byte 8))
                             (else (bad-thing thing))))
                          (else
                           (bad-thing thing))))
                  things)
        (case decode
          ((ints)
           (peek-ints count n-byte byte-order signedness base))
          ((chars)
           (peek-chars count n-byte byte-order signedness))
          ((floats)
           (peek-floats count n-byte byte-order)))))

    (define-syntax peek
      (syntax-rules ()
        ((peek things ...)
         (peek* '(things ...)))))))
