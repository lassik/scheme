(define-library (humanize)
  (export human-size human-size-si
          letteri)
  (import (scheme base) (scheme write))
  (begin

    (define letters '("B" "K" "M" "G" "T"))
    (define letterb '("B" "KB" "MB" "GB" "TB"))
    (define letteri '("B" "KiB" "MiB" "GiB" "TiB"))
    (define long-si
      '("bytes" "kilobytes" "megabytes" "gigabytes" "terabytes"))
    (define long-bi
      '("bytes" "kibibytes" "mebibytes" "gibibytes" "tebibytes"))

    (define (human-size-generic step units bytes)
      (let ((bytes (exact (truncate bytes))) (two-steps (* step step)))
        (if (or (< bytes step) (not (pair? (cdr units))))
            (values bytes 0 (car units))
            (let loop ((units (cdr units)) (bytes bytes))
              (if (or (< bytes two-steps) (not (pair? (cdr units))))
                  (let-values (((whole rem) (truncate/ bytes step)))
                    (let ((fractional-digit (floor-quotient (* 10 rem) step)))
                      (values whole fractional-digit (car units))))
                  (loop (cdr units) (truncate-quotient bytes step)))))))

    (define (human-size    units bytes) (human-size-generic 1024 units bytes))
    (define (human-size-si units bytes) (human-size-generic 1000 units bytes))

    ))
