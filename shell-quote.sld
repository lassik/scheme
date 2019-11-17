;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC

(define-library (shell-quote)
  (export shell-quote)
  (import (scheme base) (scheme write) (chibi match))
  (cond-expand
    ((library (srfi 175)) (import (srfi 175)))
    (else (import (scheme char))
          (begin (define (ascii-alphanumeric? char)
                   (or (char-alphabetic? char) (char-numeric? char))))))
  (include "shell-quote.scm"))
