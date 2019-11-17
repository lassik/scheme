;; Emacs: this is -*- Scheme -*- code, not a Dockerfile.

;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC

(define-library (dockerfile)
  (export quote-dockerfile-instruction
          quote-dockerfile)
  (import (scheme base) (scheme write) (srfi 1)
          (chibi match) (unpack-assoc) (shell-quote))
  (include "dockerfile.scm"))
