;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC

(define-library (unpack-assoc)
  (export unpack
          unpack-case
          unpack-using
          at-most-one-of
          exactly-one-of

          unpack-using/many
          unpack-using/one
          unpack-using/let
          unpack-using/case
          unpack-using/rule
          unpack-using/set

          unpack-case/aux)
  (import (scheme base))
  (include "unpack-assoc.scm"))
