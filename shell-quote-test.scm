;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC

(import (scheme base) (scheme write) (shell-quote))

(define (q x)
  (display (shell-quote x))
  (newline))

(q '(pipe (cmd "date" "+%Y/%m/%d")
          (cmd "tr" "/" "X")))

(q '(and (cmd "test" "-e" "/etc/rc")
         (cmd "test" "-l" "/etc/localtime")))

(q '(begin (cmd "echo" "hello world")))

(q '(redirect (cmd "echo" "hello world")
              (out 2 "/dev/null")))
