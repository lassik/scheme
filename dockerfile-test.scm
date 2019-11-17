;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC

(import (scheme base) (scheme write) (dockerfile))

(display
 (quote-dockerfile
  '((from (image "debian") (as "build"))
    (copy (dst "bar") (src "foo") (from "build"))
    (entrypoint (exec "executable" "arg1" "arg2"))
    (entrypoint (shell (cmd "executable")))
    (run (shell (and (cmd "apt-get" "update")
                     (cmd "apt-get" "-y" "--no-install-recommends"
                          "install" "build-essential")
                     (cmd "rm" "-rf" (arg "/var/lib/apt/lists/"
                                          (verbatim "*")))))))))
