(define-library (command-line)
  (export join-posix-command-line
          join-windows-command-line)
  (import (scheme base))
  (include "command-line.scm"))
