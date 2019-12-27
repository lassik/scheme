(define-library (trivial-tar-writer)
  (export tar-owner
          tar-group
          tar-unix-mode
          tar-unix-time
          tar-write-file
          tar-write-end)
  (import (scheme base)
          (scheme char)
          (scheme write))
  (include "trivial-tar-writer.scm"))
