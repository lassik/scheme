(define-library (trivial-tar-writer)
  (export tar-unix-time
          tar-unix-mode
          tar-owner-name
          tar-group-name
          tar-write-file
          tar-write-end)
  (import (scheme base)
          (scheme char)
          (scheme write))
  (include "trivial-tar-writer.scm"))
