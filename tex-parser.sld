(define-library (tex-parser)
  (export read-tex-document)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (read-char-if))
  (include "tex-parser.scm"))