(define-library (lassik library-inspection)
  (export library-list
          library-exports)
  (import (scheme base))
  (cond-expand
   (chibi
    (import (only (meta)
                  *modules*
                  env-exports
                  module-env)))
   (gauche
    (import (only (gauche base)
                  all-modules
                  find-module
                  module-exports
                  module-name))))
  (cond-expand
   (chibi  (include "library-inspection.chibi.scm"))
   (gauche (include "library-inspection.gauche.scm"))))
