(define-library (alist)
  (export alist->plist
          plist->alist
          alist-fold
          plist-fold
          alist-for-each
          plist-for-each
          alist-map
          plist-map)
  (import (scheme base))
  (include "aplist.scm"))
