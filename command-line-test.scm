(import (scheme base) (command-line) (srfi 64))

(test-begin "command-line")

(define (esc-char char)
  (case char
    ((#\B) #\\)
    ((#\Q) #\")
    (else char)))

(define (esc s) (string-map esc-char s))

(define examples
  '(((args)
     (posix "")
     (windows ""))
    ((args "foo")
     (posix "foo")
     (windows "foo"))
    ((args "-+foo/bar@baz.qux")
     (posix "-+foo/bar@baz.qux")
     (windows "-+foo/bar@baz.qux"))
    ((args "foo" "hello world" "bar")
     (posix "foo Qhello worldQ bar")
     (windows "foo Qhello worldQ bar"))
    ((args "foo" "helloBworld" "bar")
     (posix "foo QhelloBBworldQ bar")
     (windows "foo QhelloBworldQ bar"))
    ((args "argument1" "argument 2" "BsomeBpath withBspaces")
     (posix "argument1 Qargument 2Q QBBsomeBBpath withBBspacesQ")
     (windows "argument1 Qargument 2Q QBsomeBpath withBspacesQ"))
    ((args "argument1" "she said, Qyou had me at helloQ" "BsomeBpath withBspaces")
     (posix
      "argument1 Qshe said, BQyou had me at helloBQQ QBBsomeBBpath withBBspacesQ")
     (windows
      "argument1 Qshe said, BQyou had me at helloBQQ QBsomeBpath withBspacesQ"))
    ((args "argument1" "argumentQ2" "argument3" "argument4")
     (posix "argument1 QargumentBQ2Q argument3 argument4")
     (windows "argument1 QargumentBQ2Q argument3 argument4"))
    ((args "foo" "helloBworld" "bar")
     (posix "foo QhelloBBworldQ bar")
     (windows "foo QhelloBworldQ bar"))
    ((args "BsomeBdirectory withBspacesB")
     (posix "QBBsomeBBdirectory withBBspacesBBQ")
     (windows "QBsomeBdirectory withBspacesBBQ"))))

(for-each
 (lambda (example)
   (let ((args (map esc (cdr (assoc 'args example))))
         (posix (esc (cadr (assoc 'posix example))))
         (windows (esc (cadr (assoc 'windows example)))))
     (test-equal "posix" posix (join-posix-command-line args))
     (test-equal "windows" windows (join-windows-command-line args))))
 examples)

(test-end)
