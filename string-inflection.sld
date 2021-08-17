;; Copyright 2020 Lassi Kortela
;; SPDX-License-Identifier: ISC

;; The algorithm is based on the Emacs Lisp string-inflection package
;; by akicho8, but rewritten to use recursion instead of regular
;; expressions and expanded to recognize special characters in Lisp.

(define-library (lassik string-inflection)
  (export string-inflection-split
          string-inflection-lisp
          string-inflection-underscore
          string-inflection-caps-upper
          string-inflection-caps-lower)
  (import (scheme base) (scheme char))
  (begin

    (define (string-inflection-split str)
      (define (char-alphanumeric? char)
        (or (char-alphabetic? char) (char-numeric? char)))
      (define (split-off run runs)
        (if (null? run) runs (cons (list->string (reverse run)) runs)))
      (let loop ((runs '()) (run '()) (chars (string->list str)))
        (if (null? chars) (reverse (split-off run runs))
            (let ((char (car chars)))
              (cond ((or (char=? #\- char) (char=? #\_ char))
                     (loop (split-off run runs) '() (cdr chars)))
                    ((or (char=? #\! char) (char=? #\? char))
                     (loop (cons (string char) (split-off run runs))
                           '() (cdr chars)))
                    ((and (not (null? run))
                          (or (and (char-upper-case? char)
                                   (not (char-upper-case? (car run))))
                              (not (eqv? (char-alphanumeric? char)
                                         (char-alphanumeric? (car run))))))
                     (loop (split-off run runs) (list char) (cdr chars)))
                    ((and (not (char-upper-case? char))
                          (not (null? run))
                          (not (null? (cdr run)))
                          (char-upper-case? (car run))
                          (char-upper-case? (cadr run)))
                     (loop (split-off (cdr run) runs)
                           (list char (car run))
                           (cdr chars)))
                    (else
                     (loop runs (cons char run) (cdr chars))))))))

    (define (string-titlecase str)
      (string-append (string (char-upcase (string-ref str 0)))
                     (string-downcase (substring str 1 (string-length str)))))

    (define (join-between between runs)
      (if (null? runs) ""
          (let loop ((so-far (car runs)) (runs (cdr runs)))
            (if (null? runs) so-far
                (loop (string-append so-far between (car runs))
                      (cdr runs))))))

    (define (string-inflection-lisp str)
      (join-between "-" (string-inflection-split str)))

    (define (string-inflection-underscore str)
      (string-downcase (join-between "_" (string-inflection-split str))))

    (define (string-inflection-caps-upper str)
      (apply string-append
             (map string-titlecase (string-inflection-split str))))

    (define (string-inflection-caps-lower str)
      (let ((runs (string-inflection-split str)))
        (apply string-append (cons (string-downcase (car runs))
                                   (map string-titlecase (cdr runs))))))))
