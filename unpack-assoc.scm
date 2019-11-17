;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC

(define (unpack-using/many need? type key entry)
  (if (not entry)
      (and need? (error "Required key missing" key))
      (let check ((tail (cdr entry)))
        (cond ((null? tail)
               (cdr entry))
              ((not (pair? tail))
               (error "Values do not form a proper list" key (cdr entry)))
              ((not (type (car tail)))
               (error "Key has value of the wrong type" key (car tail)))
              (else (check (cdr tail)))))))

(define (unpack-using/one need? type key entry)
  (cond ((not entry)
         (and need? (error "Required key missing" key)))
        ((not (pair? (cdr entry)))
         (error "Key is present but has no value" key))
        ((not (null? (cddr entry)))
         (error "Key has more than one value" key (cdr entry)))
        ((not (type (cadr entry)))
         (error "Key has value of the wrong type" key (cadr entry)))
        (else (cadr entry))))

(define-syntax unpack-using/let
  (syntax-rules ()
    ((_ (lets ...) () body ...)
     (let (lets ...) body ...))
    ((_ (lets ...) ((key more-stuff ...) more-rules ...) body ...)
     (unpack-using/let (lets ... (key #f)) (more-rules ...) body ...))))

(define-syntax unpack-using/case
  (syntax-rules ()
    ((_ (cases ...) this-key val)
     (case this-key
       cases ...
       (else (error "Unknown key" this-key))))
    ((_ (cases ...) this-key val (key more-stuff ...) more-rules ...)
     (unpack-using/case (cases ... ((key)
                                    (when key (error "Duplicate key" 'key))
                                    (set! key (cons 'key val))))
                        this-key val more-rules ...))))

(define-syntax unpack-using/rule
  (syntax-rules (* + ?)
    ((_ (key type *)) (set! key (unpack-using/many #f type 'key key)))
    ((_ (key type +)) (set! key (unpack-using/many #t type 'key key)))
    ((_ (key type ?)) (set! key (unpack-using/one  #f type 'key key)))
    ((_ (key type))   (set! key (unpack-using/one  #t type 'key key)))))

(define-syntax unpack-using/set
  (syntax-rules ()
    ((_ (sets ...))
     (begin sets ...))
    ((_ (sets ...) rule rules ...)
     (unpack-using/set (sets ... (unpack-using/rule rule)) rules ...))))

(define-syntax unpack-using
  (syntax-rules (key val)
    ((_ for-each-pair pairs (rules ...) body ...)
     (unpack-using/let () (rules ...)
                       (for-each-pair
                        (lambda (key val) (unpack-using/case () key val rules ...))
                        pairs)
                       (unpack-using/set () rules ...)
                       body ...))))

;;

(define (alist-for-each proc alist)
  (cond ((null? alist) #f)
        ((not (pair? alist)) (error "Not a proper list" alist))
        ((not (pair? (car alist))) (error "Alist entry is not a pair" alist))
        (else (proc (caar alist) (cdar alist))
              (alist-for-each proc (cdr alist)))))

(define-syntax unpack
  (syntax-rules ()
    ((_ alist (rules ...) body ...)
     (unpack-using alist-for-each alist (rules ...) body ...))))

(define-syntax unpack-case/aux
  (syntax-rules ()
    ((_ (cases ...) entry)
     (case (car entry) cases ...))
    ((_ (cases ...) entry (else body ...))
     (unpack-case/aux
      (cases ... (else body ...))
      entry))
    ((_ (cases ...) entry ((head rules ...) body ...))
     (unpack-case/aux
      (cases ... ((head) (unpack (cdr entry) (rules ...) body ...)))
      entry))
    ((_ (cases ...) entry ((head rules ...) body ...) more-cases ...)
     (unpack-case/aux
      (cases ... ((head) (unpack (cdr entry) (rules ...) body ...)))
      entry more-cases ...))))

(define-syntax unpack-case
  (syntax-rules (entry)
    ((_ entry-expr cases ...)
     (let ((entry entry-expr))
       (unpack-case/aux () entry cases ...)))))

;;

(define (count-truthy . lis)
  (let loop ((lis lis) (n 0))
    (if (null? lis) n (loop (cdr lis) (if (car lis) (+ n 1) n)))))

(define-syntax at-most-one-of
  (syntax-rules ()
    ((_ identifiers ...)
     (unless (<= (count-truthy identifiers ...) 1)
       (error "At most one of these must be given" '(identifiers ...))))))

(define-syntax exactly-one-of
  (syntax-rules ()
    ((_ identifiers ...)
     (unless (= (count-truthy identifiers ...) 1)
       (error "Exactly one of these must be given" '(identifiers ...))))))
