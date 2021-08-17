(library (lassik immutable-hashtable)
  (export make-eq-hashtable
          make-eqv-hashtable
          make-hashtable

          hashtable-contains?
          hashtable-entries
          hashtable-equivalence-function
          hashtable-hash-function
          hashtable-keys
          hashtable-mutable?
          hashtable-ref
          hashtable-size
          hashtable?
          immutable-hashtable?

          equal-hash
          string-ci-hash
          string-hash
          symbol-hash

          hashtable-delete
          hashtable-set
          hashtable-update)
  (import (rnrs base)
          (rename (except (rnrs hashtables)
                          make-hashtable
                          make-eq-hashtable
                          make-eqv-hashtable)
                  (hashtable?
                   rnrs-hashtable?)
                  (hashtable-mutable?
                   rnrs-hashtable-mutable?)
                  (hashtable-size
                   rnrs-hashtable-size)
                  (hashtable-contains?
                   rnrs-hashtable-contains?)
                  (hashtable-entries
                   rnrs-hashtable-entries)
                  (hashtable-equivalence-function
                   rnrs-hashtable-equivalence-function)
                  (hashtable-hash-function
                   rnrs-hashtable-hash-function)
                  (hashtable-keys
                   rnrs-hashtable-keys)
                  (hashtable-ref
                   rnrs-hashtable-ref)))
  (begin

    ;; From SRFI 1.
    (define (append-reverse list1 list2)
      (append (reverse list1) list2))

    ;; From R7RS.
    (define (assoc key alist equivalent?)
      (cond ((null? alist) #f)
            ((equivalent? key (caar alist)) (car alist))
            (else (assoc key (cdr alist) equivalent?))))

    ;;

    (define (immutable-hashtable? obj)
      (and (vector? obj) (eq? 'immutable-hashtable (vector-ref obj 0))))

    (define (hashtable? obj)
      (or (rnrs-hashtable? obj)
          (immutable-hashtable? obj)))

    (define (hashtable-mutable? table)
      (if (rnrs-hashtable? table)
          (rnrs-hashtable-mutable? table)
          #f))

    ;;

    (define (make-hashtable* hash equivalent? alist)
      (vector 'immutable-hashtable hash equivalent? alist))

    (define (make-hashtable hash equivalent?)
      (make-hashtable* hash equivalent? '()))

    (define (make-eq-hashtable)
      (make-hashtable #f eq?))

    (define (make-eqv-hashtable)
      (make-hashtable #f eqv?))

    ;;

    (define (hashtable-hash-function table)
      (if (rnrs-hashtable? table)
          (rnrs-hashtable-hash-function table)
          (vector-ref table 1)))

    (define (hashtable-equivalence-function table)
      (if (rnrs-hashtable? table)
          (rnrs-hashtable-equivalence-function table)
          (vector-ref table 2)))

    (define (hashtable-alist table)
      (vector-ref table 3))

    (define (hashtable-assoc table key)
      (assoc key
             (hashtable-alist table)
             (hashtable-equivalence-function table)))

    ;;

    (define (hashtable-contains? table key)
      (if (rnrs-hashtable? table)
          (rnrs-hashtable-contains? table key)
          (not (not (hashtable-assoc table key)))))

    (define (hashtable-keys table)
      (if (rnrs-hashtable? table)
          (rnrs-hashtable-keys table)
          (list->vector (map car (hashtable-alist table)))))

    (define (hashtable-entries table)
      (if (rnrs-hashtable? table)
          (rnrs-hashtable-entries table)
          (values (list->vector (map car (hashtable-alist table)))
                  (list->vector (map cdr (hashtable-alist table))))))

    (define (hashtable-ref table key default)
      (if (rnrs-hashtable? table)
          (rnrs-hashtable-ref table key default)
          (let ((entry (hashtable-assoc table key)))
            (if entry (cdr entry) default))))

    (define (hashtable-size table)
      (if (rnrs-hashtable? table)
          (rnrs-hashtable-size table)
          (length (hashtable-alist table))))

    ;;

    (define (hashtable-delete table key)
      (let ((equivalent? (hashtable-equivalence-function table)))
        (make-hashtable*
         (hashtable-hash-function table)
         equivalent?
         (let loop ((old-alist (hashtable-alist table))
                    (new-alist '()))
           (if (null? old-alist) (reverse new-alist)
               (loop (cdr old-alist)
                     (if (equivalent? key (caar old-alist))
                         new-alist
                         (cons (car old-alist) new-alist))))))))

    (define (hashtable-update table key update default)
      (let ((equivalent? (hashtable-equivalence-function table)))
        (make-hashtable*
         (hashtable-hash-function table)
         equivalent?
         (let loop ((old-alist (hashtable-alist table))
                    (new-alist '()))
           (cond ((null? old-alist)
                  (reverse (cons (cons key (update default))
                                 new-alist)))
                 ((equivalent? key (caar old-alist))
                  (append-reverse (cons (cons (caar old-alist)
                                              (update (cdar old-alist)))
                                        new-alist)
                                  (cdr old-alist)))
                 (else
                  (loop (cdr old-alist)
                        (cons (car old-alist)
                              new-alist))))))))

    (define (hashtable-set table key value)
      (hashtable-update table key (lambda (_) value) #f))))
