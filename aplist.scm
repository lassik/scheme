;; Copyright 2021 Lassi Kortela
;; SPDX-License-Identifier: MIT

;;; ->

(define (alist->plist alist)
  (let loop ((tail alist) (plist '()))
    (if (null? tail) (reverse plist)
        (let ((entry (car tail)))
          (if (pair? entry)
              (loop (cdr tail)
                    (cons (cdr entry) (cons (car entry) plist)))
              (error "Invalid alist" alist))))))

(define (plist->alist plist)
  (let loop ((tail plist) (alist '()))
    (if (null? tail) (reverse alist)
        (if (and (pair? tail) (pair? (cdr tail)))
            (loop (cddr tail)
                  (cons (cons (car tail) (cadr tail)) alist))
            (error "Invalid plist" plist)))))

;;; fold

(define (alist-fold merge state alist)
  (let loop ((tail alist) (state state))
    (if (null? tail) state
        (let ((entry (car tail)))
          (if (pair? entry)
              (loop (cdr tail) (merge (car entry) (cdr entry) state))
              (error "Invalid alist" alist))))))

(define (plist-fold merge state plist)
  (let loop ((tail plist) (state state))
    (if (null? tail) state
        (if (and (pair? tail) (pair? (cdr tail)))
            (loop (cddr tail) (merge (car tail) (cadr tail) state))
            (error "Invalid plist" plist)))))

;;; for-each

(define (alist-for-each fn alist)
  (alist-fold (lambda (key val _) (fn key val) #f)
              '() alist))

(define (plist-for-each fn plist)
  (plist-fold (lambda (key val _) (fn key val) #f)
              '() plist))

;;; map

(define (alist-map fn alist)
  (reverse (alist-fold (lambda (key val acc) (cons (fn key val) acc))
                       '() alist)))

(define (plist-map fn plist)
  (reverse (plist-fold (lambda (key val acc) (cons (fn key val) acc))
                       '() plist)))
