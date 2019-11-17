;; Emacs: this is -*- Scheme -*- code, not a Dockerfile.

;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC

(define-syntax only-when
  (syntax-rules ()
    ((_ condition body ...)
     (if condition (begin body ...) #f))))

(define (delimit delimiter strings)
  (if (null? strings) ""
      (let loop ((strings (cdr strings)) (acc (car strings)))
        (if (null? strings) acc
            (loop (cdr strings)
                  (string-append acc delimiter (car strings)))))))

(define (stringify s)
  (cond ((string? s) s)
        ((number? s) (number->string s))
        (else (error "Don't know how to stringify that"))))

(define (tight . ss) (delimit "" (map stringify ss)))

(define (words . ss) (delimit " " (delete #f ss)))

(define (name=value name value)
  (string-append name "=" value))

(define (string-list->json strings)
  (call-with-port (open-output-string)
                  (lambda (out)
                    (parameterize ((current-output-port out))
                      (write-char #\[)
                      (unless (null? strings)
                        (write (car strings))
                        (for-each (lambda (s) (write-string ", ") (write s))
                                  (cdr strings)))
                      (write-char #\]))
                    (get-output-string out))))

(define (quote-string-with-vars s)
  s)

(define string-or-glob? string?)
(define string-with-vars string?)
(define name? string?)
(define shellcmd list?)
(define (any? x) #t)
(define (nat x) (and (integer? x) (exact? x) (>= x 0)))

;;

(define (quote-dockerfile-instruction instruction)
  (unpack-case instruction
               ((blank-line)
                "")
               ((comment (text string?))
                ;; TODO: Multi-line string should produce multi-line comment.
                (string-append "# " text))
               ((from (image name?) (as name? ?))
                (words "FROM" image (only-when as (words "AS" as))))
               ((run (shell shellcmd ?) (exec string? *))
                (exactly-one-of shell exec)
                (words "RUN" (cond (shell (shell-quote shell))
                                   (exec  (string-list->json exec)))))
               ((cmd (shell shellcmd ?) (exec string? *))
                (exactly-one-of shell exec)
                (words "CMD" (cond (shell (shell-quote shell))
                                   (exec  (string-list->json exec)))))
               ((label (name string?) (value string?))
                (words "LABEL" (name=value name value)))
               ((expose (port nat) (protocol string? ?))
                (words "EXPOSE" (if protocol (tight port "/" protocol) port)))
               ((env (name string?) (value string?))
                (words "ENV" (name=value name value)))
               ((add (user name? ?) (group name? ?) (dst string?) (src string-or-glob? +))
                (words "ADD"
                       (only-when (or user group) (tight "--chown=" user ":" group))
                       (string-list->json (append src (list dst)))))
               ((copy (from string? ?) (user string? ?) (group string? ?)
                      (dst string?) (src string-or-glob? +))
                (words "COPY"
                       (only-when from (tight "--from=" from))
                       (only-when (or user group) (tight "--chown=" user ":" group))
                       (string-list->json (append src (list dst)))))
               ((entrypoint (shell shellcmd ?) (exec string? *))
                (exactly-one-of shell exec)
                (words "ENTRYPOINT" (cond (shell (shell-quote shell))
                                          (exec  (string-list->json exec)))))
               ((volume (mountpoint string?))
                (words "VOLUME" (string-list->json (list mountpoint))))
               ((user (user string? ?) (group string? ?) (uid nat ?) (gid nat ?))
                (exactly-one-of uid user)
                (at-most-one-of gid group)
                (at-most-one-of uid group)
                (at-most-one-of gid user)
                (words "USER" (tight (or user uid)
                                     (only-when (or group gid)
                                                (tight ":" (or group gid))))))
               ((workdir (path string-with-vars))
                (words "WORKDIR" (quote-string-with-vars path)))
               ((arg (name string?) (value string?))
                (words "ARG" (name=value name value)))
               ((stopsignal (signal name?))
                (words "STOPSIGNAL" signal))
               ((healthcheck (shell shellcmd ?) (exec string? *) (none any? *) #;option*)
                #;(at-most-one-of none option*)
                (exactly-one-of shell exec none)
                (words "HEALTHCHECK"
                       (cond (shell (words "CMD" (shell-quote shell)))
                             (exec  (words "CMD" (string-list->json exec)))
                             (none  "NONE"))))
               (else (error "Unknown Dockerfile instruction" instruction))))

(define (quote-dockerfile instructions)
  (call-with-port
   (open-output-string)
   (lambda (out)
     (for-each (lambda (x)
                 (write-string (quote-dockerfile-instruction x) out)
                 (newline out))
               instructions)
     (get-output-string out))))
