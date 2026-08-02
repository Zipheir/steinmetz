;;; SPDX-FileCopyrightText: Copyright 2022--2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz syntax)
  (export options
          flag
          option
          )
  (import (rnrs base)
          (only (rnrs lists) member)
          (steinmetz exceptions)
          (steinmetz utility)
          (steinmetz options)
          )

  ;; Returns an ordinary argument parser that checks missing & invalid
  ;; arguments and applies *conv* to the argument token.
  (define (make-argument-parser name conv allowed-args)
    (lambda (tokens)
      (if (null? tokens)
          (missing-argument-exception name)
          (let ((t (car tokens)) (rest (cdr tokens)))
            (if (or (not allowed-args) (member t allowed-args))
                (values (conv t) rest)
                (invalid-argument-exception name t))))))

  ;;; TODO: An exception should be raised if the names of two or more
  ;;; clauses overlap.  If we switch to syntax-case, this can be an
  ;;; expand-time exception.

  (define-syntax options
    (syntax-rules ()
      ((options (e ...) ...)
       (list (opt-clause e ...) ...))))

  (define-syntax normalize
    (syntax-rules ()
      ((normalize (name0 . names))
       (map stringify '(name0 . names)))
      ((normalize name)
       (list (stringify 'name)))))

  (define-syntax opt-clause
    (syntax-rules (option flag)
      ((opt-clause flag names)
       (opt-clause flag names #f))
      ((opt-clause flag names docstr)
       (let ((nnames (normalize names)))
         (make-option nnames
                      #f
                      #f
                      docstr
                      (car nnames))))
      ((opt-clause option names)
       (opt-clause option names 'X #f))
      ((opt-clause option names arg-spec)
       (opt-clause option names arg-spec #f))
      ((opt-clause option names arg-spec docstr)
       (let ((nnames (normalize names)))
         (option/arg-spec nnames arg-spec docstr)))))

  (define-syntax option/arg-spec
    (syntax-rules ()
      ((option/arg-spec nnames (arg-name) docstr)
       (option/arg-spec nnames (arg-name values) docstr))
      ((option/arg-spec nnames (arg-name (id ...)) docstr)
       (let ((enums (map stringify '(id ...)))
             (cname (car nnames)))
         (make-option nnames
                      'arg-name
                      (make-argument-parser cname values enums)
                      docstr
                      cname
                      enums)))
      ((option/arg-spec nnames (arg-name conv) docstr)
       (let ((cname (car nnames)))
         (make-option nnames
                      'arg-name
                      (make-argument-parser cname conv #f)
                      docstr
                      cname)))
      ((option/arg-spec nnames arg-name docstr)
       (option/arg-spec nnames (arg-name values) docstr))))

  (define-syntax flag (syntax-rules ()))
  (define-syntax option (syntax-rules ()))

  )
