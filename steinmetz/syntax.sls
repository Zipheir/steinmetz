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
  (define (make-argument-parser opt conv)
    (let ((allowed-args (option-allowed-arguments opt)))
      (lambda (tokens)
        (if (null? tokens)
            (missing-argument-exception opt)
            (let ((t (car tokens)) (rest (cdr tokens)))
              (if (or (not allowed-args) (member t allowed-args))
                  (values (conv t) rest)
                  (invalid-argument-exception opt t)))))))

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
       (let* ((enums (map stringify '(id ...)))
              (opt (make-option nnames
                                'arg-name
                                #f
                                docstr
                                (car nnames)
                                enums)))
         (set-option-argument-parser!
          opt
          (make-argument-parser opt values))
         opt))
      ((option/arg-spec nnames (arg-name conv) docstr)
       (let ((opt (make-option nnames
                               'arg-name
                               #f
                               docstr
                               (car nnames))))
         (set-option-argument-parser!
          opt
          (make-argument-parser opt conv))
         opt))
      ((option/arg-spec nnames arg-name docstr)
       (option/arg-spec nnames (arg-name values) docstr))))

  (define-syntax flag (syntax-rules ()))
  (define-syntax option (syntax-rules ()))

  )
