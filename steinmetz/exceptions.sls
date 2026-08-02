;;; SPDX-FileCopyrightText: Copyright 2022--2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz exceptions)
  (export parser-condition?
          make-invalid-option-condition
          invalid-option-condition?
          invalid-option-exception
          make-invalid-argument-condition
          invalid-argument-condition?
          invalid-argument-condition-option
          invalid-argument-exception
          make-extra-argument-condition
          extra-argument-condition?
          extra-argument-condition-option
          extra-argument-exception
          make-missing-argument-condition
          missing-argument-condition?
          missing-argument-condition-option
          missing-argument-exception
          )
  (import (rnrs base)
          (rnrs exceptions)
          (rnrs conditions)
          (prefix (steinmetz options) smo:)
          (prefix (srfi :152) s152:)
          )

  ;;; Inheritance is the least-portable part of the R6RS condition
  ;;; system (outside of R6RS), so I have avoided relying on it here.

  (define (parser-condition? x)
    (or (invalid-argument-condition? x)
        (invalid-option-condition? x)
        (extra-argument-condition? x)
        (missing-argument-condition? x)))

  ;; Raised when an option receives an invalid argument.
  (define-condition-type &invalid-option &condition
    make-invalid-option-condition
    invalid-option-condition?)

  (define (invalid-option-exception . irritants)
    (raise-continuable
     (condition (make-invalid-option-condition)
                (make-message-condition "invalid option")
                (make-irritants-condition irritants))))

  ;; Raised when an option receives an invalid argument.
  (define-condition-type &invalid-argument &condition
    make-invalid-argument-condition
    invalid-argument-condition?
    (option invalid-argument-condition-option))

  (define (invalid-argument-exception opt . irritants)
    (assert (smo:option? opt))
    (let* ((allowed (smo:option-allowed-arguments opt))
           (msg
            (apply string-append
                   "invalid argument"
                   (if (pair? allowed)
                       (list ": must be one of "
                             (s152:string-join allowed ", "))
                       '()))))
      (raise-continuable
       (condition (make-invalid-argument-condition opt)
                  (make-message-condition msg)
                  (make-irritants-condition irritants)))))

  ;; Raised when an option without arguments got one anyway.
  (define-condition-type &extra-argument &condition
    make-extra-argument-condition
    extra-argument-condition?
    (option extra-argument-condition-option))

  (define (extra-argument-exception opt . irritants)
    (assert (smo:option? opt))
    (raise-continuable
     (condition (make-extra-argument-condition opt)
                (make-message-condition
                 "option doesn't take an argument")
                (make-irritants-condition irritants))))

  ;; Raised when an option taking an argument did not get one.
  (define-condition-type &missing-argument &condition
    make-missing-argument-condition
    missing-argument-condition?
    (option missing-argument-condition-option))

  (define (missing-argument-exception opt . irritants)
    (assert (smo:option? opt))
    (raise-continuable
     (condition (make-missing-argument-condition opt)
                (make-message-condition "missing option argument")
                (make-irritants-condition irritants))))

  )
