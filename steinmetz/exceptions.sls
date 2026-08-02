;;; SPDX-FileCopyrightText: Copyright 2022--2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz exceptions)
  (export parser-condition?
          make-invalid-option-condition
          invalid-option-condition?
          invalid-option-exception
          make-invalid-argument-condition
          invalid-argument-condition?
          invalid-argument-condition-option-name
          invalid-argument-exception
          make-extra-argument-condition
          extra-argument-condition?
          extra-argument-condition-option-name
          extra-argument-exception
          make-missing-argument-condition
          missing-argument-condition?
          missing-argument-condition-option-name
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
  ;; TODO: Maybe include a field for an allowed-arguments list?
  (define-condition-type &invalid-argument &error
    make-invalid-argument-condition
    invalid-argument-condition?
    (option-name invalid-argument-condition-option-name))

  (define (invalid-argument-exception name . irritants)
    (assert (string? name))
    (raise-continuable
     (condition (make-invalid-argument-condition name)
                (make-message-condition "invalid argument")
                (make-irritants-condition irritants))))

  ;; Raised when an option without arguments got one anyway.
  (define-condition-type &extra-argument &error
    make-extra-argument-condition
    extra-argument-condition?
    (option-name extra-argument-condition-option-name))

  (define (extra-argument-exception name . irritants)
    (assert (string? name))
    (raise-continuable
     (condition (make-extra-argument-condition name)
                (make-message-condition
                 "option doesn't take an argument")
                (make-irritants-condition irritants))))

  ;; Raised when an option taking an argument did not get one.
  (define-condition-type &missing-argument &error
    make-missing-argument-condition
    missing-argument-condition?
    (option-name missing-argument-condition-option-name))

  (define (missing-argument-exception name . irritants)
    (assert (string? name))
    (raise-continuable
     (condition (make-missing-argument-condition name)
                (make-message-condition "missing option argument")
                (make-irritants-condition irritants))))

  )
