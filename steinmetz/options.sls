;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz options)
  (export make-option
          option?
          option-parser
          option-properties
          option-names
          option-argument-name
          option-help
          option-get-property
          option-add-property
          )
  (import (rnrs base)
          (rnrs records syntactic)
          (only (rnrs lists) assv)
          (prefix (srfi :1) s1:)
          )

  ;;; This library defines the basic option type and some useful
  ;;; procedures for manipulating it.  The parsers associated with
  ;;; the 'parser' slot are defined in (steinmetz parse).

  ;;;; Utility

  ;; Add (key . val) to alist, replacing any existing pair with
  ;; car key. May derange alist.
  (define (alist-update key val alist)
    (cons (cons key val)
          (s1:remove (lambda (p) (eqv? key (car p))) alist)))

  ;;;; Options

  (define-record-type (option make-option option?)
    (fields
      (immutable names option-names) ; a list of symbols
      (immutable parser option-parser)  ; an argument parser
      ;; a key/value map of option properties
      (immutable properties option-properties)))

  ;;; (Symbol . list) alist implementation of properties.

  (define (option-get-property opt key)
    (assert (option? opt))
    (cond ((assv key (option-properties opt)) => cdr)
          (else #f)))

  (define (option-add-property opt key val)
    (assert (option? opt))
    (make-option (option-names opt)
                 (option-parser opt)
                 (alist-update key val (option-properties opt))))

  (define alist->properties values)

  ;;; "Virtual" accessors.  Not every option will have these fields,
  ;;; so they are currently implemented as properties.

  (define (option-argument-name opt)
    (option-get-property opt 'argument-name))

  (define (option-help opt)
    (option-get-property opt 'help))

  ;;; Option combinators

  ;; Add a help string to opt.
  (define (option-add-help help opt)
    (assert (string? help))
    (assert (option? opt))
    (option-add-property opt 'help help))

  ;; Add an argument name (symbol) to opt.
  (define (option-add-argument-name name opt)
    (assert (symbol? name))
    (assert (option? opt))
    (option-add-property opt 'argument-name name))

  )
