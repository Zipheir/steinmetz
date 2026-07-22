;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz options)
  (export make-option
          option?
          option-argument-parser
          option-properties->alist
          option-names
          option-argument-name
          option-property-ref
          option-set-property
          )
  (import (rnrs base)
          (rnrs records syntactic)
          (only (rnrs lists) assv)
          (prefix (srfi :1) s1:)
          )

  ;;; This library defines the basic option type and some useful
  ;;; procedures for manipulating it.  The parsers associated with
  ;;; the 'parser' slot are defined in (steinmetz parse).

  ;;; Idea for solving the "canonical name" problem: Make it a field.
  ;;; This way, the high-level constructors can pick one according to
  ;;; some rule, but it'll still be possible to choose your own if you
  ;;; use the core constructor.

  ;;;; Utility

  ;; Add (key . val) to alist, replacing any existing pair with
  ;; car key. May derange alist.
  (define (alist-update key val alist)
    (cons (cons key val)
          (s1:remove (lambda (p) (eqv? key (car p))) alist)))

  ;;;; Options

  ;; TODO: Type-checked version of constructor for export.
  (define-record-type (option make-option option?)
    (fields
      (immutable names option-names) ; a list of strings
      (immutable argument-name option-argument-name) ; a symbol or #f
      (immutable argument-parser option-argument-parser)  ; procedure
      ;; a key/value map of option properties
      (immutable properties option-properties)))

  (define (option-properties->alist opt)
    (option-properties opt))

  ;;; (Symbol . list) alist implementation of properties.

  (define (option-property-ref opt key)
    (assert (option? opt))
    (cond ((assv key (option-properties->alist opt)) => cdr)
          (else #f)))

  (define (option-set-property opt key val)
    (assert (option? opt))
    (make-option (option-names opt)
                 (option-argument-name opt)
                 (option-argument-parser opt)
                 (alist-update key val (option-properties->alist opt))))

  )
