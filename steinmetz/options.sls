;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz options)
  (export make-option
          option?
          option-argument-parser
          option-properties->alist
          option-names
          option-argument-name
          option-allowed-arguments
          option-default-argument
          option-docstring
          option-canonical-name
          option-user-data
          )
  (import (rnrs base)
          (rnrs records syntactic)
          )

  ;;; This library defines the basic option type.  The parsers
  ;;; associated with the 'parser' slot are defined in
  ;;; (steinmetz parse).

  ;;;; Options

  ;; TODO: Type-checked version of constructor for export.
  (define-record-type (option make-option option?)
    (fields
      (immutable names option-names) ; a list of strings
      (immutable argument-name option-argument-name) ; a symbol or #f
      (immutable argument-parser option-argument-parser)  ; procedure
      (immutable docstring option-docstring)  ; a string or #f
      (immutable canonical-name option-canonical-name)  ; a string or #f
      (immutable default-argument option-default-argument) ; anything
      ;; a list of strings or #f
      (immutable allowed-arguments option-allowed-arguments)
      (immutable user-data option-user-data))) ; anything

  )
