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
          (prefix (steinmetz utility) util:)
          )

  ;;; This library defines the basic option type.  The parsers
  ;;; associated with the 'parser' slot are defined in
  ;;; (steinmetz parse).

  ;;;; Options

  (define-record-type (option make-raw-option option?)
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

  ;; Ugly case-lambda, sorry.
  (define make-option
    (case-lambda
      ((names arg-name arg-parser)
       (make-raw-option names arg-name arg-parser #f #f #f #f #f))
      ((names arg-name arg-parser docstring)
       (make-option names arg-name arg-parser docstring #f #f #f #f))
      ((names arg-name arg-parser docstring cname)
       (make-option names
                    arg-name
                    arg-parser
                    docstring
                    cname
                    #f
                    #f
                    #f))
      ((names arg-name arg-parser docstring cname default)
       (make-option names
                    arg-name
                    arg-parser
                    docstring
                    cname
                    default
                    #f
                    #f))
      ((names arg-name arg-parser docstring cname default allowed)
       (make-option names
                    arg-name
                    arg-parser
                    docstring
                    cname
                    default
                    allowed
                    #f))
      ((names arg-name arg-parser docstring cname default allowed udata)
       (assert (util:list-of-strings? names))
       (assert (symbol? arg-name))
       (assert (procedure? arg-parser))
       (assert (and docstring (string? docstring)))
       (assert (and cname (string? cname)))
       (assert (and allowed (util:list-of-strings? allowed)))
       (make-raw-option names
                        arg-name
                        arg-parser
                        docstring
                        cname
                        default
                        allowed
                        udata))))
       
  )
