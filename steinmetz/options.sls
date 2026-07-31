;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz options)
  (export make-option
          option?
          option-argument-parser
          option-names
          option-argument-name
          option-allowed-arguments
          option-docstring
          option-canonical-name
          option-user-data
          flag?
          )
  (import (rnrs base)
          (rnrs control)
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
      ;; a list of strings or #f
      (immutable allowed-arguments option-allowed-arguments)
      (immutable user-data option-user-data))) ; anything

  (define (flag? opt)
    (not (option-argument-name opt)))

  ;; Ugly case-lambda, sorry.
  (define make-option
    (case-lambda
      ((names arg-name arg-parser)
       (make-raw-option names arg-name arg-parser #f #f #f #f))
      ((names arg-name arg-parser docstring)
       (make-option names arg-name arg-parser docstring #f #f #f))
      ((names arg-name arg-parser docstring cname)
       (make-option names
                    arg-name
                    arg-parser
                    docstring
                    cname
                    #f
                    #f))
      ((names arg-name arg-parser docstring cname allowed)
       (make-option names
                    arg-name
                    arg-parser
                    docstring
                    cname
                    allowed
                    #f))
      ((names arg-name arg-parser docstring cname allowed udata)
       (assert (util:list-of-strings? names))
       (assert (if arg-name (symbol? arg-name) #t))
       (assert (if arg-parser (procedure? arg-parser) #t))
       (assert (if docstring (string? docstring) #t))
       (assert (if cname (string? cname) #t))
       (assert (if allowed (util:list-of-strings? allowed) #t))
       (make-raw-option names
                        arg-name
                        arg-parser
                        docstring
                        cname
                        allowed
                        udata))))
       
  )
