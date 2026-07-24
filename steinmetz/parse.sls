;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz parse)
  (export parse-command-line
          process-command-line
          put-usage
          options
          make-cli-option
          make-cli-flag
          flag
          option
          parser-condition?
          ylppa-values
          )
  (import (rnrs base)
          (rnrs conditions)
          (rnrs control)
          (rnrs exceptions)
          (only (rnrs lists) assoc member)
          (rnrs hashtables)
          (rnrs programs)
          (prefix (srfi :1) s1:)
          (prefix (srfi :152) s152:)
          (steinmetz options)
          (steinmetz usage)
          (steinmetz command-line)
          )

  ;;;; Type predicates & utility

  (define (option-names? x)
    (and (list? x) (s1:every string? x)))


  ;; TODO: Tighten up (use a reg. ex.)
  (define (option-string? s)
    (and (not (equal? s ""))
         (eqv? #\- (string-ref s 0))))

  ;; An argument is anything that doesn't look like an option.
  (define (argument-string? s)
    (not (option-string? s)))

  ;; If *s* is a string describing a long or short option,
  ;; return its name as a symbol. Otherwise, return #f.
  (define (option-string->name s)
     (and (option-string? s)
          (s152:string-drop-while s (lambda (c) (eqv? c #\-)))))

  ;; Return the contents of *vs* and each of *rest* as values.
  (define (ylppa-values vs . rest)
    (apply values (append vs rest)))

  ;;;; Parser utilities

  (define-condition-type &parser &condition
    make-parser-condition
    parser-condition?)

  (define (parser-exception msg . irritants)
    (raise-continuable
     (condition (make-parser-condition)
                (make-message-condition msg)
                (make-irritants-condition irritants))))

  ;;;; Argument parsers

  ;; A flag takes no arguments, so this always succeeds and consumes
  ;; no tokens.
  (define (flag-parser tokens)
    (values #t tokens))

  ;;;; Exported constructors

  (define make-cli-option
    (case-lambda
      ((names) (make-cli-option names 'ARG values '()))
      ((names arg-name)
       (make-cli-option names arg-name values '()))
      ((names arg-name conv)
       (make-cli-option names arg-name conv '()))
      ((names arg-name conv props)
       (assert (option-names? names))
       (assert (or (symbol? arg-name) (not arg-name)))
       (assert (procedure? conv))
       (assert (list? props))
       (let*
        ((allowed-args
          (cond ((assoc 'allowed-arguments props) =>
                 (lambda (p)
                   (let ((args (cdr p)))
                     (assert (and (list? args) (s1:every string? args)))
                     args)))
                (else #f)))
         (argument-parser
          (lambda (tokens)
            (let ((t (car tokens)) (rest (cdr tokens)))
              (if (and (argument-string? t)
                       (or (not allowed-args)
                           (member t allowed-args)))
                  (values (conv t) rest)
                  (parser-exception "missing option argument"
                                    names))))))
         (make-option names
                      arg-name
                      (if arg-name argument-parser flag-parser)
                      props)))))

  (define make-cli-flag
    (case-lambda
      ((names) (make-cli-flag names '()))
      ((names props)
       (assert (option-names? names))
       (assert (list? props))
       (make-option names #f flag-parser props))))

  ;;;; Driver

  ;; Nuts-&-bolts general interface.
  ;;
  ;; Currently, an operand is signaled to *proc* by passing #f as
  ;; the first (option) argument and the token itself as the second
  ;; (argument) argument.  This may be a little too subtle.
  ;;
  ;; TODO: Determine how to handle --.  Currently parse-command-line
  ;; does not treat it specially, since not every program will want
  ;; that.  Handling it at a higher level, though, is awkward, and
  ;; every program that *does* want special handling of -- will have to
  ;; do additional work (as process-command-line does below).  Maybe
  ;; parse-command-line could take an additional parameter indicating
  ;; whether to support -- as "operand guard".
  (define (parse-command-line opts proc cli-lis . seeds)
    (assert (and (list? opts) (s1:every option? opts)))
    (assert (procedure? proc))
    ;; TODO: Check listiness here & check strings bit by bit.
    (assert (and (list? cli-lis) (s1:every string? cli-lis)))
    (letrec*
     ((opt-tab
       (let ((table (make-hashtable string-hash string=?)))
         (for-each (lambda (opt)
                     (for-each (lambda (name)
                                 (hashtable-set! table name opt))
                               (option-names opt)))
                   opts)
         table))

      ;; Assoc *name* in opt-tab.
      (lookup-option-by-name
       (lambda (name)
         (cond ((hashtable-ref opt-tab name #f))
               (else (parser-exception "invalid option" name)))))

      ;; FIXME: Split this up.
      (parse-loop
       (lambda (seeds ts)
         (if (null? ts)
             (ylppa-values seeds '())
             (let ((t (car ts)) (ts* (cdr ts)))
               (cond ((option-string->name t) =>
                      (lambda (name)
                        (let*-values (((opt)
                                       (lookup-option-by-name name))
                                      ((aparser)
                                       (option-argument-parser opt))
                                      ((arg ts**) (aparser ts*))
                                      ((continue . seeds*)
                                       (apply proc opt arg seeds)))
                          (if continue
                              (parse-loop seeds* ts**)
                              (ylppa-values seeds ts)))))
                     (else
                      (let-values (((continue . seeds*)
                                    (apply proc #f t seeds)))
                        (if continue
                            (parse-loop seeds* ts*)
                            (ylppa-values seeds ts))))))))))

      (parse-loop seeds (normalize-command-line opt-tab cli-lis))))

  ;;; TODO: Decide on a canonical form for options with multiple names.
  ;;; If -o and --output are names for the same option, then the same
  ;;; option name should be produced for both.
  ;;;
  ;;; This would mean restricting the number of names an option can
  ;;; have (e.g. short, long, or short and long), or explicitly asking
  ;;; the library user to select a canonical name.  I lean toward the
  ;;; former, since having multiple names of the same format for a
  ;;; single option seems confusing in general.

  ;; Easy high-level interface.  Parses *cl-list* and returns two
  ;; values: an alist associating each option with its arguments, and
  ;; a list of operands (objects not associated with options).
  ;;
  ;; TODO: Support the -- operand guard.  Probably not here, though.
  (define process-command-line
    (case-lambda
      ((opts) (process-command-line opts (cdr (command-line))))
      ((opts cl-list)
       (let*-values (((opts opers)
                      (parse-command-line opts
                                          accumulate
                                          cl-list
                                          '())))
         (values (map (lambda (p) (cons (car p) (reverse (cdr p))))
                      opts)
                 opers)))))

  ;; If *name* has an association in *alist*, then push *arg*
  ;; onto the cdr of *name*'s pair.  Otherwise, just add
  ;; (name . (arg)) to *alist*.
  (define (adjoin/push name arg alist)
    (cond ((assoc name alist) =>
           (lambda (p)
             (cons (cons (car p) (cons arg (cdr p)))
                   (s1:remove (lambda (p) (equal? name (car p)))
                              alist))))
          (else (cons (list name arg) alist))))

  ;; FIXME: Uses *opt*'s first name as canonical.  This should
  ;; at least ensure that all occurrences of an option get
  ;; accumulated the same name.
  (define (accumulate opt arg opts)
     (and opt
          (values #t
                  (adjoin/push (car (option-names opt)) arg opts))))

  ;;;; Syntax

  ;;; TODO: An exception should be raised if the names of two or more
  ;;; clauses overlap.  If we switch to syntax-case, this can be an
  ;;; expand-time exception.

  (define (stringify-names names)
    (map (lambda (x)
           (cond ((symbol? x) (symbol->string x))
                 ((string? x) x)
                 (else
                  (assertion-violation 'options
                                       "invalid option name"
                                       x))))
         names))

  (define-syntax options
    (syntax-rules ()
      ((options (e ...) ...)
       (list (opt-clause e ...) ...))))

  (define-syntax normalize
    (syntax-rules ()
      ((normalize (name0 . names))
       (stringify-names '(name0 . names)))
      ((normalize name)
       (stringify-names '(name)))))

  (define-syntax opt-clause
    (syntax-rules (option flag)
      ((opt-clause flag names)
       (make-cli-flag (normalize names)))
      ((opt-clause flag names help-str)
       (make-cli-flag (normalize names) '((help . help-str))))
      ((opt-clause option names arg)
       (make-cli-option (normalize names) 'arg))
      ((opt-clause option names arg help-str)
       (make-cli-option (normalize names)
                        'arg
                        values
                        '((help . help-str))))
      ((opt-clause option names arg help-str conv)
       (make-cli-option (normalize names)
                        'arg
                        conv
                        '((help . help-str))))))

  (define-syntax flag (syntax-rules ()))
  (define-syntax option (syntax-rules ()))

)
