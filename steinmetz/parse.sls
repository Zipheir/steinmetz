;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz parse)
  (export fold-cli
          process-cli
          put-usage
          options
          make-cli-option
          make-cli-flag
          flag
          option
          )
  (import (rnrs base)
          (rnrs conditions)
          (rnrs control (6))
          (rnrs exceptions)
          (rnrs lists (6))
          (rnrs hashtables (6))
          (rnrs io ports (6))
          (rnrs programs)
          (prefix (srfi :1) s1:)
          (srfi :9 records)
          (srfi :115)
          (only (srfi :152) string-index string-skip string-drop-while
                            string-concatenate string-join)
          (steinmetz options)
          (steinmetz usage)
          (steinmetz command-line)
          )

  ;;;; Type predicates & utility

  ;; Add (key . val) to alist, replacing any existing pair with
  ;; car key. May derange alist.
  (define (alist-update key val alist)
    (cons (cons key val)
          (remove (lambda (p) (eqv? key (car p))) alist)))

  (define (option-names? x)
    (and (list? x) (s1:every symbol? x)))

  ;; TODO: Tighten up (use a reg. ex.)
  (define (option-string? s)
    (and (not (equal? s ""))
         (eqv? #\- (string-ref s 0))))

  ;; An argument is anything that doesn't look like an option.
  (define (argument-string? s)
    (not (option-string? s)))

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

  (define (parser-map proc parser)
    (lambda (in)
      (let-values (((val rest) (parser in)))
        (values (proc val) rest))))

  ;; Parse an argument.
  ;; TODO: Additional predicate for excluding ill-typed arguments?
  (define (make-argument-parser opt-names)
    (assert (option-names? opt-names))
    (lambda (tokens)
      (let ((t (car tokens)) (rest (cdr tokens)))
        (if (argument-string? t)
            (values t rest)
            (parser-exception "missing option argument" opt-names)))))

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
       (assert (and (list? names) (s1:every symbol? names)))
       (assert (or (symbol? arg-name) (not arg-name)))
       (assert (procedure? conv))
       (assert (list? props))
       (let ((parser (if arg-name
                         (make-argument-parser names)
                         flag-parser)))
         (make-option names
                      arg-name
                      (parser-map conv parser)
                      props)))))

  (define make-cli-flag
    (case-lambda
      ((names) (make-cli-flag names '()))
      ((names props)
       (assert (and (list? names) (s1:every symbol? names)))
       (assert (list? props))
       (make-option names #f flag-parser props))))

  ;;;; Driver

  ;;; TODO: Decide on a canonical form for options with multiple names.
  ;;; If -o and --output are names for the same option, then the same
  ;;; option name should be produced for both.
  ;;;
  ;;; This would mean restricting the number of names an option can
  ;;; have (e.g. short, long, or short and long), or explicitly asking
  ;;; the library user to select a canonical name.  I lean toward the
  ;;; former, since having multiple names of the same format for a
  ;;; single option seems confusing in general.

  ;; Could be a perfect hash table.
  (define (make-option-table opts)
    (let ((table (make-eqv-hashtable)))
      (for-each (lambda (opt)
                  (for-each (lambda (name)
                              (hashtable-set! table name opt))
                            (option-names opt)))
                opts)
      table))

  (define (lookup-option-by-name opt-tab name)
    (cond ((hashtable-ref opt-tab name #f))
          (else (parser-exception "invalid option" name))))

  ;; Nuts-&-bolts general interface.
  ;;
  ;; TODO: Determine how to handle --.  Currently fold-cli does not
  ;; treat it specially, since not every program will want that. 
  ;; Handling it at a higher level, though, is awkward, and every
  ;; program that *does* want special handling of -- will have to
  ;; do additional work (as process-cli does below).  Maybe fold-cli
  ;; could take an additional parameter indicating whether to support
  ;; -- as "operand guard".
  (define (fold-cli options proc cli-lis . seeds)
    (assert (and (list? options) (s1:every option? options)))
    (assert (procedure? proc))
    ;; TODO: Check listiness here & check strings bit by bit.
    (assert (and (list? cli-lis) (s1:every string? cli-lis)))
    (letrec*
     ((opt-tab (make-option-table options))
      (tokens (clean-command-line cli-lis))
      (accum-option
       (lambda (name ts seeds cont)
         (let*-values (((arg rest) (process-option name opt-tab ts))
                       (seeds* (apply proc name arg seeds)))
           (cont seeds* rest))))
      (fold-loop
       (lambda (seeds ts)
         (if (null? ts)
             (apply values seeds)
             (let ((t (car ts)) (ts* (cdr ts)))
               (cond ((option-string->name t) =>
                      (lambda (name)
                        (accum-option name ts* seeds fold-loop)))
                     (else
                      (let-values ((seeds* (apply proc #f t seeds)))
                        (fold-loop seeds* ts*)))))))))

      (fold-loop seeds tokens)))

  ;; If s is a string describing a long or short option, returns its
  ;; name as a symbol. Otherwise, returns #f.
  (define (option-string->name s)
    (and (option-string? s)
         (string->symbol
          (string-drop-while s (lambda (c) (eqv? c #\-))))))

  ;; Match *name* to an option structure and apply the associated
  ;; parser to *tokens*.
  (define (process-option name opt-table tokens)
    (let ((opt (lookup-option-by-name opt-table name)))
      ((option-parser opt) tokens)))

  ;; Easy high-level interface.  Parses *cli-list* and returns two
  ;; values: an alist associating each option with its arguments, and
  ;; a list of operands (objects not associated with options).
  ;;
  ;; TODO: Support the -- operand guard.  Probably not here, though.
  (define process-cli
    (case-lambda
      ((opts) (process-cli opts (cdr (command-line))))
      ((opts cli-list)
       (let-values (((opts opers)
                     (fold-cli options accum cli-list '() '())))
         (values (reverse opts) (reverse opers))))))

  (define (accum name arg opts opers)
    (if name
        (values (adjoin/pool name arg opts) opers)
        (values opts (cons arg opers))))

  ;; If *name* has an association in *alist*, then append *arg* to the
  ;; cdr of *name*'s pair.  Otherwise, just add (name .  arg) to
  ;; *alist*.
  (define (adjoin/pool name arg alist)
    (cond ((assv name alist) =>
           (lambda (p)
             (cons (cons (car p) (append (cdr p) (list arg)))
                   (remove (lambda (p) (eqv? name (car p))) alist))))
          (else (cons (cons name arg) alist))))

  ;;;; Syntax

  (define-syntax options
    (syntax-rules ()
      ((options (e ...) ...)
       (list (%opt-clause e ...) ...))))

  (define-syntax flag (syntax-rules ()))
  (define-syntax option (syntax-rules ()))

  (define-syntax %opt-clause
    (syntax-rules (option flag)
      ((%opt-clause flag names)
       (make-cli-flag (%normalize names)))
      ((%opt-clause flag names help-str)
       (make-cli-flag (%normalize names) '((help . help-str))))
      ((%opt-clause option names arg)
       (make-cli-option (%normalize names) 'arg))
      ((%opt-clause option names arg help-str)
       (make-cli-option (%normalize names)
                        'arg
                        values
                        '((help . help-str))))
      ((%opt-clause option names arg help-str conv)
       (make-cli-option (%normalize names)
                        'arg
                        conv
                        '((help . help-str))))))

  (define-syntax %normalize
    (syntax-rules ()
      ((%normalize (name0 . names)) '(name0 . names))
      ((%normalize name) '(name))))

)
