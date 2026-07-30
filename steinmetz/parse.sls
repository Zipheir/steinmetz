;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz parse)
  (export parse-command-line
          process-command-line
          options
          flag
          option
          parser-condition?
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
          (steinmetz utility)
          (steinmetz command-line)
          )

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

  (define (make-argument-parser cname conv allowed-args)
    (let ((invalid-arg-message
           (apply string-append
                  "invalid argument"
                  (if (pair? allowed-args)
                      (list ": must be one of "
                            (s152:string-join allowed-args ", "))
                      '()))))
      (lambda (tokens)
        (if (null? tokens)
            (parser-exception "missing option argument" cname)
            (let ((t (car tokens)) (rest (cdr tokens)))
              (if (or (not allowed-args) (member t allowed-args))
                  (values (conv t) rest)
                  (parser-exception invalid-arg-message cname t)))))))

  ;;;; Driver

  ;; Nuts-&-bolts general interface.
  ;;
  ;; Currently, an operand is signaled to *proc* by passing #f as
  ;; the first (option) argument and the token itself as the second
  ;; (argument) argument.  This may be a little too subtle.
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

      ;; Have we seen '--' yet?
      (more-options #t)

      ;; FIXME: Split this up.
      (parse-loop
       (lambda (seeds ts)
         (if (null? ts)
             (ylppa-values seeds '())
             (let ((t (car ts)) (ts* (cdr ts)))
               (cond ((equal? t "--")
                      (set! more-options #f)
                      (parse-loop seeds ts*))
                     ((and more-options (option-string->name t)) =>
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

  ;; Easy high-level interface.  Parses *cl-list* and returns two
  ;; values: an alist associating each option with its arguments, and
  ;; a list of operands (objects not associated with options).
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

  (define (accumulate opt arg opts)
     (and opt
          (let ((name (or (option-canonical-name opt)
                          (car (option-names opt)))))
            (values #t (adjoin/push name arg opts)))))

  ;;;; Syntax

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
       (let ((names* (normalize names)))
         (make-option names*
                      #f
                      flag-parser
                      docstr
                      (car names*))))
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
       (option/arg-spec nnames (arg-name #f) docstr))
      ((option/arg-spec nnames (arg-name default) docstr)
       (make-option nnames
                    'arg-name
                    (make-argument-parser (car nnames) values #f)
                    docstr
                    (car nnames)
                    default))
      ((option/arg-spec nnames (arg-name default (id ...)) docstr)
       (make-option nnames
                    'arg-name
                    (make-argument-parser (car nnames) values #f)
                    docstr
                    (car nnames)
                    default
                    '(id ...)))
      ((option/arg-spec nnames (arg-name default conv) docstr)
       (make-option nnames
                    'arg-name
                    (make-argument-parser (car nnames) conv #f)
                    docstr
                    (car nnames)
                    default))))

  (define-syntax flag (syntax-rules ()))
  (define-syntax option (syntax-rules ()))

)
