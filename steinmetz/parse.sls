;;; SPDX-FileCopyrightText: Copyright 2022--2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz parse)
  (export parse-command-line
          process-command-line
          parser-condition?
          make-argument-parser
          )
  (import (rnrs base)
          (rnrs conditions)
          (rnrs control)
          (rnrs exceptions)
          (only (rnrs lists) assoc member)
          (rnrs hashtables)
          (rnrs programs)
          (prefix (srfi :1) s1:)
          (prefix (srfi :115) s115:)
          (prefix (srfi :152) s152:)
          (steinmetz options)
          (steinmetz utility)
          (prefix (chezscheme) c:)
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

  ;;;; Parser

  (define cluster
    (s115:rx (submatch #\- alphanumeric)
             (submatch (+ alphanumeric))))

  (define (cluster? s)
    (s115:regexp-matches? cluster s))

  (define long-option/equals
    (s115:rx (submatch (: "--" alphanumeric (+ (or alphanumeric #\-))))
             #\=
             (submatch (+ alphanumeric))))

  (define (long-option/equals? s)
    (s115:regexp-matches? long-option/equals s))

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

      ;; Assoc the option name of *s* in *opt-tab*.
      (lookup-option
       (lambda (s)
         (cond ((option-string->name s) =>
                (lambda (name)
                  (cond ((hashtable-ref opt-tab name #f))
                        (else
                         (parser-exception "invalid option" name)))))
               (else (assertion-violation "invalid argument" s)))))

      (parse-closed-long-option
       (lambda (tok)
         (let ((m (s115:regexp-matches long-option/equals tok)))
           (values (lookup-option (s115:regexp-match-submatch m 1))
                   (s115:regexp-match-submatch m 2)))))

      (parse-cluster
       (lambda (tok tokens)
         (let* ((m (s115:regexp-matches cluster tok))
                (opt (lookup-option (s115:regexp-match-submatch m 1)))
                (suffix (s115:regexp-match-submatch m 2)))
           (if (flag? opt)
               (values opt #t (cons (string-append "-" suffix) ; yuck
                                    tokens))
               (let-values (((arg tokens*)
                             ((option-argument-parser opt)
                              (cons suffix tokens))))
                 (values opt arg tokens*))))))

      (parse-option
       (lambda (tok tokens)
         (let ((opt (lookup-option tok)))
           (if (flag? opt)
               (values opt #t tokens)
               (if (pair? tokens)
                   (let-values (((arg tokens*)
                                 ((option-argument-parser opt) tokens)))
                     (values opt arg tokens*))
                   (parser-exception "missing argument"
                                     (option-canonical-name opt)))))))

      ;; Parse *tok* and return three values: a boolean indicating
      ;; whether to keep parsing, a list of new seeds, and a list of
      ;; unparsed tokens.
      (parse-token
       (lambda (tok seeds tokens)
         (cond ((long-option/equals? tok)
                (let*-values (((opt arg)
                               (parse-closed-long-option tok))
                              ((continue . new-seeds)
                               (apply proc opt arg seeds)))
                  (values continue new-seeds tokens)))
               ((cluster? tok)
                (let*-values (((first-opt arg tokens*)
                               (parse-cluster tok tokens))
                              ((continue . new-seeds)
                               (apply proc first-opt arg seeds)))
                  (values continue new-seeds tokens*)))
               ((option-string? tok) ; long or short option
                (let*-values (((opt arg tokens*)
                               (parse-option tok tokens))
                              ((continue . new-seeds)
                               (apply proc opt arg seeds)))
                  (values continue new-seeds tokens*)))
               (else ; operand
                (let*-values (((continue . new-seeds)
                               (apply proc #f tok seeds)))
                  (values continue new-seeds tokens))))))

      (parse-loop
       (lambda (no-more-options seeds tokens)
         (if (null? tokens)
             (ylppa-values seeds tokens)
             (let-values (((tok more) (s1:car+cdr tokens)))
               (cond (no-more-options
                      (let-values (((continue . new-seeds)
                                    (apply proc #f tok seeds)))
                        (c:format #t "new-seeds = ~s~%" new-seeds)
                        (if continue
                            (parse-loop no-more-options new-seeds more)
                            (ylppa-values seeds tokens))))
                     ((equal? tok "--") (parse-loop #t seeds more))
                     (else
                      (let-values (((continue new-seeds rest)
                                    (parse-token tok seeds more)))
                        (if continue
                            (parse-loop no-more-options new-seeds rest)
                            (ylppa-values seeds tokens))))))))))

      (parse-loop #f seeds cli-lis)))

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

)
