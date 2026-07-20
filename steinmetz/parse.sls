;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz parse)
  (export option-map
          fold-cli
          process-cli
          put-usage
          options
          make-flag
          flag)
  (import (rnrs base)
          (rnrs control (6))
          (rnrs lists (6))
          (rnrs hashtables (6))
          (rnrs io ports (6))
          (prefix (srfi :1) s1:)
          (srfi :9 records)
          (srfi :115)
          (only (srfi :152) string-index string-skip string-drop-while
                            string-concatenate string-join)
          (steinmetz options)
          (steinmetz usage)
          (steinmetz command-line)
          )

  ;;;; Utility

  ;; Add (key . val) to alist, replacing any existing pair with
  ;; car key. May derange alist.
  (define (alist-update key val alist)
    (cons (cons key val)
          (remove (lambda (p) (eqv? key (car p))) alist)))

  ;;;; Matching options and arguments

  (define (option-string? s)
    (and (not (equal? s ""))
         (eqv? #\- (string-ref s 0))))

  ;; An argument is anything that doesn't look like an option.
  (define (argument-string? s)
    (not (option-string? s)))

  ;;;; Parser utilities

  (define parser-satisfies
    (case-lambda
      ((pred) (parser-satisfies pred "parse failed"))
      ((pred fail-msg)
       (lambda (in succeed fail)
         (if (and (pair? in) (pred (car in)))
             (succeed (car in) (cdr in))
             (fail fail-msg))))))

  (define (parser-map f p)
    (lambda (in succeed fail)
      (p in
         (lambda (x rest) (succeed (f x) rest))
         fail)))

  ;;; Argument parsers

  ;;; An argument parser is a function that takes a list of strings
  ;;; and two continuations, 'succeed' and 'fail'. It is expected to
  ;;; either invoke 'succeed' on a result and the rest of the input, or
  ;;; invoke 'fail' on an error message (string).

  ;; Parse an argument.
  (define (make-argument-parser opt-names)
    (let* ((nm (format-option-names opt-names))
           (err-msg (string-append "missing arguments for " nm)))
      (parser-satisfies argument-string? err-msg)))

  ;; Should be continuable.
  (define parser-exception error)

  ;; No arguments; returns #t.
  (define (flag-parser ts succeed _fail)
    (succeed #t ts))

       (let ((arg-p (if arg-name (argument names) flag-parser))
             (props (alist->properties `((names . ,names)
                                         (argument-name . ,arg-name)))))
         (option-map conv (raw-option arg-p props))))))

  (define make-cli-option
    (case-lambda
      ((names) (make-cli-option names 'ARG values))
      ((names arg-name) (make-cli-option names arg-name values))
      ((names arg-name conv)
       (assert (and (list? names) (s1:every symbol? names)))
       (assert (or (symbol? arg-name) (not arg-name)))
       (assert (procedure? conv))
       (let ((parser (if arg-name
                         (make-argument-parser names)
                         flag-parser))
             (props (if arg-name
                        `((argument-name . ,arg-name))
                        '())))
         (make-option names parser props)))))

  (define (make-cli-flag names)
    (make-cli-option names
                 flag-parser
                 (alist->properties `((names . ,names)))))

  (define (option-map proc opt)
    (assert (procedure? proc))
    (assert (option? opt))
    (make-cli-option (option-names opt)
                 (parser-map proc (option-parser opt))
                 (option-properties opt)))

  ;;;; Driver

  ;; Could be a perfect hash table.
  (define (make-option-table opts)
    (let ((table (make-eqv-hashtable)))
      (for-each (lambda (opt)
                  (for-each (lambda (name)
                              (hashtable-set! table name opt))
                            (option-get-property opt 'names)))
                opts)
      table))

  (define (lookup-option-by-name opt-tab name)
    (cond ((hashtable-ref opt-tab name #f))
          (else (parser-exception "invalid option" name))))

  (define (fold-cli options proc cli-lis . seeds)
    (letrec*
     ((opt-tab (make-option-table options))
      (tokens (clean-command-line cli-lis))
      (accum-option
       (lambda (name ts seeds cont)
         (process-option
          name
          opt-tab
          tokens
          (lambda (v ts*)
            (let-values ((seeds* (apply proc name v seeds)))
              (cont seeds* ts*)))
          parser-exception)))
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

  (define (process-option name opt-table in succeed fail)
    (let ((opt (lookup-option-by-name opt-table name)))
      ((option-parser opt) in succeed fail)))

  ;; Parses ts and returns two values: an alist associating each option with
  ;; its arguments, and a list of "operands"--tokens without a preceding
  ;; option.
  (define (process-cli options ts)
    (let-values (((opts opers junk)
                  (fold-cli options accum ts '() '() #t)))
      (values (reverse opts) (reverse opers))))

  ;; If name has an association in alis, then append val to the cdr
  ;; of name's pair. Otherwise, just add (name . val) to alis.
  (define (adjoin/pool name val alis)
    (cond ((assv name alis) =>
           (lambda (p)
             (cons (cons (car p) (append (cdr p) (list val)))
                   (remove (lambda (p) (eqv? name (car p))) alis))))
          (else (cons (cons name val) alis))))

  (define (accum name val opts opers more-opts?)
    (if (and name more-opts?)
        (accum-option name val opts opers more-opts?)
        (values opts (cons val opers) more-opts?)))

  (define (accum-option name val opts opers more-opts?)
    (if (equal? name "--")  ; special "end of options" token
        (values opts opers #f)  ; discard it and set flag
        (values (adjoin/pool name val opts) opers more-opts?)))

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
       (make-flag (%normalize names)))
      ((%opt-clause flag names help)
       (option-add-help help (%opt-clause flag names)))
      ((%opt-clause option names arg)
       (make-cli-option (%normalize names) 'arg))
      ((%opt-clause option names arg help)
       (option-add-help help
                        (make-cli-option (%normalize names) 'arg)))
      ((%opt-clause option names arg help conv)
       (option-add-help
        help
        (make-cli-option (%normalize names) 'arg conv)))))

  (define-syntax %normalize
    (syntax-rules ()
      ((%normalize (name0 . names)) '(name0 . names))
      ((%normalize name) '(name))))

)
