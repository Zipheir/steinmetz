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
    ((pred p) (parser-satisfies pred p "parse failed"))
    ((pred p fail-msg)
     (lambda (in succeed fail)
       (if (and (pair? in) (pred (car in)))
           (succeed (car in) (cdr in))
           (fail fail-msg))))))

(define (parser-map f p)
  (lambda (in succeed fail)
    (p in
       fail
       (lambda (x rest) (succeed (f x) rest)))))

(define (parser-pure x)
  (lambda (in succeed _fail)
    (succeed x in)))

(define (parser-ap pf px)
  (lambda (in succeed fail)
    (pf in
        (lambda (f in*)
          (px in*
              (lambda (x in**)
                (succeed (f x) in**))
              fail)
          fail)
        fail)))

;;; Argument parsers

;;; An argument parser is a function that takes a list of strings
;;; and two continuations, 'succeed' and 'fail'. It is expected to
;;; either invoke 'succeed' on a result and the rest of the input, or
;;; invoke 'fail' on an error message (string).

;; Parse an argument.
(define (argument opt-names)
  (let* ((nm (fmt-names opt-names))
         (err-msg (string-append "missing arguments for " nm)))
    (parser-satisfies argument-string? err-msg)))

;; Should be continuable.
(define parser-exception error)

;; No arguments; returns #t.
(define (flag-parser ts succeed _fail)
  (succeed #t ts))

;;;; Options

(define-record-type <option>
  (raw-option parser properties)
  option?
  (parser option-parser)           ; an argument parser
  (properties option-properties))  ; a key/value map of option properties

;;; (Symbol . list) alist implementation of properties.

(define (option-get-property opt key)
  (cond ((assv key (option-properties opt)) => cdr)
        (else #f)))

(define (option-add-property opt key val)
  (raw-option (option-parser opt)
              (alist-update key val (option-properties opt))))

(define alist->properties values)

;;; Convenient property accessors

(define (option-names opt)
  (or (option-get-property opt 'names)
      (error 'option-names "option has no defined names")))

(define (option-argument-name opt)
  (or (option-get-property opt 'argument-name)
      (error 'option-argument-name
             "option has no defined argument name")))

(define (option-help opt)
  (option-get-property opt 'help))

;; Exported constructor. Defaults to an option that takes a single
;; string argument.
(define make-option
  (case-lambda
    ((names) (make-option names 'ARG values))
    ((names arg-name) (make-option names arg-name values))
    ((names arg-name conv)
     (let ((arg-p (if arg-name (argument names) flag-parser))
           (props (alist->properties `((names . ,names)
                                       (argument-name . ,arg-name)))))
       (option-map conv (raw-option arg-p props))))))

(define (make-flag names)
  (raw-option flag-parser
              (alist->properties `((names . ,names)))))

(define (option-map f opt)
  (raw-option (parser-map f (option-parser opt))
              (option-properties opt)))

;;; Option combinators

;; Add a help string to opt.
(define (option-add-help s opt)
  (option-add-property opt 'help s))

;; Add an argument name (symbol) to opt.
(define (option-add-argument-name name opt)
  (option-add-property opt 'argument-name name))

;;;; Driver

;; Could be a perfect hash table.
(define (make-option-table opts)
  (let ((table (make-hashtable symbol-hash eq?)))
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
  (let ((opt-tab (make-option-table options))
        (ts (clean-command-line cli-lis)))

    (define (accum-option name ts seeds cont)
      (process-option name
                      opt-tab
                      ts
                      (lambda (v ts*)
                        (let-values ((seeds* (apply proc name v seeds)))
                          (cont seeds* ts*)))
                      parser-exception))

    (let loop ((seeds seeds) (ts ts))
      (if (null? ts)
          (apply values seeds)
          (let ((t (car ts)) (ts* (cdr ts)))
            (cond ((option-string->name t) =>
                   (lambda (name) (accum-option name ts* seeds loop)))
                  (else
                   (let-values ((seeds* (apply proc #f t seeds)))
                     (loop seeds* ts*)))))))))

;; If s is a string describing a long or short option, returns its
;; name as a symbol. Otherwise, returns #f.
(define (option-string->name s)
  (and (option-string? s)
       (string->symbol
        (string-drop-while s (lambda (c) (eqv? c #\-))))))

(define (process-option name opt-table in succeed fail)
  (let ((opt (lookup-option-by-name opt-table name)))
    ((option-parser opt) in succeed fail)))

;;; Convenience

;; Parses ts and returns two values: an alist associating each option with
;; its arguments, and a list of "operands"--tokens without a preceding
;; option.
(define (process-cli options ts)
  ;; If name has an association in alis, then append val to the cdr
  ;; of name's pair. Otherwise, just add (name . val) to alis.
  (define (adjoin/pool name val alis)
    (cond ((assv name alis) =>
           (lambda (p)
             (cons (cons (car p) (append (cdr p) (list val)))
                   (remove (lambda (p) (eqv? name (car p))) alis))))
          (else (cons (list name val) alis))))

  (define (accum name val opts opers more-opts?)
    (if (and name more-opts?)
        (accum-option name val opts opers more-opts?)
        (values opts (cons val opers) more-opts?)))

  (define (accum-option name val opts opers more-opts?)
    (if (equal? name "--")  ; special "end of options" token
        (values opts opers #f)  ; discard it and set flag
        (values (adjoin/pool name val opts) opers more-opts?)))

  (call-with-values
   (lambda ()
     (fold-cli options accum ts '() '() #f))
   (lambda (opts opers)
     (values (reverse opts) (reverse opers)))))

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
     (make-option (%normalize names) 'arg))
    ((%opt-clause option names arg help)
     (option-add-help help (make-option (%normalize names) 'arg)))
    ((%opt-clause option names arg help conv)
     (option-add-help
      help
      (make-option (%normalize names) 'arg conv)))))

(define-syntax %normalize
  (syntax-rules ()
    ((%normalize (name0 . names)) '(name0 . names))
    ((%normalize name) '(name))))
