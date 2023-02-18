;;;; Utility

(define (consc a) (lambda (d) (cons a d)))

(define (first . vals)
  (if (null? vals)
      (error 'first "no values")
      (car vals)))

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

;;;; Parsers

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
;; The 'conv' procedure takes the argument string and an error
;; continuation. It either returns a value or calls the error
;; continuation on a message.
(define (argument opt-names conv)
  (let* ((name-string (symbol->string (car opt-names)))  ; hack
         (make-msg     ; error message template
          (lambda (msg-body)
            (string-append "option " name-string ": "
                           msg-body))))
    (lambda (lis succeed fail)
      (if (and (pair? lis) (argument-string? (car lis)))
          (let ((val (conv (car lis)
                           (lambda (s) (fail (make-msg s))))))
            (succeed val (cdr lis)))
          (fail (make-msg "missing argument"))))))

;; Should be continuable.
(define parser-exception error)

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

(define (singleton-properties key val)
  (list (cons key val)))

;; Exported constructor. Defaults to an option that takes a single
;; string argument.
(define option
  (case-lambda
    ((names) (option names 'ARG first))
    ((names arg-name) (option names arg-name first))
    ((names arg-name conv)
     (let ((arg-p (if arg-name (argument names conv) flag)))
       (raw-option arg-p (singleton-properties 'names names))))))

(define (option-map f opt)
  (raw-option (parser-map f (option-parser opt))
              (option-properties opt)))

;;; Option combinators

;; Add a help string to opt.
(define (opt-help s opt)
  (option-add-property opt 'help s))

;; Add an argument name (symbol) to opt.
(define (opt-arg-name name opt)
  (option-add-property opt 'argument-name name))

;;;; Driver

;; Uses SRFI 69, but could be a perfect hash table.
(define (make-option-table opts)
  (let ((table (make-hash-table eq? symbol-hash)))
    (for-each (lambda (opt)
                (for-each (lambda (name)
                            (hash-table-set! table name opt))
                          (option-get-property opt 'names)))
              opts)
    table))

(define (lookup-option-by-name opt-tab name)
  (hash-table-ref opt-tab
                  name
                  (lambda ()
                    (parser-exception "invalid option" name))))

;; No arguments; returns #t.
(define (flag ts succeed _fail)
  (succeed #t ts))

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

  (call-with-values
   (lambda ()
     (fold-cli options
               (lambda (name val opts opers)
                 (if name
                     (values (adjoin/pool name val opts) opers)
                     (values opts (cons val opers))))
               ts
               '()
               '()))
   (lambda (opts opers)
     (values (reverse opts) (reverse opers)))))

;;;; Syntax

(define-syntax options
  (syntax-rules ()
    ((options (e ...) ...)
     (list (%opt-clause e ...) ...))))

(define-syntax %opt-clause
  (syntax-rules ()
    ((%opt-clause names)   ; flag
     (option (%normalize-names names) #f))
    ((%opt-clause names arg)
     (option (%normalize-names names) 'arg))
    ((%opt-clause names arg conv)
     (option (%normalize-names names) 'arg))
    ((%opt-clause names arg conv help)
     (opt-help help (%opt-clause names arg conv)))))

(define-syntax %normalize-names
  (syntax-rules ()
    ((%normalize-names (name0 . names)) '(name0 . names))
    ((%normalize-names name) '(name))))
