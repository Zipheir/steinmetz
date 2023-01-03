(import (srfi 1)
        (srfi 69)
        (srfi 189)
        )

(include "string-util.scm")

;;;; Utility

(define (either-ap ef ex)
  (either-bind ef
               (lambda (f) (either-map f ex))))

;; (list Either) -> Either list.
;; SRFI 189's sequence is a bit odd. This is simpler.
(define (either-seq es)
  (if (null? es)
      (right '())
      (either-ap (either-map consc (car es))
                 (either-seq (cdr es)))))

(define (consc a) (lambda (d) (cons a d)))

(define (first . vals)
  (if (null? vals)
      (error 'first "no values")
      (car vals)))

;; Combine bindings from two alists whose values are lists.
;; Inefficient.
(define (alist-merge ps qs)
  (fold (lambda (q res)
          (let ((x (car q)))
            (cond ((assv x res) =>
                   (lambda (p)
                     (cons (cons x (append (cdr p) (cdr q)))
                           (remove (lambda (r) (eqv? (car r) x))
                                   res))))
                  (else (cons q res)))))
        ps
        qs))

(define (option-string? s)
  (and (not (equal? s ""))
       (eqv? #\- (string-ref s 0))))

;; An argument is anything that doesn't look like an option.
(define (argument-string? s)
  (not (option-string? s)))

;;;; Parsers

(define (parser-map f p)
  (lambda (lis)
    (either-map (lambda (x rest) (values (f x) rest))
                (p lis))))

(define (parser-pure x)
  (lambda (in)
    (right x in)))

(define (parser-ap pf px)
  (lambda (in)
    (either-bind (pf in)
                 (lambda (f in*)
                   (either-bind (px in*)
                                (lambda (x in**)
                                  (right (f x) in**)))))))

(define (parser-seq ps)
  (fold-right (lambda (px pacc)
                (parser-ap (parser-map consc px) pacc))
              (parser-pure '())
              ps))

;;; Argument parsers

;;; An argument parser is a function that takes a list of strings
;;; and returns either a Right[vals, rest] or a Left[msg].
;;; vals is a list of argument values, rest is the remaining input,
;;; and msg is string giving an error message.

;; Parse an argument for option 'name'.
;; The 'conv' procedure takes the argument string and an error
;; continuationd 'fail'. It either returns a value or calls 'fail'
;; on a message.
(define (raw-argument name conv)
  (let ((make-msg     ; error message template
         (lambda (msg-body)
           (string-append "option " (symbol->string name) ": "
                          msg-body))))
    (lambda (lis)
      (if (and (pair? lis) (argument-string? (car lis)))
          (call-with-current-continuation
           (lambda (k)
             (let ((val (conv (car lis)
                              (lambda (s)
                                (k (left (make-msg s)))))))
               (right val (cdr lis)))))
          (left
           (string-append "option " (symbol->string name)
                          ": missing argument"))))))

;; Parses k arguments, converts them, and returns them as
;; a list.
(define (arguments name k conv)
  (parser-seq (make-list k (raw-argument name conv))))

;; Should be continuable.
(define parser-exception error)

;;;; Options

(define-record-type <option>
  (raw-option arity parser properties)
  option?
  (arity option-arity)             ; maximum number of arguments
  (parser option-parser)           ; an argument parser
  (properties option-properties))  ; a key/value map of option properties

;;; (Symbol . list) alist implementation of properties.

(define (option-get-property opt key)
  (cond ((assv key (option-properties opt)) => cdr)
        (else #f)))

(define (option-add-property opt key val)
  (raw-option (option-parser opt)
              (prop-+ (list (cons key (list val)))
                      (option-properties opt))))

;; Monoidal unit.
(define prop-zero '())

;; Monoidal sum.
(define prop-+ alist-merge)

(define (singleton-properties key val)
  (list (cons key val)))

;; Exported constructor. Defaults to an option that takes a single
;; string argument.
(define option
  (case-lambda
    ((names) (option names 1 first))
    ((names n) (option names n first))
    ((names n conv)
     (let ((arg-p (if (zero? n)
                      flag
                      (arguments name n conv))))
       (raw-option n arg-p (singleton-properties 'names names))))))

(define (option-map f opt)
  (raw-option (parser-map f (option-parser opt))
              (option-properties opt)))

;;; Option combinators

;; Transform the arguments of 'opt' with 'proc', which should
;; take a list to a list.
(define (option-add-arg-processor proc opt)
  (option-map proc opt))

;; Add an option name (long or short) to opt.
(define (opt-name name opt)
  (option-add-property opt 'names name))

;; Add a help string to opt.
(define (opt-help s opt)
  (option-add-property opt 'help s))

;; Add a default value to opt.
;; TODO: What if opt takes no arguments?
(define (opt-default x opt)
  (option-add-property opt 'default x))

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
(define (flag ts)
  (values #t ts))

;; Issues:
;;
;; At the moment, this only handles short options. We should support
;; long options with =-delimiter syntax. We might be able to handle the
;; latter by pre-splitting the input list.

(define (fold-cli options proc knil ts)
  (let ((opt-tab (make-option-table options)))
    (let loop ((res knil) (ts ts))
      (cond ((null? ts) (values res '()))  ; no operands
            ((option-string? (car ts))
             (let ((name (option-string->name (car ts))))
               (either-ref (process-option name opt-tab (cdr ts))
                           parser-exception
                           (lambda (v ts*)
                             (loop (proc name v res) ts*)))))
            (else (values res ts))))))      ; rest are operands

(define (option-string->name s)
  (string->symbol (string-drop-while s (lambda (c) (eqv? c #\-)))))

(define (process-option name opt-table in)
  (let ((opt (lookup-option-by-name opt-table name)))
    ((option-parser opt) in)))

;;; Convenience
(define (parse-cli->alist options ts)
  (fold-cli options
            (lambda (name args res)
              (cons (cons name args) res))
            '()
            ts))
