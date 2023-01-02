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
  (raw-option name args parser help)
  option?
  (name option-name)            ; a symbol
  (args option-args)            ; a list of argument names (symbols)
  (parser option-parser)        ; argument parser
  (help option-help))           ; option description (string) or #f

;; Exported constructor.
(define option
  (case-lambda
    ((name) (option name '() first #f))
    ((name args) (option name args first #f))
    ((name args conv) (option name args conv #f))
    ((name args conv help)
     (let* ((arg-p (case (length args)
                     ((0) flag)
                     (else => (lambda (n) (arguments name n conv))))))
       (raw-option name args arg-p help)))))

(define (option-map f opt)
  (raw-option (option-name opt)
              (option-args opt)
              (parser-map f (option-parser opt))
              (option-help opt)))

;; Transform the arguments of 'opt' with 'proc', which should
;; take a list to a list.
(define (option-add-arg-processor proc opt)
  (option-map proc opt))

;; Uses SRFI 69, but could be a perfect hash table.
(define (make-option-table opts)
  (let ((table (make-hash-table eq? symbol-hash)))
    (for-each (lambda (opt)
                (hash-table-set! table (option-name opt) opt))
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

;;;; Driver

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
