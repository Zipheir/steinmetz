(import (srfi 1)
        (srfi 69)
        (only (srfi 152) string-trim))

;;;; Utility

;; Add (key . value) to 'alist'. If 'key' already exists in 'alist',
;; then add (key . (proc key value old-value)) instead. This assumes
;; that 'alist' contains no duplicated keys.
(define (alist-adjoin/combinator alist proc key value)
  (cond ((assv key alist) =>
         (lambda (p)
           (cons (cons (car p) (proc key value (cdr p)))
                 (filter (lambda (q) (eqv? key (car q))) alist))))
        (else (cons (cons key value) alist))))

;;;; CLI parsers

(define (option-string? s)
  (and (not (equal? s ""))
       (eqv? #\- (string-ref s 0))))

;; An argument is anything that doesn't look like an option.
(define (argument-string? s)
  (not (option-string? s)))

;; Parses a single argument and returns it wrapped in a list.
(define (argument name)
  (lambda (lis)
    (if (and (pair? lis) (argument-string? (car lis)))
        (values (list (car lis)) (cdr lis))
        (parser-exception
         (string-append "option " (symbol->string name)
                        ": missing argument")))))

;; Parses k arguments and returns them as a list.
;; This is something like a partition/split-at hybrid.
(define (arguments name k)
  (let ((parser (argument name)))
    (lambda (lis)
      (let recur ((k k) (lis lis))
        (if (zero? k)
            (values '() lis)
            (let*-values (((arg lis*) (parser lis))
                          ((args rest) (recur (- k 1) lis*)))
              (values (cons arg args) rest)))))))

;; Should be continuable.
(define parser-exception error)

;;;; Options

(define-record-type <option>
  (raw-option name args parser processor help)
  option?
  (name option-name)            ; a symbol
  (args option-args)            ; a list of argument names (symbols)
  (parser option-parser)        ; argument parser
  (processor option-processor)  ; processor procedure
  (help option-help))           ; option description (string) or #f

;; Exported constructor.
(define option
  (case-lambda
    ((name) (option name '() id-processor #f))
    ((name args) (option name args id-processor #f))
    ((name args proc) (option name args proc #f))
    ((name args proc help)
     (let ((arg-parser (case (length args)
                         ((0) flag)
                         ((1) (argument name))
                         (else => (lambda (k) (arguments name k))))))
       (raw-option name args arg-parser proc help)))))

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

;; An identity option processor that returns the argument list.
(define (id-processor opt name args)
  args)

;; No arguments; returns #t.
(define (flag ts)
  (values #t ts))

;;;; Driver

;; Issues:
;;
;; At the moment, this only handles short options. We should support
;; long options with =-delimiter syntax. We might be able to handle the
;; latter by pre-splitting the input list.
;;
;; Arguments of duplicated options are pooled. That is, the
;; command line "-a foo -a bar" produces the alist
;; ((a . ("foo" "bar"))), not ((a . ("foo")) (a . ("bar"))).

(define (parse-cli options ts)
  (let ((opt-tab (make-option-table options)))
    (let loop ((vals '()) (ts ts))
      (cond ((null? ts) (values vals '()))  ; no operands
            ((option-string? (car ts))
             (let*-values (((name) (option-string->name (car ts)))
                           ((vs ts*)
                            (process-option name opt-tab (cdr ts))))
               (loop (append-args name vs vals) ts*)))
            (else (values vals ts))))))      ; rest are operands

(define (option-string->name s)
  (string->symbol (string-trim s (lambda (c) (eqv? c #\-)))))

(define (process-option name opt-table in)
  (let*-values (((opt) (lookup-option-by-name opt-table name))
                ((args in*) ((option-parser opt) in)))
    (values ((option-processor opt) opt name args) in*)))

;; If 'name' already appears as a key in 'alist', then append 'args'
;; to name's list value. Otherwise, add the pair (name . args) to
;; alist.
(define (append-args name args alist)
  (alist-adjoin/combinator alist
                           (lambda (_key news olds) (append news olds))
                           name
                           args))
