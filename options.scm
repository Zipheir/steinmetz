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

;; Parse an argument.
;; The 'conv' procedure takes the argument string and an error
;; continuationd 'fail'. It either returns a value or calls 'fail'
;; on a message.
(define (raw-argument opt-names conv)
  (let* ((name-string (symbol->string (car opt-names)))  ; hack
         (make-msg     ; error message template
          (lambda (msg-body)
            (string-append "option " name-string ": "
                           msg-body))))
    (lambda (lis)
      (if (and (pair? lis) (argument-string? (car lis)))
          (call-with-current-continuation
           (lambda (k)
             (let ((val (conv (car lis)
                              (lambda (s)
                                (k (left (make-msg s)))))))
               (right val (cdr lis)))))
          (left (make-msg "missing argument"))))))

;; Parses k arguments, converts them, and returns them as
;; a list.
(define (arguments names k conv)
  (parser-seq (make-list k (raw-argument names conv))))

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
  (raw-option (option-arity opt)
              (option-parser opt)
              (alist-update key val (option-properties opt))))

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
                      (arguments names n conv))))
       (raw-option n arg-p (singleton-properties 'names names))))))

(define (option-map f opt)
  (raw-option (option-arity opt)
              (parser-map f (option-parser opt))
              (option-properties opt)))

;;; Option combinators

;; Add a help string to opt.
(define (opt-help s opt)
  (option-add-property opt 'help s))

;; Add a list of argument names (symbols) to opt.
(define (opt-arg-names names opt)
  (option-add-property opt 'argument-names names))

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
  (right #t ts))

(define (fold-cli options proc cli-lis . seeds)
  (let ((opt-tab (make-option-table options))
        (ts (clean-command-line cli-lis)))

    (define (accum-option name ts seeds cont)
      (either-ref (process-option name opt-tab (cdr ts))
                  parser-exception
                  (lambda (v ts*)
                    (let-values ((seeds* (apply proc name v seeds)))
                      (cont seeds* ts*)))))

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

(define (process-option name opt-table in)
  (let ((opt (lookup-option-by-name opt-table name)))
    ((option-parser opt) in)))

;;; Convenience
(define (parse-cli->alist options ts)
  (fold-cli options
            (lambda (name args res)
              (cons (cons name args) res))
            ts
            '()))
