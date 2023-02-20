(module steinmetz
  (make-option option-map opt-help
   opt-arg-name fold-cli process-cli
   make-usage options)

(import scheme
        (chicken base)
        (chicken irregex)
        (srfi 1)
        (srfi 69)
        fmt
        )

;; SRFI 69/R6RS shim
(begin
 (define (make-hashtable hash-func equiv)
   (make-hash-table equiv hash-func))

 (define hashtable-set! hash-table-set!)

 (define hashtable-ref hash-table-ref/default)
 )

;; SRFI 115/irregex shim
(begin
 (define regexp-match? irregex-match?)
 (define regexp-matches irregex-match)
 (define regexp-match-submatch irregex-match-substring)
 (define sre->regexp sre->irregex)
 )

(include "command-line.scm")
(include "string-util.scm")
(include "doc-fmt.scm")
(include "options.scm")
)
