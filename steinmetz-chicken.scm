(module steinmetz
  (make-option make-flag option-map opt-help
   opt-arg-name fold-cli process-cli
   make-usage options)

(import scheme
        (except (chicken base) alist-update)
        (srfi 1)
        (srfi 69)
        (srfi 115)
        fmt
        )

;; SRFI 69/R6RS shim
(begin
 (define (make-hashtable hash-func equiv)
   (make-hash-table equiv hash-func))

 (define hashtable-set! hash-table-set!)

 (define hashtable-ref hash-table-ref/default)
 )

(include "command-line.scm")
(include "string-util.scm")
(include "doc-fmt.scm")
(include "options.scm")
)
