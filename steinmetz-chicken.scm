(module steinmetz
  (option option-map opt-help
   opt-arg-names fold-cli parse-cli->alist
   make-usage)

(import scheme
        (chicken base)
        (chicken irregex)
        (srfi 1)
        (srfi 69)
        (srfi 189)
        fmt
        )

(include "command-line.scm")
(include "string-util.scm")
(include "doc-fmt.scm")
(include "options.scm")
)
