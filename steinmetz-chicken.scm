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

(include "command-line.scm")
(include "string-util.scm")
(include "doc-fmt.scm")
(include "options.scm")
)
