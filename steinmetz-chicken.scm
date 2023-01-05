(module steinmetz
  (option option-map opt-help
   opt-arg-names fold-cli parse-cli->alist)

(import scheme
        (chicken base)
        (srfi 1)
        (srfi 69)
        (srfi 189)
        )

(include "string-util.scm")
(include "options.scm")
)
