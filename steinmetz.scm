;;;; R6RS library file (for Chez Scheme; other R6 Schemes will
;;;; need some adjustments).

(library (steinmetz)
  (export make-option option-map opt-help opt-arg-name fold-cli
          process-cli make-usage options make-flag)
  (import (rnrs base)
          (rnrs control (6))
          (rnrs lists (6))
          (rnrs hashtables (6))
          (only (srfi :1 lists) append-map)
          (srfi :9 records)
          (srfi :115)
          (only (srfi :152) string-index string-skip string-drop-while
                            string-concatenate string-join)
          (only (chezscheme) include)
          )
  (include "command-line.scm")
  (include "doc-portable.scm")
  (include "options.scm")
)
