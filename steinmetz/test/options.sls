;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz test options)
  (export run-tests)
  (import (rnrs)
          (srfi :64)
          (steinmetz options)
          )

  (define (run-tests)
    (test-group "(steinmetz options)"
      (let ((opt (make-option '(f file)
                              'FILE
                              values  ; not a real parser
                              '((help . "input file")))))
        (test-assert "option satisfies option?"
          (option? opt))

        (test-equal "option-names"
          '(f file)
          (option-names opt))

        (test-equal "option-argument-name"
          'FILE
          (option-argument-name opt))

        (test-eqv "option-argument-parser"
          values
          (option-argument-parser opt))

        (test-equal "option-properties->alist"
          '((help . "input file"))
          (option-properties->alist opt))

        (test-equal "option-property-ref on existing key"
          "input file"
          (option-property-ref opt 'help))

        (test-equal "option-property-ref on missing key"
          #f
          (option-property-ref opt 'flabs))

        (test-equal "add new key with option-set-property"
          "hello"
          (option-property-ref
           (option-set-property opt 'flabs "hello")
           'flabs))

        (test-equal "change existing key with option-set-property"
          "read from FILE"
          (option-property-ref
           (option-set-property opt 'help "read from FILE")
           'help))
        )))
  )
