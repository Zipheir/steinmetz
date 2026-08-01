;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz test syntax)
  (export run-tests)
  (import (rnrs base)
          (prefix (srfi :1) s1:)
          (srfi :64)
          (steinmetz options)
          (steinmetz syntax)
          )

  (define (find-option-by-names names opts)
    (s1:find (lambda (o) (equal? names (option-names o))) opts))

  (define (run-tests)
    (test-group "options macro"
      (let ((opts (options
                    (option (f file) FILE "input file")
                    (option (e) (ENDIANNESS (big little))
                      "stream endianness")
                    (option (o output) (FILE "-") "output file")
                    (option (k) START)
                    (option (p) (PORT string->number))
                    (flag (v)))))
        (test-assert "returns a list"
          (list? opts))

        (let ((opt (find-option-by-names '("f" "file") opts)))
          ;; If we found opt, it must have the expected names.
          (test-assert "names of option (1)"
            (option? opt))

          (test-equal "argument name of option (1)"
            'FILE
            (option-argument-name opt))

          (test-equal "docstring of option (1)"
            "input file"
            (option-docstring opt)))

        (let ((opt (find-option-by-names '("v") opts)))
          (test-assert "names of option (2)"
            (option? opt))

          (test-assert "argument name of flag"
            (not (option-argument-name opt)))

          (test-assert "docstring of option (2)"
            (not (option-docstring opt))))

        (let ((opt (find-option-by-names '("e") opts)))
          (test-assert "names of option (3)"
            (option? opt))

          (test-equal "allowed argument values of option"
            '("big" "little")
            (option-allowed-arguments opt)))

        (let ((opt (find-option-by-names '("p") opts)))
          (test-assert "names of option (4)"
            (option? opt)))
        ))
    )
  )
