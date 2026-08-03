;;; SPDX-FileCopyrightText: Copyright 2022--2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz test options)
  (export run-tests)
  (import (rnrs)
          (srfi :64)
          (steinmetz options)
          )

  (define (run-tests)
    (test-group "(steinmetz options)"
      (test-assert "make-option (3 arguments)"
        (option? (make-option '("f" "file") 'FILE values)))

      (test-assert "make-option (4 arguments)"
        (option? (make-option '("f" "file")
                              'FILE
                              values
                              "foo")))

      (test-assert "make-option (5 arguments)"
        (option? (make-option '("f" "file")
                              'FILE
                              values
                              "foo"
                              "f")))

      (test-assert "make-option (6 arguments)"
        (option? (make-option '("f" "file")
                              'FILE
                              values
                              "foo"
                              "f"
                              '("a"))))

      (test-assert "make-option (7 arguments)"
        (option? (make-option '("f" "file")
                              'FILE
                              values
                              "foo"
                              "f"
                              '("a")
                              'z)))

      (let ((opt (make-option '("f" "file")
                              'FILE
                              values
                              "foo"
                              "f"
                              '("a")
                              'z)))
        (test-equal "option-names"
          '("f" "file")
          (option-names opt))

        (test-equal "option-argument-name"
          'FILE
          (option-argument-name opt))

        (test-equal "option-docstring"
          "foo"
          (option-docstring opt))

        (test-equal "option-canonical-name"
          "f"
          (option-canonical-name opt))

        (test-equal "option-allowed-arguments"
          '("a")
          (option-allowed-arguments opt))

        (test-equal "option-user-data"
          'z
          (option-user-data opt))
        )
      ))
  )
