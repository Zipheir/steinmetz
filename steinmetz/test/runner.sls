;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz test runner)
  (export custom-test-runner-factory)
  (import (rnrs base)
          (rnrs io simple)
          (srfi :64)
          )

  (define (custom-test-runner-factory)
    (let*
     ((runner (test-runner-null))
      (test-end
       (lambda (runner)
         (case (test-result-kind runner)
           ((pass)
            (display "Pass: ")
            (display (test-runner-test-name runner))
            (newline))
           ((fail)
            (display "FAIL: ")
            (display (test-runner-test-name runner))
            (display ". Expected ")
            (display (test-result-ref runner 'expected-value))
            (display ", got ")
            (display (test-result-ref runner 'actual-value))
            (display ".\n")))))
      (test-final
       (lambda (runner)
         (display "===============================\n")
         (display "Total passes: ")
         (display (test-runner-pass-count runner))
         (newline)
         (display "Total failures: ")
         (display (test-runner-fail-count runner))
         (newline)
         (display "Total skips: ")
         (display (test-runner-skip-count runner))
         (newline))))

      (test-runner-on-test-end! runner test-end)
      (test-runner-on-final! runner test-final)
      runner))

  )
