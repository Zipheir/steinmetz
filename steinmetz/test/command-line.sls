;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz test command-line)
  (export run-tests)
  (import (rnrs base)
          (rnrs hashtables)
          (srfi :64)
          (steinmetz command-line)
          (steinmetz options)
          )

  (define (dummy tokens)
    (values #t tokens))

  (define (run-tests)
    (test-group "command-line"
      ;; Only a little info about options is needed for command-line
      ;; preprocessing--namely, whether each option takes an argument.
      (let ((opt-tab (make-hashtable string-hash string=?)))
        (for-each (lambda (opt)
                    (hashtable-set! opt-tab
                                    (car (option-names opt))
                                    opt))
                  (list (make-option '("f") 'ARG dummy '())
                        (make-option '("a") #f dummy '())
                        (make-option '("b") #f dummy '())
                        (make-option '("c") 'ARG dummy '())
                        (make-option '("1") #f dummy '())
                        (make-option '("long") 'ARG dummy '())))

        (let ((tokens '("-f" "foo" "-a" "-1" "--long" "bar" "bash")))
          (test-equal "command line that needs no processing"
            tokens
            (clean-command-line opt-tab tokens)))

        (test-equal "short options with run-in arguments"
          '("-f" "abc")
          (clean-command-line opt-tab '("-fabc")))

        (test-equal "short option cluster, last takes no argument"
          '("-a" "-b")
          (clean-command-line opt-tab '("-ab")))

        (test-equal "short option cluster, last has run-in argument (1)"
          '("-a" "-b" "-c" "bash")
          (clean-command-line opt-tab '("-abcbash")))

        (test-equal "short option cluster, last has run-in argument (2)"
          '("-a" "-b" "-c" "1a")
          (clean-command-line opt-tab '("-abc1a")))

        (test-equal "long option with run-in argument"
          '("--long" "short")
          (clean-command-line opt-tab '("--long=short")))
        )))
  )

