;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz test parse)
  (export run-tests)
  (import (rnrs base)
          (rnrs exceptions)
          (only (rnrs lists) assoc)
          (rnrs sorting)
          (rnrs io simple)
          (prefix (srfi :1) s1:)
          (srfi :64)
          (steinmetz options)
          (steinmetz parse)
          (steinmetz syntax)
          )

  (define (string-key<? p1 p2)
    (string<? (car p1) (car p2)))

  ;; Returns a list of the two lists returned by
  ;; 'process-command-line'.  The option alist is sorted
  ;; by option name.
  (define (pcl->list/sorted-opts opts cl-list)
    (let-values (((opts rands)
                  (process-command-line opts cl-list)))
      (list (list-sort string-key<? opts) rands)))

  ;; SRFI 64's test-error is outdated & approximately useless.
  (define-syntax our-test-error
    (syntax-rules ()
      ((out-test-error name con-pred expr)
       (test-assert name
         (guard (con
                  ((con-pred con) #t)
                  (else #f))
           expr)))))

  (define (run-tests)
    (test-group "parse-command-line"
      (let ((opts (options
                    (option (f file) FILE)
                    (flag (v verbose)))))
        (test-eqv "count options, ignore operands"
          4
          (guard (con
                   ((parser-condition? con) -1)
                   (else (raise-continuable con)))
            (let-values (((total _rest)
                          (parse-command-line
                           opts
                           (lambda (_name _arg n) (values #t (+ n 1)))
                           '("-v" "-f" "foo" "--verbose" "--file" "bar")
                           0)))
              total)))

        (test-equal
          "ignore options, return operands (1)"
          '("a" "b")
          (guard (con
                   ((parser-condition? con) '())
                   (else (raise-continuable con)))
            (let-values (((rands _junk)
                          (parse-command-line
                           opts
                           (lambda (name arg rands)
                             (if name
                                 (values #t rands)
                                 (values #t (cons arg rands))))
                           '("-v" "a" "-f" "foo" "--file" "bar" "b")
                           '())))
              (list-sort string<? rands))))

        (test-equal
          "ignore options, return operands (2)"
          '("a" "--file" "bar" "b")
          (guard (con
                   ((parser-condition? con) '())
                   (else (raise-continuable con)))
            (let-values (((rands _junk)
                          (parse-command-line
                           opts
                           (lambda (name arg rands)
                             (if name
                                 (values #t rands)
                                 (values #t (cons arg rands))))
                           '("-v" "a" "-f" "foo" "--" "--file" "bar" "b")
                           '())))
              (reverse rands))))

        (test-equal
          "return options (semi-canonicalized) and operands"
          '((("f" . "bar") ("f" . "foo") ("v" . #t) ("v" . #t))
            ("a" "b"))
          (guard (con
                   ((parser-condition? con) '())
                   (else (raise-continuable con)))
            (let*-values
             (((cli)
               '("-v" "-f" "foo" "--file" "bar" "--verbose" "a" "b"))
              ((opt-alist rands)
               (parse-command-line
                opts
                (lambda (opt arg os)
                  (and opt  ; halt at first operand
                       (let ((name (car (option-names opt))))
                         (values #t (cons (cons name arg) os)))))
                cli
                '())))
              (list (list-sort (lambda (p1 p2)
                                 (string<? (car p1) (car p2)))
                               opt-alist)
                    (list-sort string<? rands)))))

        (our-test-error
          "parser exception on missing argument"
          parser-condition?
          (let ((cl '("--verbose" "--file")))
            (parse-command-line
             opts
             (lambda (opt arg os)
               (and opt  ; halt at first operand
                    (let ((name (car (option-names opt))))
                      (values #t (cons (cons name arg) os)))))
             cl
             '())))
        ))

    (test-group "process-command-line"
      (let* ((opts (options
                    (option (file f) FILE)
                    (flag (verbose v))
                    (flag ("1")))))
        (test-equal "process-command-line"
          '((("file" "foo"))
            ("bash" "ksh" "csh"))
          (pcl->list/sorted-opts opts
                                 '("--file" "foo" "bash" "ksh" "csh")))

        (test-equal "numeric flag"
          '((("1" #t) ("file" "foo"))
            ("bash" "ksh" "csh"))
          (pcl->list/sorted-opts
           opts
           '("--file" "foo" "-1" "bash" "ksh" "csh")))

        (test-equal
          "duplicate options"
          '((("1" #t) ("file" "foo" "bar") ("verbose" #t))
            ("bash"))
          (pcl->list/sorted-opts
           opts
           '("--file" "foo" "-v" "-f" "bar" "-1" "bash")))

        (test-equal "clusters"
          '((("file" "foo") ("verbose" #t))
            ("bash" "csh"))
          (pcl->list/sorted-opts opts '("-vf" "foo" "bash" "csh")))

        (test-equal "'=' syntax"
          '((("file" "foo"))
            ("bash" "csh"))
          (pcl->list/sorted-opts opts '("--file=foo" "bash" "csh")))

        (test-equal "clusters & '=' syntax"
          '((("file" "foo" "bar") ("verbose" #t))
            ("bash" "csh"))
          (pcl->list/sorted-opts
           opts
           '("-vf" "foo" "--file=bar" "bash" "csh")))

        (test-equal
          "end-of-options symbol (--)"
          '((("file" "foo"))
            ("-v" "-f" "bar" "-1" "bash"))
          (pcl->list/sorted-opts
           opts
           '("--file" "foo" "--" "-v" "-f" "bar" "-1" "bash")))

        (test-equal
          "-- argument shouldn't end opt. parsing"
          '((("1" #t) ("file" "foo" "--") ("verbose" #t))
            ("bash"))
          (pcl->list/sorted-opts
           opts
           '("--file" "foo" "-v" "-f" "--" "-1" "bash")))

        (our-test-error "parser exception on missing argument"
          parser-condition?
          (process-command-line opts '("--file")))
        )

      (let ((opts
             (options
              (option (e) (ENDIANNESS (big little)))
              (flag (v) "verbosity")
              (option (a sort-algorithm)
                (ALGORITHM-NAME (quick merge bubble bogo))))))
        (test-equal "valid fixed arguments (1)"
          '((("a" "bubble") ("e" "big"))
            ("csh" "rc"))
          (pcl->list/sorted-opts
           opts
           '("-e" "big" "-a" "bubble" "csh" "rc")))

        (test-equal "valid fixed arguments (2)"
          '((("a" "merge") ("e" "little"))
            ("csh" "rc"))
          (pcl->list/sorted-opts
           opts
           '("--sort-algorithm=merge" "-elittle" "csh" "rc")))

        (our-test-error "invalid fixed arguments"
          parser-condition?
          (pcl->list/sorted-opts
           opts
           '("-e" "medium" "-a" "bogo" "csh" "rc")))
        )
      )
    )
  )
