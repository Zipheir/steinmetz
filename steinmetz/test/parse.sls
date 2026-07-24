;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz test parse)
  (export run-tests)
  (import (rnrs base)
          (rnrs exceptions)
          (rnrs sorting)
          (prefix (srfi :1) s1:)
          (srfi :64)
          (steinmetz options)
          (steinmetz parse)
          )

  (define (find-option-by-names names opts)
    (s1:find (lambda (o) (equal? names (option-names o))) opts))

  (define (string-key<? p1 p2)
    (string<? (car p1) (car p2)))

  (define (run-tests)
    (test-group "options macro"
      (let ((opts (options
                    (option (f file) FILE "input file")
                    (flag (v)))))
        (test-assert "options returns a list"
          (list? opts))

        (let ((opt (find-option-by-names '("f" "file") opts)))
          ;; If we found opt, it must have the expected names.
          (test-assert "names of option created by 'options' (1)"
            (option? opt))

          (test-equal "argument name of option created by 'options' (1)"
            'FILE
            (option-argument-name opt))

          (test-equal "help text of option created by 'options' (1)"
            "input file"
            (option-property-ref opt 'help)))

        (let ((opt (find-option-by-names '("v") opts)))
          (test-assert "names of option created by 'options' (2)"
            (option? opt))

          (test-assert "argument name of flag created by 'options'"
            (not (option-argument-name opt)))

          (test-assert "help text of option created by 'options' (2)"
            (not (option-property-ref opt 'help))))
        ))

    (test-group "make-cli-option"
      (let ((opt (make-cli-option '("o" "output")
                                  'FILE
                                  values
                                  '((help . "output file")
                                    (animal . "badger")))))
        (test-assert "'make-cli-option' returns an option"
          (option? opt))

        (test-equal "names of option created by 'make-cli-option'"
          '("o" "output")
          (option-names opt))

        (test-equal "arg. name of option created by 'make-cli-option'"
          'FILE
          (option-argument-name opt))

        (test-equal "properties of option created by 'make-cli-option'"
          '("output file" "badger")
          (list (option-property-ref opt 'help)
                (option-property-ref opt 'animal)))))

    (test-group "make-cli-flag"
      (let ((opt (make-cli-flag '("v" "verbose")
                                '((help . "verbose output")
                                  (animal . "badger")))))
        (test-assert "'make-cli-flag' returns an option"
          (option? opt))

        (test-equal "names of option created by 'make-cli-flag'"
          '("v" "verbose")
          (option-names opt))

        (test-equal "properties of option created by 'make-cli-flag'"
          '("verbose output" "badger")
          (list (option-property-ref opt 'help)
                (option-property-ref opt 'animal)))))

    (test-group "parse-command-line"
      (let ((opts (options
                    (option (f file) FILE)
                    (flag (v verbose)))))
        (test-eqv "parse-command-line: count options, ignore operands"
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
          "parse-command-line: ignore options, return operands"
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
          "parse-command-line: return options (semi-canonicalized) \
           and operands"
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
        ))

    (test-group "process-command-line"
      (let* ((opts (options
                    (option (file f) FILE)
                    (flag (verbose v))
                    (flag ("1"))))
             ;; Returns a list of the two lists returned by
             ;; 'process-command-line'.  The option alist is sorted
             ;; by option name.
             (pcl->list/sorted-opts
              (lambda (cl-list)
                (let-values (((opts rands)
                              (process-command-line opts cl-list)))
                  (list (list-sort string-key<? opts) rands)))))

        (test-equal "process-command-line"
          '((("file" "foo"))
            ("bash" "ksh" "csh"))
          (pcl->list/sorted-opts '("--file" "foo" "bash" "ksh" "csh")))

        (test-equal "process-command-line, numeric flag"
          '((("1" #t) ("file" "foo"))
            ("bash" "ksh" "csh"))
          (pcl->list/sorted-opts
           '("--file" "foo" "-1" "bash" "ksh" "csh")))

        (test-equal
          "process-command-line, duplicate options"
          '((("1" #t) ("file" "foo" "bar") ("verbose" #t))
            ("bash"))
          (pcl->list/sorted-opts
           '("--file" "foo" "-v" "-f" "bar" "-1" "bash")))

        (test-equal "process-command-line, clusters"
          '((("file" "foo") ("verbose" #t))
            ("bash" "csh"))
          (pcl->list/sorted-opts '("-vf" "foo" "bash" "csh")))

        (test-equal "process-command-line, '=' syntax"
          '((("file" "foo"))
            ("bash" "csh"))
          (pcl->list/sorted-opts '("--file=foo" "bash" "csh")))

        (test-equal "process-command-line, clusters & '=' syntax"
          '((("file" "foo" "bar") ("verbose" #t))
            ("bash" "csh"))
          (pcl->list/sorted-opts
           '("-vf" "foo" "--file=bar" "bash" "csh")))
        ))
    )
  )
