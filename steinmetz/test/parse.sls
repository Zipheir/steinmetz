;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz test parse)
  (export run-tests)
  (import (rnrs base)
	  (prefix (srfi :1) s1:)
	  (srfi :64)
	  (steinmetz options)
	  (steinmetz parse)
	  )

  (define (find-option-by-names names opts)
    (s1:find (lambda (o) (equal? names (option-names o))) opts))

  (define (run-tests)
    (test-group "options macro"
      (let ((opts (options
		    (option (f file) FILE "input file")
		    (flag (v)))))
	(test-assert "options returns a list"
	  (list? opts))

	(let ((opt (find-option-by-names '(f file) opts)))
	  ;; If we found opt, it must have the expected names.
	  (test-assert "names of option created by 'options' (1)"
	    (option? opt))

	  (test-equal "argument name of option created by 'options' (1)"
	    'FILE
	    (option-argument-name opt))

	  (test-equal "help text of option created by 'options' (1)"
	    "input file"
	    (option-property-ref opt 'help)))

	(let ((opt (find-option-by-names '(v) opts)))
	  (test-assert "names of option created by 'options' (2)"
	    (option? opt))

	  (test-assert "argument name of flag created by 'options'"
	    (not (option-argument-name opt)))

	  (test-assert "help text of option created by 'options' (2)"
	    (not (option-property-ref opt 'help))))
        ))

    (test-group "make-cli-option"
      (let ((opt (make-cli-option '(o output)
                                  'FILE
                                  values
                                  '((help . "output file")
                                    (animal . "badger")))))
        (test-assert "'make-cli-option' returns an option"
          (option? opt))

        (test-equal "names of option created by 'make-cli-option'"
          '(o output)
          (option-names opt))

        (test-equal "arg. name of option created by 'make-cli-option'"
          'FILE
          (option-argument-name opt))

        (test-equal "properties of option created by 'make-cli-option'"
          '("output file" "badger")
          (list (option-property-ref opt 'help)
                (option-property-ref opt 'animal)))))
    )
  )
