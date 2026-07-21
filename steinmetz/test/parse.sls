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
	  (test-assert "names of option created by 'options' (1)"
	    opt)  ; if we found opt, it must have the expected names

	  (test-eqv "argument name of option created by 'options' (1)"
	    'FILE
	    (option-argument-name opt))

	  (test-equal "help text of option created by 'options' (1)"
	    "input file"
	    (option-property-ref opt 'help)))

	(let ((opt (find-option-by-names '(v) opts)))
	  (test-assert "names of option created by 'options' (2)"
	    opt)

	  (test-assert "argument name of flag created by 'options'"
	    (not (option-argument-name opt)))

	  (test-assert "help text of option created by 'options' (2)"
	    (not (option-property-ref opt 'help))))

        )))
  )
