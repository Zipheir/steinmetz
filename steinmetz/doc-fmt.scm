;;;; Option documentation using foof's fmt library

(define (option-name->string sym)
  (let ((s (symbol->string sym)))
    (if (= (string-length s) 1)  ; short option
        (string-append "-" s)
        (string-append "--" s))))

(define (fmt-names names)
  (fmt-join dsp (map option-name->string names) (dsp ", ")))

(define (fmt-names->string names)
  (fmt #f (fmt-names names)))

(define (make-usage options header footer)
  (let ((fmt-option
         (lambda (opt)
           (let ((names (option-get-property opt 'names))
                 (arg (option-get-property opt 'argument-name))
                 (help (option-get-property opt 'help)))
             (cat (space-to 2)
                  (columnar (cat (fmt-names names)
                                 (dsp " ")
                                 (dsp (or arg "")))
                            (dsp (or help ""))))))))
    (fmt #f (dsp header) nl
            (fmt-join fmt-option options)
            (dsp footer) nl)))
