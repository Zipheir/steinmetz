;;;; Option documentation using foof's fmt library

(define (option-name->string sym)
  (let ((s (symbol->string sym)))
    (if (= (string-length s) 1)  ; short option
        (string-append "-" s)
        (string-append "--" s))))

(define (fmt-names names)
  (dsp (map option-name->string names)))

(define (fmt-arguments args)
  (fmt-join dsp args " "))

(define (make-usage options header footer)
  (let* ((fmt-option
          (lambda (opt)
            (let ((names (option-get-property opt 'names))
                  (args (option-get-property opt 'argument-names))
                  (help (option-get-property opt 'help)))
              (columnar (cat (fmt-names names)
                             (dsp " ")
                             (if args (fmt-arguments args) (dsp "")))
                        (dsp (or help ""))))))
         (opt-fmts (map fmt-option options)))
    (fmt #f (dsp header) nl
            (pad/left 2 (fmt-join dsp opt-fmts nl))
            (dsp footer) nl)))
