;;;; Portable option documentation procedures

;;; This formats and prints option documentation using standard
;;; string procedures only (plus string-join, which is provided).
;;; The results aren't beautiful, but they do the job.

(define (option-name->string sym)
  (let ((s (symbol->string sym)))
    (if (= (string-length s) 1)  ; short option
        (string-append "-" s)
        (string-append "--" s))))

;; Returns a list of strings, each one giving the forms and
;; help text for an option.
(define (make-option-descriptions options)
  (let ((fmt-names
         (lambda (nms)
           (case (length nms)
             ((0) "")
             ((1) (option-name->string (car nms)))
             (else
              (let ((os (map option-name->string nms)))
                (string-append "(" (string-join os " ") ")"))))))
        (fmt-arg
         (lambda (arg) (or arg ""))))

    (map (lambda (opt)
           (string-append
            (fmt-names (option-get-property opt 'names))
            (fmt-arg (option-get-property opt 'argument-name))
            "  "
            (or (option-get-property opt 'help) "")))
         options)))

;; Portable (and hence low-budget) pretty-printing.
(define (make-usage options header footer)
  (string-append header
                 "\n"
                 (string-join
                  (map (lambda (s)
                         (string-append "  " s))
                       (make-option-descriptions options))
                  "\n")
                 "\n"
                 footer
                 "\n"))
