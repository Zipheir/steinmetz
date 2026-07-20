;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz usage)
  (export format-option-names
          put-usage)
  (import (rnrs base)
          (rnrs control)
          (prefix (srfi :152) s152:)
          (steinmetz options)
          )

  ;;;; Option & usage documentation

  ;;; Sadly, there is next to nothing portable in the formatted-output
  ;;; area.  The following could be improved significantly if SRFI 166
  ;;; or a full 'printf' or FORMAT-style procedure were available.

  (define (option-name->string sym)
    (let ((s (symbol->string sym)))
      (if (= (string-length s) 1)  ; short option?
          (string-append "-" s)
          (string-append "--" s))))

  (define (format-option-names names)
    (string-join (map option-name->string names) ", "))

  ;; Write a description of *option* to *port*.
  (define (put-option-doc-line port option)
    (assert (output-port? port))
    (assert (option? option))
    (let ((names (option-get-property option 'names))
          (argname (option-get-property option 'argument-name))
          (help (option-get-property option 'help)))
      (put-string port "  ") ; indent
      ;; Print option names.
      (case (length names)
        ((1)
         (put-string port (option-name->string (car names))))
        (else
         (put-string port "(")
         (put-string port (format-option-names names))
         (put-string port ")")))
      (when argname
        (assert (symbol? argname))
        (put-string port " ")
        (put-string port (symbol->string argname)))
      (when help
        (assert (string? help))
        (put-string port "  ")
        (put-string port help))
      (put-string port "\n")))

  ;; Writes a usage message to *port*.
  (define (put-usage port options header footer)
    (assert (output-port? port))
    (assert (list? options))
    (assert (string? header))
    (assert (string? footer))
    (put-string port header)
    (put-char port #\newline)
    (for-each (lambda (opt)
                (put-option-doc-line port opt))
              options)
    (put-string port footer)
    (put-char port #\newline))

  )
