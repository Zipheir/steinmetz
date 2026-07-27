;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz usage)
  (export format-option-names
          display-usage)
  (import (rnrs base)
          (rnrs control)
          (rnrs io ports)
          (prefix (srfi :1) s1:)
          (prefix (srfi :152) s152:)
          (steinmetz options)
          )

  ;;;; Option & usage documentation

  ;;; Sadly, there is next to nothing portable in the formatted-output
  ;;; area.  This is all rather nuts-&-bolts.

  (define (longest-string strings)
    (s1:fold (lambda (s long) (max long (string-length s)))
             0
             strings))

  ;; TODO: Simple paragraph flow-er.
  (define (wrap-string s width)
    s)

  (define (format-option-names names)
    (let ((dashed (map (lambda (name)
                         (if (= (string-length name) 1)
                             (string-append "-" name)
                             (string-append "--" name)))
                       names)))
      (s152:string-join dashed ", ")))

  (define (format-option-signature option)
    (let ((names (option-names option))
          (arg-str
           (cond ((option-get-property option 'allowed-arguments) =>
                  (lambda (args)
                    (s152:string-join args "|")))
                 ((option-argument-name option) => symbol->string)
                 (else ""))))
      (string-append (format-option-names names) " " arg-str)))

  ;; Write descriptions of the *options* to *port*.
  ;;
  ;; FIXME: Given the tendency of some programs to use very long option
  ;; names, I think there should be a bound set on *left-width*.  If
  ;; this bound is exceeded, the left-column width is set to the max
  ;; allowable and the help text for a too-long is printed on the
  ;; following line.
  (define (put-option-doc-lines port options width)
    (let* ((indent
            (lambda ()
              (put-string port "  ")))
           (sigs (map format-option-signature options))
           (left-width (+ 2 (longest-string sigs)))
           (right-width (- width left-width))
           (helps (map (lambda (opt)
                         (option-get-property opt 'docstring))
                       options)))
      (for-each
       (lambda (sig help)
         (indent)
         (cond (help
                (put-string port
                            (s152:string-pad-right sig left-width))
                (put-string port (wrap-string help right-width)))
               (else (put-string port sig)))
         (put-char port #\newline))
       sigs
       helps)))

  ;; Maximum width, in characters, of usage output.
  (define default-width 75)

  ;; Writes a usage message to *port*.
  (define display-usage
    (case-lambda
      ((options)
       (display-usage options
                      (current-output-port)
                      ""
                      ""
                      default-width))
      ((options port)
       (display-usage options port "" "" default-width))
      ((options port header)
       (display-usage options port header "" default-width))
      ((options port header footer)
       (display-usage options port header footer default-width))
      ((options port header footer width)
       (assert (output-port? port))
       (assert (and (list? options) (s1:every option? options)))
       (assert (string? header))
       (assert (string? footer))
       (assert (and (integer? width) (positive? width)))
       (when (not (equal? "" header))
         (put-string port header)
         (put-char port #\newline))
       (put-option-doc-lines port options width)
       (when (not (equal? "" footer))
         (put-string port footer)
         (put-char port #\newline)))))

  )
