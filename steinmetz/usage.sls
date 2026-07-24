;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz usage)
  (export format-option-names
          put-usage)
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
  (define (put-option-doc-lines port options)
    (assert (output-port? port))
    (assert (and (list? options) (s1:every option? options)))
    (let* ((indent
            (lambda ()
              (put-string port "  ")))
           (sigs (map format-option-signature options))
           (helps (map (lambda (opt)
                         (option-get-property opt 'help))
                       options))
           (left-width (+ 2 (longest-string sigs))))
      (for-each
       (lambda (sig help)
         (indent)
         (cond (help
                (put-string port
                            (s152:string-pad-right sig left-width))
                (put-string port help))
               (else (put-string port sig)))
         (put-char port #\newline))
       sigs
       helps)))

  ;; Writes a usage message to *port*.
  (define (put-usage port options header footer)
    (assert (output-port? port))
    (assert (list? options))
    (assert (string? header))
    (assert (string? footer))
    (when (not (equal? "" header))
      (put-string port header)
      (put-char port #\newline))
    (put-option-doc-lines port options)
    (when (not (equal? "" footer))
      (put-string port footer)
      (put-char port #\newline)))

  )
