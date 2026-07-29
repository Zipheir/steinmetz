;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz usage)
  (export format-option-names
          display-usage)
  (import (rnrs base)
          (rnrs control)
          (rnrs io ports)
          (rnrs io simple)
          (prefix (srfi :1) s1:)
          (prefix (srfi :115) s115:)
          (prefix (srfi :152) s152:)
          (prefix (chezscheme) c:)
          )

  ;;;; Option & usage documentation

  ;;; Sadly, there is next to nothing portable in the formatted-output
  ;;; area.  This is all rather nuts-&-bolts.

  (define (longest-string strings)
    (s1:fold (lambda (s long) (max long (string-length s)))
             0
             strings))

  (define indent-width 2)

  ;; Simple one-solution paragraph reflow-er akin to UNIX fmt(1).
  ;;
  ;; Some limitations:
  ;;
  ;; Eliminates whitespace between words and discards any non-space
  ;; whitespace characters.
  ;;
  ;; Doesn't produce beautiful output, but the more complex algorithms
  ;; (like the one used in GNU fmt(1)) would fill twice the current file.
  (define (string->lines width str)
    (let ((words (s1:filter (lambda (s) (not (equal? s "")))
                            (s115:regexp-split (s115:rx (+ whitespace))
                                               str))))
      (letrec
       ((%lines
         (lambda (len ls line ws)
           (if (null? ws)
               (reverse (map reverse (cons line ls)))
               (let ((w (car ws))
                     (wlen (string-length (car ws)))
                     (rest (cdr ws)))
                 (if (>= (+ wlen 1 len) width)
                     (%lines 0 (cons line ls) '() ws)  ; push to next line
                     (%lines (+ wlen 1 len) ls (cons w line) rest)))))))
        (%lines 0 '() '() words))))

  (define (put-wrapped port str width indent)
    (cond ((<= (string-length str) width)
           (put-string port (make-string indent))
           (put-string port str)
           (newline port))
          (else
           (let ((lines (string->lines width str)))
             (for-each
              (lambda (words)
                (put-string port (make-string indent))
                (put-string port (s152:string-join words))
                (newline port))
              lines)))))

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
  ;; FIXME: The handling of *width* and *indent* is incorrect: the
  ;; lines of a help paragraph after the first are printed flush
  ;; with the left margin (of the whole terminal window).
  (define (put-option-doc-lines port options width)
    (let* ((indent
            (lambda ()
              (put-string port (make-string indent-width #\space))))
           (left-width (exact (ceiling (* width 0.4))))
           (right-width (- width (+ indent-width left-width)))
           (sigs (map format-option-signature options))
           (helps (map (lambda (opt)
                         (option-get-property opt 'docstring))
                       options)))
      (for-each
       (lambda (sig help)
         (indent)
         (cond (help
                (cond ((> (string-length sig) left-width)
                       (put-string port sig)
                       (newline port)
                       (put-wrapped port
                                    help
                                    right-width
                                    (+ indent-width left-width)))
                      (else
                       ; safe to pad--remember, string-pad truncates!
                       (put-string port
                                   (s152:string-pad-right sig
                                                          left-width))
                       (put-wrapped port help right-width 0))))
               (else
                (put-string port sig)
                (newline port))))
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
