;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz usage)
  (export put-usage)
  (import (rnrs base)
          (rnrs control)
          (rnrs io ports)
          (rnrs io simple)
          (prefix (srfi :1) s1:)
          (prefix (srfi :115) s115:)
          (prefix (srfi :152) s152:)
          (steinmetz options)
          )

  ;;;; Option & usage documentation

  ;;; Sadly, there is next to nothing portable in the formatted-output
  ;;; area.  This is all rather nuts-&-bolts.

  ;;; FIXME: The formatting procedures should either use a functional
  ;;; idiom (like string->lines) or an imperative one (like
  ;;; put-option-doc-lines), not a mix of both.

  (define (longest-string strings)
    (s1:fold (lambda (s long) (max long (string-length s)))
             0
             strings))

  (define margin-width 2)

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
                     ;; Never add empty lines, even if a one-word
                     ;; line is overlong.
                     (if (null? line)
                         (%lines 0 (cons (list w) ls) '() rest)
                         (%lines 0 (cons line ls) '() ws))
                     (%lines (+ wlen 1 len) ls (cons w line) rest)))))))
        (map s152:string-join (%lines 0 '() '() words)))))

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
  ;; FIXME: Clean this up.
  (define (put-option-doc-lines port options width)
    (let* ((left-width (exact (ceiling (* width 0.4))))
           (sig-width (- left-width margin-width))
           (right-width (- width left-width))
           (help-width (- right-width margin-width))
           (sigs (map format-option-signature options))
           (helps (map (lambda (opt)
                         (option-get-property opt 'docstring))
                       options))
           (column 0)
           (next-line
            (lambda ()
              (newline port)
              (set! column 0)))
           (space-to
            (lambda (col)
              (do ((c column (+ c 1)))
                  ((>= c col) (set! column c))
                (put-char port #\space))))
           (put-help-wrapped
            (lambda (help)
              (let ((lines (string->lines right-width help)))
                (for-each (lambda (line)
                            (space-to (+ left-width margin-width))
                            (put-string port line)
                            (next-line))
                          lines)))))
      (for-each
       (lambda (sig help)
         (space-to margin-width)
         (cond (help
                (cond ((<= (string-length sig) sig-width)
                       (put-string port sig)
                       (set! column (+ column (string-length sig)))
                       (space-to left-width)
                       (put-help-wrapped help))
                      (else  ; signature is overlong
                       (put-string port sig)
                       (next-line)
                       (put-help-wrapped help))))
               (else
                (put-string port sig)
                (next-line))))
       sigs
       helps)))

  ;; Maximum width, in characters, of usage output.
  (define default-width 75)

  ;; Writes a usage message to *port*.
  (define put-usage
    (case-lambda
      ((port options header)
       (put-usage port options header "" default-width))
      ((port options header footer)
       (put-usage port options header footer default-width))
      ((port options header footer width)
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
