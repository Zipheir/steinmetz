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

  (define (longest-string strings)
    (s1:fold (lambda (s long) (max long (string-length s)))
             0
             strings))

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

  ;; Returns a comma-separated list of dashed option names.
  (define (format-option-names names)
    (let ((dashed (map (lambda (name)
                         (if (= (string-length name) 1)
                             (string-append "-" name)
                             (string-append "--" name)))
                       names)))
      (s152:string-join dashed ", ")))

  ;; Returns a "signature" string for *option*, giving its names and
  ;; a description of its arguments.
  (define (format-option-signature option)
    (let ((names (option-names option))
          (arg-str
           (cond ((option-allowed-arguments option) =>
                  (lambda (args)
                    (s152:string-join args "|")))
                 ((option-argument-name option) => symbol->string)
                 (else ""))))
      (string-append (format-option-names names) " " arg-str)))

  (define column-left-margin-width 2)
  (define margin-spaces (make-string column-left-margin-width #\space))

  ;; Write descriptions of the *options* to *port*.  Output is split
  ;; into two columns, with the left column giving the form of each
  ;; option and the right displaying its docstring.
  ;;
  ;; FIXME: Split this up.
  (define (put-option-doc-lines port options width)
    (let* ((put (lambda (s) (put-string port s)))
           (nl (lambda () (put-char port #\newline)))
           (left-width (exact (ceiling (* width 0.4))))
           (filler (make-string left-width #\space))
           (sig-width (- left-width column-left-margin-width))
           (right-width (- width left-width))
           (docstring-width (- right-width column-left-margin-width))
           (signatures (map format-option-signature options))
           (docstrings (map option-docstring options))
           (put-right-col-lines
            (lambda (lines)
              (for-each (lambda (s)
                          (put filler)
                          (put margin-spaces)
                          (put s)
                          (nl))
                        lines))))
      (for-each
       (lambda (sig ds)
         (let ((sig-len (string-length sig)))
           (put margin-spaces)
           (put sig)
           (and ds
                (put (make-string (max 0 (- sig-width sig-len))
                                  #\space))
                (let ((ds-lines (string->lines docstring-width ds)))
                  (assert (pair? ds-lines))
                  (cond ((> sig-len sig-width)
                         (nl)
                         (put-right-col-lines ds-lines))
                        (else
                         (put margin-spaces)
                         (put (car ds-lines))
                         (nl)
                         (put-right-col-lines (cdr ds-lines))))))))
       signatures
       docstrings)))

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
