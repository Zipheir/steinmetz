;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz utility)
  (export list-of-strings?
          option-string?
          option-string->name
          ylppa-values
          stringify
          )
  (import (rnrs base)
          (prefix (srfi :1) s1:)
          (prefix (srfi :152) s152:)
          )

  (define (list-of-strings? x)
    (and (list? x) (s1:every string? x)))

  (define (option-string? s)
    (and (not (equal? s ""))
         (eqv? #\- (string-ref s 0))))

  ;; If *s* is a string describing a long or short option,
  ;; return its name as a symbol. Otherwise, return #f.
  (define (option-string->name s)
     (and (option-string? s)
          (s152:string-drop-while s (lambda (c) (eqv? c #\-)))))

  ;; Return the contents of *vs* and each of *rest* as values.
  (define (ylppa-values vs . rest)
    (apply values (append vs rest)))

  (define (stringify x)
    (cond ((symbol? x) (symbol->string x))
          ((string? x) x)
          (else (assertion-violation 'options
                                     "not a string or symbol"
                                     x))))
  )
