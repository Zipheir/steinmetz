;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz command-line)
  (export clean-command-line)
  (import (rnrs base)
          (prefix (srfi :1) srfi-1:)
          (prefix (srfi :115) srfi-115:))

  ;; Lex a raw command line (list of strings) and return a
  ;; canonical line:
  ;;
  ;; * Short option clusters like -xvf are split into multiple options.
  ;;
  ;; * Long options using --OPT=ARG syntax are split into an option
  ;;   followed by its argument.
  ;;
  ;; There is a conflict with "X style" options (one dash followed by
  ;; a multi-character option name). These are lexed as clusters.
  (define (clean-command-line lis)
    (let ((maybe-split
           (lambda (s)
             (cond ((srfi-115:regexp-matches? short-option-cluster s)
                    (cluster->strings s))
                   ((srfi-115:regexp-matches long-option/equals s) =>
                    (lambda (m)
                      (list (srfi-115:regexp-match-submatch m 1)
                            (srfi-115:regexp-match-submatch m 2))))
                   (else (list s))))))
      (srfi-1:append-map maybe-split lis)))

  (define short-option-cluster
    (srfi-115:regexp '(: #\- alphabetic (+ alphabetic))))

  (define long-option/equals
    (srfi-115:regexp '(: (submatch (: "--" (+ (or alphabetic #\-))))
                         #\=
                         (submatch (+ graphic)))))

  ;; String -> (list String)
  ;; Break up a cluster of short options.
  (define (cluster->strings s)
    (map (lambda (c) (string #\- c))
         (cdr (string->list s))))

  )
