;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz command-line)
  (export clean-command-line)
  (import (rnrs base)
          (prefix (srfi :1) s1:)
          (prefix (srfi :115) s115:))

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
    (let*
     ((short-option-cluster
       (s115:regexp '(: #\- alphabetic (+ alphabetic))))
      (long-option/equals
       (s115:regexp '(: (submatch (: "--" (+ (or alphabetic #\-))))
                        #\=
                        (submatch (+ graphic)))))
      ;; Break up a cluster into a list of short options.
      (cluster->strings
       (lambda (s)
         (map (lambda (c) (string #\- c))
              (cdr (string->list s)))))
      (maybe-split
       (lambda (s)
         (cond ((s115:regexp-matches? short-option-cluster s)
                (cluster->strings s))
               ((s115:regexp-matches long-option/equals s) =>
                (lambda (m)
                  (list (s115:regexp-match-submatch m 1)
                        (s115:regexp-match-submatch m 2))))
               (else (list s))))))

      (s1:append-map maybe-split lis)))
  )
