;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz command-line)
  (export normalize-command-line)
  (import (rnrs base)
          (rnrs hashtables)
          (prefix (srfi :1) s1:)
          (prefix (srfi :115) s115:)
          (steinmetz options)
          )

  ;; Pre-process *tokens* to remove option clusters and run-in
  ;; arguments.  According to my reading of POSIX, a cluster (like
  ;; -abc) must be handled in one of two ways, depending on whether the
  ;; first option of the cluster takes an argument or not.
  ;;
  ;; * If it does, then the cluster is split into an option followed
  ;;   by its argument.  For example, if -a takes an argument, then
  ;;   "-abc" should be split into "-a" and "bc".
  ;;
  ;; * If it doesn't, then the first option is split off and the rest
  ;;   of the cluster is examined (as if it had its own leading dash).
  ;;   e.g. in the case of "-abc", if -a doesn't take an argument,
  ;;   then the "-a" token is added to the final command line and the
  ;;   "-bc" cluster is examined recursively.
  ;;
  ;; (If we were to support optional arguments, the situation with
  ;; clusters would be completely ambiguous.)
  (define (normalize-command-line opt-tab tokens)
    (letrec*
     ((cluster
       (s115:regexp '(: "-" (submatch (at-least 2 alphanumeric)))))
      (long-option/equals
       (s115:regexp
        '(: (submatch (: "--" alphanumeric (+ (or alphanumeric #\-))))
            #\=
            (submatch (+ alphanumeric)))))

      (split-cluster
       (lambda (chars opts)
         (if (null? chars)
             (reverse opts)
             (let* ((first (car chars))
                    (rest (cdr chars))
                    (opt (hashtable-ref opt-tab (string first) #f)))
               ;; If the first char denotes an option with no argument,
               ;; then add it to the opts list and continue examining
               ;; the rest of the cluster.  Otherwise, consider the rest
               ;; of the cluster an argument (even if *opt* doesn't
               ;; exist).
               (if (and opt (not (option-argument-name opt)))
                   (split-cluster rest (cons (string #\- first) opts))
                   (reverse (s1:cons* (apply string rest)
                                      (string #\- first)
                                      opts)))))))

      (process-token
       (lambda (token)
         (assert (string? token))
         (cond ((s115:regexp-matches cluster token) =>
                (lambda (m)
                  (split-cluster
                   (string->list (s115:regexp-match-submatch m 1))
                   '())))
               ((s115:regexp-matches long-option/equals token) =>
                (lambda (m)
                  (list (s115:regexp-match-submatch m 1)
                        (s115:regexp-match-submatch m 2))))
               (else (list token))))))

      (assert (hashtable? opt-tab))
      (assert (list? tokens))
      (s1:append-map process-token tokens)))
  )
