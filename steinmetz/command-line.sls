;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz command-line)
  (export clean-command-line)
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
  ;;
  ;; To do all of this, the to-be-processed tokens are kept on a
  ;; stack.  'split-cluster' sometimes pushes sub-clusters back onto
  ;; this stack.
  (define (clean-command-line opt-tab tokens)
    (letrec*
     ((cluster (s115:regexp '(: (submatch #\- alphanumeric)
                                (submatch (+ alphanumeric)))))
      (long-option/equals
       (s115:regexp
        '(: (submatch (: "--" alphanumeric (+ (or alphanumeric #\-))))
            #\=
            (submatch (+ alphanumeric)))))
      (split-cluster
       (lambda (match s)
         (let* ((first (s115:regexp-match-submatch match 1))
                (rest (s115:regexp-match-submatch match 2))
                (name (substring first 1 2))
                (opt (hashtable-ref opt-tab name #f)))
           (cond ((not opt)  ; parser error in the making
                  (values (list s) '()))
                 ;; If *opt* takes an argument, then we are dealing
                 ;; with a run-in argument ("-OARG" syntax).
                 ((option-argument-name opt)
                  (values (list rest first) '()))
                 (else  ; first option of cluster; push the rest
                  ;; FIXME: Prepending a dash is rather silly.
                  (values (list first)
                          (list (string-append "-" rest))))))))
      ;; Returns two values: a list of processed tokens and a new
      ;; stack of tokens still to be processed.
      (process-token
       (lambda (ts)
         (let ((top (car ts)) (rest (cdr ts)))
           (cond ((s115:regexp-matches cluster top) =>
                  (lambda (m)
                    (let-values (((proc todo) (split-cluster m top)))
                      (values proc (append todo rest)))))
                 ((s115:regexp-matches long-option/equals top) =>
                  (lambda (m)
                    (values (list (s115:regexp-match-submatch m 2)
                                  (s115:regexp-match-submatch m 1))
                            rest)))
                 (else (values (list top) rest))))))
      (process-loop
        (lambda (processed ts)
          (if (null? ts)
              (reverse processed)
              (let-values (((proc todo) (process-token ts)))
                (process-loop (append proc processed) todo))))))

      (process-loop '() tokens)))
  )
