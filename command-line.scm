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
           (cond ((irregex-match short-option-cluster s)
                  (cluster->strings s))
                 ((irregex-match long-option/equals s) =>
                  (lambda (m)
                    (list (irregex-match-substring m 1)    ; option
                          (irregex-match-substring m 2)))) ; argument
                 (else (list s))))))
    (append-map maybe-split lis)))

(define short-option-cluster
  (sre->irregex '(: #\- alphabetic (+ alphabetic))))

(define long-option/equals
  (sre->irregex '(: (submatch (: "--" (+ (or alphabetic #\-))))
                    #\=
                    (submatch (+ graphic)))))

;; String -> (list String)
;; Break up a cluster of short options.
(define (cluster->strings s)
  (map (lambda (c) (string #\- c))
       (cdr (string->list s))))
