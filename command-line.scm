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
           (cond ((regexp-matches? short-option-cluster s)
                  (cluster->strings s))
                 ((regexp-matches long-option/equals s) =>
                  (lambda (m)
                    (list (regexp-match-submatch m 1)    ; option
                          (regexp-match-submatch m 2)))) ; argument
                 (else (list s))))))
    (append-map maybe-split lis)))

(define short-option-cluster
  (regexp '(: #\- alphabetic (+ alphabetic))))

(define long-option/equals
  (regexp '(: (submatch (: "--" (+ (or alphabetic #\-))))
              #\=
              (submatch (+ graphic)))))

;; String -> (list String)
;; Break up a cluster of short options.
(define (cluster->strings s)
  (map (lambda (c) (string #\- c))
       (cdr (string->list s))))
