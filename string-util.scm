;;;; Simplified, portable implementations of SRFI 152/130/13 procedures.

(define (string-index s pred)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (cond ((= i len) #f)
            ((pred (string-ref s i)) i)
            (else (loop (+ i 1)))))))

(define (string-skip s pred)
  (string-index s (lambda (c) (not (pred c)))))

(define (string-drop-while s pred)
  (cond ((string-skip s pred) =>
         (lambda (k) (substring s k (string-length s))))
        (else s)))
