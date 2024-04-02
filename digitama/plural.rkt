#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plural : (-> Integer String String)
  (lambda [n word]
    (cond [(= n 1) word]
          [else (let ([size-1 (sub1 (string-length word))])
                  (cond [(= size-1 -1) word]
                        [(= size-1 0) (string-append word "s")]
                        [(char-ci=? (string-ref word size-1) #\y) (string-append (substring word 0 size-1) "ies")]
                        [else (string-append word "s")]))])))

(define singular : (-> String String)
  (lambda [words]
    (define size : Index (string-length words))
    (or (case size
          [(0 1) words]
          [(3) (and (string-ci=? words "ies") (substring words 0 (sub1 size)))]
          [else #false])
        (let ([slast (string-ref words (sub1 size))])
          (cond [(not (eq? slast #\s)) words]
                [(regexp-match? #px"ics$" words) words] ; for branch name of study or action
                [(regexp-match? #px"ies$" words) (string-append (substring words 0 (- size 3)) "y")]
                [else (substring words 0 (sub1 size))])))))
