#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out digimon/cmdopt))

(require digimon/cmdopt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cmdopt-string->chapter-index : (-> Symbol String (U Positive-Index Char))
  (lambda [option s]
    (if (= (string-length s) 1)
        (let ([idx (string-ref s 0)])
          (cond [(char<=? #\A idx #\Z) idx]
                [(char<=? #\a idx #\z) (char-upcase idx)]
                [else (cmdopt-string+>index option s)]))
        (cmdopt-string+>index option s))))
