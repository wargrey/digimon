#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Continuation-Stack (Pairof Symbol (Option (Vector (U String Symbol) Integer Integer))))

(define continuation-mark->stacks : (->* () ((U Continuation-Mark-Set Thread exn)) (Listof Continuation-Stack))
  (lambda [[cm (current-continuation-marks)]]
    ((inst map (Pairof Symbol (Option (Vector (U String Symbol) Integer Integer))) (Pairof (Option Symbol) Any))
     (λ [[stack : (Pairof (Option Symbol) Any)]]
       (define maybe-name (car stack))
       (define maybe-srcinfo (cdr stack))
       (cons (or maybe-name 'λ)
             (and (srcloc? maybe-srcinfo)
                  (let ([src (srcloc-source maybe-srcinfo)]
                        [line (srcloc-line maybe-srcinfo)]
                        [column (srcloc-column maybe-srcinfo)])
                    (vector (if (symbol? src) src (~a src))
                            (or line -1)
                            (or column -1))))))
     (cond [(continuation-mark-set? cm) (continuation-mark-set->context cm)]
           [(exn? cm) (continuation-mark-set->context (exn-continuation-marks cm))]
           [else (continuation-mark-set->context (continuation-marks cm))]))))
