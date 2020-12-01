#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Continuation-Stack (Pairof Symbol (Option (Vector (U String Symbol) Integer Integer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-continuation-prompt-control : (All (a b c) (->* () (Symbol)
                                                             (Values (-> (Option Symbol) (-> a) (-> b c) (U a c))
                                                                     (-> (-> b) Nothing))))
  (lambda [[default-prompt-name (gensym 'prompt)]]
    (define default-prompt : (Parameterof (Prompt-Tagof a (-> (-> b) c)))
      (make-parameter ((inst make-continuation-prompt-tag a (-> (-> b) c)) default-prompt-name)))

    (define call-with-prompt : (-> (Option Symbol) (-> a) (-> b c) (U a c))
      (lambda [tagname do-task handle]
        (define current-prompt : (Prompt-Tagof a (-> (-> b) c))
          (make-continuation-prompt-tag (or tagname default-prompt-name)))
        
        (parameterize ([default-prompt current-prompt])
          (call-with-continuation-prompt do-task current-prompt
            (λ [[at-collapse : (-> b)]] : c
              (handle (at-collapse)))))))
    
    (define abort : (-> (-> b) Nothing)
      (lambda [handlee]
        (abort-current-continuation (default-prompt)
                                    (λ _ (handlee)))))

    (values call-with-prompt abort)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define display-continuation-stacks : (->* () ((U Continuation-Mark-Set Thread exn) Output-Port) Void)
  (lambda [[errobj (current-continuation-marks)] [/dev/errout (current-error-port)]]
    (let display-stack ([stacks : (Listof Continuation-Stack) (continuation-mark->stacks errobj)]
                        [idx : Byte 0])
      (when (pair? stacks)
        (define stack (car stacks))
        (define maybe-location (cdr stack))
        (cond [(not maybe-location) (display-stack (cdr stacks) idx)]
              [else (when (> idx 0) (display #\newline /dev/errout))
          
                    (display "»»»» " /dev/errout)
                    (display (car stack) /dev/errout)
                    (display #\: /dev/errout)
                    (display #\space /dev/errout)
                    (display (vector-ref maybe-location 0) /dev/errout)
                    (display #\: /dev/errout)
                    (display (vector-ref maybe-location 1) /dev/errout)
                    (display #\: /dev/errout)
                    (display (vector-ref maybe-location 2) /dev/errout)

                    (display-stack (cdr stacks) 1)])))))
