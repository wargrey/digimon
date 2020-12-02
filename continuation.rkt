#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-for-syntax (prompt-type stx)
  (syntax-case stx []
    [(a b) (list #'a #'b)]
    [(a b c d ...) (raise-syntax-error 'define-continuation-prompt-control "too many range types" #'c #false (syntax->list #'(d ...)))]
    [(a) (list #'a #'a)]))

(define-syntax (define-continuation-prompt-control stx)
  (syntax-parse stx #:datum-literals []
    [(_ (call abort) (~optional name:id) #:-> [Type ...]
        (~alt (~optional (~seq #:with AbortInType (~optional (~seq #:default default-datum)) abort-map:expr) #:defaults ([abort-map #'values]))
              (~optional (~seq #:default-abort-callback default-abort-callback) #:defaults ([default-abort-callback #'void]))
              (~optional (~seq #:default-handler-map default-handler-map)))
        ...)
     (with-syntax* ([default-prompt-name (or (attribute name) (generate-temporary 'prompt))]
                    [handler-map (generate-temporary 'map)]
                    [ret-arg (generate-temporary 'arg)]
                    [(a b) (prompt-type #'(Type ...))]
                    [c (or (attribute AbortInType) #'b)]
                    [(Call [def-map ...] call-map)
                     (if (attribute default-handler-map)
                         (list #'(->* ((Option Symbol) (-> a)) ((-> b b)) (U a b)) (list #'[handler-map default-handler-map]) #'handler-map)
                         (list #'(-> (Option Symbol) (-> a) (U a b)) null #'values))]
                    [(Abort def-arg)
                     (if (attribute default-datum)
                         (list #'(->* () (c (-> b Any)) Nothing) #'[ret-arg default-datum])
                         (list #'(->* (c) ((-> b Any)) Nothing) #'ret-arg))])
       (syntax/loc stx
         (begin (define default-prompt : (Parameterof (Prompt-Tagof a (-> (-> b) b)))
                  (make-parameter ((inst make-continuation-prompt-tag a (-> (-> b) b)) 'default-prompt-name)))
                
                (define call : Call
                  (lambda [tagname do-task def-map ...]
                    (define current-prompt ((inst make-continuation-prompt-tag a (-> (-> b) b)) (or tagname 'default-prompt-name)))
                    
                    (parameterize ([default-prompt current-prompt])
                      (call-with-continuation-prompt do-task current-prompt
                        (λ [[at-collapse : (-> b)]] : b
                          (call-map (at-collapse)))))))
                
                (define abort : Abort
                  (let ([abort-datum-transform : (-> c b) abort-map])
                    (lambda [def-arg [on-abort default-abort-callback]]
                      (let ([ret-datum (abort-datum-transform ret-arg)])
                        (abort-current-continuation (default-prompt)
                                                    (λ [] (on-abort ret-datum) ret-datum)))))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Continuation-Stack (Pairof Symbol (Option (Vector (U String Symbol) Integer Integer))))

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
