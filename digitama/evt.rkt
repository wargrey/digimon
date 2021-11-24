#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type EvtSelf (Rec Evt (Evtof Evt)))
(define-type Place-EvtExit (Evtof (Pairof Place Integer)))
(define-type Timer-EvtSelf (Rec Timer-Evt (Evtof (Vector Timer-Evt Nonnegative-Fixnum Nonnegative-Fixnum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define port-always-write-evt : (-> Bytes Natural Natural (Evtof Integer))
  (λ [bytes start end]
    (wrap-evt always-evt (λ [x] (- end start)))))

(define port-always-write-special-evt : (-> Any (Evtof Boolean))
  (λ [datum]
    (wrap-evt always-evt (λ [x] #true))))
