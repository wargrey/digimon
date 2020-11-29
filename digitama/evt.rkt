#lang typed/racket/base

(provide (all-defined-out))

(define-type EvtSelf (Rec Evt (Evtof Evt)))
(define-type Place-EvtExit (Evtof (Pairof Place Integer)))
(define-type Timer-EvtSelf (Rec Timer-Evt (Evtof (Vector Timer-Evt Fixnum Fixnum))))
