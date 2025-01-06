#lang racket/base

(provide (all-defined-out))

(provide (rename-out [tamer-deftech tamer-defterm]))
(provide (rename-out [tamer-deftech handbook-deftech]))
(provide (rename-out [tamer-deftech handbook-defterm]))

(require scribble/core)
(require scribble/manual)

(require "texbook.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-deftech
  (lambda [#:key [key #false] #:normalize? [normalize? #true] #:origin [origin #false] #:abbr [abbr #false] . body]
    (define term
      (list ($tex:phantomsection)
            (apply deftech #:key key #:normalize? normalize? #:style? #true body)))

    (cond [(and origin abbr) (list term "(" (tamer-deftech abbr) "," ~ (tamer-deftech origin) ")")]
          [(or origin abbr) (list term "(" (tamer-deftech (or origin abbr)) ")")]
          [else term])))


(define tamer-elemtag*
  (lambda [tag #:style [style #false] #:type [type 'tamer] . body]
    (list ($tex:phantomsection)
          (make-target-element style body `(,type ,tag)))))

(define tamer-elemtag
  (lambda [tag #:style [style #false] #:type [type 'tamer] . body]
    (make-target-element style body `(,type ,tag))))

(define tamer-elemref
  (lambda [tag #:style [style #false] #:type [type 'tamer] . body]
    (make-link-element style body `(,type ,tag))))
