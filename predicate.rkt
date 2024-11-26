#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/predicate.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define maybe? : (All (a) (-> Any (-> Any Boolean : #:+ a) Boolean : #:+ (Option a)))
  (lambda [val ?]
    (or (not val)
        (? val))))

(define disjoin? : (All (a b c) (case-> [Any (-> Any Boolean : a) (-> Any Boolean : b) -> Boolean : #:+ (U a b) #:- (! (U a b))]
                                        [Any (-> Any Boolean : a) (-> Any Boolean : b) (-> Any Boolean : c) -> Boolean : #:+ (U a b c) #:- (! (U a b c))]))
  (case-lambda
    [(v p1? p2?) (or (p1? v) (p2? v))]
    [(v p1? p2? p3?) (or (p1? v) (p2? v) (p3? v))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define listof? : (All (T) (case-> [Any (-> Any Boolean : T) -> Boolean : #:+ (Listof T) #:- (! (Listof T))]
                                   [Any (-> Any Boolean) -> Boolean]))
  (lambda [v ?]
    (and (list? v)
         (andmap ? v))))

(define listof+? : (All (T) (case-> [Any (-> Any Boolean : T) -> Boolean : #:+ (Pairof T (Listof T)) #:- (! (Pairof T (Listof T)))]
                                    [Any (-> Any Boolean) -> Boolean]))
  (lambda [v ?]
    (and (list? v)
         (pair? v)
         (andmap ? v))))

(define make-listof? : (All (T) (case-> [(-> Any Boolean : T) -> (-> Any Boolean : #:+ (Listof T) #:- (! (Listof T)))]
                                        [(-> Any Boolean) -> (-> Any Boolean)]))
  (lambda [?]
    (λ [v] (listof? v ?))))

(define make-listof+? : (All (T) (case-> [(-> Any Boolean : T) -> (-> Any Boolean : #:+ (Pairof T (Listof T)) #:- (! (Pairof T (Listof T))))]
                                         [(-> Any Boolean) -> (-> Any Boolean)]))
  (lambda [?]
    (λ [v] (listof+? v ?))))

(define listof-zero? : (-> Any Boolean) (make-listof? real-zero?))
(define listof+zero? : (-> Any Boolean) (make-listof+? real-zero?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bs-regexp? : (-> Any Boolean : (U Regexp Byte-Regexp))
  (lambda [v]
    (or (byte-regexp? v)
        (regexp? v))))

(define string-like? : (-> Any Boolean : (U String Symbol))
  (lambda [v]
    (or (string? v)
        (symbol? v))))

(define string-null? : (-> Any Boolean : #:+ String)
  (lambda [str]
    (and (string? str)
         (string=? str ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read:+? : (All (a) (-> Any (-> Any Boolean : #:+ a) [#:from-string Boolean] a))
  (lambda [src type? #:from-string [? #true]]
    (define v : Any
      (cond [(and ? (string? src)) (read (open-input-string src))]
            [(and ? (bytes? src)) (read (open-input-bytes src))]
            [(or (path? src) (path-string? src)) (call-with-input-file src read)]
            [(input-port? src) (read src)]
            [else src]))
    (cond [(type? v) v]
          [(not (eof-object? v)) (raise-result-error 'read:+? (format "~a" (object-name type?)) v)]
          [else (raise (make-exn:fail:read:eof (format "read:+?: ~a: unexpected <eof>" (object-name type?))
                                               (current-continuation-marks)
                                               null))])))

(define hash-ref:+? : (All (a b) (->* (HashTableTop Any (-> Any Boolean : #:+ a)) ((-> b)) (U a b)))
  (lambda [src key type? [defval #false]]
    (define v : Any (if defval (hash-ref src key defval) (hash-ref src key)))
    (cond [(type? v) v]
          [(not defval) (raise-result-error 'hash-ref:+? (format "~a" (object-name type?)) v)]
          [else (defval)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define datum-filter : (All (a b) (case-> [Any (-> Any Boolean : a) -> (Option a)]
                                          [Any (-> Any Boolean : a) (-> Any Boolean : b) -> (U a b False)]))
  (case-lambda
    [(v ?) (and (? v) v)]
    [(v p1? p2?) (and (or (p1? v) (p2? v)) v)]))

(define datum-map : (All (a b c) (case-> [Any (-> Any Boolean : a) a (-> a c) -> c]
                                         [Any (-> Any Boolean : a) (-> Any Boolean : b) (U a b) (-> (U a b) c) -> c]))
  (case-lambda
    [(v ? fallback map) (map (if (? v) v fallback))]
    [(v p1? p2? fallback map) (map (if (or (p1? v) (p2? v)) v fallback))]))

(define datum-filter-map : (All (a b c) (case-> [Any (-> Any Boolean : a) (-> a c) -> (Option c)]
                                                [Any (-> Any Boolean : a) (-> Any Boolean : b) (-> (U a b) c) -> (Option c)]))
  (case-lambda
    [(v ? map) (and (? v) (map v))]
    [(v p1? p2? map) (and (or (p1? v) (p2? v)) (map v))]))
