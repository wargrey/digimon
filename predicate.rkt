#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/minimal/regexp.rkt"))

(require "digitama/predicate.rkt")
(require "digitama/minimal/regexp.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define maybe? : (All (a) (case-> [Any (-> Any Boolean : a) -> Boolean : #:+ (Option a) #:- (! (Option a))]
                                  [Any (-> Any Boolean : #:+ a) -> Boolean : #:+ (Option a)]))
  (lambda [val ?]
    (or (not val)
        (? val))))

(define pairof? : (All (a b) (case-> [Any (-> Any Boolean : a) (-> Any Boolean : b) -> Boolean : #:+ (Pairof a b) #:- (! (Pairof a b))]
                                     [Any (-> Any Boolean : #:+ a) (-> Any Boolean : #:+ b) -> Boolean : #:+ (Pairof a b)]))
  (lambda [v car? cdr?]
    (and (pair? v)
         (car? (car v))
         (cdr? (cdr v)))))

(define #:forall (a b c) disjoin? : (case-> [Any (-> Any Boolean : a) (-> Any Boolean : b) -> Boolean : #:+ (U a b) #:- (! (U a b))]
                                            [Any (-> Any Boolean : #:+ a) (-> Any Boolean : #:+ b) -> Boolean : #:+ (U a b)]
                                            [Any (-> Any Boolean : a) (-> Any Boolean : b) (-> Any Boolean : c) -> Boolean : #:+ (U a b c) #:- (! (U a b c))]
                                            [Any (-> Any Boolean : #:+ a) (-> Any Boolean : #:+ b) (-> Any Boolean : #:+ c) -> Boolean : #:+ (U a b c)])
  (case-lambda
    [(v p1? p2?)     (or (p1? v) (p2? v))]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(define datum-filter : (All (a b) (case-> [Any (-> Any Boolean : #:+ a) -> (Option a)]
                                          [Any (-> Any Boolean : #:+ a) (-> Any Boolean : #:+ b) -> (U a b False)]))
  (case-lambda
    [(v ?) (and (? v) v)]
    [(v p1? p2?) (and (or (p1? v) (p2? v)) v)]))

(define datum-map : (All (a b c) (case-> [Any (-> Any Boolean : #:+ a) a (-> a c) -> c]
                                         [Any (-> Any Boolean : #:+ a) (-> Any Boolean : #:+ b) (U a b) (-> (U a b) c) -> c]))
  (case-lambda
    [(v ? fallback map) (map (if (? v) v fallback))]
    [(v p1? p2? fallback map) (map (if (or (p1? v) (p2? v)) v fallback))]))

(define datum-filter-map : (All (a b c) (case-> [Any (-> Any Boolean : #:+ a) (-> a c) -> (Option c)]
                                                [Any (-> Any Boolean : #:+ a) (-> Any Boolean : #:+ b) (-> (U a b) c) -> (Option c)]))
  (case-lambda
    [(v ? map) (and (? v) (map v))]
    [(v p1? p2? map) (and (or (p1? v) (p2? v)) (map v))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (a) make-maybe? : (case-> [(-> Any Boolean : a) -> (-> Any Boolean : (Option a))]
                                           [(-> Any Boolean : #:+ a) -> (-> Any Boolean : #:+ (Option a))])
  (lambda [?]
    (λ [val]
      (or (not val)
          (? val)))))

(define #:forall (a b) make-pairof? : (case-> [(-> Any Boolean : a) (-> Any Boolean : b) -> (-> Any Boolean : #:+ (Pairof a b) #:- (! (Pairof a b)))]
                                              [(-> Any Boolean : #:+ a) (-> Any Boolean : #:+ b) -> (-> Any Boolean : #:+ (Pairof a b))])
  (lambda [car? cdr?]
    (λ [val]
      (and (pair? val)
           (car? (car val))
           (cdr? (cdr val))))))

(define #:forall (a b c) make-disjoin? : (case-> [(-> Any Boolean : a) (-> Any Boolean : b) -> (-> Any Boolean : (U a b))]
                                                 [(-> Any Boolean : #:+ a) (-> Any Boolean : #:+ b) -> (-> Any Boolean : #:+ (U a b))]
                                                 [(-> Any Boolean : a) (-> Any Boolean : b) (-> Any Boolean : c) -> (-> Any Boolean : (U a b c))]
                                                 [(-> Any Boolean : #:+ a) (-> Any Boolean : #:+ b) (-> Any Boolean : #:+ c) -> (-> Any Boolean : #:+ (U a b c))])
  (case-lambda
    [(p1? p2?)     (λ [v] (or (p1? v) (p2? v)))]
    [(p1? p2? p3?) (λ [v] (or (p1? v) (p2? v) (p3? v)))]))

(define #:forall (T) make-listof? : (case-> [(-> Any Boolean : T) -> (-> Any Boolean : (Listof T))]
                                            [(-> Any Boolean) -> (-> Any Boolean)])
  (lambda [?]
    (λ [v] (listof? v ?))))

(define #:forall (T) make-listof+? : (case-> [(-> Any Boolean : T) -> (-> Any Boolean : (Pairof T (Listof T)))]
                                             [(-> Any Boolean) -> (-> Any Boolean)])
  (lambda [?]
    (λ [v] (listof+? v ?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define listof-zero? : (-> Any Boolean) (make-listof? real-zero?))
(define listof+zero? : (-> Any Boolean) (make-listof+? real-zero?))
