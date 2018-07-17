#lang typed/racket/base

(provide (all-defined-out))

(require typed/setup/getinfo)

(require "../filesystem.rkt")

(struct pkg-info ([ref : Info-Ref] [zone : Path-String] [name : String]) #:transparent)

(define single-collection-info : (->* () (Path-String #:namespace (Option Namespace) #:bootstrap? Any) (Values (Option pkg-info)))
  (lambda [[dir (current-directory)] #:namespace [namespace #false] #:bootstrap? [bootstrap? #false]]
    (define zone : (Option Path-String) (collection-root dir))
    (define info-ref : (Option Info-Ref) (and zone (get-info/full zone #:namespace namespace #:bootstrap? bootstrap?)))
    (define collection : Any (and info-ref (info-ref 'collection)))
    (and zone
         info-ref
         collection
         (not (eq? collection 'multi))
         (pkg-info info-ref
                   zone
                   (cond [(string? collection) collection]
                         [else (dirname zone)])))))

(define collection-info : (->* () (Path-String #:namespace (Option Namespace) #:bootstrap? Any)
                               (U False pkg-info (Pairof Info-Ref (Listof pkg-info))))
  (lambda [[dir (current-directory)] #:namespace [namespace #false] #:bootstrap? [bootstrap? #false]]
    (define zone : (Option Path-String) (collection-root dir))
    (define info-ref : (Option Info-Ref) (and zone (get-info/full zone #:namespace namespace #:bootstrap? bootstrap?)))
    (and zone
         info-ref
         (let ([collection (and info-ref (info-ref 'collection))])
           (cond [(string? collection) (pkg-info info-ref zone collection)]
                 [(eq? collection 'use-pkg-name) (pkg-info info-ref zone (dirname zone))]
                 [else (let subcollection-info ([subdirs : (Listof Path) (directory-list zone #:build? #true)]
                                                [subinfos : (Listof pkg-info) null])
                         (cond [(null? subdirs) (cons info-ref (reverse subinfos))]
                               [else (let ([subinfo (single-collection-info (car subdirs) #:namespace namespace #:bootstrap? bootstrap?)])
                                       (subcollection-info (cdr subdirs)
                                                           (cond [(and subinfo) (cons subinfo subinfos)]
                                                                 [else subinfos])))]))])))))

(define collection-root : (->* () (Path-String) (Option Path-String))
  (lambda [[dir (current-directory)]]
    (define info-ref (get-info/full dir))
    (cond [(and info-ref) dir]
          [else (let-values ([(base name dir?) (split-path (simple-form-path dir))])
                  (and (path? base) (collection-root base)))])))
