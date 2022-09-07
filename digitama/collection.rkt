#lang typed/racket/base

(provide (all-defined-out))

(require typed/setup/getinfo)

(require "../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Pkg-Info pkg-info)

(struct pkg-info
  ([ref : Info-Ref]
   [zone : Path-String]
   [name : String])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define single-collection-info : (->* () (Path-String #:namespace (Option Namespace) #:bootstrap? Any) (Values (Option Pkg-Info)))
  (lambda [[dir (current-directory)] #:namespace [namespace #false] #:bootstrap? [bootstrap? #false]]
    (define zone : (Option Path-String) (collection-root dir))
    (and zone
         (let ([info-ref (get-info/full zone #:namespace namespace #:bootstrap? bootstrap?)])
           (and info-ref
                (let ([collection (info-ref 'collection)])
                  (and collection
                       (not (eq? collection 'multi))
                       (pkg-info info-ref
                                 zone
                                 (cond [(string? collection) collection]
                                       [else (dirname zone)])))))))))
    
(define collection-info : (->* () (Path-String #:namespace (Option Namespace) #:bootstrap? Any)
                               (U False Pkg-Info (Pairof Info-Ref (Listof Pkg-Info))))
  (lambda [[dir (current-directory)] #:namespace [namespace #false] #:bootstrap? [bootstrap? #false]]
    (define zone : (Option Path-String) (collection-root dir))
    (and zone
         (let ([info-ref (get-info/full zone #:namespace namespace #:bootstrap? bootstrap?)])
           (and info-ref
                (let ([collection (info-ref 'collection)])
                  (cond [(string? collection) (pkg-info info-ref zone collection)]
                        [(eq? collection 'use-pkg-name) (pkg-info info-ref zone (dirname zone))]
                        [else (let subcollection-info ([subdirs : (Listof Path) (directory-list zone #:build? #true)]
                                                       [subinfos : (Listof pkg-info) null])
                                (cond [(null? subdirs) (cons info-ref (reverse subinfos))]
                                      [else (let ([subinfo (single-collection-info (car subdirs) #:namespace namespace #:bootstrap? bootstrap?)])
                                              (subcollection-info (cdr subdirs)
                                                                  (cond [(and subinfo) (cons subinfo subinfos)]
                                                                        [else subinfos])))]))])))))))

(define collection-root : (->* () (Path-String) (Option Path-String))
  (lambda [[dir (current-directory)]]
    (define info-ref (get-info/full dir))
    (cond [(and info-ref) dir]
          [else (let-values ([(base name dir?) (split-path (simple-form-path dir))])
                  (and (path? base) (collection-root base)))])))
