#lang typed/racket/base

(provide (all-defined-out))

(require "../../../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type C-Link-Library (U Symbol (Pairof Keyword (Listof Symbol))))

(struct c-link-config
  ([system : (Option Symbol)]
   [libraries : (Listof C-Link-Library)])
  #:type-name C-Link-Config
  #:transparent)

(struct c-path-config
  ([system : (Option Symbol)]
   [directories : (Listof Path)])
  #:type-name C-Path-Config
  #:transparent)

(struct c-libpath-config c-path-config () #:type-name C-Libpath-Config #:transparent)
(struct c-include-config c-path-config () #:type-name C-Include-Config #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define link-config-flatten : (-> (Listof (U C-Link-Library C-Link-Config)) Symbol (Listof C-Link-Library))
  (lambda [libs digimon-system]
    (for/fold ([libraries : (Listof C-Link-Library) null])
              ([config (in-list libs)])
      (cond [(not (c-link-config? config)) ; rarely reached
             (append libraries (list config))]
            [(let ([t (c-link-config-system config)]) (or (not t) (eq? t digimon-system)))
             (append libraries (c-link-config-libraries config))]
            [else libraries]))))

(define path-config-flatten : (-> (Listof (U Path-String C-Path-Config)) Symbol (Listof Path))
  (lambda [dirs digimon-system]
    (for/fold ([libpaths : (Listof Path) null])
              ([config (in-list dirs)])
      (cond [(not (c-path-config? config)) ; rarely reached
             (append libpaths (list (path-normalize/system config)))]
            [(let ([t (c-path-config-system config)]) (or (not t) (eq? t digimon-system)))
             (append libpaths (c-path-config-directories config))]
            [else libpaths]))))
