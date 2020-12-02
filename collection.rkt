#lang racket/base

(provide (all-defined-out))

(require syntax/location)

(require "digitama/collection.rkt")
(require "digitama/system.rkt")
(require "tongue.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (enter-digimon-zone! stx)
  (syntax-case stx []
    [(_)
     (syntax/loc stx
       (let ([maybe-info (single-collection-info (quote-source-file))])
         (when (pkg-info? maybe-info) ; always true
           (current-digimon (pkg-info-name maybe-info))
           (current-free-zone (pkg-info-zone maybe-info))
           (default-tongue-paths (list (digimon-path 'tongue))))))]))
