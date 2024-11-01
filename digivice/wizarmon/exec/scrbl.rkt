#lang typed/racket/base

(provide (all-defined-out))

(require "../parameter.rkt")

(require "../../wisemon/phony/typeset.rkt")
(require (only-in "../../wisemon/parameter.rkt"
                  current-make-real-targets
                  current-make-phony-goal))

(require "../../../filesystem.rkt")

(require "../../../digitama/exec.rkt")
(require "../../../digitama/system.rkt")
(require "../../../digitama/collection.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-typeset : (-> Path Symbol Any)
  (lambda [path.scrbl lang-name]
    (define maybe-info : (Option Pkg-Info)
      (single-collection-info #:bootstrap? #true
                              (or (path-only path.scrbl)
                                  (current-directory))))

    (parameterize ([current-make-phony-goal 'exec]
                   [current-make-real-targets (list path.scrbl)]
                   [current-digimon (if maybe-info (pkg-info-name maybe-info) (current-digimon))]
                   [current-directory (if maybe-info (pkg-info-zone maybe-info) (assert (path-only path.scrbl)))])
      (define all-typesettings : (Listof Tex-Info)
        (if (not maybe-info)
            (make-typeset-prepare "" #false)
            (make-typeset-prepare (pkg-info-name maybe-info)
                                  (pkg-info-ref maybe-info))))

      (when (pair? all-typesettings)
        (define-values (always-files ignored-files specs targets)
          (make-typeset-specs+targets the-name all-typesettings (wizarmon-verbose)))
      
        (make-typeset specs always-files ignored-files targets (wizarmon-remake))
        (fg-recon-open-file 'exec (car targets))))))
