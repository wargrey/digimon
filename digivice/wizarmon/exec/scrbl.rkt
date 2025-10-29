#lang typed/racket/base

(provide (all-defined-out))

(require digimon/filesystem)

(require digimon/digitama/exec)
(require digimon/digitama/system)
(require digimon/digitama/collection)

(require "../parameter.rkt")

(require "../../wisemon/phony/typeset.rkt")
(require "../../wisemon/parameter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-typeset : (-> Path Symbol Any)
  (lambda [path.scrbl lang-name]
    (define maybe-info : (Option Pkg-Info)
      (single-collection-info #:bootstrap? #true
                              (or (path-only path.scrbl)
                                  (current-directory))))

    (parameterize ([make-verbose (wizarmon-verbose)]
                   [current-make-phony-goal 'exec]
                   [current-make-real-targets (list path.scrbl)]
                   [current-digimon (if maybe-info (pkg-info-name maybe-info) (current-digimon))]
                   [current-directory (if maybe-info (pkg-info-zone maybe-info) (assert (path-only path.scrbl)))])
      (define all-typesettings : (Listof Tex-Info)
        (if (not maybe-info)
            (make-typeset-prepare "" #false)
            (make-typeset-prepare (pkg-info-name maybe-info)
                                  (pkg-info-ref maybe-info))))

      (when (pair? all-typesettings)
        (define targets (make-typeset all-typesettings (wizarmon-remake)))

        (let try-open ([targets : (Listof Path) targets])
          (when (pair? targets)
            (if (regexp-match? #px"\\.pdf$" (car targets))
                (fg-recon-open-file 'exec (car targets))
                (try-open (cdr targets)))))))))
