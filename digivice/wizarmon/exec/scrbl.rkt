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

(require "../../../digitama/tamer/selector.rkt")

(require "../../../cmdopt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cmdopt-string->chapter-index : (-> Symbol String (U Positive-Index Char))
  (lambda [option s]
    (if (= (string-length s) 1)
        (let ([idx (string-ref s 0)])
          (cond [(char<=? #\A idx #\Z) idx]
                [(char<=? #\a idx #\z) (char-upcase idx)]
                [else (cmdopt-string+>index option s)]))
        (cmdopt-string+>index option s))))

(define-cmdlet-option scrbl-flags #: Scrbl-Flags
  #:program 'scrbl
  #:args args

  #:usage-help "set and overload the offprint configuration"
  #:once-each
  [[(#\f flatten) "perform a granular offprinting"]]
  #:multi
  [[(chapter seq) #:=> cmdopt-string->chapter-index id #: Handbook-Chapter-Index  "build the part or chapter whose number is ~1"]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-typeset : (-> Path Symbol Any)
  (lambda [path.scrbl lang-name]
    (define-values (options λargv) (parse-scrbl-flags))

    (cond [(scrbl-flags-help? options) (display-scrbl-flags)]
          [(pair? (scrbl-flags-chapter options))
           (let ([selector (make-user-specified-selector (scrbl-flags-chapter options) (scrbl-flags-flatten options))])
             (parameterize ([current-user-specified-selector selector]
                            [current-user-request-no-volume? #true])
               (shell-typeset-do path.scrbl)))]
          [else (shell-typeset-do path.scrbl)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-typeset-do : (-> Path Any)
  (lambda [path.scrbl]
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

        (let try-open ([targets : (Listof Path) targets])
          (when (pair? targets)
            (if (regexp-match? #px"\\.pdf$" (car targets))
                (fg-recon-open-file 'exec (car targets))
                (try-open (cdr targets)))))))))
