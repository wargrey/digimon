#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

(require "../parameter.rkt")

(require "../../../wisemon.rkt")
(require "../../../digitama/exec.rkt")
(require "../../../digitama/system.rkt")
(require "../../../digitama/diagram/d2.rkt")
(require "../../../digitama/collection.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-d2 : (-> Path Symbol Bytes Any)
  (lambda [src.d2 layout extension]
    (define maybe-info : (Option Pkg-Info)
      (single-collection-info #:bootstrap? #true
                              (or (path-only src.d2)
                                  (current-directory))))

    (parameterize ([current-digimon (if maybe-info (pkg-info-name maybe-info) (current-digimon))]
                   [current-directory (if maybe-info (pkg-info-zone maybe-info) (assert (path-only src.d2)))])
      (define d2.ext (assert (d2-script-destination src.d2 extension #true)))
      (define d2-spec
        (wisemon-spec d2.ext #:^ (filter file-exists? (d2-smart-dependencies src.d2)) #:-
                      (d2-render src.d2 d2.ext #:debug? (wizarmon-verbose))))

      (wisemon-make #:name the-name #:keep-going? #false #:always-run? (wizarmon-remake)
                    (list d2-spec) (list d2.ext))
      

      (if (>= (file-or-directory-modify-seconds d2.ext)
              (file-or-directory-modify-seconds src.d2))
          (fg-recon-open-file 'exec d2.ext)
          
          (let ([subdir (path-replace-extension d2.ext #"")])
            (fg-recon-open-file 'exec (build-path subdir (path-add-extension "index" extension))))))))
