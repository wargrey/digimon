#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require "dist.rkt")

(require "../parameter.rkt")
(require "../phony.rkt")
(require "../path.rkt")
(require "../rule.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make~clean : Make-Phony
  (lambda [digimon info-ref]
    (define submakes : (Listof Path) (filter file-exists? (list (build-path (current-directory) "submake.rkt"))))
    (define px.compiled : PRegexp (pregexp (format "/~a(?![^/])/?" (car (use-compiled-file-paths)))))

    (when (memq (current-make-phony-goal) '[distclean maintainer-clean])
      (for ([submake (in-list submakes)])
        (define clbpath `(submod ,submake make:files clobber))
        (when (module-declared? clbpath #true)
          (dynamic-require clbpath #false))))

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake make:files))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        (parameterize ([current-namespace (module->namespace modpath)])
          (define cleans : (Option (Listof String))
            (let ([maybe-phony (current-make-phony-goal)])
              (and maybe-phony
                   (member (string-replace (symbol->string maybe-phony) #px"(?<!^)-?clean" "")
                           '["maintainer" "dist" "clean" "mostly"]))))
          (when (list? cleans)
            (define px.filter : PRegexp (pregexp (string-join cleans "|" #:before-first "^(.+?:)?" #:after-last ":.+:")))
            (for ([var (in-list (namespace-mapped-symbols))]
                  #:when (regexp-match? px.filter (symbol->string var)))
              (define maybe-dirties (namespace-variable-value var #false (λ [] #false)))
              (cond [(not (list? maybe-dirties)) (do-clean maybe-dirties)]
                    [else (for ([maybe-dirty (in-list maybe-dirties)])
                            (do-clean (if (list? maybe-dirty) (car maybe-dirty) maybe-dirty)))]))))))
    
    (for-each do-clean (map (λ [[r : Wisemon-Rule]] : Path (car r)) (make-implicit-dist-rules info-ref)))
    (for-each do-clean (reverse (find-digimon-files (λ [[file : Path]] : Boolean (regexp-match? px.compiled file))
                                                    (current-directory) #:search-compiled? #true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define do-clean : (-> Any Void)
  (lambda [dirty]
    (when (path? dirty)
      (cond [(file-exists? dirty) (delete-file dirty)]
            [(directory-exists? dirty) (delete-directory dirty)])
      (printf "make: deleted ~a~n" (simplify-path dirty)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(wisemon-register-phony 'mostlyclean      make~clean "Delete all except that can be however hard to be remade.")
(wisemon-register-phony 'clean            make~clean "Delete all except that record the configuration.")
(wisemon-register-phony 'distclean        make~clean "Delete all that are excluded in the distribution.")
(wisemon-register-phony 'maintainer-clean make~clean "Delete all that can be remade. [For Maintainers]")
