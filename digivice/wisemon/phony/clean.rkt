#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/symbol)

(require "dist.rkt")

(require "../parameter.rkt")
(require "../phony.rkt")
(require "../path.rkt")
(require "../spec.rkt")

(require "../../../digitama/exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make~clean : Make-Info-Phony
  (lambda [digimon info-ref]
    (define submakes : (Listof Path) (filter file-exists? (list (build-path (current-directory) "submake.rkt"))))
    (define px.compiled : PRegexp (pregexp (format "/~a(?![^/])/?" (car (use-compiled-file-paths)))))
    (define px.compiled/mostly : PRegexp (pregexp (format "/~a(?![^/])/.+[.](dep|zo)" (car (use-compiled-file-paths)))))
    (define clean-phony : Symbol (current-make-phony-goal))

    (make-do-clean 
     (if (eq? clean-phony 'mostlyclean)
         (list (let ([compiled (find-digimon-files (λ [[file : Path]] (regexp-match? px.compiled/mostly file)) (current-directory) #:search-compiled? #true)])
                 (wisemon-path->clean-spec (reverse #| ensure directories second |# compiled) clean-phony)))
         (cons (let ([compiled (find-digimon-files (λ [[file : Path]] (regexp-match? px.compiled file)) (current-directory) #:search-compiled? #true)])
                 (wisemon-path->clean-spec (reverse #| ensure directories second |# compiled) clean-phony))
               (for/list : Wisemon-Specification ([spec (in-list (make-implicit-dist-specs info-ref))]
                                                  #:when (wisemon-spec-target-exists? spec))
                 (wisemon-spec->clean-spec spec clean-phony)))))
    
    (when (memq clean-phony '[distclean maintainer-clean])
      (for ([submake (in-list submakes)])
        (define clbpath `(submod ,submake make:files clobber))
        (when (module-declared? clbpath #true)
          (dynamic-require clbpath #false))))
    
    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake make:files))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        (define cleans : (Option (Listof String))
          (member (string-replace (symbol->immutable-string clean-phony) #px"(?<!^)-?clean" "")
                  '["maintainer" "dist" "clean" "mostly"]))
        (when (pair? cleans)
          (define px.filter : PRegexp (pregexp (string-join cleans "|" #:before-first "^(.+?:)?" #:after-last ":.+:")))
          (define ns : Namespace (module->namespace modpath))
          (define clean-specs : Wisemon-Specification
            (for/fold ([clean-specs : Wisemon-Specification null])
                      ([var (in-list (namespace-mapped-symbols ns))]
                       #:when (regexp-match? px.filter (symbol->immutable-string var)))
              (define maybe-spec (namespace-variable-value var #false (λ [] #false) ns))
              (cond [(wisemon-spec? maybe-spec) (append clean-specs (list maybe-spec))]
                    [(list? maybe-spec) (append clean-specs (filter wisemon-spec? maybe-spec))]
                    [else clean-specs])))
          (make-do-clean (for/list ([spec (in-list clean-specs)])
                          (wisemon-spec->clean-spec spec clean-phony))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-do-clean : (->* (Wisemon-Specification) (Symbol) Void)
  (lambda [clean-specs [operation 'clean]]
    (for ([path (in-list (wisemon-targets-flatten clean-specs))])
      (fg-recon-rm operation path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mostlyclean-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'mostlyclean      #:phony make~clean #:desc "Delete all except that can be however hard to be remade."))

(define clean-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'clean            #:phony make~clean #:desc "Delete all except that record the configuration."))

(define distclean-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'distclean        #:phony make~clean #:desc "Delete all that are excluded in the distribution."))

(define maintainer-clean-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'maintainer-clean #:phony make~clean #:desc "Delete all that can be remade. [For Maintainers]"))
