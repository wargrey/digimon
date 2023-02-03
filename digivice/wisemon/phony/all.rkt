#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "dist.rkt")

(require "../spec.rkt")
(require "../phony.rkt")
(require "../racket.rkt")
(require "../native.rkt")
(require "../parameter.rkt")

(require "cc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make~all : Make-Info-Phony
  (lambda [digimon info-ref]
    (define submakes (filter file-exists? (list (build-path (current-directory) "submake.rkt"))))
    (define natives (map (inst car Path CC-Launcher-Info) (find-digimon-native-launcher-names info-ref)))

    (wisemon-make (make-native-library-specs info-ref natives) px.so)
    (wisemon-compile (current-directory) digimon info-ref)
    
    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake premake))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        ;;; the next two lines should useless but who knows
        (wisemon-make (make-native-library-specs info-ref natives) px.so)
        (wisemon-compile (current-directory) digimon info-ref)))

    (do-make (make-implicit-dist-specs info-ref))

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake make:files))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        (parameterize ([current-namespace (module->namespace modpath)])
          (do-make (for/fold ([specs : Wisemon-Specification null])
                             ([var (in-list (namespace-mapped-symbols))])
                     (define maybe-spec (namespace-variable-value var #false (λ _ #false)))
                     (cond [(list? maybe-spec) (append specs (filter wisemon-spec? maybe-spec))]
                           [(wisemon-spec? maybe-spec) (cons maybe-spec specs)]
                           [else specs]))))))

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake make:files make))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)))

    (wisemon-make null (current-make-real-targets))

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake postmake))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define do-make : (-> Wisemon-Specification Void)
  (lambda [specs]
    (unless (null? specs)
      (let-values ([(imts exts) (partition (λ [[t : Path-String]] (wisemon-spec-ref specs t)) (current-make-real-targets))])
        (wisemon-make specs (if (null? (current-make-real-targets)) (wisemon-targets-flatten specs) imts))
        (current-make-real-targets exts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define all-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'all #:phony make~all #:desc "Build the entire project without documentation [default]"))
