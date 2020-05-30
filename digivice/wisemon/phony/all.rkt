#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/match)

(require typed/racket/unsafe)

(require "dist.rkt")

(require "../rule.rkt")
(require "../phony.rkt")
(require "../racket.rkt")
(require "../native.rkt")
(require "../parameter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make~all : Make-Phony
  (lambda [digimon info-ref]
    (define submakes (filter file-exists? (list (build-path (current-directory) "submake.rkt"))))

    (wisemon-make (make-native-library-rules info-ref))
    (wisemon-compile (current-directory) digimon info-ref)
    
    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake premake))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        ;;; the next two lines should useless but who knows
        (wisemon-make (make-native-library-rules info-ref))
        (wisemon-compile (current-directory) digimon info-ref)))

    (do-make (make-implicit-dist-rules info-ref))

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake make:files))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)
        (parameterize ([current-namespace (module->namespace modpath)])
          (unsafe-do-make (for/fold ([rules : Unsafe-Wisemon-Rules null])
                                    ([var (in-list (namespace-mapped-symbols))])
                            (match (namespace-variable-value var #false (λ _ #false))
                              [(list (? path-string? t) (list (? path-string? ds) ...) (? procedure? make))
                               (cond [(not (procedure-arity-includes? make 1)) rules]
                                     [else (list* (list t (filter path-string? ds) make) rules)])]
                              [_ rules]))))))

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake make:files make))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)))
    
    (make/proc (list (list (current-directory) null (λ [] (void '|I don't know how to make all these files|))))
               (current-make-real-targets))

    (for ([submake (in-list submakes)])
      (define modpath `(submod ,submake postmake))
      (when (module-declared? modpath #true)
        (dynamic-require modpath #false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define do-make : (-> Wisemon-Rules Void)
  (lambda [rules]
    (unless (null? rules)
      (let-values ([(imts exts) (partition (λ [[t : Path-String]] (assoc t rules)) (current-make-real-targets))])
        (wisemon-make rules (if (null? (current-make-real-targets)) (map (λ [[r : Wisemon-Rule]] (car r)) rules) imts))
        (current-make-real-targets exts)))))

(define unsafe-do-make : (-> Unsafe-Wisemon-Rules Void)
  (lambda [rules]
    (unless (null? rules)
      (let-values ([(imts exts) (partition (λ [[t : Path-String]] (assoc t rules)) (current-make-real-targets))])
        (unsafe-wisemon-make rules (if (null? (current-make-real-targets)) (map (λ [[r : Unsafe-Wisemon-Rule]] (car r)) rules) imts))
        (current-make-real-targets exts)))))
