#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require typed/setup/getinfo)

(require "path.rkt")
(require "spec.rkt")

(require "../../cc.rkt")
(require "../../digitama/system.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-native-library-specs : (-> Info-Ref Wisemon-Specification)
  (lambda [info-ref]
    (define cs : (Listof Path) (find-digimon-files (λ [[file : Path]] (regexp-match? #px"\\.c$" file)) (current-directory)))
    (cond [(null? cs) null]
          [else (let ([stone-dir (path->string (digimon-path 'stone))])
                  (for/fold ([specs : Wisemon-Specification null])
                            ([c (in-list cs)])
                    (define contained-in-package?  : Boolean (string-prefix? (path->string c) stone-dir))
                    (define tobj : Path (assert (c-object-destination c contained-in-package?) path?))
                    (define target : Path (assert (c-library-destination c contained-in-package?) path?))
                    (list* (wisemon-spec tobj #:^ (c-include-headers c) #:- (c-compile c tobj))
                           (wisemon-spec target #:^ (list tobj) #:- (c-link tobj target #:modelines (c-source-modelines c)))
                           specs)))])))
