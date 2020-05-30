#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require typed/setup/getinfo)

(require "path.rkt")
(require "rule.rkt")

(require "../../cc.rkt")
(require "../../digitama/system.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-native-library-rules : (-> Info-Ref Wisemon-Rules)
  (lambda [info-ref]
    (define cs : (Listof Path) (find-digimon-files (λ [[file : Path]] (regexp-match? #px"\\.c$" file)) (current-directory)))
    (cond [(null? cs) null]
          [else (let ([stone-dir (path->string (digimon-path 'stone))])
                  (for/fold ([rules : Wisemon-Rules null])
                            ([c (in-list cs)])
                    (define contained-in-package?  : Boolean (string-prefix? (path->string c) stone-dir))
                    (define tobj : Path (assert (c-object-destination c contained-in-package?) path?))
                    (define t : Path (assert (c-library-destination c contained-in-package?) path?))
                    (list* (list tobj (c-include-headers c) (λ [[target : Path]] (c-compile c target)))
                           (list t (list tobj) (λ [[target : Path]] (c-link tobj target #:modelines (c-source-modelines c))))
                           rules)))])))
