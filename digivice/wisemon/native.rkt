#lang typed/racket/base

(provide c-compiler-filter λfalse)
(provide (all-from-out "../../cc.rkt"))
(provide (all-from-out "../../digitama/system.rkt"))
(provide (all-from-out "../../digitama/toolchain/cc/configuration.rkt"))

(require typed/setup/getinfo)

(require "../../cc.rkt")
(require "../../function.rkt")

(require "../../digitama/system.rkt")
(require "../../digitama/toolchain/cc/configuration.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-compiler-filter : (-> Info-Ref String Symbol (Listof Symbol))
  (lambda [info-ref toolchain-names native-type]
    (define id-suffix : Symbol (string->symbol (format toolchain-names native-type)))
    (define all-cc (info-ref id-suffix λfalse))
    (define sys-cc (info-ref (string->symbol (format "~a-~a" digimon-system id-suffix)) λfalse))
    
    (c-compilers-merge (c-compilers-merge null sys-cc)
                        all-cc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-compilers-merge : (-> (Listof Symbol) Any (Listof Symbol))
  (lambda [ccs opt]
    (append ccs
            (for/list : (Listof Symbol)
              ([cc (cond [(list? opt) (in-list opt)]
                         [(vector? opt) (in-vector opt)]
                         [else (in-value opt)])]
               #:when (and (symbol? cc) (not (memq cc ccs))))
              cc))))
