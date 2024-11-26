#lang typed/racket/base

(provide (all-defined-out) current-digimon current-digivice current-free-zone)
(provide #%info digimon-waketime digimon-uptime digimon-partner digimon-system digimon-path digivice-path)

(require "digitama/system.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(void (print-boolean-long-form #true)
        
      ;;; Ignore DrRacket's convention. But don't change "compiled" since
      ;;; the compiler checks bytecodes in the core collection
      ;;; which have already been compiled into <path:compiled>.
      (use-compiled-file-paths (list (build-path "compiled"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define call-as-normal-termination : (-> (-> Any) [#:atinit (-> Any)] [#:atexit (-> Any)] Void)
  (lambda [#:atinit [atinit/0 void] main/0 #:atexit [atexit/0 void]]
    (define exit-racket : (-> Any AnyValues) (exit-handler))
    (define codes : (HashTable Symbol Byte) #hasheq((FATAL . 95) (ECONFIG . 96) (ENOSERVICE . 99) (EPERM . 100)))
    
    (define (terminate [status : Any]) : Any
      (parameterize ([exit-handler exit-racket])
        (cond [(exact-nonnegative-integer? status) (exit (min status 255))]
              [(hash-ref codes status (λ [] #false)) => exit]
              [else (exit 0)])))
    
    (parameterize ([exit-handler terminate])
      (exit (with-handlers ([exn? (λ [[e : exn]] (and (eprintf "~a~n" (exn-message e)) 'FATAL))]
                            [void (λ [e] (and (eprintf "(uncaught-exception-handler) => ~a~n" e) 'FATAL))])
              (dynamic-wind (λ [] (with-handlers ([exn? (λ [[e : exn]] (atexit/0) (raise e))]) (atinit/0)))
                            (λ [] (main/0))
                            (λ [] (atexit/0))))))))

(define immutable-guard : (-> Symbol (Any -> Nothing))
  (lambda [pname]
    (λ [pval] (error pname "Immutable Parameter: ~a" pval))))

(define car.eval : (->* (Any) (Namespace) Any)
  (lambda [sexp [ns (current-namespace)]]
    (call-with-values (λ [] (eval sexp ns))
                      (λ result (car result)))))

(define void.eval : (->* (Any) (Namespace) Void)
  (lambda [sexp [ns (current-namespace)]]
    (call-with-values (λ [] (eval sexp ns)) void)))
