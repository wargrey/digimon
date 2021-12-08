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
    (append (make-c-library-specs info-ref)
            (make-cpp-library-specs info-ref))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-c-library-specs : (-> Info-Ref Wisemon-Specification)
  (lambda [info-ref]
    (define cs : (Listof Path) (find-digimon-files (λ [[file : Path]] (regexp-match? #px"\\.c$" file)) (current-directory)))
    (cond [(null? cs) null]
          [else (let ([stone-dir (path->string (digimon-path 'stone))])
                  (for/fold ([specs : Wisemon-Specification null])
                            ([c (in-list cs)])
                    (define contained-in-package?  : Boolean (string-prefix? (path->string c) stone-dir))
                    (define tobj : Path (assert (c-object-destination c contained-in-package?) path?))
                    (define target : Path (assert (c-library-destination c contained-in-package?) path?))
                    (list* (wisemon-spec tobj #:^ (c-include-headers c) #:- (c-compile c tobj #:cpp? #false))
                           (wisemon-spec target #:^ (list tobj) #:- (c-link tobj target #:cpp? #false #:modelines (c-source-modelines c)))
                           specs)))])))

(define make-cpp-library-specs : (-> Info-Ref Wisemon-Specification)
  (lambda [info-ref]
    (define cpps : (Listof Path) (find-digimon-files (λ [[file : Path]] (regexp-match? #px"\\.cpp$" file)) (current-directory)))
    (cond [(null? cpps) null]
          [else (let ([stone-dir (path->string (digimon-path 'stone))])
                  (for/fold ([specs : Wisemon-Specification null])
                            ([cpp (in-list cpps)])
                    (define contained-in-package?  : Boolean (string-prefix? (path->string cpp) stone-dir))
                    (define tobj : Path (assert (c-object-destination cpp contained-in-package?) path?))
                    (define target : Path (assert (c-library-destination cpp contained-in-package?) path?))
                    (list* (wisemon-spec tobj #:^ (c-include-headers cpp) #:- (c-compile cpp tobj #:cpp? #true))
                           (wisemon-spec target #:^ (list tobj) #:- (c-link tobj target #:cpp? #true #:modelines (c-source-modelines cpp)))
                           specs)))])))