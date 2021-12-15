#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "../../../cc.rkt")

(require "../parameter.rkt")
(require "../phony.rkt")
(require "../spec.rkt")
(require "../racket.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CC-Launcher-Info (List (Option Symbol) (Option String) (Listof Symbol)))
(define-type CC-Launcher-Name (Pairof Path CC-Launcher-Info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-digimon-native-launcher-names : (-> Info-Ref (Listof CC-Launcher-Name))
  (lambda [info-ref]
    (define maybe-launchers (info-ref 'native-launcher-names (λ [] null)))

    (unless (list? maybe-launchers)
      (raise-user-error 'info.rkt "malformed `native-launcher-names`: ~a" maybe-launchers))

    (filter-map cc-launcher-filter maybe-launchers)))

(define make-cc-specs : (-> Info-Ref Wisemon-Specification)
  (lambda [info-ref]
    (for/fold ([specs : Wisemon-Specification null])
              ([native (in-list (find-digimon-native-launcher-names info-ref))])

      (displayln native)
      (append specs))))
    
(define make~cc : Make-Phony
  (lambda [digimon info-ref]
    (wisemon-compile (current-directory) digimon info-ref)
    (wisemon-make (make-cc-specs info-ref) (current-make-real-targets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-launcher-filter : (-> Any (Option CC-Launcher-Name))
  (lambda [native]
    (if (and (pair? native) (path-string? (car native)))
        (let ([native.cc (build-path (current-directory) (car native))])
          (and (file-exists? native.cc)
               (cons native.cc (cc-filter-name (cdr native)))))
        (raise-user-error 'info.rkt "malformed `native-launcher-names`: ~a" native))))

(define cc-filter-name : (-> Any CC-Launcher-Info)
  (lambda [argv]
    (let partition ([lang : (Option Symbol) #false]
                    [name : (Option String) #false]
                    [srehto : (Listof Symbol) null]
                    [options : (Listof Any) (if (list? argv) argv (list argv))])
      (cond [(null? options) (list lang name (reverse srehto))]
            [else (let-values ([(opt rest) (values (car options) (cdr options))])
                    (cond [(memq opt '(C C++)) (partition (or lang opt) name srehto rest)]
                          [(string? opt) (partition lang (or name opt) srehto rest)]
                          [(symbol? opt) (partition lang name (cons opt srehto) rest)]
                          [else (partition lang name srehto rest)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cc-phony-goal : Wisemon-Phony
  (wisemon-make-phony #:name 'cc #:phony make~cc
                      #:desc "Build the collection as a C/C++ project preprocessed by Racket"))
