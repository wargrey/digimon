#lang typed/racket/base

(provide (all-defined-out))

(require "../../../digitama/diagram/d2.rkt")
(require "../../../digitama/system.rkt")

(require "../parameter.rkt")
(require "../path.rkt")
(require "../spec.rkt")
(require "../phony.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-d2-specs : (-> Info-Ref Wisemon-Specification)
  (lambda [info-ref]
    (define local-rootdir : Path (digimon-path 'zone))
    
    (for/fold ([specs : Wisemon-Specification null])
              ([src.d2 (in-list (find-digimon-files d2-src-filter local-rootdir))])
      (define deps : (Listof Path) (filter file-exists? (d2-smart-dependencies src.d2)))
      (define d2.svg (assert (d2-script-destination src.d2 #".svg" #true)))
      (define d2.pdf (assert (d2-script-destination src.d2 #".pdf" #true)))
      (define d2.gif (assert (d2-script-destination src.d2 #".gif" #true)))
      
      (append specs
              (list (wisemon-spec d2.svg #:^ deps #:- (d2-render src.d2 d2.svg #:debug? (make-verbose)))
                    (wisemon-spec d2.pdf #:^ deps #:- (d2-render src.d2 d2.pdf #:debug? (make-verbose)))
                    (wisemon-spec d2.gif #:^ deps #:- (d2-render src.d2 d2.gif #:debug? (make-verbose))))))))
    
(define make~d2 : Make-Info-Phony
  (lambda [digimon info-ref]
    (wisemon-make (make-d2-specs info-ref) (current-make-real-targets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define d2-src-filter : (-> Path Boolean)
  (lambda [file]
    (regexp-match? #px"\\.d2$" file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define d2-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'd2 #:phony make~d2 #:desc "Render D2 sources into SVG, PDF, and GIF"))
