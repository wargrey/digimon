#lang typed/racket/base

(provide (all-defined-out))

(require "../../../digitama/graphviz.rkt")
(require "../../../digitama/system.rkt")
(require "../../../dtrace.rkt")

(require "../parameter.rkt")
(require "../path.rkt")
(require "../spec.rkt")
(require "../phony.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-dot-specs : (-> Info-Ref Wisemon-Specification)
  (lambda [info-ref]
    (define local-rootdir : Path (digimon-path 'zone))
    (define local-stone : Path (digimon-path 'stone))
    
    (for/fold ([specs : Wisemon-Specification null])
              ([dot.gv (in-list (find-digimon-files dot-src-filter local-rootdir))])
      (define dot.svg (assert (gv-script-destination dot.gv #".svg" #true)))
      (define dot.pdf (assert (gv-script-destination dot.gv #".pdf" #true)))
      (define dot.png (assert (gv-script-destination dot.gv #".png" #true)))
      
      (append specs
              (list (wisemon-spec dot.svg #:^ (list dot.gv) #:- (gv-render dot.gv 'svg #:outfile dot.svg))
                    (wisemon-spec dot.pdf #:^ (list dot.gv) #:- (gv-render dot.gv 'pdf #:outfile dot.pdf))
                    (wisemon-spec dot.png #:^ (list dot.gv) #:- (gv-render dot.gv 'png #:outfile dot.png)))))))
    
(define make~dot : Make-Info-Phony
  (lambda [digimon info-ref]
    (wisemon-make (make-dot-specs info-ref) (current-make-real-targets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dot-src-filter : (-> Path Boolean)
  (lambda [file]
    (regexp-match? #px"\\.(gv|dot)$" file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dot-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'dot #:phony make~dot #:desc "Render graphviz sources into SVG, PDF and PNG via dot"))
