#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "../../../digitama/graphviz.rkt")
(require "../../../digitama/system.rkt")
(require "../../../dtrace.rkt")

(require "../parameter.rkt")
(require "../path.rkt")
(require "../spec.rkt")
(require "../phony.rkt")

(require/typed
 "../../../digitama/tamer.rkt"
 [handbook-metainfo (-> Path-String String (Values String String))])

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
      ;(define dot.gif (assert (gv-script-destination dot.gv #".gif" #true)))
      (define dot.xgv (assert (gv-script-destination dot.gv #".dot" #true)))
      
      (append specs
              (list (wisemon-spec (list dot.svg dot.pdf dot.png #;dot.gif dot.xgv) #:^ (list dot.gv) #:-
                                  (dot-note 'dot dot.gv)
                                  (gv-render dot.gv 'svg #:outfile dot.svg)
                                  (gv-render dot.gv 'pdf #:outfile dot.pdf)
                                  (gv-render dot.gv 'png #:outfile dot.png)
                                  ;(gv-render dot.gv 'gif #:outfile dot.gif)
                                  (gv-render dot.gv 'xdot #:outfile dot.xgv)))))))
    
(define make~dot : Make-Info-Phony
  (lambda [digimon info-ref]
    (wisemon-make (make-dot-specs info-ref) (current-make-real-targets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dot-note : (-> Symbol Path Void)
  (lambda [layout dot.gv]
    (dtrace-note "~a ~a: ~a" the-name layout dot.gv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dot-src-filter : (-> Path Boolean)
  (lambda [file]
    (regexp-match? #px"\\.(gv|dot)$" file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dot-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'dot #:phony make~dot #:desc "Render graphviz sources into SVG, PDF and PNG via dot"))
