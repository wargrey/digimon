#lang racket/base

(provide (all-defined-out))

(require file/convertible)

(require "../graphviz.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dot (script layout size minsize?)
  #:constructor-name unsafe-dot
  #:transparent
  #:property prop:convertible
  (λ [self mime fallback]
    (cond [(not (dot? self)) fallback]
          [else (let ([src (graphviz-dotin (dot-script self))]
                      [layout (or (dot-layout self) 'dot)]
                      [size (dot-size self)]
                      [min? (and (dot-minsize? self) #true)])
                  (case mime
                    [(svg-bytes) (gv-render src 'svg layout #:size size #:min-size? min?)]
                    [(pdf-bytes) (gv-render src 'pdf layout #:size size #:min-size? min?)]
                    [(png-bytes) (gv-render src 'png layout #:size size #:min-size? min?)]
                    [else fallback]))])))

(define graphviz
  (lambda [script #:layout [layout 'dot] #:size [size #false] #:min-size? [min-size? #false]]
    (unsafe-dot script layout size min-size?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphviz-dotin
  (lambda [script]
    (cond [(file-exists? script) script]
          [(path? script) script]
          [else (string->bytes/utf-8 script)])))
