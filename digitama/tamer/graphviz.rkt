#lang racket/base

(provide (all-defined-out))

(require file/convertible)

(require "../graphviz.rkt")
(require "../../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dot (script layout size minsize? log-level)
  #:constructor-name unsafe-dot
  #:transparent
  #:property prop:convertible
  (λ [self mime fallback]
    (cond [(not (dot? self)) fallback]
          [else (let ([src (dot-script self)]
                      [layout (or (dot-layout self) 'dot)]
                      [size (dot-size self)]
                      [min? (and (dot-minsize? self) #true)]
                      [level (dot-log-level self)])
                  (case mime
                    [(svg-bytes) (gv-render src 'svg layout #:size size #:min-size? min? #:stdin-log-level level)]
                    [(pdf-bytes) (gv-render src 'pdf layout #:size size #:min-size? min? #:stdin-log-level level)]
                    [(png-bytes) (gv-render src 'png layout #:size size #:min-size? min? #:stdin-log-level level)]
                    [(script-path src-path) (if (path-literal? src) src fallback)] ; For `scrbl.rkt`
                    [else fallback]))])))

(define graphviz
  (lambda [script #:layout [layout 'dot] #:size [size #false] #:min-size? [min-size? #false]]
    (unsafe-dot script layout size min-size? #false)))

(define digraph.gv
  (lambda [#:id [id #false] #:strict? [strict? #false] #:layout [layout 'dot]
           #:size [size #false] #:min-size? [min-size? #false]
           #:debug [level #false] . body]
    (unsafe-dot (graphviz-script 'digraph id strict? body)
                layout size min-size? level)))

(define graph.gv
  (lambda [#:id [id #false] #:strict? [strict? #false] #:layout [layout 'dot]
           #:size [size #false] #:min-size? [min-size? #false]
           #:debug [level #false] . body]
    (unsafe-dot (graphviz-script 'graph id strict? body)
                layout size min-size? level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphviz-script
  (lambda [type id strict? body]
    (define /dev/dotout (open-output-bytes '/dev/dotout))

    (when (and strict?)
      (write 'strict /dev/dotout)
      (write-char #\space /dev/dotout))

    (write type /dev/dotout)
    (write-char #\space /dev/dotout)

    (when (and id)
      (write id /dev/dotout)
      (write-char #\space /dev/dotout))

    (write-char #\{ /dev/dotout)
    (write-char #\newline /dev/dotout)

    (for/fold ([indent? #true])
              ([line (in-list body)])
      (unless (not indent?)
        (write-string "    " /dev/dotout))
      (display line /dev/dotout)
      (equal? line "\n"))

    (write-char #\newline /dev/dotout)
    (write-char #\} /dev/dotout)

    (get-output-bytes /dev/dotout #true)))
