#lang racket/base

(provide (all-defined-out))

(require scribble/core)
(require scribble/base)

(require file/convertible)

(require "backend.rkt")
(require "style.rkt")

(require "../diagram/d2.rkt")
(require "../../filesystem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct elk (script layout target theme inset scale interval sketch? appendix? log-level dependencies always-remake?)
  #:constructor-name unsafe-d2
  #:transparent
  #:property prop:convertible
  (λ [self mime fallback]
    (cond [(not (elk? self)) fallback]
          [else (let ([src (elk-script self)])
                  (case mime
                    [(svg-bytes) (d2->diagram self #".svg")]
                    [(pdf-bytes) (d2->diagram self #".pdf")]
                    [(gif-bytes) (d2->diagram self #".gif")]
                    [(png-bytes) (d2->diagram self #".png")]
                    [(script-path src-path) (elk-dependencies self)]
                    [else fallback]))])))

(define d2
  (lambda [#:target [target #false] #:layout [layout 'elk] #:theme [theme #false]
           #:inset [inset #false] #:scale [scale #false] #:interval [interval #false]
           #:sketch? [sketch? #false] #:force-appendix? [appendix? #false] #:always-remake? [remake? #false]
           #:style [style #false]
           script0]
    (define script (simple-form-path script0))
    (define diagram
      (unsafe-d2 script layout target theme inset scale interval sketch? appendix?
                 #false (if (path-literal? script) (d2-smart-dependencies script) null) remake?))

    (make-traverse-element
     (λ [get set!]
       (cond [(handbook-stat-renderer? get) diagram #| <- for collecting dependencies |#]
             [(handbook-latex-renderer? get)
              (cond [(or (not scale) (<= scale 0)) (handbook-element style diagram)]
                    [else (image #:scale scale #:style style (d2->diagram-path diagram #".pdf" #true))])]
             [else (handbook-element style diagram)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define d2-scrbl-destination
  (lambda [src.d2 ext]
    (d2-script-destination src.d2 ext #false)))

;;; NOTE
; Meanwhile d2 exported PDF doesn't scale its page size,
; hence the `ignore-scale?` for latex renderer
(define d2->diagram-path
  (lambda [self ext ignore-scale?]
    (define src.d2 (elk-script self))
    (define d2.ext (d2-scrbl-destination src.d2 ext))

    (when (or (elk-always-remake? self)
              (not (file-exists? d2.ext))
              (let ([mtime (file-or-directory-modify-seconds d2.ext)])
                (for/or ([dep (in-list (elk-dependencies self))])
                  (< mtime (file-or-directory-modify-seconds dep)))))
      (d2-render #:target (elk-target self) #:theme (elk-theme self)
                 #:inset (elk-inset self) #:scale (if ignore-scale? #false (elk-scale self)) #:interval (elk-interval self)
                 #:sketch? (elk-sketch? self) #:appendix? (elk-appendix? self) #:bundle? #true #:center? #true
                 #:stdin-log-level (elk-log-level self)
                 src.d2 d2.ext (elk-layout self)))

    d2.ext))

(define d2->diagram
  (lambda [self ext]
    (file->bytes (d2->diagram-path self ext #false))))
