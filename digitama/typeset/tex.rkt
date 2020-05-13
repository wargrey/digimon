#lang typed/racket/base

(provide (all-defined-out))

(require "renderer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define latex-database : (HashTable Symbol Tex-Renderer) (make-hasheq))

(define tex-register-renderer : (-> Symbol [#:filter (Option Tex-Renderer-Filter)] [#:basename (Option Symbol)] Void)
  (lambda [name #:filter [filter #false] #:basename [basename #false]]
    (define program : (Option Path) (tex-find-binary-path (or basename name)))
    
    (when (path? program)
      (hash-set! latex-database name
                 (make-tex-renderer program filter)))))
