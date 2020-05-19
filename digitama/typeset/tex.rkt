#lang typed/racket/base

(provide (all-defined-out))
(provide Tex-Post-Exec Tex-Preamble-Filter)

(require "renderer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define latex-database : (HashTable Symbol Tex-Renderer) (make-hasheq))

(define tex-register-renderer : (->* (Symbol)
                                     (Bytes #:on-error-logging? Boolean
                                            #:filter (Option Tex-Preamble-Filter)
                                            #:post-exec (Option Tex-Post-Exec)
                                            #:basename (Option Symbol)) Void)
  (lambda [name [ext #".pdf"] #:on-error-logging? [logging? #true] #:filter [filter #false] #:post-exec [exec #false] #:basename [basename #false]]
    (define program : (Option Path) (tex-find-binary-path (or basename name)))
    
    (when (path? program)
      (hash-set! latex-database name
                 (make-tex-renderer program ext logging? filter exec)))))
