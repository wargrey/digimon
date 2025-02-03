#lang typed/racket/base

(provide (all-defined-out))
(provide Tex-Post-Exec Tex-Preamble-Filter)

(require "engine.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define latex-database : (HashTable Symbol Tex-Engine) (make-hasheq))

(define tex-register-engine : (->* (Symbol #:draftmode (Option String))
                                   (Bytes #:filter (Option Tex-Preamble-Filter)
                                          #:post-exec (Option Tex-Post-Exec)
                                          #:basename (Option Symbol)) Void)
  (lambda [name [ext #".pdf"] #:draftmode draftmode #:filter [filter #false] #:post-exec [exec #false] #:basename [basename #false]]
    (define program : (Option Path) (tex-find-binary-path (or basename name)))
    
    (when (path? program)
      (hash-set! latex-database name
                 (make-tex-engine program ext filter exec draftmode)))))
