#lang typed/racket/base

(require "../tex.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (tex-register-engine 'xelatex #:draftmode #false #;"-no-pdf" #:filter #false))
