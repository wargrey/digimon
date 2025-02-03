#lang typed/racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ register
  (require "../tex.rkt")

  (tex-register-engine 'lualatex #:draftmode "-draftmode" #:filter #false))
