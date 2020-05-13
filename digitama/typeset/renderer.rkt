#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Tex-Renderer-Filter (-> String Any (Values (U String (Listof String) False) Any)))

(struct tex-renderer
  ([program : Path]
   [filter : (Option Tex-Renderer-Filter)])
  #:constructor-name make-tex-renderer
  #:type-name Tex-Renderer
  #:transparent)

(define tex-find-binary-path : (-> Symbol (Option Path))
  (lambda [basename]
    (define bname : String (symbol->string basename))
    (case (system-type 'os)
      [(windows) (or (find-executable-path bname #false #false)
                     (find-executable-path (string-append bname ".exe")))]
      [else (find-executable-path bname)])))
