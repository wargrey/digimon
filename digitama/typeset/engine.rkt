#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Tex-Preamble-Filter (-> String Any (Values (U String (Listof String) False) Any)))
(define-type Tex-Post-Exec (-> Symbol Path Symbol Path))

(struct tex-engine
  ([program : Path]
   [extension : Bytes]
   [preamble-filter : (Option Tex-Preamble-Filter)]
   [post-exec : (Option Tex-Post-Exec)])
  #:constructor-name make-tex-engine
  #:type-name Tex-Engine
  #:transparent)

(define tex-find-binary-path : (-> Symbol (Option Path))
  (lambda [basename]
    (define bname : String (symbol->immutable-string basename))
    (case (system-type 'os)
      [(windows) (or (find-executable-path bname #false #false)
                     (find-executable-path (string-append bname ".exe")))]
      [else (find-executable-path bname)])))
