#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/path))

(require racket/path)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(define-syntax (define-file-reader stx)
  (syntax-case stx [lambda λ]
    [(_ id #:+ Type #:lambda do-read)
     #'(define id : (-> Path-String [#:mode (U 'binary 'text)] [#:count-lines? Boolean] Type)
         (let ([up-to-dates : (HashTable Path (Pairof Nonnegative-Fixnum Type)) (make-hash)]
               [read-datum : (-> Input-Port Path Type) do-read])
           (lambda [src #:mode [mode-flag 'binary] #:count-lines? [count-lines? (port-count-lines-enabled)]]
             (define mtime : Nonnegative-Fixnum (file-or-directory-modify-seconds src))
             (define src-key : Path (simplify-path src))
             (define mdatum : (Option (Pairof Nonnegative-Fixnum Type)) (hash-ref up-to-dates src-key (λ [] #false)))
             (cond [(and mdatum (<= mtime (car mdatum))) (cdr mdatum)]
                   [else (let ([datum (parameterize ([port-count-lines-enabled count-lines?])
                                        (call-with-input-file* src #:mode mode-flag
                                          (λ [[/dev/stdin : Input-Port]] : Type
                                            (read-datum /dev/stdin src-key))))])
                           (hash-set! up-to-dates src-key (cons mtime datum))
                           datum)]))))]
    [(_ id #:+ Type (lambda [/dev/stdin src] body ...))
     (with-syntax ([id* (format-id #'id "~a*" (syntax-e #'id))])
       #'(begin (define id : (-> Input-Port Path Type)
                  (lambda [/dev/stdin src]
                    body ...))

                (define-file-reader id* #:+ Type #:lambda id)))]
    [(_ id #:+ Type (λ [/dev/stdin] body ...))
     #'(define-file-reader id #:+ Type (lambda [/dev/stdin] body ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dirname : (-> Path-String [#:rootname String] String)
  (lambda [path #:rootname [root "/"]]
    (define dir : (Option Path-String)
      (let ([dir : Path (simple-form-path path)])
        (cond [(directory-exists? path) path]
              [else (path-only path)])))
    (cond [(not dir) #| this should not happen |# root]
          [else (let-values ([(_b name _?) (split-path dir)])
                  (cond [(path? name) (path->string name)]
                        [else root]))])))

(define file-readable? : (-> Path-String Boolean)
  (lambda [p]
    (and (file-exists? p)
         (memq 'read (file-or-directory-permissions p))
         #true)))

(define file-mtime : (->* (Path-String) (Natural) Natural)
  (lambda [f [fallback 0]]
    (cond [(file-exists? f) (file-or-directory-modify-seconds f)]
          [else fallback])))
