#lang typed/racket/base

(provide (all-defined-out))

(require syntax/location)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (#%full-module stx)
  #'(let ([rmp (variable-reference->resolved-module-path (#%variable-reference))])
      (if (not rmp) '<nota-module> (resolved-module-path-name rmp))))

(define-syntax (#%file stx)
  #'(let ([full (ann (#%full-module) (U Symbol Path (Pairof Path (Listof Symbol))))])
      (cond [(path? full) full]
            [(pair? full) (car full)]
            [else (current-directory)])))

(define-syntax (#%module stx)
  #'(let ([full (ann (#%full-module) (U Symbol Path (Pairof Path (Listof Symbol))))])
      (cond [(path? full) (string->symbol (path->string (path-replace-extension (assert (file-name-from-path full) path?) "")))]
            [(pair? full) (last (cdr full))]
            [else '<anonymous>])))

(define-syntax (#%modules stx)
  #'(let ([full (ann (#%full-module) (U Symbol Path (Pairof Path (Listof Symbol))))])
      (cond [(path? full) (list (string->symbol (path->string (path-replace-extension full ""))))]
            [(pair? full) (cons (string->symbol (path->string (path-replace-extension (car full) ""))) (cdr full))]
            [else (list '<anonymous>)])))

(define-syntax (#%line stx)
  #`(quote #,(syntax-line stx)))

(define-syntax (#%column stx)
  #`(quote #,(syntax-column stx)))

(define-syntax (#%position stx)
  #`(quote #,(syntax-position stx)))

(define-syntax (#%location stx)
  #`(cons #,(syntax-line stx)
          #,(syntax-column stx)))

(define-syntax (#%function stx) ; class method has a symbol name looks like "[name] method in [class%]"
  #'(let use-next-id : Symbol ([stacks (continuation-mark-set->context (current-continuation-marks))])
      (if (null? stacks) 'λ
          (or (caar stacks)
              (use-next-id (cdr stacks))))))