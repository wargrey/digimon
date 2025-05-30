#lang racket/base

(provide tamer-chunk Tamer-Chunk)

(require scribble/scheme)
(require scribble/decode)
(require scribble/manual)
(require scribble/struct)

(require (for-syntax racket/base))
(require (for-syntax syntax/boundmap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-syntax
  ;; maps chunk identifiers to a counter, so we can distinguish multiple uses
  ;; of the same name
  (define chunk-numbers (make-free-identifier-mapping))
  
  (define (get-chunk-number id)
    (free-identifier-mapping-get chunk-numbers id (lambda () #f)))

  (define (inc-chunk-number id)
    (free-identifier-mapping-put! chunk-numbers id (+ 1 (free-identifier-mapping-get chunk-numbers id))))

  (define (init-chunk-number id)
    (free-identifier-mapping-put! chunk-numbers id 2)))

(define-syntax (chunkref stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([tag (format "~a:1" (syntax-e #'id))]
                   [str (format "~a" (syntax-e #'id))])
       #'(elemref '(chunk tag) #:underline? #f str))]))

(define-syntax-rule (define-chunk chunk-id racketblock)
  (define-syntax (chunk-id stx)
    (syntax-case stx ()
      [(_ name expr (... ...)) 
       ;; no need for more error checking, using chunk for the code will do that
       (identifier? #'name)
       (let* ([n (get-chunk-number (syntax-local-introduce #'name))]
              [str (symbol->string (syntax-e #'name))]
              [tag (format "~a:~a" str (or n 1))])
         
         (when n
           (inc-chunk-number (syntax-local-introduce #'name)))
         
         (syntax-local-lift-expression #'(quote-syntax (a-chunk name expr (... ...))))
         
         (with-syntax ([tag tag]
                       [str str]
                       [((for-label-mod (... ...)) (... ...))
                        (map (λ [expr]
                               (syntax-case expr (require)
                                 [(require mod (... ...))
                                  (let loop ([mods (syntax->list #'(mod (... ...)))])
                                    (cond
                                      [(null? mods) null]
                                      [else 
                                       (syntax-case (car mods) (for-syntax)
                                         [(for-syntax x (... ...))
                                          (append (loop (syntax->list #'(x (... ...))))
                                                  (loop (cdr mods)))]
                                         [x (cons #'x (loop (cdr mods)))])]))]
                                 [else null]))
                             (syntax->list #'(expr (... ...))))]
                       [(rest (... ...)) (if n #`((subscript #,(format "~a" n))) #`())])
           #`(begin
               (require (for-label for-label-mod (... ...) (... ...)))
               #,@(if n
                    #'()
                    #'((define-syntax name (make-element-id-transformer (λ (stx) #'(chunkref name))))
                       (begin-for-syntax (init-chunk-number #'name))))
               (make-splice
                (list (make-toc-element
                       #f
                       (list (elemtag '(chunk tag) (bold (italic (racket name)) " ::=")))
                       (list (smaller (elemref '(chunk tag) #:underline? #f str rest (... ...)))))
                      (nested #:style 'inset (racketblock expr (... ...))))))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-chunk tamer-chunk racketblock0)
(define-chunk Tamer-Chunk RACKETBLOCK0)
