#lang racket/base

(module t1 digimon
  (define-syntax (t stx)
    (syntax-parse stx
      [(_ who-cares ...)
       (for/list ([s (in-syntax #'(who-cares ...))]) (syntax-e s))
       #'(void)])))
  
(module t2 digimon/no-check
  (define-syntax (t stx)
    (syntax-parse stx
      [(_ who-cares ...)
       (for/list ([s (in-syntax #'(who-cares ...))]) (syntax-e s))
       #'(void)])))

(module t3 digimon/gui
  (define-syntax (t stx)
    (syntax-parse stx
      [(_ who-cares ...)
       (for/list ([s (in-syntax #'(who-cares ...))]) (syntax-e s))
       #'(void)])))

(module t4 digimon/gui/no-check
  (define-syntax (t stx)
    (syntax-parse stx
      [(_ who-cares ...)
       (for/list ([s (in-syntax #'(who-cares ...))]) (syntax-e s))
       #'(void)])))

(module t5 digimon/sugar
  (define-syntax (t stx)
    (syntax-parse stx
      [(_ who-cares ...)
       (for/list ([s (in-syntax #'(who-cares ...))]) (syntax-e s))
       #'(void)])))
