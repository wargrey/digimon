#lang typed/racket/base

(provide (all-defined-out))

(require "../debug.rkt")
(require "ioexn.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-bytes->integer stx)
  (syntax-case stx [:]
    [(_ bytes->integer do-bytes->integer [argl : Argl] ... #:-> Integer_t)
     (syntax/loc stx
       (define bytes->integer : (All (a) (case-> [Bytes Integer Argl ... -> Integer_t]
                                                 [Bytes Integer Argl ... (-> Any Boolean : (∩ Integer_t a)) -> (∩ Integer_t a)]
                                                 [Bytes Integer Argl ... (-> Any Boolean : (∩ Integer_t a)) Symbol -> (∩ Integer_t a)]
                                                 [Bytes Integer Argl ... (-> Any Boolean : (∩ Integer_t a)) Symbol Throw-Range-Error -> (∩ Integer_t a)]))
         (case-lambda
           [(src start argl ...) (do-bytes->integer src start argl ...)]
           [(src start argl ... subinteger?) (assert (do-bytes->integer src start argl ...) subinteger?)]
           [(src start argl ... subinteger? op) (assert* (do-bytes->integer src start argl ...) subinteger? throw-range-error op)]
           [(src start argl ... subinteger? op throw) (assert* (do-bytes->integer src start argl ...) subinteger? throw op)])))]
    [(_ bytes->integer do-bytes->integer signed? #:-> Integer_t)
     (syntax/loc stx
       (define bytes->integer : (All (a) (case-> [Bytes Integer -> Integer_t]
                                                 [Bytes Integer (-> Any Boolean : (∩ Integer_t a)) -> (∩ Integer_t a)]
                                                 [Bytes Integer (-> Any Boolean : (∩ Integer_t a)) Symbol -> (∩ Integer_t a)]
                                                 [Bytes Integer (-> Any Boolean : (∩ Integer_t a)) Symbol Throw-Range-Error -> (∩ Integer_t a)]))
         (case-lambda
           [(src start) (do-bytes->integer src start signed?)]
           [(src start subinteger?) (assert (do-bytes->integer src start signed?) subinteger?)]
           [(src start subinteger? op) (assert* (do-bytes->integer src start signed?) subinteger? throw-range-error op)]
           [(src start subinteger? op throw) (assert* (do-bytes->integer src start signed?) subinteger? throw op)])))]))

(define-syntax (define-bytes->integer* stx)
  (syntax-case stx [:]
    [(_ [do-bytes->integer [bytes->integer #:-> Integer] [bytes->natural #:-> Natural]] ...)
     (syntax/loc stx
       (begin (define-bytes->integer bytes->integer do-bytes->integer #true  #:-> Integer) ...
              (define-bytes->integer bytes->natural do-bytes->integer #false #:-> Natural) ...))]))
