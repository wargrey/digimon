#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

(require "../../emoji.rkt")

(define-type Spec-Issue-Type (U 'misbehaved 'todo 'skipped 'fatal 'pass))
(define-type Spec-Issue-Brief (Option String))
(define-type Spec-Issue-Info (Pairof Symbol Any))

(define default-spec-issue-brief : (Parameterof Spec-Issue-Brief) (make-parameter #false))
(define default-spec-issue-info : (Parameterof (Listof Spec-Issue-Info)) (make-parameter null))
(define default-spec-issue-rootdir : (Parameterof Path-String) current-directory)
(define default-spec-issue-indention : (Parameterof Index) (make-parameter 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct spec-issue
  ([type : Spec-Issue-Type]
   [brief : Spec-Issue-Brief]
   [indention : Index]
   [info : (Listof Spec-Issue-Info)])
  #:type-name Spec-Issue
  #:transparent)

(define make-spec-issue : (-> Spec-Issue-Type Spec-Issue)
  (lambda [type]
    (spec-issue type
                (default-spec-issue-brief)
                (default-spec-issue-indention)
                (default-spec-issue-info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define with-spec-issue-info : (All (a) (-> (Listof Spec-Issue-Info) (-> a) a))
  (lambda [infos do]
    (parameterize ([default-spec-issue-info (append (default-spec-issue-info) infos)])
      (do))))

(define spec-issue-info-ref : (-> (Listof Spec-Issue-Info) Symbol Any)
  (lambda [infos name]
    (define maybe-info (assq name infos))

    (and maybe-info
         (let ([v (cdr maybe-info)])
           v))))

(define spec-issue-fgcolor : (-> Spec-Issue-Type Symbol)
  (lambda [type]
    (case type
      [(pass) 'lightgreen]
      [(misbehaved) 'lightred]
      [(skipped) 'lightblue]
      [(todo) 'lightmagenta]
      [(fatal) 'darkred])))

(define spec-issue-moji : (-> Spec-Issue-Type Char)
  (lambda [type]
    (case type
      [(pass) green-heart#]
      [(misbehaved) broken-heart#]
      [(skipped) arrow-heart#]
      [(todo) growing-heart#]
      [(fatal) bomb#])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-spec-issue-display : (->* (Spec-Issue) (Output-Port #:indention Index) Void)
  (lambda [issue [/dev/stdout (current-output-port)] #:indention [indention 0]]
    (fprintf /dev/stdout "~a - ~a~n"
             (case (spec-issue-type issue)
               [(pass) 'ok]
               [else '|not ok|])
             (spec-issue-info-ref (spec-issue-info issue) 'message))))
