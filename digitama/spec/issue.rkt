#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

(define-type Spec-Issue-Type (U 'pass 'fail 'todo 'skip))
(define-type Spec-Issue-Name (U String Symbol Boolean))
(define-type Spec-Issue-Info (Pairof Symbol Any))

(define default-spec-issue-brief : (Parameterof Spec-Issue-Name) (make-parameter #false))
(define default-spec-issue-info : (Parameterof (Listof Spec-Issue-Info)) (make-parameter null))
(define default-spec-issue-rootdir : (Parameterof Path-String) current-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct spec-issue
  ([type : Spec-Issue-Type]
   [brief : Spec-Issue-Name]
   [info : (Listof Spec-Issue-Info)])
  #:type-name Spec-Issue
  #:transparent)

(define make-spec-issue : (-> Spec-Issue-Type Spec-Issue)
  (lambda [type]
    (spec-issue type
                (default-spec-issue-brief)
                (default-spec-issue-info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define with-issue-info : (All (a) (-> (Listof Spec-Issue-Info) (-> a) a))
  (lambda [infos do]
    (parameterize ([default-spec-issue-info (append (default-spec-issue-info) infos)])
      (do))))

(define issue-location : (-> Syntax (Option (List Path-String Positive-Integer Natural)))
  (lambda [stx]
    (define src (syntax-source stx))
    (define line (syntax-line stx))
    (define column (syntax-column stx))

    (and (or (path? src) (path-string? src))
         line column
         (list src line column))))

(define issue-info-ref : (-> (Listof Spec-Issue-Info) Symbol Any)
  (lambda [infos name]
    (define maybe-info (assq name infos))

    (and maybe-info
         (let ([v (cdr maybe-info)])
           v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-spec-issue-display : (->* (Spec-Issue) (Output-Port #:indention Index) Void)
  (lambda [issue [/dev/stdout (current-output-port)] #:indention [indention 0]]
    (fprintf /dev/stdout "~a - ~a~n"
             (case (spec-issue-type issue)
               [(pass) 'ok]
               [else '|not ok|])
             (issue-info-ref (spec-issue-info issue) 'message))))
