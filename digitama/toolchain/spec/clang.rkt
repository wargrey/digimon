#lang typed/racket/base

(provide (all-defined-out))

(require "../problem.rkt")

(require "../../spec/dsl.rkt")
(require "../../spec/behavior.rkt")
(require "../../spec/expect/exec.rkt")

(require "../../../token.rkt")
(require "../../../string.rkt")
(require "../../../dtrace.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-clang-problem-info : (-> Path (Option Problem-Info))
  (lambda [main.cpp]
    (call-with-input-file* main.cpp
      (λ [[/dev/stdin : Input-Port]]
        (let try-next-comment-block : (Option Problem-Info) ()
          (syn-token-skip-whitespace /dev/stdin)
          
          (cond [(regexp-try-match #px"^[/][*][*]\\s*" /dev/stdin)
                 (let*-values ([(head continue?) (read-clang-problem-title /dev/stdin)]
                               [(body) (if (not continue?) null (read-clang-problem-description /dev/stdin))]
                               [(args result rest) (problem-description-split-input-output body)]
                               [(specs description) (problem-description-split-spec main.cpp rest)])
                   (if (pair? specs)
                       (make-problem-info head description args result specs)
                       (try-next-comment-block)))]
                [(regexp-try-match #px"^[/][*]" /dev/stdin)
                 (regexp-match #px".+?[*][/]" /dev/stdin)
                 (try-next-comment-block)]
                [(regexp-try-match #px"^[/][/]" /dev/stdin)
                 (read-line /dev/stdin)
                 (try-next-comment-block)]
                [else #false]))))))

(define read-clang-problem-feature : (-> Path Path (Vectorof String) (Option Spec-Feature))
  (lambda [main.cpp a.out args]
    (define problem-info : (Option Problem-Info) (read-clang-problem-info main.cpp))
    
    (and problem-info
        (clang-problem->feature problem-info a.out args))))

(define clang-problem->feature : (-> Problem-Info Path (Vectorof String)  Spec-Feature)
  (lambda [problem-info a.out args]
    (describe ["~a" (or (problem-info-title problem-info) (path->string a.out))]
      #:do #:before dtrace-sync
      #:do (for/spec ([t (in-list (problem-info-specs problem-info))])
             (define-values (bargs result) (values (problem-spec-input t) (problem-spec-output t)))
             (define brief (problem-spec-brief t))
             (cond [(and (string-blank? bargs) (not result))
                    (it brief #:do #:after dtrace-sync #:do #;(pending))]
                   [(regexp? result)
                    (it brief #:do #:after dtrace-sync #:do (expect-match-stdout a.out args bargs result))]
                   [else
                    (it brief #:do #:after dtrace-sync #:do (expect-stdout a.out args bargs result))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-clang-problem-title : (-> Input-Port (Values (Option String) Boolean))
  (lambda [/dev/stdin]
    (define self (read-line /dev/stdin))
      
    (if (or (eof-object? self)
            (regexp-match #px"[*]+[/].*" self))
        (values #false #false)
        (values (string-trim self) #true))))

(define read-clang-problem-description : (-> Input-Port (Listof String))
  (lambda [/dev/stdin]
    (let read-desc ([senil : (Listof String) null])
      (define self (read-line /dev/stdin))
      
      (if (string? self)
          (if (regexp-match #px"[*]+[/].*" self)
              
              (let ([last (string-trim self #px"(^\\s*[*]?\\s*)|(\\s*[*]+[/].*$)")])
                (if (string-blank? last)
                    (reverse senil)
                    (reverse (cons last senil))))

              (read-desc (cons (string-trim self #px"(^\\s*[*]\\s)|(\\s*$)") senil)))
          (reverse senil)))))
