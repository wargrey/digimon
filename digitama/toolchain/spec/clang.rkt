#lang typed/racket/base

(provide (all-defined-out))

(require "../problem.rkt")

(require "../../collection.rkt")
(require "../../spec/dsl.rkt")
(require "../../spec/behavior.rkt")
(require "../../spec/expect/exec.rkt")

(require "../../../token.rkt")
(require "../../../string.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-clang-problem-info : (-> Path (Option Problem-Info))
  (lambda [main.cpp]
    (call-with-input-file* main.cpp
      (λ [[/dev/stdin : Input-Port]]
        (let try-next-comment-block : (Option Problem-Info) ()
          (syn-token-skip-whitespace /dev/stdin)
          
          (cond [(regexp-try-match #px"^[/][*][*]" /dev/stdin)
                 (let*-values ([(head continue?) (read-clang-problem-title /dev/stdin)]
                               [(body) (if (not continue?) null (read-clang-problem-description /dev/stdin))]
                               [(args results rest) (problem-description-split-input-output body)]
                               [(specs description) (problem-description-split-spec main.cpp rest)])
                   (if (pair? specs)
                       (make-problem-info head description args results specs #false)
                       (try-next-comment-block)))]
                [(regexp-try-match #px"^[/][*]" /dev/stdin)
                 (regexp-match #px".+?[*][/]" /dev/stdin)
                 (try-next-comment-block)]
                [(regexp-try-match #px"^[/][/]" /dev/stdin)
                 (read-line /dev/stdin 'any)
                 (try-next-comment-block)]
                [else #false]))))))

(define read-clang-problem-feature : (-> Path Path (Vectorof String) (Option Spec-Feature))
  (lambda [main.cpp a.out cmd-argv]
    (define problem-info : (Option Problem-Info) (read-clang-problem-info main.cpp))
    
    (and problem-info
         (clang-problem->feature problem-info a.out cmd-argv))))

(define clang-problem->feature : (-> Problem-Info Path (Vectorof String) Spec-Feature)
  (lambda [problem-info a.out cmd-argv]
    (describe ["~a" (or (problem-info-title problem-info) (path->string a.out))]
      #:do (for/spec ([t (in-list (problem-info-specs problem-info))])
             (define-values (args result) (values (problem-spec-input t) (problem-spec-output t)))
             (define brief (problem-spec-brief t))
             (define timeout (problem-spec-timeout t))
             (if (and (string-blank? args) (not result))
                 (it brief #:do #;(pending))
                 (it brief #:do #:millisecond (or timeout 0)
                   #:do (parameterize ([default-spec-exec-strict? (or (problem-spec-strict? t) (default-spec-exec-strict?))]
                                       [default-spec-exec-stdin-line-limit (or (problem-spec-stdio-lines t) (default-spec-exec-stdin-line-limit))]
                                       [default-spec-exec-stdout-line-limit (or (problem-spec-stdio-lines t) (default-spec-exec-stdout-line-limit))])
                          (expect-stdout a.out cmd-argv args (or result null)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-clang-problem-title : (-> Input-Port (Values (Option String) Boolean))
  (lambda [/dev/stdin]
    (define self (read-line /dev/stdin 'any))
    
    (cond [(or (eof-object? self) (regexp-match #px"[*]+[/].*" self)) (values #false #false)]
          [(string-blank? self) (values #false #true)]
          [else (values (string-trim self) #true)])))

(define read-clang-problem-description : (-> Input-Port (Listof String))
  (lambda [/dev/stdin]
    (let read-desc ([senil : (Listof String) null])
      (define self (read-line /dev/stdin 'any))

      (if (string? self)
          (if (regexp-match #px"[*]+[/].*" self)
              
              (let ([last (string-trim self #px"(^\\s*[*]?\\s?)|([*]+[/].*$)")])
                (if (string-blank? last)
                    (reverse senil)
                    (reverse (cons last senil))))

              (read-desc (cons (string-trim self #px"^\\s*[*]\\s?" #:right? #false) senil)))
          (reverse senil)))))
