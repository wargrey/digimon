#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/format)

(require "../expectation.rkt")
(require "../prompt.rkt")
(require "../issue.rkt")

(require "../../exec.rkt")

(require "type.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Spec-Stdout-Expect-Bytes (U Bytes Byte-Regexp))
(define-type Spec-Stdout-Expect-String (U String Regexp))
(define-type Spec-Stdout-Expectation (Listof (U Spec-Stdout-Expect-Bytes Spec-Stdout-Expect-String)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-spec-exec-stdin-log-level : (Parameterof (Option Symbol)) (make-parameter #false))
(define default-spec-exec-stdout-port : (Parameterof (Option Output-Port)) (make-parameter #false))
(define default-spec-exec-stderr-port : (Parameterof (Option Output-Port)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-spec-expectation (stdout [bin : Path-String] [argv : (Vectorof String)] [stdin : (U String Bytes)] [expected-any : Spec-Stdout-Expectation])
  (define /dev/stdin : Input-Port (if (bytes? stdin) (open-input-bytes stdin) (open-input-string stdin)))
  (define /dev/stdout : (Option Output-Port) (default-spec-exec-stdout-port))
  
  (define raw : Bytes
    (fg-recon-exec/pipe #:/dev/stdin /dev/stdin #:stdin-log-level (default-spec-exec-stdin-log-level)
                        #:/dev/stdout /dev/stdout #:/dev/stderr (default-spec-exec-stderr-port)
                        'exec (if (string? bin) (string->path bin) bin) argv))

  ;;; NOTE: Here we only check empty lines after the output in case that whitespaces are important 
  (define eols (regexp-match #px#"[\r\n]+$" raw))
  (when (and (not eols) /dev/stdout)
    (newline /dev/stdout))

  (unless (null? expected-any)
    (define given0 : Bytes (if eols (subbytes raw 0 (- (bytes-length raw) (bytes-length (car eols)))) raw))
    (define unicode? : Boolean (and (or (string? (car expected-any)) (regexp? (car expected-any)))))
    (define given : (U String Bytes) (if (not unicode?) given0 (bytes->string/utf-8 given0)))

    ; the issue format should be combined with existing `default-spec-issue-format`
    ; nevertheless, it is not a big deal here               
    (parameterize ([default-spec-issue-extra-arguments (list (vector 'given given #false))]
                   [default-spec-issue-format (if (not unicode?) spec-exec-bytes-format spec-exec-string-format)])
      (or (for/or : Boolean ([expected (if (pair? expected-any) (in-list expected-any) (in-value expected-any))])
            (cond [(bytes? expected) (equal? given expected)]
                  [(string? expected) (equal? given expected)]
                  [else (regexp-match? expected given)]))
          (spec-misbehave)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-exec-string-format : Spec-Issue-Format
  (lambda [v fallback]
    (cond [(string? v) (spec-expected-string v)]
          [(bytes? v) (spec-expected-string v)]
          [(list? v) (string-join (map spec-expected-string v) "\n------\n")]
          [(path? v) (path->string v)]
          [else (fallback v)])))

(define spec-exec-bytes-format : Spec-Issue-Format
  (lambda [v fallback]
    (cond [(string? v) (spec-expected-bytes v)]
          [(bytes? v) (spec-expected-bytes v)]
          [(list? v) (string-join (map spec-expected-bytes v) "\n------\n")]
          [(path? v) (path->string v)]
          [else (fallback v)])))

(define spec-expected-string : (-> Any String)
  (lambda [v]
    (cond [(bytes? v) (bytes->string/utf-8 v)]
          [(string? v) v]
          [else (~s v)])))

(define spec-expected-bytes : (-> Any String)
  (lambda [v]
    (cond [(bytes? v) (~s v)]
          [(string? v) (~s (string->bytes/utf-8 v))]
          [else (~s v)])))
