#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/format)

(require "../expectation.rkt")
(require "../prompt.rkt")
(require "../issue.rkt")
(require "../timeout.rkt")

(require "../../exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Spec-Stdout-Expect-Bytes (U Bytes Byte-Regexp))
(define-type Spec-Stdout-Expect-String (U String Regexp))
(define-type Spec-Stdout-Expectation (Listof (U Spec-Stdout-Expect-Bytes Spec-Stdout-Expect-String)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-spec-exec-operation-name : (Parameterof Symbol) (make-parameter 'exec))
(define default-spec-exec-strict? : (Parameterof Boolean) (make-parameter #true))
(define default-spec-exec-stdin-log-level : (Parameterof (Option Symbol)) (make-parameter #false))
(define default-spec-exec-stdin-line-limit : (Parameterof (Option Natural)) (make-parameter #false))
(define default-spec-exec-stdout-line-limit : (Parameterof (Option Natural)) (make-parameter #false))
(define default-spec-exec-stdout-port : (Parameterof (Option Output-Port)) (make-parameter #false))
(define default-spec-exec-stderr-port : (Parameterof (Option Output-Port)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-spec-expectation (stdout [program : Path-String] [cmd-argv : (Vectorof String)] [/dev/stdin : (U String Bytes)] [outputs : Spec-Stdout-Expectation])
  (define /dev/stdout : (Option Output-Port) (default-spec-exec-stdout-port))

  (define raw : Bytes
    (fg-recon-exec/pipe #:/dev/stdin (if (bytes? /dev/stdin) (open-input-bytes /dev/stdin) (open-input-string /dev/stdin))
                        #:/dev/stdout /dev/stdout #:/dev/stderr (default-spec-exec-stderr-port)
                        #:pre-fork spec-timeout-start #:post-fork spec-timeout-start #:atexit spec-timeout-terminate
                        #:stdin-log-level (default-spec-exec-stdin-log-level)
                        #:stdin-line-limit (default-spec-exec-stdin-line-limit)
                        #:stdout-line-limit (default-spec-exec-stdout-line-limit)
                        (default-spec-exec-operation-name) (if (string? program) (string->path program) program) cmd-argv))

  ;;; NOTE: Here we only check empty lines after the output in case that whitespaces are important
  (define eols (regexp-match #px#"(\r|\n)+$" raw))
  (when (and (not eols) /dev/stdout (> (bytes-length raw) 0))
    (newline /dev/stdout))

  (unless (null? outputs)
    (define given : Bytes (if eols (subbytes raw 0 (- (bytes-length raw) (bytes-length (car eols)))) raw))
    (define givens : (Listof Bytes) (if (regexp-match? #px"(\r|\n)" given) (regexp-split #px"(\r|\n)+" given) null))
    (define strict? : Boolean (default-spec-exec-strict?))
    
    (or (for/or : Boolean ([expected (if (pair? outputs) (in-list outputs) (in-value outputs))])
          (cond [(bytes? expected)
                 (if (null? givens)
                     (spec-bytes=? given expected strict?)
                     (spec-mbytes=? givens expected strict?))]
                [(string? expected)
                 (if (null? givens)
                     (spec-string=? (bytes->string/utf-8 given) expected strict?)
                     (spec-mbytes=? givens (string->bytes/utf-8 expected) strict?))]
                [(byte-regexp? expected) (regexp-match? expected given)]
                [else (regexp-match? expected given)]))
        (let ([str? (spec-string-output? outputs)])
          ; the issue format should be combined with existing `default-spec-issue-format`
          ; nevertheless, it is not a big deal here
          (parameterize ([default-spec-issue-extra-arguments (list (vector 'given (spec-clear-string (bytes->string/utf-8 given)) #false))]
                         [default-spec-issue-format (if (not str?) spec-exec-bytes-format spec-exec-string-format)])
            (spec-misbehave))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-string-output? : (-> Spec-Stdout-Expectation Boolean)
  (lambda [outputs]
    (for/or : Boolean ([expected (if (pair? outputs) (in-list outputs) (in-value outputs))])
      (or (string? expected)
          (regexp? expected)))))

(define spec-bytes=? : (-> Bytes Bytes Boolean Boolean)
  (lambda [given expected strict?]
    (if (and strict?)
        (bytes=? given expected)
        (or (bytes=? given expected)
            (bytes=? (regexp-replace #px"\\s+$" given #"")
                     (regexp-replace #px"\\s+$" expected #""))))))

(define spec-string=? : (-> String String Boolean Boolean)
  (lambda [given expected strict?]
    (if (and strict?)
        (string=? given expected)
        (or (string=? given expected)
            (string=? (string-trim given #:left? #false #:repeat? #true)
                      (string-trim expected #:left? #false #:repeat? #true))))))

(define spec-mbytes=? : (-> (Listof Bytes) Bytes Boolean Boolean)
  (lambda [givens expected strict?]
    (define es (regexp-split #px"(\r|\n)+" expected))

    (and (= (length givens) (length es))
         (for/and : Boolean ([g (in-list givens)]
                             [e (in-list es)])
           (spec-bytes=? g e strict?)))))

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

(define spec-clear-string : (-> String String)
  (lambda [s]
    (if (regexp-match? #px"[[:blank:]]" s)
        (string-replace s #px"[[:blank:]]" "·")
        s)))
