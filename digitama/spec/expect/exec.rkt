#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/format)
(require racket/list)

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

    (for/fold ([failures : (Listof Spec-Issue-Extra-Argument) null]
               #:result (and (pair? failures)
                             (parameterize ([default-spec-issue-extra-arguments failures]
                                            [default-spec-issue-ignored-arguments (list 'outputs)]

                                            ; the issue format should be combined with existing `default-spec-issue-format`
                                            ; nevertheless, it is not a big deal here
                                            [default-spec-issue-format (if (spec-string-output? outputs) spec-exec-string-format spec-exec-bytes-format)])
                               (spec-misbehave))))
              ([expected (if (pair? outputs) (in-list outputs) (in-value outputs))])
      (define failure (spec-fetch-failure expected given givens strict?))
      (cond [(null? failure) failures]
            [else (append failures failure)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-fetch-failure : (-> (U Spec-Stdout-Expect-Bytes Spec-Stdout-Expect-String) Bytes (Listof Bytes) Boolean
                                 (Listof Spec-Issue-Extra-Argument))
  (lambda [expected given givens strict?]
    (cond [(string? expected)
           (if (null? givens)
               (spec-string-failure (bytes->string/utf-8 given) expected strict?)
               (spec-mbytes-failures given givens (string->bytes/utf-8 expected) (default-spec-exec-stdout-line-limit) strict?))]
          [(bytes? expected) (spec-bytes-failure given expected #true #true)] ; binary mode
          [else (spec-match-failure given expected)])))

(define spec-string-output? : (-> Spec-Stdout-Expectation Boolean)
  (lambda [outputs]
    (for/or : Boolean ([expected (if (pair? outputs) (in-list outputs) (in-value outputs))])
      (or (string? expected)
          (regexp? expected)))))

(define spec-match-failure : (-> Bytes (U Byte-Regexp Regexp) (Listof Spec-Issue-Extra-Argument))
  (lambda [given expected]
    (cond [(regexp-match? expected given) null]
          [else (list (make-spec-extra-argument 'pattern expected)
                      (make-spec-extra-argument 'actual given))])))

(define spec-bytes-failure : (->* (Bytes Bytes Boolean Boolean) (Symbol Symbol Byte) (Listof Spec-Issue-Extra-Argument))
  (lambda [given expected strict? binary? [expected-name 'expected] [given-name 'given] [indent 0]]
    (cond [(bytes=? given expected) null]
          [(and (not strict?)
                (bytes=? (regexp-replace #px"\\s+$" given #"")
                         (regexp-replace #px"\\s+$" expected #""))) null]
          [else (list (make-spec-extra-argument expected-name (spec-clear-string expected binary?) #:indent indent)
                      (make-spec-extra-argument given-name (spec-clear-string given binary?) #:indent indent))])))

(define spec-string-failure : (-> String String Boolean (Listof Spec-Issue-Extra-Argument))
  (lambda [given expected strict?]
    (cond [(string=? given expected) null]
          [(and (not strict?)
                (string=? (string-trim given #:left? #false #:repeat? #true)
                          (string-trim expected #:left? #false #:repeat? #true))) null]
          [else (list (make-spec-extra-argument 'expected (spec-clear-string expected))
                      (make-spec-extra-argument 'given (spec-clear-string given)))])))

(define spec-mbytes-failures : (-> Bytes (Listof Bytes) Bytes (Option Natural) Boolean (Listof Spec-Issue-Extra-Argument))
  (lambda [given givens expected line-limit strict?]
    (define expectations (regexp-split #px"(\r|\n)+" expected))

    (if (= (length givens) (length expectations))
        (for/fold ([failures : (Listof Spec-Issue-Extra-Argument) null]
                   [failure-in-window? : Boolean #false]
                   #:result (cond [(null? failures) null]
                                  [(not failure-in-window?) (reverse failures)]
                                  [else (list* (make-spec-extra-argument 'expected (spec-clear-string expected expectations line-limit))
                                               (make-spec-extra-argument 'given (spec-clear-string given givens line-limit))
                                               (reverse failures))]))
                  ([g (in-list givens)]
                   [e (in-list expectations)]
                   [i (in-naturals 1)])
          (define failure (spec-bytes-failure g e strict? #false 'should-be '|  but-was|))
          (cond [(null? failure) (values failures failure-in-window?)]
                [(and line-limit (> (length failures) line-limit)) (values failures failure-in-window?)]
                [else (values (cons (make-spec-extra-argument (string->symbol (format "@line:~a" i))
                                                              (string-join (for/list : (Listof String) ([arg (in-list failure)])
                                                                             (format "~a: ~a"
                                                                               (spec-extra-argument-name arg)
                                                                               (spec-extra-argument-value arg)))
                                                                           "\n"))
                                    failures)
                              (or failure-in-window? (not line-limit) (<= i line-limit)))]))
        (list (make-spec-extra-argument 'reason "mismatched lines of output")
              (make-spec-extra-argument 'diff (- (length expectations) (length givens)))
              (make-spec-extra-argument 'expected (spec-clear-string expected expectations line-limit))
              (make-spec-extra-argument 'given (spec-clear-string given givens line-limit))))))

(define spec-exec-string-format : Spec-Issue-Format
  (lambda [v fallback]
    (cond [(string? v) (spec-expected-string v)]
          [(bytes? v) (spec-expected-string v)]
          [(list? v) (string-join (map spec-expected-string v) "\n------\n")]
          [(path? v) (path->string v)]
          [else (fallback v)])))

; TODO: should we display bytes in hexadecimal?
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

(define spec-clear-string : (case-> [Bytes -> Bytes]
                                    [String -> String]
                                    [Bytes Boolean -> (U Bytes String)]
                                    [Bytes (Listof Bytes) (Option Natural) -> String])
  (case-lambda
    [(s)
     (if (bytes? s)
         (regexp-replace* #px#"[[:blank:]]" s #"·")
         (regexp-replace*  #px"[[:blank:]]" s  "·"))]
    [(s binary?)
     (if (not binary?)
         (spec-clear-string (bytes->string/utf-8 s))
         (spec-clear-string s))]
    [(s lines limit)
     (if (or (not limit) (>= limit (length lines)))
         (spec-clear-string (bytes->string/utf-8 s))
         (string-join (for/list : (Listof String) ([given (in-list (take lines limit))])
                        (spec-clear-string (bytes->string/utf-8 given)))
                      "\n"))]))
