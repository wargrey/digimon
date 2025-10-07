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
(define default-spec-exec-stdout-port : (Parameterof (Option Output-Port)) (make-parameter #false))
(define default-spec-exec-stderr-port : (Parameterof (Option Output-Port)) (make-parameter #false))
(define default-spec-exec-stdin-echo-lines : (Parameterof (Option Natural)) (make-parameter #false))
(define default-spec-exec-stdout-echo-lines : (Parameterof (Option Natural)) (make-parameter #false))

;;; TODO: we should treat bytes as in binary mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-spec-expectation (stdout [program : Path-String] [cmd-argv : (Vectorof String)] [/dev/stdin : (U String Bytes)] [outputs : Spec-Stdout-Expectation])
  (define /dev/stdout : (Option Output-Port) (default-spec-exec-stdout-port))

  (define raw : Bytes
    (fg-recon-exec/pipe #:/dev/stdin (if (bytes? /dev/stdin) (open-input-bytes /dev/stdin) (open-input-string /dev/stdin))
                        #:/dev/stdout /dev/stdout #:/dev/stderr (default-spec-exec-stderr-port)
                        #:pre-fork spec-timeout-start #:post-fork spec-timeout-start #:atexit spec-timeout-terminate
                        #:stdin-log-level (default-spec-exec-stdin-log-level)
                        #:stdin-echo-lines (and (string? /dev/stdin) (default-spec-exec-stdin-echo-lines))
                        #:stdout-echo-lines (default-spec-exec-stdout-echo-lines)
                        (default-spec-exec-operation-name) (if (string? program) (string->path program) program) cmd-argv))

  ;;; NOTE: Here we only check empty lines after the output in case that whitespaces are important
  (define eols (regexp-match #px#"(\r|\n)+$" raw))
  (when (and (not eols) /dev/stdout (> (bytes-length raw) 0))
    (newline /dev/stdout))

  (unless (null? outputs)
    (define given : Bytes (if eols (subbytes raw 0 (- (bytes-length raw) (bytes-length (car eols)))) raw))
    (define givens : (Listof Bytes) (if (regexp-match? #px"(\r|\n)" given) (regexp-split #px"(\r|\n)+" given) null))
    (define strict? : Boolean (default-spec-exec-strict?))

    (for/fold ([failures : (Option (Listof Spec-Issue-Extra-Argument)) #false]
               #:result (and failures
                             (let ([argin (spec-stdin-filter /dev/stdin (default-spec-exec-stdin-echo-lines))])
                               (parameterize ([default-spec-issue-extra-arguments (cons (make-spec-extra-argument '/dev/stdin argin) failures)]
                                              [default-spec-issue-ignored-arguments (list '/dev/stdin 'outputs)]
                                              
                                              ; the issue format should be combined with existing `default-spec-issue-format`
                                              ; nevertheless, it is not a big deal here
                                              [default-spec-issue-format (if (spec-string-output? outputs) spec-exec-string-format spec-exec-bytes-format)])
                                 (spec-misbehave)))))
              ([expected (if (pair? outputs) (in-list outputs) (in-value outputs))])
      (define failure (spec-fetch-failure expected given givens strict?))
      (cond [(not failure) failures]
            [(not failures) failure]
            [else (append failures failure)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-fetch-failure : (-> (U Spec-Stdout-Expect-Bytes Spec-Stdout-Expect-String) Bytes (Listof Bytes) Boolean
                                 (Option (Listof Spec-Issue-Extra-Argument)))
  (lambda [expected given givens strict?]
    (cond [(string? expected)
           (if (null? givens)
               (spec-string-failure (bytes->string/utf-8 given) expected strict?)
               (spec-mbytes-failures given givens (string->bytes/utf-8 expected) (default-spec-exec-stdout-echo-lines) strict?))]
          [(bytes? expected) (spec-bytes-failure given expected #true #true)] ; binary mode
          [else (spec-match-failure given expected)])))

(define spec-string-output? : (-> Spec-Stdout-Expectation Boolean)
  (lambda [outputs]
    (for/or : Boolean ([expected (if (pair? outputs) (in-list outputs) (in-value outputs))])
      (or (string? expected)
          (regexp? expected)))))

(define spec-match-failure : (-> Bytes (U Byte-Regexp Regexp) (Option (Listof Spec-Issue-Extra-Argument)))
  (lambda [given expected]
    (cond [(regexp-match? expected given) #false]
          [else (list (make-spec-extra-argument 'pattern expected)
                      (make-spec-extra-argument 'actual given))])))

(define spec-bytes-failure : (->* (Bytes Bytes Boolean Boolean) (Symbol Symbol Byte) (Option (Listof Spec-Issue-Extra-Argument)))
  (lambda [given expected strict? binary? [expected-name 'expected] [given-name 'given] [indent 0]]
    (cond [(bytes=? given expected) #false]
          [(and (not strict?)
                (bytes=? (regexp-replace #px"\\s+$" given #"")
                         (regexp-replace #px"\\s+$" expected #""))) #false]
          [else (list (make-spec-extra-argument expected-name (spec-clear-string expected binary?) #:indent indent)
                      (make-spec-extra-argument given-name (spec-clear-string given binary?) #:indent indent))])))

(define spec-string-failure : (-> String String Boolean (Option (Listof Spec-Issue-Extra-Argument)))
  (lambda [given expected strict?]
    (cond [(string=? given expected) #false]
          [(and (not strict?)
                (string=? (string-trim given #:left? #false #:repeat? #true)
                          (string-trim expected #:left? #false #:repeat? #true))) #false]
          [else (list (make-spec-extra-argument 'expected (spec-clear-string expected))
                      (make-spec-extra-argument 'given (spec-clear-string given)))])))

(define spec-mbytes-failures : (-> Bytes (Listof Bytes) Bytes (Option Natural) Boolean (Option (Listof Spec-Issue-Extra-Argument)))
  (lambda [given givens expected echo-lines strict?]
    (define expectations (regexp-split #px"(\r|\n)+" expected))

    (if (= (length givens) (length expectations))
        (for/fold ([failures : (Option (Listof Spec-Issue-Extra-Argument)) #false]
                   [failure-in-window? : Boolean #false]
                   #:result (cond [(not failures) #false]
                                  [(not failure-in-window?) (reverse failures)]
                                  [else (list* (make-spec-extra-argument 'expected (spec-clear-string expected expectations echo-lines))
                                               (make-spec-extra-argument 'given (spec-clear-string given givens echo-lines))
                                               (reverse failures))]))
                  ([g (in-list givens)]
                   [e (in-list expectations)]
                   [i (in-naturals 1)])
          (define failure (spec-bytes-failure g e strict? #false 'should-be '|  but-was|))
          (cond [(not failure) (values failures failure-in-window?)]
                [(and echo-lines (>= (length (or failures null)) echo-lines)) (values (or failures null) failure-in-window?)]
                [else (values (cons (make-spec-extra-argument (string->symbol (format "@line:~a" i))
                                                              (string-join (for/list : (Listof String) ([arg (in-list failure)])
                                                                             (format "~a: ~a"
                                                                               (spec-extra-argument-name arg)
                                                                               (spec-extra-argument-value arg)))
                                                                           "\n"))
                                    (or failures null))
                              (or failure-in-window? (not echo-lines) (<= i echo-lines)))]))
        (list (make-spec-extra-argument 'reason "mismatched lines of output")
              (make-spec-extra-argument 'diff (- (length expectations) (length givens)))
              (make-spec-extra-argument 'expected (spec-clear-string expected expectations echo-lines))
              (make-spec-extra-argument 'given (spec-clear-string given givens echo-lines))))))

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

(define spec-stdin-filter : (-> (U String Bytes) (Option Natural) (U Bytes String))
  (lambda [/dev/stdin echo-lines]
    (cond [(not echo-lines) /dev/stdin]
          [(bytes? /dev/stdin) /dev/stdin]
          [(zero? echo-lines) ""]
          [(not (regexp-match? #px"(\r|\n)" /dev/stdin)) /dev/stdin]
          [else (let ([lines (regexp-split #px"(\r|\n)+" /dev/stdin)])
                  (cond [(<= (length lines) echo-lines) /dev/stdin]
                        [else (string-join (take lines echo-lines) "\n")]))])))

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
