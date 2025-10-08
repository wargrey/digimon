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
(require "../../minimal/dtrace.rkt")

(require "../../../struct.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Spec-Stdout-Expect-Bytes (U Bytes Byte-Regexp))
(define-type Spec-Stdout-Expect-String (U String Regexp))
(define-type Spec-Stdout-Expectation (Listof (U Spec-Stdout-Expect-Bytes Spec-Stdout-Expect-String)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct spec-exec.cfg : Spec-Exec.Cfg
  ([prompt : Symbol 'exec]
   [stdin-log-level : (Option Dtrace-Level) #false]
   [strict? : Boolean #true]
   [echo-limit : (Option Natural) #false]
   [error-limit : (Option Natural) #false]
   [stdout : (Option Output-Port) #false]
   [stderr : (Option Output-Port) #false]
   [extras : (Listof (Pairof Symbol Any)) null])
  #:transparent)

(define spec-optional-arguments : (-> Spec-Exec.Cfg (U String Bytes) (Listof Spec-Extra-Argument))
  (lambda [self stdin]
    (define stdio-limit (spec-exec.cfg-echo-limit self))
    (define error-limit (spec-exec.cfg-error-limit self))
    (define stdin-value (spec-stdin-filter (if (bytes? stdin) (bytes->string/utf-8 stdin) stdin) stdio-limit))

    (append (for/list : (Listof Spec-Extra-Argument) ([extra (in-list (spec-exec.cfg-extras self))])
              (make-spec-extra-argument (car extra) (cdr extra)))
            
            (filter spec-extra-argument?
                    (list (make-spec-extra-argument 'strict? (spec-exec.cfg-strict? self))
                          (and stdio-limit (make-spec-extra-argument 'echo-limit stdio-limit))
                          (and error-limit (make-spec-extra-argument 'error-limit error-limit))
                          (and (> (string-length stdin-value) 0)
                               (make-spec-extra-argument '/dev/stdin stdin-value)))))))

(define the-spec-exec-config : Spec-Exec.Cfg (make-spec-exec.cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
; the `expect-stdout` works for text based subprocesses.
; we still allow bytes as input arguments for efficiency.

;;; TODO: define `expect-binary-stdout`
(define-spec-expectation (stdout [program : Path-String] [argv : (Vectorof String)]
                                 [/dev/stdin : (U String Bytes)] [outputs : Spec-Stdout-Expectation]
                                 [config : (Option Spec-Exec.Cfg)])
  (define cfg : Spec-Exec.Cfg (or config the-spec-exec-config))
  (define /dev/stdout : (Option Output-Port) (spec-exec.cfg-stdout cfg))
  
  (define raw : Bytes
    (fg-recon-exec/pipe #:/dev/stdin (if (bytes? /dev/stdin) (open-input-bytes /dev/stdin) (open-input-string /dev/stdin))
                        #:/dev/stdout /dev/stdout #:/dev/stderr (spec-exec.cfg-stderr cfg)
                        #:pre-fork spec-timeout-start #:post-fork spec-timeout-start #:atexit spec-timeout-terminate
                        #:stdin-echo-lines (spec-exec.cfg-echo-limit cfg) #:stdout-echo-lines (spec-exec.cfg-echo-limit cfg)
                        #:stdin-log-level (spec-exec.cfg-stdin-log-level cfg)
                        (spec-exec.cfg-prompt cfg) (if (string? program) (string->path program) program) argv))

  ;;; NOTE: Here we only check empty lines after the output in case that whitespaces are important
  (define eols (regexp-match #px#"(\r|\n)+$" raw))
  (when (and (not eols) /dev/stdout (> (bytes-length raw) 0))
    (newline /dev/stdout))

  (unless (null? outputs)
    (define given : Bytes (if eols (subbytes raw 0 (- (bytes-length raw) (bytes-length (car eols)))) raw))
    (define givens : (Listof Bytes) (if (regexp-match? #px"(\r|\n)" given) (regexp-split #px"(\r|\n)+" given) null))
    (define strict? : Boolean (spec-exec.cfg-strict? cfg))

    (for/fold ([failures : (Option (Listof Spec-Extra-Argument)) #false]
               #:result (and failures
                             (parameterize ([default-spec-issue-ignored-arguments (list '/dev/stdin 'outputs 'config)]
                                            [default-spec-issue-extra-arguments (append (spec-optional-arguments cfg /dev/stdin) failures)]
                                            
                                            ; the issue format should be combined with existing `default-spec-issue-format`
                                            ; nevertheless, it is not a big deal here
                                            [default-spec-issue-format spec-exec-string-format])
                               (spec-misbehave))))
              ([expected (if (pair? outputs) (in-list outputs) (in-value outputs))])
      (define failure (spec-fetch-text-failure expected given givens strict? (spec-exec.cfg-echo-limit cfg) (spec-exec.cfg-error-limit cfg)))
      (cond [(not failure) failures]
            [(not failures) failure]
            [else (append failures failure)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-fetch-text-failure : (-> (U Spec-Stdout-Expect-Bytes Spec-Stdout-Expect-String) Bytes (Listof Bytes) Boolean
                                      (Option Natural) (Option Natural)
                                      (Option (Listof Spec-Extra-Argument)))
  (lambda [expected given givens strict? stdout-echo-lines error-limit]
    (cond [(string? expected)
           (if (null? givens)
               (spec-fetch-string-failure (bytes->string/utf-8 given) expected strict?)
               (spec-fetch-mbytes-failures given givens (string->bytes/utf-8 expected) stdout-echo-lines error-limit strict? #false))]
          [(bytes? expected)
           (if (null? givens)
               (spec-fetch-bytes-failure given expected strict? #false)
               (spec-fetch-mbytes-failures given givens expected stdout-echo-lines error-limit strict? #false))]
          [else (spec-fetch-match-failure given expected)])))

(define spec-fetch-match-failure : (-> Bytes (U Byte-Regexp Regexp) (Option (Listof Spec-Extra-Argument)))
  (lambda [given expected]
    (cond [(regexp-match? expected given) #false]
          [else (list (make-spec-extra-argument 'pattern expected)
                      (make-spec-extra-argument 'actual given))])))

(define spec-fetch-bytes-failure : (->* (Bytes Bytes Boolean Boolean) (Symbol Symbol Byte) (Option (Listof Spec-Extra-Argument)))
  (lambda [given expected strict? binary? [expected-name 'expected] [given-name 'given] [indent 0]]
    (cond [(bytes=? given expected) #false]
          [(and (not strict?)
                (bytes=? (regexp-replace #px"\\s+$" given #"")
                         (regexp-replace #px"\\s+$" expected #""))) #false]
          [else (list (make-spec-extra-argument expected-name (spec-clear-string expected binary?) #:indent indent)
                      (make-spec-extra-argument given-name (spec-clear-string given binary?) #:indent indent))])))

(define spec-fetch-string-failure : (-> String String Boolean (Option (Listof Spec-Extra-Argument)))
  (lambda [given expected strict?]
    (cond [(string=? given expected) #false]
          [(and (not strict?)
                (string=? (string-trim given #:left? #false #:repeat? #true)
                          (string-trim expected #:left? #false #:repeat? #true))) #false]
          [else (list (make-spec-extra-argument 'expected (spec-clear-string expected))
                      (make-spec-extra-argument 'given (spec-clear-string given)))])))

(define spec-fetch-mbytes-failures : (-> Bytes (Listof Bytes) Bytes (Option Natural) (Option Natural) Boolean Boolean (Option (Listof Spec-Extra-Argument)))
  (lambda [given givens expected echo-lines error-limit strict? binary?]
    (define expectations (regexp-split #px"(\r|\n)+" expected))

    (if (= (length givens) (length expectations))
        (for/fold ([failures : (Option (Listof Spec-Extra-Argument)) #false]
                   [failure-in-window? : Boolean #false]
                   #:result (cond [(not failures) #false]
                                  [(not failure-in-window?) (reverse failures)]
                                  [else (list* (make-spec-extra-argument 'expected (spec-clear-string expected expectations echo-lines))
                                               (make-spec-extra-argument 'given (spec-clear-string given givens echo-lines))
                                               (reverse failures))]))
                  ([g (in-list givens)]
                   [e (in-list expectations)]
                   [i (in-naturals 1)])
          (define failure (spec-fetch-bytes-failure g e strict? binary? 'should-be '|  but-was|))
          (cond [(not failure) (values failures failure-in-window?)]
                [(and error-limit (>= (length (or failures null)) error-limit)) (values (or failures null) failure-in-window?)]
                [else (values (cons (make-spec-extra-argument (string->symbol (format "@line:~a" i)) ; emoji make it hard to align the info
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
          [(boolean? v) (if (not v) "false" "true")]
          [(list? v) (string-join (map spec-expected-string v) "\n------\n")]
          [(path? v) (path->string v)]
          [else (fallback v)])))

(define spec-expected-string : (-> Any String)
  (lambda [v]
    (cond [(bytes? v) (bytes->string/utf-8 v)]
          [(string? v) v]
          [else (~s v)])))

(define spec-stdin-filter : (-> String (Option Natural) String)
  (lambda [/dev/stdin echo-lines]
    (cond [(not echo-lines) /dev/stdin]
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
