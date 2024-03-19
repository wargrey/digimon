#lang typed/racket/base

(provide (all-defined-out))

(require "../expectation.rkt")
(require "../prompt.rkt")
(require "../issue.rkt")

(require "../../exec.rkt")

(require "type.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-spec-exec-stdin-log-level : (Parameterof (Option Symbol)) (make-parameter #false))
(define default-spec-exec-stdout-port : (Parameterof (Option Output-Port)) (make-parameter #false))
(define default-spec-exec-stderr-port : (Parameterof (Option Output-Port)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-spec-expectation (stdout [bin : Path-String] [argv : (Vectorof String)] [stdin : (U String Bytes)] [expected : (U String Bytes False)])
  #:default-format spec-exec-format
  (define /dev/stdin : Input-Port (if (bytes? stdin) (open-input-bytes stdin) (open-input-string stdin)))
  (define /dev/stdout : (Option Output-Port) (default-spec-exec-stdout-port))
  
  (define raw : Bytes
    (fg-recon-exec/pipe #:/dev/stdin /dev/stdin #:stdin-log-level (default-spec-exec-stdin-log-level)
                        #:/dev/stdout /dev/stdout #:/dev/stderr (default-spec-exec-stderr-port)
                        'exec (if (string? bin) (string->path bin) bin) argv))

  (define eols (regexp-match #px#"[\r\n]+$" raw))
  (when (and (not eols) /dev/stdout)
    (newline /dev/stdout))

  (unless (not expected)
    (define given (if eols (subbytes raw 0 (- (bytes-length raw) (bytes-length (car eols)))) raw))
    (define given-val (if (bytes? expected) given (bytes->string/utf-8 given)))
    (define given-argv : Spec-Issue-Extra-Argument (vector 'given given-val #false))
    
    (parameterize ([default-spec-issue-extra-arguments (list given-argv)])
      (or (equal? given-val expected)
          (spec-misbehave)))))

(define-spec-expectation (match-stdout [bin : Path-String] [argv : (Vectorof String)] [stdin : (U String Bytes)] [expected : (Option Spec-Match-Datum)])
  #:default-format spec-exec-format
  (define /dev/stdin : Input-Port (if (bytes? stdin) (open-input-bytes stdin) (open-input-string stdin)))
  (define /dev/stdout : (Option Output-Port) (default-spec-exec-stdout-port))
  
  (define given : Bytes
    (fg-recon-exec/pipe #:/dev/stdin /dev/stdin #:stdin-log-level (default-spec-exec-stdin-log-level)
                        #:/dev/stdout /dev/stdout #:/dev/stderr (default-spec-exec-stderr-port)
                        'exec (if (string? bin) (string->path bin) bin) argv))

  (when (and /dev/stdout (not (regexp-match #px#"[\r\n]+$" given)))
    (newline /dev/stdout))
  
  (unless (not expected)
    (define given-val (if (bytes? expected) given (bytes->string/utf-8 given)))
    (define given-argv : Spec-Issue-Extra-Argument (vector 'given given-val #false))
    
    (parameterize ([default-spec-issue-extra-arguments (list given-argv)])
      (or (regexp-match? expected given-val)
          (spec-misbehave)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spec-exec-format : Spec-Issue-Format
  (lambda [v fallback]
    (cond [(bytes? v) (bytes->string/utf-8 v)]
          [(string? v) v]
          [(path? v) (path->string v)]
          [else (fallback v)])))
