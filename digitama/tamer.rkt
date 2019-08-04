#lang racket

(provide (all-defined-out) quote-module-path)

(require rackunit)

(require racket/sandbox)
(require racket/undefined)
(require syntax/location)

(require setup/xref)
(require setup/dirs)
(require scribble/core)
(require scribble/xref)
(require scribble/manual)
(require scribble/example)

(require "monad.rkt")
(require "../spec.rkt")
(require "../echo.rkt")
(require "../emoji.rkt")
(require "../format.rkt")
(require "../system.rkt")

(require (for-syntax racket/base))

(define tamer-zone (make-parameter #false))

(struct test-skip test-result (result #| : String |#))
(struct test-todo test-result (result #| : String |#))

(define skip
  (lambda [fmt . arglist]
    (raise (exn:fail:unsupported (apply format fmt arglist) (current-continuation-marks)))))

(define todo
  (lambda [fmt . arglist]
    (raise (exn:fail:unsupported (apply format fmt arglist) (current-continuation-marks)))))
  
(define tamer-story (make-parameter #false))
(define tamer-cite (make-parameter void))
(define tamer-cites (make-parameter void))
(define tamer-reference (make-parameter void))
  
(define-syntax (tamer-story->tag stx)
  (syntax-case stx []
    [(_ story-sexp)
     #'(let ([modpath (with-handlers ([exn? (λ [e] (quote-source-file))]) (cadr story-sexp))])
         (path->string (find-relative-path (digimon-path 'tamer) modpath)))]))

(define tamer-story->modpath
  (lambda [story-path]
    `(submod ,story-path tamer story)))

(define make-tamer-zone
  (lambda [zone]
    (define tamer-module
      (cond [(module-declared? zone #true) zone]
            [(let ([tamer.rkt (build-path (digimon-path 'tamer) "tamer.rkt")])
               (and (file-exists? tamer.rkt) tamer.rkt)) => values]
            [else (collection-file-path "tamer.rkt" "digimon")]))
    (dynamic-require tamer-module #false)
    (parameterize ([sandbox-namespace-specs (cons (thunk (module->namespace tamer-module)) null)])
      (make-base-eval #:pretty-print? #true))))

(struct summary (success failure error skip todo) #:prefab)

(define summary++
  (lambda [summary0 result]
    (summary (+ (summary-success summary0) (if (test-success? result) 1 0))
             (+ (summary-failure summary0) (if (test-failure? result) 1 0))
             (+ (summary-error summary0) (if (test-error? result) 1 0))
             (+ (summary-skip summary0) (if (test-skip? result) 1 0))
             (+ (summary-todo summary0) (if (test-todo? result) 1 0)))))
  
;;; Tamer Monad
(struct tamer-seed (datum brief namepath exns) #:mutable)

(define make-tamer-monad
  (lambda []
    (monad (void) (tamer-seed (void) (summary 0 0 0 0 0) null null))))

(define (monad-return value)
  (lambda [tamer-monad]
    (set-monad-value! tamer-monad value)
    tamer-monad))

(define (monad-put seed-set! val)
  (lambda [tamer-monad]
    (seed-set! (monad-state tamer-monad) val)
    tamer-monad))

(define (monad-get seed-ref)
  (lambda [tamer-monad]
    (let ([val (seed-ref (monad-state tamer-monad))])
      (set-monad-value! tamer-monad val)
      tamer-monad)))
;;; End Tamer Monad

(define-values (handbook-stories handbook-records) (values (make-hash) (make-hash)))

(define tamer-record-story
  (lambda [name unit]
    (define htag (tamer-story->tag (tamer-story)))
    (define units (hash-ref handbook-stories htag null))
    (unless (dict-has-key? units name)
      (hash-set! handbook-stories htag (cons (cons name unit) units)))
    (let ([books (hash-ref handbook-stories books# null)])  ;;; Readme.md needs it staying here
      (unless (member htag books) (hash-set! handbook-stories books# (cons htag books))))))

(define tamer-record-handbook
  (lambda [name:case«suites action]
    (define case-name (car name:case«suites))
    (hash-ref! (hash-ref! handbook-records (tamer-story) make-hash)
               (string-join name:case«suites " « ")
               (λ [] (let/ec return
                       (parameterize ([current-error-port (open-output-string '/dev/case/stderr)]
                                      [exit-handler (λ [v] (let* ([errmsg (string-trim (get-output-string (current-error-port)))]
                                                                  [routine (λ [] (with-check-info (('exitcode v)) (fail errmsg)))])
                                                             (return (run-test-case case-name routine))))])
                         (return (let ([result (run-test-case case-name action)])
                                   (cond [(and (test-error? result) (test-error-result result))
                                          => (λ [?] (cond [(not (exn:fail:unsupported? ?)) result]
                                                          [(let ([stack (continuation-mark-set->context (exn-continuation-marks ?))])
                                                             (and (not (null? stack)) (eq? (caar stack) 'todo)))
                                                           (test-todo case-name (exn-message ?))]
                                                          [else (test-skip case-name (exn-message ?))]))]
                                         [else result])))))))))

(define ~result
  (lambda [result]
    (case (object-name result)
      [(test-error) (string bomb#)]
      [(test-success) (string green-heart#)]
      [(test-failure) (string broken-heart#)]
      [(test-skip) (string arrow-heart#)]
      [(test-todo) (string growing-heart#)])))

(define ~fgcolor
  (lambda [result]
    (define rslt (if (string? result) result (~result result)))
    (cond [(string=? rslt (~result test-error)) 'darkred]
          [(string=? rslt (~result test-success)) 'lightgreen]
          [(string=? rslt (~result test-failure)) 'lightred]
          [(string=? rslt (~result test-skip)) 'lightblue]
          [(string=? rslt (~result test-todo)) 'lightmagenta])))

(define ~markdown
  (lambda [line]
    (define padding (λ [line] (make-string (- 72 (remainder (string-length (format "~a" line)) 72)) #\space)))
    (cond [(string? line) (literal (format "~a~a" line (padding line)))]
          [else (list line (literal (padding (car (element-content line)))))])))

(define ~url
  (lambda [digimon]
    (format "http://~a/~~~a:~a" (#%info 'pkg-domain) (#%info 'pkg-idun) digimon)))

(define ~github
  (lambda [projname username]
    (format "https://github.com/~a/~a" username projname)))
  
(define exn->test-case
  (lambda [name e]
    (delay-test (test-case (format "(~a ⧴ ~a)" name (object-name e))
                           (raise e) #| no thunk, make test-error |#))))

(define tr-d (λ [p] (string-replace p (path->string (digimon-path 'zone)) "")))
(define tr-if-path (λ [p] (if (path? p) (build-path (tr-d (format "~a" p))) p)))

(define readwrotten
  (make-readtable (current-readtable)
                  #\< 'dispatch-macro
                  (λ [< port [src #false] [line #false] [col #false] [pos #false]]
                    (fix (match (regexp-match #px"<?(procedure|path)?(:)?(.+?)?>" port)
                           [(list _ #"path" _ fname) (string->path (bytes->string/utf-8 fname))]
                           [(list _ _ #false #false) '(lambda _ ...)]
                           [(list _ #false #false <unprintable-value>) (string->symbol (format "~a?" <unprintable-value>))]
                           [(list _ _ _ (pregexp #px"function\\.rkt")) '(negate λ)]
                           [(list _ _ _ #"composed") '(compose λ ...)]
                           [(list _ _ _ #"curried") '(curry λ ...)]
                           [(list _ _ _ name) (with-handlers ([exn? (λ [ev] (list procedure-rename 'λ (exn:fail:contract:variable-id ev)))])
                                                (eval (string->symbol (bytes->string/utf-8 name))
                                                      (let ([mod (build-path (digimon-path 'tamer) "tamer.rkt")])
                                                        (dynamic-require mod #false)
                                                        (module->namespace mod))))])))))

(define fix
  (lambda [val]
    (cond [(or (procedure? val) (symbol? val))
           (let*-values ([(modpath) (build-path (digimon-path 'tamer) "tamer.rkt")]
                         [(export) (or (object-name val) val)]
                         [(xref) (load-collections-xref)]
                         [(tag) (xref-binding->definition-tag xref (list modpath export) #false)]
                         [(path anchor) (with-handlers ([exn? (λ _ (values #false #false))])
                                          (xref-tag->path+anchor xref tag #:external-root-url #false))])
             (or (and path anchor (racketvalfont (hyperlink (format "/~~:/~a#~a" (find-relative-path (find-doc-dir) path) anchor)
                                                            (symbol->string export))))
                 val))]
          [(vector? val) #| also for (struct? val) |#
           (vector-map fix val)]
          [(pair? val) #| also for lists |#
           (cons (fix (car val)) (fix (cdr val)))]
          [else val])))
    