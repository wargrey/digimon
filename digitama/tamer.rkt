#lang racket

(provide (all-defined-out) quote-module-path)

(require rackunit)

(require racket/undefined)
(require syntax/location)

(require setup/xref)
(require setup/dirs)
(require scribble/xref)
(require scribble/manual)
(require scribble/core)

(require "monad.rkt")
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
    (let ([books (hash-ref handbook-stories books# null)])  ;;; Readme.md needs it stay here
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
  (lambda [projname]
    (format "https://github.com/digital-world/~a" projname)))
  
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

(define display-failure
  (lambda [result [color 'darkred] #:indent [headspace ""]]
    (define echo (λ [prefix v] (eechof #:fgcolor color "~a»» ~a: ~s~n" headspace prefix v)))
    (define recho (λ [prefix v] (eechof #:fgcolor color "~a»»» ~a~a~n" headspace prefix v)))
    (for ([info (in-list (exn:test:check-stack (test-failure-result result)))])
      (case (check-info-name info)
        [(params) (for ([param (in-list (map tr-if-path (check-info-value info)))]
                        [index (in-naturals 1)])
                    (echo (format "param:~a" index) param))]
        [(message) (let ([messages (call-with-input-string (tr-d (check-info-value info)) port->lines)])
                     (echo "message" (car messages))
                     (for-each (λ [msg] (recho (~a #:min-width 8) msg)) (cdr messages)))]
        [else (echo (check-info-name info)
                    (case (check-info-name info)
                      [(location) (tr-d (srcloc->string (apply srcloc (check-info-value info))))]
                      [(exception-message) (tr-d (check-info-value info))]
                      [else ((if (string? (check-info-value info)) tr-d tr-if-path) (check-info-value info))]))]))))
  
(define display-error
  (lambda [result [color 'darkred] #:indent [headspace0 ""]]
    (define errobj (test-error-result result))
    (define messages (call-with-input-string (tr-d (exn-message errobj)) port->lines))
    (eechof #:fgcolor color #:attributes '(inverse) "~a»» name: ~a~n" headspace0 (object-name errobj))
    (unless (null? messages)
      (define msghead " message: ")
      (define msgspace (~a #:min-width (sub1 (string-length msghead))))
      (eechof #:fgcolor color #:attributes '(inverse) "~a»»~a~a~n" headspace0 msghead (car messages))
      (for-each (λ [msg] (eechof #:fgcolor color #:attributes '(inverse) "~a»»»~a~a~n" headspace0 msgspace msg)) (cdr messages)))
    (for ([stack (in-list (continuation-mark-set->context (exn-continuation-marks errobj)))])
      (when (cdr stack)
        (define srcinfo (srcloc->string (cdr stack)))
        (unless (or (not srcinfo) (regexp-match? #px"^/" srcinfo))
          (eechof #:fgcolor 'darkgrey "~a»»»» ~a: ~a~n" headspace0
                  (tr-d srcinfo) (or (car stack) 'λ)))))))

(define display-skip
  (lambda [result [color 'darkblue] #:indent [headspace0 ""]]
    (define reason (test-skip-result result))
    (define messages (call-with-input-string (tr-d reason) port->lines))
    (unless (null? messages)
      (define msghead " SKIP: ")
      (define msgspace (~a #:min-width (sub1 (string-length msghead))))
      (eechof #:fgcolor color "~a»»~a~a~n" headspace0 msghead (car messages))
      (for-each (λ [msg] (eechof #:fgcolor color "~a»»»~a~a~n" headspace0 msgspace msg)) (cdr messages)))))

(define display-todo
  (lambda [result [color 'darkmagenta] #:indent [headspace0 ""]]
    (define reason (test-todo-result result))
    (define messages (call-with-input-string (tr-d reason) port->lines))
    (unless (null? messages)
      (define msghead " TODO: ")
      (define msgspace (~a #:min-width (sub1 (string-length msghead))))
      (eechof #:fgcolor color "~a»»~a~a~n" headspace0 msghead (car messages))
      (for-each (λ [msg] (eechof #:fgcolor color "~a»»»~a~a~n" headspace0 msgspace msg)) (cdr messages)))))
  
(define fold-test-suite
  (lambda [seed:datum testsuite #:fdown fdown #:fup fup #:fhere fhere]
    (parameterize ([current-custodian (make-custodian)]) ;;; Prevent test routines happen to shutdown the custodian.
      (monad-value ((monad-get tamer-seed-brief)
                    (foldts-test-suite (λ [testsuite name pre-action post-action seed]
                                         (define $exn (make-parameter undefined))
                                         (with-handlers ([void $exn]) ;; catch all
                                           (call-with-values pre-action void))
                                         ((>=> (>>= (monad-get tamer-seed-datum)
                                                    (λ [seed:datum] (monad-put set-tamer-seed-datum! (fdown name seed:datum))))
                                               (>>= (monad-get tamer-seed-namepath)
                                                    (λ [seed:namepath] (monad-put set-tamer-seed-namepath! (cons name seed:namepath))))
                                               (>>= (monad-get tamer-seed-exns)
                                                    (λ [seed:exns] (monad-put set-tamer-seed-exns! (cons ($exn) seed:exns)))))
                                          seed))
                                       (λ [testsuite name pre-action post-action seed children-seed]
                                         (with-handlers ([exn? (λ [e] (display-error (make-test-error (format "#:after ~a" name) e)))])
                                           (call-with-values post-action void))
                                         ((>=> (>>= (monad-get tamer-seed-datum)
                                                    (λ [children:datum] (monad-put set-tamer-seed-datum! (fup name children:datum children:datum))))
                                               (>>= (monad-get tamer-seed-namepath)
                                                    (λ [children:namepath] (monad-put set-tamer-seed-namepath! (cdr children:namepath))))
                                               (>>= (monad-get tamer-seed-exns)
                                                    (λ [children:exns] (monad-put set-tamer-seed-exns! (cdr children:exns)))))
                                          children-seed #| monad is a stateful structure, so seed === children-seed |#))
                                       (λ [testcase name action seed]
                                         (define-values (fixed-name fixed-action)
                                           (cond [(findf (lambda [e] (not (eq? e undefined))) (monad-value ((monad-get tamer-seed-exns) seed)))
                                                  => (lambda [e] (values (format "#:before ~a" name)
                                                                         (λ [] (raise e))))]
                                                 [(not name)
                                                  (values (format "(⧴ ~a)" (object-name struct:exn:fail:user))
                                                          (λ [] (raise-user-error "Unnamed Testcase!")))]
                                                 [else (values name action)]))
                                         (define fixed-namepath (cons fixed-name (monad-value ((monad-get tamer-seed-namepath) seed))))
                                         (define record (tamer-record-handbook fixed-namepath fixed-action))
                                         ((>=> (>>= (monad-get tamer-seed-datum)
                                                    (λ [seed:datum] (monad-put set-tamer-seed-datum! (fhere record seed:datum))))
                                               (>>= (monad-get tamer-seed-brief)
                                                    (λ [seed:summary] (monad-put set-tamer-seed-brief! (summary++ seed:summary record))))
                                               (monad-put set-tamer-seed-namepath! fixed-namepath))
                                          seed))
                                       ((monad-put set-tamer-seed-datum! seed:datum)
                                        (make-tamer-monad))
                                       testsuite))))))
  
(define prove
  (lambda [unit]
    (fold-test-suite #:fdown (λ [name seed:ordered]
                               (cond [(null? seed:ordered) (echof #:fgcolor 'darkgreen #:attributes '(dim underline) "λ ~a~n" (tr-d name))]
                                     [else (echof "~aλ~a ~a~n" (~a #:min-width (* (length seed:ordered) 2))
                                                  (string-join (map number->string (reverse seed:ordered)) ".") (tr-d name))])
                               (cons 1 seed:ordered))
                     #:fup   (λ [name maybe-children-if-monad children:ordered]
                               (cond [(< (length children:ordered) 2) null]
                                     [else (cons (add1 (cadr children:ordered))
                                                 (cddr children:ordered))]))
                     #:fhere (λ [result seed:ordered]
                               (define headline (format "~a~a  ~a - " (~a #:min-width (* (length seed:ordered) 2))
                                                  (~result result) (if (null? seed:ordered) 1 (car seed:ordered))))
                               (define headspace (~a #:min-width (string-length headline)))
                               (echof #:fgcolor (~fgcolor result) "~a~a~n" headline (tr-d (test-result-test-case-name result)))
                               (cond [(test-success? result) (void)]
                                     [(test-failure? result) (display-failure result #:indent headspace)]
                                     [(test-error? result) (display-error result #:indent headspace)]
                                     [(test-skip? result) (display-skip result #:indent headspace)]
                                     [(test-todo? result) (display-todo result #:indent headspace)]
                                     [else (error "RackUnit has new test result type supported!")])
                               (if (null? seed:ordered) null (cons (add1 (car seed:ordered)) (cdr seed:ordered))))
                     null ; seed:datum
                     unit)))
