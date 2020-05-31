#lang typed/racket/base

(provide main)

(require racket/path)

(require "wisemon/parameter.rkt")
(require "wisemon/racket.rkt")
(require "wisemon/phony.rkt")
(require "wisemon/spec.rkt")

(require "../digitama/system.rkt")
(require "../digitama/collection.rkt")

(require "../cmdopt.rkt")
(require "../echo.rkt")
(require "../debug.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option wisemon-flags #: Wisemon-Flags
  #:program 'wisemon
  #:args phony-target丨file-path

  #:usage-help "Carefully options are not exactly the same as those of GNU Make"
  #:once-each
  [[(#\B always-make)        #:=> make-always-run                               "Unconditionally make all targets"]
   [(#\i ignore-errors)      #:=> (λ _ (make-errno 0))                          "Do not tell shell there are errors"]
   [(#\n dry-run)            #:=> make-dry-run                                  "Just make without updating targets [Except *.rkt]"]
   [(#\s slient quiet)       #:=> (λ _ (current-output-port /dev/null))         "Just make and only display errors"]
   [(#\t touch)              #:=> make-just-touch                               "Touch targets instead of remaking them if existed"]
   [(#\d debug)              #:=> make-trace-log                                "Print lots of debug information"]
   [(#\v verbose)            #:=> make-set-verbose!                             "Build with verbose messages"]
   [(#\k keep-going)         #:=> make-keep-going                               "Keep going when some targets cannot be made"]
   [(#\j jobs)               #:=> (make-cmdopt-string->integer byte?) n #: Byte ["Allow ~1 jobs at once [0 for default: ~a]" (parallel-workers)]]]

  #:multi
  [[(#\W what-if assume-new) #:=> cmdopt-string->path FILE #: Path              "Consider ~1 to be infinitely new"]])

(define wisemon-display-help : (->* () ((Option Byte)) Void)
  (lambda [[retcode 0]]
    (define phonies : (Immutable-HashTable Symbol Wisemon-Phony) (wisemon-list-phony-goals))
    (define phony-helps : (Listof String)
      (for/list ([p (in-list '(all prove typeset dist mostlyclean clean distclean maintainer-clean))]
                 #:when (hash-has-key? phonies p))
        (format "    ~a : ~a" p (wisemon-phony-description (hash-ref phonies p)))))
    
    (display-wisemon-flags #:more-ps (cons "  where <phony-target> is one of" phony-helps)
                           #:exit retcode)))

(define wisemon-goal-partition : (-> (Listof String) (Values (Pairof Wisemon-Phony (Listof Wisemon-Phony)) (Listof Path)))
  (lambda [goals]
    (define-values (seinohp slaer)
      (for/fold ([seinohp : (Listof Wisemon-Phony) null]
                 [slaer : (Listof Path) null])
                ([g (in-list goals)])
        (cond [(path-get-extension g) (values seinohp (cons (simple-form-path g) slaer))]
              [(wisemon-phony-goal-ref (string->symbol g)) => (λ [[p : Wisemon-Phony]] (values (cons p seinohp) slaer))]
              [else (values seinohp (cons (simple-form-path g) slaer))])))
    (let ([phonies (reverse seinohp)])
      (values (if (pair? phonies) phonies (list (assert (wisemon-phony-goal-ref 'all))))
              (reverse slaer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-digimon : (-> (U Pkg-Info (Pairof Info-Ref (Listof Pkg-Info))) (Listof Path) (Pairof Wisemon-Phony (Listof Wisemon-Phony)) Byte)
  (lambda [info reals phonies]
    (cond [(pair? info) (for/fold ([retcode : Byte 0]) ([subinfo (in-list (cdr info))]) (make-digimon subinfo reals phonies))]
          [else (let ([zone (pkg-info-zone info)]
                      [info-ref (pkg-info-ref info)])
                  (parameterize ([current-make-real-targets reals]
                                 [current-digimon (pkg-info-name info)]
                                 [current-free-zone zone]
                                 [current-directory zone])
                    (dynamic-wind (λ [] (echof #:fgcolor 'green "Enter Digimon Zone: ~a~n" (current-digimon)))
                                  (λ [] (for/fold ([retcode : Byte 0])
                                                  ([phony (in-list phonies)])
                                          (parameterize ([current-make-phony-goal (wisemon-phony-name phony)])
                                            (with-handlers ([exn:break? (λ [[e : exn:break]] 130)]
                                                            [exn:fail? (λ [[e : exn]]
                                                                         (let ([/dev/stderr (open-output-string)])
                                                                           (parameterize ([current-error-port /dev/stderr])
                                                                             ((error-display-handler) (exn-message e) e))
                                                                           (eechof #:fgcolor 'red "~a" (get-output-string /dev/stderr))
                                                                           (make-errno)))])
                                              (file-or-directory-modify-seconds zone (current-seconds) void) ; Windows complains, no such directory
                                              ((wisemon-phony-make phony) (current-digimon) info-ref)
                                              retcode))))
                                  (λ [] (echof #:fgcolor 'green "Leave Digimon Zone: ~a~n" (current-digimon))))))])))

(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (make-restore-options!)
    (define-values (options λargv) (parse-wisemon-flags argument-list))

    (when (wisemon-flags-help? options)
      (wisemon-display-help))

    (let ([jobs (wisemon-flags-jobs options)])
      (when (and jobs (> jobs 0))
        (parallel-workers jobs)))

    (for ([wif (in-list (wisemon-flags-what-if options))])
      (when (file-exists? wif)
        (file-or-directory-modify-seconds wif (current-seconds) void)))

    (dynamic-wind (λ [] (thread racket-trace-log))
                  (λ [] (let ([digimons (collection-info)])
                          (cond [(not digimons) (eechof #:fgcolor 'red "fatal: not in a digimon zone.~n") (exit 1)]
                                [else (let-values ([(phonies reals) (wisemon-goal-partition (car (λargv)))])
                                        (exit (time-apply* (λ [] (make-digimon digimons reals phonies)))))])))
                  (λ [] (log-message (current-logger) 'info the-name "Job Done!" eof)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(main (current-command-line-arguments))
