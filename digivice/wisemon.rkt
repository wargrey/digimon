#lang typed/racket/base

(provide main)

(require racket/path)

(require "wisemon/parameter.rkt")
(require "wisemon/racket.rkt")
(require "wisemon/phony.rkt")

(require "../digitama/collection.rkt")

(require "../digitama/minimal/system.rkt")
(require "../digitama/minimal/port.rkt")

(require "../dtrace.rkt")
(require "../cmdopt.rkt")
(require "../debug.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option wisemon-flags #: Wisemon-Flags
  #:program the-name
  #:args phony-target丨file-path

  #:usage-help "Carefully options are not exactly the same as those of GNU Make"
  #:once-each
  [[(#\B always-make)         #:=> make-always-run                       "Unconditionally make all targets"]
   [(#\C directory)           #:=> cmdopt-string->path DIR #: Path       "Change to directory ~1 before making"]
   [(#\i ignore-errors)       #:=> (λ _ (make-errno 0))                  "Do not tell shell there are errors"]
   [(#\n dry-run recon)       #:=> make-dry-run                          "Don't actually run any commands; just print them [Except *.rkt]"]
   [(#\s slient quiet)        #:=> (λ _ (current-output-port /dev/null)) "Just make and only display errors"]
   [(#\t touch)               #:=> make-just-touch                       "Touch targets instead of remaking them if existed"]
   [(#\d debug)               #:=> make-trace-log                        "Print lots of debug information"]
   [(#\v verbose)             #:=> make-set-verbose!                     "Build with verbose messages"]
   [(#\k keep-going)          #:=> make-keep-going                       "Keep going when some targets cannot be made"]
   [(#\j jobs)                #:=> cmdopt-string->byte n #: Byte         ["Allow ~1 jobs at once [0 for default: ~a]" (parallel-workers)]]]

  #:multi
  [[(#\W new-file assume-new) #:=> cmdopt-string->path FILE #: Path      "Consider ~1 to be infinitely new"]
   [(#\o old-file assume-old) #:=> cmdopt-string->path FILE #: Path      "Consider ~1 to be infinitely old and do not remaking them"]])

(define wisemon-display-help : (->* () ((Option Byte)) Void)
  (lambda [[retcode 0]]
    (define phonies : (Immutable-HashTable Symbol Wisemon-Phony) (wisemon-list-phony-goals))
    (define phony-helps : (Listof String)
      (for/list ([p (in-list ; Watch here to add new phony goals
                     '(all prove typeset cc cc-dbg dot dist mostlyclean clean distclean maintainer-clean))]
                 #:when (hash-has-key? phonies p))
        (format "    ~a : ~a" p (wisemon-phony-description (hash-ref phonies p)))))
    (define foreign-helps : (Listof String)
      (for/list : (Listof String) ([p (in-hash-values (wisemon-list-foreign-phony-goals))])
        (format "    ~a : ~a" (wisemon-phony-name p) (wisemon-phony-description p))))
    
    (display-wisemon-flags #:more-ps (cons "  where <phony-target> is one of"
                                           (cond [(null? foreign-helps) phony-helps]
                                                 [else (append phony-helps (list " ") foreign-helps)]))
                           #:exit retcode)))

(define wisemon-goal-partition : (-> (Listof String) (Values (Pairof Wisemon-Phony (Listof Wisemon-Phony)) (Listof Path)))
  (lambda [goals]
    (define-values (seinohp slaer)
      (for/fold ([seinohp : (Listof Wisemon-Phony) null]
                 [slaer : (Listof Path) null])
                ([g (in-list goals)])
        (cond [(path-get-extension g)
               (values seinohp (cons (simple-form-path g) slaer))]
              [(wisemon-phony-goal-ref (string->symbol g)) => (λ [[p : Wisemon-Phony]] (values (cons p seinohp) slaer))]
              [else (values seinohp (cons (simple-form-path g) slaer))])))
    (let ([phonies (reverse seinohp)])
      (values (if (pair? phonies) phonies (list (assert (wisemon-phony-goal-ref 'all))))
              (reverse slaer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-digimon : (-> (U False Pkg-Info (Pairof Info-Ref (Listof Pkg-Info))) (Listof Path) (Pairof Wisemon-Phony (Listof Wisemon-Phony)) Byte)
  (lambda [info reals phonies]
    (cond [(pair? info)
           (for/fold ([retcode : Byte 0])
                     ([subinfo (in-list (cdr info))])
             (make-digimon subinfo reals phonies))]
          [(pkg-info? info)
           (let ([zone (pkg-info-zone info)]
                 [info-ref (pkg-info-ref info)]
                 [tracer (thread (make-racket-log-trace))])
             (parameterize ([current-make-real-targets reals]
                            [current-digimon (pkg-info-name info)]
                            [current-free-zone zone]
                            [current-directory zone])
               (dtrace-notice "Enter Digimon Zone: ~a" (current-digimon))
               (begin0 (for/fold ([retcode : Byte 0])
                                 ([phony (in-list phonies)])
                         (parameterize ([current-make-phony-goal (wisemon-phony-name phony)]
                                        [current-custodian (make-custodian)])
                           (begin0 (with-handlers ([exn:break? (λ [[e : exn:break]] (newline) 130)]
                                                   [exn:fail? (λ [[e : exn]] (dtrace-exception e #:level 'fatal #:brief? (not (make-verbose))) (make-errno))])
                                     (cond [(wisemon-info-phony? phony) ((wisemon-info-phony-make phony) (current-digimon) info-ref)]
                                           [(wisemon-free-phony? phony) ((wisemon-free-phony-make phony) (current-digimon) info-ref)])
                                     retcode)
                                   (custodian-shutdown-all (current-custodian)))))
                       
                       (dtrace-datum-notice eof "Leave Digimon Zone: ~a" (current-digimon))
                       (thread-wait tracer))))]
          [else ; some phonies allow non-package projects
           (let ([tracer (thread (make-racket-log-trace))])
             (parameterize ([current-make-real-targets reals])
               (begin0 (for/fold ([retcode : Byte 0])
                                 ([phony (in-list phonies)])
                         (parameterize ([current-make-phony-goal (wisemon-phony-name phony)]
                                        [current-custodian (make-custodian)])
                           (dtrace-notice "Enter Free Phony: ~a" (current-make-phony-goal))
                           (begin0 (with-handlers ([exn:break? (λ [[e : exn:break]] (newline) 130)]
                                                   [exn:fail? (λ [[e : exn]] (dtrace-exception e #:level 'fatal #:brief? (not (make-verbose))) (make-errno))])
                                     (cond [(wisemon-free-phony? phony) ((wisemon-free-phony-make phony) (current-digimon) #false) retcode]
                                           [else (dtrace-fatal "fatal: not in a digimon zone") (make-errno)]))
                                   (custodian-shutdown-all (current-custodian))
                                   (dtrace-notice "Leave Free Phony: ~a" (current-make-phony-goal)))))
                       
                       (dtrace-datum-notice eof "")
                       (thread-wait tracer))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (make-restore-options!)
    (define-values (options λargv) (parse-wisemon-flags argument-list))

    (when (wisemon-flags-help? options)
      (wisemon-display-help))

    (let ([jobs (wisemon-flags-jobs options)])
      (when (and jobs (> jobs 0))
        (parallel-workers jobs)))

    (parameterize ([make-assumed-oldfiles (wisemon-flags-old-file options)]
                   [make-assumed-newfiles (wisemon-flags-new-file options)]
                   [current-logger /dev/dtrace])
      (let-values ([(phonies reals) (wisemon-goal-partition (λargv))])
        (exit (time* (make-digimon (collection-info (or (wisemon-flags-directory options)
                                                        (current-directory))
                                                    #:bootstrap? #true)
                                   reals phonies)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(main (current-command-line-arguments))
