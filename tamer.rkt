#lang racket

(provide (all-defined-out) skip todo)
(provide (all-from-out racket rackunit scribble/core scribble/manual scriblib/autobib scribble/example scribble/html-properties))
(provide (all-from-out "tongue.rkt" "system.rkt" "format.rkt" "echo.rkt"))

(require racket/sandbox)
(require rackunit)

(require scribble/core)
(require scribble/example)
(require scribble/manual)
(require scriblib/autobib)
(require scribble/html-properties)

(require (for-syntax syntax/parse))

(require (for-label racket))

(require "digitama/tamer.rkt")

(require "echo.rkt")
(require "emoji.rkt")
(require "tongue.rkt")
(require "system.rkt")
(require "format.rkt")
(require "collection.rkt")

(define #%handbook (seclink "tamer-book" (italic "Handbook")))

(define $out (open-output-bytes '/dev/tamer/stdout))
(define $err (open-output-bytes '/dev/tamer/stderr))
(define $? (make-parameter +NaN.0))

(define $shell
  (lambda [routine . arglist]
    (get-output-bytes $out #true)
    (get-output-bytes $err #true)
    ($? +NaN.0)
    (parameterize ([current-output-port $out]
                   [current-error-port $err]
                   [exit-handler $?])
      (apply routine arglist))))

(define make-tamer-zone
  (lambda [zone]
    (define tamer-module (if (module-declared? zone #true) zone (build-path (digimon-path 'tamer) "tamer.rkt")))
    (dynamic-require tamer-module #false)
    (parameterize ([sandbox-namespace-specs (cons (thunk (module->namespace tamer-module)) null)])
      (make-base-eval #:pretty-print? #true))))

(define-syntax (tamer-taming-start stx)
  (syntax-case stx [scribble +]
    [(_ scribble)
     #'(let ([modpath (quote-module-path)])
         (enter-digimon-zone!) ;; TODO: why it must enter again?
         (cond [(path? modpath) (tamer-story (tamer-story->modpath modpath))]
               [else (let ([story (tamer-story->modpath (cadr modpath))])
                       (tamer-story story)
                       (tamer-zone (make-tamer-zone story)))]))]
    [(_)
     #'(begin (tamer-taming-start scribble)
              (module+ main (call-as-normal-termination tamer-prove)))]))

(define-syntax (define-bib stx)
  (syntax-parse stx #:literals []
    [(_ id bib-args ...)
     #'(define id (in-bib (make-bib bib-args ...) (format ":~a" 'id)))]))

(define ~cite
  (lambda [bib #:same-author? [same? #false] . bibs]
    (if (false? same?)
        (apply (tamer-cites) bib bibs)
        (apply (tamer-cite) bib bibs))))

(define register-handbook-finalizer
  (lambda [atexit/0]
    (void ((curry plumber-add-flush! (current-plumber))
           (λ [this] (with-handlers ([void void])
                       (plumber-flush-handle-remove! this)
                       (void (atexit/0))))))))

(define-syntax (handbook-title stx)
  (syntax-parse stx #:literals []
    [(_ pre-contents ...)
     #'(begin (enter-digimon-zone!)
              (title #:tag "tamer-book" #:version (format "~a[~a]" (version) (#%info 'version (const "Baby")))
                     #:style (let* ([tamer.css (collection-file-path "tamer.css" "digimon" "stone")]
                                    [this.css (build-path (digimon-path 'stone) "tamer.css")])
                               (make-style #false (map make-css-addition (remove-duplicates (filter file-exists? (list tamer.css this.css))))))
                     (if (symbol=? (object-name (current-input-port)) '/dev/null)
                         (list (hyperlink (~url (current-digimon)) (string house-garden#)))
                         (list (hyperlink (~github (current-digimon)) (string house-garden#))))
                     (let ([contents (list pre-contents ...)])
                       (cond [(pair? contents) contents]
                             [else (list (literal (speak 'tamer-handbook) ":") ~
                                         (current-digimon))])))
              (apply author (map ~a (#%info 'pkg-authors (const (list (#%info 'pkg-idun)))))))]))

(define-syntax (handbook-story stx)
  (syntax-parse stx #:literals []
    [(_ (~optional (~seq #:style s:expr)) contents ...)
     #`(begin (tamer-taming-start scribble)
              (define-cite ~cite ~cites ~reference #:style number-style)
              (tamer-reference ~reference)
              (tamer-cites ~cites)
              (tamer-cite ~cite)
              (title #:tag (tamer-story->tag (tamer-story))
                     #:style #,(attribute s)
                     (literal (speak 'handbook-story) ":") ~ contents ...))]))

(define handbook-scenario
  (lambda [#:tag [tag #false] #:style [style #false] . pre-contents]
    (section #:tag tag #:style style
             (literal (speak 'handbook-scenario) ":") ~ pre-contents)))

(define handbook-reference
  (lambda []
    (list ((tamer-reference) #:tag (format "~a-reference" (path-replace-extension (tamer-story->tag (tamer-story)) ""))
                             #:sec-title (speak 'handbook-reference))
          (let ([zone-snapshots (filter-not false? (list (tamer-zone)))])
            (make-traverse-block (thunk* (for-each close-eval zone-snapshots))))
          (tamer-story #false))))

(define handbook-appendix
  (let ([entries (list (bib-entry #:key      "Racket"
                                  #:title    "Reference: Racket"
                                  #:author   (authors "Matthew Flatt" "PLT")
                                  #:date     "2010"
                                  #:location (techrpt-location #:institution "PLT Design Inc." #:number "PLT-TR-2010-1")
                                  #:url      "https://racket-lang.org/tr1")
                       (bib-entry #:key      "Scribble"
                                  #:title    "The Racket Documentation Tool"
                                  #:author   (authors "Matthew Flatt" "Eli Barzilay")
                                  #:url      "https://docs.racket-lang.org/scribble/index.html")
                       (bib-entry #:key      "Rackunit"
                                  #:title    "Rackunit: Unit Testing"
                                  #:author   (authors "Noel Welsh" "Ryan Culpepper")
                                  #:url      "https://docs.racket-lang.org/rackunit/index.html")
                       (bib-entry #:key      "LP:WEB"
                                  #:title    "Literate Programming"
                                  #:author   (authors "Donald E. Knuth")
                                  #:date     "1984"
                                  #:location (journal-location "The Computer Journal" #:number "10.1093/comjnl/27.2.97")
                                  #:url      "http://www.literateprogramming.com/knuthweb.pdf")
                       (bib-entry #:key      "LP:Issues"
                                  #:title    "Literate Programming - Issues and Problems"
                                  #:author   (authors "Kurt Nørmark")
                                  #:date     "1998"
                                  #:location (dissertation-location #:institution "Department of Computer Science Aalborg University"
                                                                    #:degree "Lektor")
                                  #:url      "http://people.cs.aau.dk/~normark/litpro/issues-and-problems.html"))])
    (lambda [#:index? [index? #true] . bibentries]
      (define appendix-style (make-style 'index '(grouper)))
      ((curry filter-not void?)
       (list (part #false '((part "handbook-appendix")) (list (speak 'handbook-appendix)) (make-style 'index '(unnumbered reverl)) null null
                   (list (part #f '((part "handbook-digimon")) (list (speak 'handbook-digimon)) appendix-style null null null)
                         (struct-copy part (apply bibliography #:tag "handbook-bibliography" (append entries bibentries))
                                      [style appendix-style]
                                      [title-content (list (speak 'handbook-bibliography))])))
             (unless (false? index?)
               (struct-copy part (index-section #:tag "handbook-index")
                            [title-content (list (speak 'handbook-index))])))))))

(define handbook-smart-table
  (lambda []
    (make-traverse-block
     (λ [get set]
       (if (false? (member 'markdown (get 'scribble:current-render-mode '(html))))
           (table-of-contents)
           (make-delayed-block
            (λ [render% pthis _]
              (define-values (/dev/tamer/stdin /dev/tamer/stdout) (make-pipe #false '/dev/tamer/stdin '/dev/tamer/stdout))
              (parameterize ([current-input-port /dev/tamer/stdin]
                             [current-error-port /dev/tamer/stdout]
                             [current-output-port /dev/tamer/stdout]
                             [tamer-story #false])
                (define summary? (make-parameter #false))
                (thread (thunk (dynamic-wind collect-garbage
                                             tamer-prove
                                             (thunk (close-output-port /dev/tamer/stdout)))))
                (para (filter-map (λ [line] (and (not (void? line)) (map ~markdown (if (list? line) line (list line)))))
                                  (for/list ([line (in-port read-line)])
                                    (cond [(regexp-match #px"^λ\\s+(.+)" line)
                                           => (λ [pieces] (format "> + ~a~a" books# (list-ref pieces 1)))]
                                          [(regexp-match #px"^(\\s+)λ\\d+\\s+(.+?.rktl?)\\s*$" line)
                                           ; markdown listitem requires at least 1 char after "+ " before
                                           ; breaking line if "[~a](~a)" is longer then 72 chars.
                                           => (λ [pieces] (match-let ([(list _ indt ctxt) pieces])
                                                            (list (format ">   ~a+ ~a" indt open-book#)
                                                                  (hyperlink (format "~a/~a" (~url (current-digimon)) ctxt) ctxt))))]
                                          [(regexp-match #px"^(\\s+)λ\\d+(.\\d)*\\s+(.+?)\\s*$" line)
                                           => (λ [pieces] (format ">   ~a+ ~a~a" (list-ref pieces 1) bookmark# (list-ref pieces 3)))]
                                          [(regexp-match #px"^$" line) (summary? #true)]
                                          [(summary?) (parameterize ([current-output-port /dev/stdout])
                                                        (echof "~a~n" line
                                                               #:fgcolor (match line
                                                                           [(regexp #px" 100.00% Okay") 'lightgreen]
                                                                           [(regexp #px"( [^0]|\\d\\d) error") 'darkred]
                                                                           [(regexp #px"( [^0]|\\d\\d) failure") 'lightred]
                                                                           [(regexp #px"( [^0]|\\d\\d) TODO") 'lightmagenta]
                                                                           [(regexp #px"( [^0]|\\d\\d) skip") 'lightblue]
                                                                           [_ 'lightcyan])))]))))))))))))

(define-syntax (define-tamer-suite stx)
  (syntax-parse stx
    [(_ varid name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) units ...)
     #`(tamer-record-story 'varid (test-suite name
                                              #:before #,(or (attribute setup) #'void)
                                              #:after #,(or (attribute teardown) #'void)
                                              units ...))]))

(define-syntax (define-tamer-case stx)
  (syntax-parse stx
    [(_ varid name bodys ...)
     #'(tamer-record-story 'varid (delay-test (test-spec name bodys ...)))]))

(define-syntax (test-spec stx)
  (syntax-parse stx
    [(_ name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) checks ...)
     #`(test-case name (around (#,(or (attribute setup) #'void))
                               checks ...
                               (#,(or (attribute teardown) #'void))))]))

(define-syntax (tamer-action stx)
  (syntax-case stx []
    [(_ s-exps ...)
     #'(let ([story-snapshot (tamer-story)]
             [zone-snapshot (tamer-zone)])
         (make-traverse-block
          (thunk* (parameterize ([tamer-story story-snapshot]
                                 [tamer-zone zone-snapshot])
                    (examples #:label #false
                              #:eval (tamer-zone)
                              s-exps ...)))))]))

(define tamer-require
  (lambda [name]
    (define htag (tamer-story->tag (tamer-story)))
    (define units (hash-ref handbook-stories htag null))
    (with-handlers ([exn? (λ [e] (let ([story (exn->test-case 'tamer-require e)])
                                   (hash-set! handbook-stories htag (cons (cons name story) units))
                                   story))])
      (dict-ref units name (thunk (raise (make-exn:fail:contract:variable (format "'~a has not yet defined!" name)
                                                                          (current-continuation-marks) name)))))))

(define tamer-story->modpath
  (lambda [story-path]
    `(submod ,story-path tamer story)))

(define tamer-prove
  (lambda []
    (define suite (if (module-path? (tamer-story))
                      (let ([htag (tamer-story->tag (tamer-story))])
                        (and (dynamic-require (tamer-story) #false)
                             (hash-has-key? handbook-stories htag)
                             (make-test-suite htag (reverse (map cdr (hash-ref handbook-stories htag))))))
                      (or (zero? (hash-count handbook-stories)) ; no story ==> no :books:
                          (let ([href (curry hash-ref handbook-stories)])
                            (make-test-suite "Behaviors and Features"
                                             (for/list ([unit (in-list (reverse (href books#)))])
                                               (make-test-suite unit (reverse (map cdr (href unit))))))))))
    (or (and (test-suite? suite)
             (let-values ([(brief-box cpu0 real0 gc0) (time-apply prove (list suite))])
               (define-values (success failure error skip todo real cpu-gc gc cpu)
                 (apply values (list* (summary-success (car brief-box))
                                      (summary-failure (car brief-box))
                                      (summary-error (car brief-box))
                                      (summary-skip (car brief-box))
                                      (summary-todo (car brief-box))
                                      (map (compose1 (curry ~r #:precision '(= 3)) (curry * 0.001))
                                           (list real0 (- cpu0 gc0) gc0 cpu0)))))
               (define population (+ success failure error skip todo))
               (and (positive? population)
                    (let ([echo (curry echof #:fgcolor 'lightcyan)])
                      (echo "~nFinished in ~a wallclock seconds (~a task + ~a gc = ~a CPU)." real cpu-gc gc cpu)
                      (echo "~n~a, ~a, ~a, ~a, ~a, ~a% Okay.~n" (~n_w population "example") (~n_w failure "failure")
                            (~n_w error "error") (~n_w skip "skip") (~n_w todo "TODO")
                            (~r  #:precision '(= 2) (/ (* (+ success skip) 100) population)))
                      (+ failure error)))))
        (and (echof #:fgcolor 'darkcyan "~nNo particular example!~n") 0))))

(define tamer-smart-summary
  (lambda []
    (define story-snapshot (tamer-story))
    (define raco-setup-forget-my-digimon (current-digimon))
    (make-traverse-block
     (λ [get set]
       (if (member 'markdown (get 'scribble:current-render-mode '(html)))
           (para (literal "---"))
           (make-delayed-block
            (λ [render% pthis infobase]
              (define get (curry hash-ref (collect-info-fp (resolve-info-ci infobase))))
              (define (story-ref htag)
                (filter-map (λ [scnr] (hash-ref (hash-ref (get 'scenario make-hash) htag make-hash) (cdr scnr) #false))
                            (reverse (hash-ref handbook-stories htag null))))
              (parameterize ([current-digimon raco-setup-forget-my-digimon])
                (nested #:style (make-style "boxed" null)
                        (filebox (if (module-path? story-snapshot)
                                     (italic (seclink "tamer-book" (string open-book#)) ~
                                             (~a "Behaviors in " (tamer-story->tag story-snapshot)))
                                     (italic (string books#) ~
                                             (~a "Behaviors of " (current-digimon))))
                                 (let ([base (cond [(module-path? story-snapshot) (story-ref (tamer-story->tag story-snapshot))]
                                                   [else (for/list ([story (in-list (reverse (hash-ref handbook-stories books# null)))])
                                                           (cons story (with-handlers ([exn:fail:contract? (const null)])
                                                                         (apply append (map cdr (story-ref story))))))])])
                                   (define statuses (map ~result (list test-error test-failure test-todo test-skip)))
                                   (define items (for/list ([spec (in-list base)])
                                                   ;;; also see (tamer-note)
                                                   (define-values (local# desc)
                                                     (cond [(string? (car spec)) (values bookmark# (car spec))] ;;; testsuite
                                                           [else (values page# (unbox (car spec)))])) ;;; toplevel testcase
                                                   (define status (car (or (ormap (curryr member (cdr spec)) statuses)
                                                                           (list (~result struct:test-success)))))
                                                   (if (module-path? story-snapshot)
                                                       (list (elem (italic (string local#)) ~
                                                                   (elemref desc (racketkeywordfont (literal desc))))
                                                             (elemref desc status #:underline? #false))
                                                       (let ([head (~a desc #:width 64 #:pad-string "." #:limit-marker "......")]
                                                             [stts (make-parameter status)])
                                                         (echof #:fgcolor 'lightyellow head)
                                                         (echof #:fgcolor (~fgcolor status) "~a~n" status)
                                                         (for ([msg (in-list (cdr spec))])
                                                           (cond [(and (member msg statuses) msg)
                                                                  => stts]
                                                                 [(and (regexp-match? #px"»»" msg) msg)
                                                                  => (curry eechof #:fgcolor 'darkgrey "~a~n")]
                                                                 [(and (string? (car spec)) msg)
                                                                  => (curry eechof  "~a~n"
                                                                            #:fgcolor (~fgcolor (stts))
                                                                            #:attributes (cond [(string=? (stts) (~result test-error)) '(inverse)]
                                                                                               [else null]))]))
                                                         (list (elem (italic (string book#)) ~ (secref (car spec)))
                                                               (seclink (car spec) status #:underline? #false))))))
                                   (match-define (list success failure error skip todo reals gcs cpus)
                                     (for/list ([meta (in-list (list 'success 'failure 'error 'skip 'todo 'real 'gc 'cpu))])
                                       (define pool (get meta make-hash))
                                       (if (module-path? story-snapshot)
                                           (hash-ref pool story-snapshot 0)
                                           (foldl + 0 (hash-values pool)))))
                                   (match-define (list real cpu-gc gc cpu)
                                     (map (compose1 (curry ~r #:precision '(= 3)) (curry * 0.001))
                                          (list reals (- cpus gcs) gcs cpus)))
                                   (define briefs
                                     (let ([population (+ success failure error skip todo)])
                                       (if (zero? population)
                                           (list "No particular test!")
                                           (list (format "~a% tests successful."
                                                   (~r #:precision '(= 2) (/ (* (+ success skip) 100) population)))
                                                 (format "~a, ~a, ~a, ~a, ~a, ~a."
                                                   (~w=n (length base) (if story-snapshot "Scenario" "Story"))
                                                   (~w=n population "Test") (~w=n failure "Failure") (~w=n error "Error")
                                                   (~w=n skip "Skip") (~w=n todo "TODO"))
                                                 (format "~a wallclock seconds (~a task + ~a gc = ~a CPU)."
                                                   real cpu-gc gc cpu)))))
                                   (unless (module-path? story-snapshot)
                                     (for ([brief (in-list (cons "" briefs))])
                                     (echof #:fgcolor 'lightcyan "~a~n" brief)))
                                   (let ([summaries (add-between (map racketoutput briefs) (linebreak))])
                                     (cond [(null? items) summaries]
                                           [else (cons (tabular items #:style 'boxed #:column-properties '(left right))
                                                       summaries)])))))))))))))

(define tamer-note
  (lambda [unit-vars . notes]
    (define story-snapshot (tamer-story))
    (define raco-setup-forget-my-digimon (current-digimon))
    (make-traverse-block
     (λ [get set]
       (parameterize ([current-digimon raco-setup-forget-my-digimon])
         (define htag (tamer-story->tag story-snapshot))
         (unless (get 'scenario #false) (set 'scenario (make-hash)))
         (define scenarios (hash-ref! (get 'scenario make-hash) htag make-hash))
         (margin-note (unless (null? notes) (append notes (list (linebreak) (linebreak))))
                      (for/list ([unit-var (in-list (if (list? unit-vars) unit-vars (list unit-vars)))])
                        (define-values (/dev/tamer/stdin /dev/tamer/stdout) (make-pipe #false '/dev/tamer/stdin '/dev/tamer/stdout))
                        (parameterize ([tamer-story story-snapshot]
                                       [current-readtable readwrotten]
                                       [current-input-port /dev/tamer/stdin])
                          (define unit (tamer-require unit-var))
                          (define unit-spec (make-parameter null))
                          (thread (λ _ (dynamic-wind (thunk (collect-garbage))
                                                     (thunk (parameterize ([current-error-port /dev/tamer/stdout]
                                                                           [current-output-port /dev/tamer/stdout])
                                                              (define-values (brief cpu real gc) (time-apply prove (list unit)))
                                                              (define-values (success failure error skip todo)
                                                                (values (summary-success (car brief))
                                                                        (summary-failure (car brief))
                                                                        (summary-error (car brief))
                                                                        (summary-skip (car brief))
                                                                        (summary-todo (car brief))))
                                                              (for ([meta (in-list (list 'cpu 'real 'gc 'success 'failure 'error 'skip 'todo))]
                                                                    [delta (in-list (list cpu real gc success failure error skip todo))])
                                                                (unless (get meta #false) (set meta (make-hash)))
                                                                (define pool (get meta make-hash))
                                                                (hash-set! pool (tamer-story) (+ (hash-ref pool (tamer-story) 0) delta)))
                                                              (if (zero? (+ failure error))
                                                                  (printf "~n~a wall seconds.~n" (~r (/ real 1000.0) #:precision '(= 3)))
                                                                  (printf "~n~a ~a~n" (~n_w failure "failure") (~n_w error "error")))))
                                                     (thunk (close-output-port /dev/tamer/stdout)))))
                          ((compose1 (curryr add-between (linebreak)) (curry filter-not void?))
                           (for/list ([line (in-port read-line)])
                             (cond [(regexp-match #px"^λ\\s+(.+)" line)
                                    => (λ [pieces] (let ([ctxt (list-ref pieces 1)])
                                                     (unit-spec (list ctxt)) ; Testsuite
                                                     (nonbreaking (racketmetafont (italic (string open-book#)) ~
                                                                                  (elemtag ctxt (literal ctxt))))))]
                                   [(regexp-match #px"^\\s+λ(\\d+(.\\d)*)\\s+(.+?)\\s*$" line)
                                    => (λ [pieces] (nonbreaking (racketoutput (italic (string bookmark#)) ~
                                                                              (larger (literal (list-ref pieces 3))))))]
                                   [(regexp-match #px"^(\\s*)(.+?)\\s+(\\d+) - (.+?)\\s*$" line)
                                    => (λ [pieces] (match-let ([(list _ spc stts idx ctxt) pieces])
                                                     (when (string=? spc "") (unit-spec (list (box ctxt)))) ; Toplevel testcase
                                                     (unless (string=? stts (~result struct:test-success))
                                                       (unit-spec (cons ctxt (cons stts (unit-spec)))))
                                                     (nonbreaking ((if (string=? spc "") (curry elemtag ctxt) elem)
                                                                   stts (racketkeywordfont ~ (italic idx))
                                                                   (racketcommentfont ~ (literal ctxt))))))]
                                   [(regexp-match #px"^\\s*»» (.+?)?:?\\s+(.+?)\\s*$" line)
                                    => (λ [pieces] (match-let ([(list _ key val) pieces])
                                                     (unit-spec (cons (string-trim line) (unit-spec)))
                                                     (cond [(member key '("message"))
                                                            (elem #:style (make-style #false (list (make-color-property (list 128 128 128))))
                                                                  (string backhand#) ~ (italic (literal val)))]
                                                           [(member key '("SKIP" "TODO"))
                                                            (elem #:style (make-style #false (list (make-color-property (list 128 128 128))))
                                                                  (string backhand#) ~ (racketparenfont key ":") ~ (italic (literal val)))]
                                                           [(member key '("exn" "exception"))
                                                            (elem (racketvalfont (string macroscope#)) ~
                                                                  (racket #,(let ([ev (fix (read (open-input-string val)))]
                                                                                  [tr (curryr call-with-input-string read-line)])
                                                                              (vector-set! ev 1 (tr (vector-ref ev 1))) ev)))]
                                                           [(regexp-match? #px"param:\\d+" key)
                                                            (elem (racketvalfont (string crystal-ball#)) ~
                                                                  (racket #,(fix (read (open-input-string val)))))])))]
                                   [(regexp-match #px"^\\s*»»» \\s+(expected|given|received):\\s+(.+?)\\s*$" line)
                                    ; only for errors that have multilined messages.
                                    => (λ [pieces] (and (unit-spec (cons (string-trim line) (unit-spec)))
                                                        (elem (racketvalfont (string paw#)) ~
                                                              (let ([message (list-ref pieces 2)])
                                                                (racket #,(fix (with-handlers ([exn? (λ _ message)])
                                                                                 (read (open-input-string message)))))))))]
                                   [(regexp-match #px"^$" line) (hash-set! scenarios unit (reverse (unit-spec)))]
                                   [(hash-has-key? scenarios unit)
                                    (nonbreaking (elem (string pin#) ~ ((if (regexp-match? #px"error" line) racketerror racketresultfont) line) ~
                                                       (seclink (tamer-story->tag (tamer-story)) ~
                                                                (string house-garden#) (smaller (string cat#)))))])))))))))))
  
(define tamer-racketbox
  (lambda [path #:line-start-with [line0 1]]
    (define story-snapshot (tamer-story))
    (define raco-setup-forget-my-digimon (current-digimon))
    (make-traverse-block
     (thunk* (parameterize ([tamer-story story-snapshot]
                            [current-digimon raco-setup-forget-my-digimon])
               (define /path/file (simplify-path (if (symbol? path) (dynamic-require/expose (tamer-story) path) path)))
               (nested #:style (make-style "boxed" null)
                       (filebox (italic (string memo#) ~ (path->string (tr-if-path /path/file)))
                                (codeblock #:line-numbers line0 #:keep-lang-line? (> line0 0) ; make sure line number starts from 1
                                           (string-trim (file->string /path/file) #:left? #false #:right? #true)))))))))

(define tamer-racketbox/region
  (lambda [path #:pxstart [pxstart #px"\\S+"] #:pxend [pxend #false] #:greedy? [greedy? #false]]
    (define story-snapshot (tamer-story))
    (define raco-setup-forget-my-digimon (current-digimon))
    (make-traverse-block
     (thunk* (parameterize ([tamer-story story-snapshot]
                            [current-digimon raco-setup-forget-my-digimon])
               (define /path/file (simplify-path (if (symbol? path) (dynamic-require/expose (tamer-story) path) path)))
               (define-values (line0 contents)
                 (call-with-input-file* /path/file
                   (lambda [in.rkt]
                     (let read-next ([lang #false] [line0 0] [contents null] [end 0])
                       (define line (read-line in.rkt))
                       ; if it does not work, please check whether your pxstart and pxend are pregexps first.
                       (cond [(eof-object? line)
                              (if (zero? end)
                                  (values line0 (cons lang (reverse contents)))
                                  (values line0 (cons lang (take (reverse contents) end))))]
                             [(and (regexp? pxend) (false? (null? contents)) (regexp-match? pxend line))
                              ; end line itself is excluded
                              (if (false? greedy?)
                                  (values line0 (cons lang (reverse contents)))
                                  (read-next lang line0 (cons line contents) (length contents)))]
                             [(regexp-match? #px"^#lang .+$" line)
                              (read-next line (add1 line0) contents end)]
                             [(and (string? lang) (null? contents) (regexp-match pxstart line))
                              (read-next lang line0 (list line) end)]
                             [(false? (null? contents)) ; still search the end line greedily
                              (read-next lang line0 (cons line contents) end)]
                             [else ; still search for the first line
                              (read-next lang (add1 line0) contents end)])))))
               (nested #:style (make-style "boxed" null)
                       (filebox (italic (string memo#) ~ (path->string (tr-if-path /path/file)))
                                (codeblock #:line-numbers line0 #:keep-lang-line? #false
                                           (string-trim #:left? #false #:right? #true ; remove tail blank lines 
                                                        (string-join contents (string #\newline)))))))))))

(module* typed typed/racket
  (provide (all-defined-out))
  (provide (all-from-out typed/rackunit))
  (provide (all-from-out "emoji.rkt" "tongue.rkt" "format.rkt"))

  (require typed/rackunit)
  (require typed/racket/unsafe)

  (require "emoji.rkt")
  (require "tongue.rkt")
  (require "format.rkt")
  
  (require (for-syntax syntax/parse))

  (unsafe-require/typed/provide
   (submod "..")
   [$out Output-Port]
   [$err Output-Port]
   [$? (Parameterof Any)]
   [$shell (-> (-> Any * Void) Any * Any)]
   [register-handbook-finalizer (-> (-> Any) Void)]
   [tamer-story->modpath (-> Path-String (U Module-Path (List* 'submod Module-Path (Listof Symbol))))]
   [tamer-prove (-> Natural)]
   [todo (-> String Any * Nothing)]
   [skip (-> String Any * Nothing)])

  (unsafe-require/typed
   "digitama/tamer.rkt"
   [tamer-record-story (-> Symbol Test Void)])

  ;;; adapted from (submod "..")
  (define-syntax (define-tamer-suite stx)
    (syntax-parse stx
      [(_ varid name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) units ...)
       #`(tamer-record-story 'varid (test-suite name
                                                #:before #,(or (attribute setup) #'void)
                                                #:after #,(or (attribute teardown) #'void)
                                                units ...))]))
  
  (define-syntax (define-tamer-case stx)
    (syntax-parse stx
      [(_ varid name bodys ...)
       #'(tamer-record-story 'varid (delay-test (test-spec name bodys ...)))]))

  (define-syntax (test-spec stx)
    (syntax-parse stx
      [(_ name (~optional (~seq #:before setup:expr)) (~optional (~seq #:after teardown:expr)) checks ...)
       #`(test-case name (around (#,(or (attribute setup) #'void))
                                 checks ...
                                 (#,(or (attribute teardown) #'void))))])))
