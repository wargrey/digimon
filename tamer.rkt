#lang racket

(provide (all-defined-out) handbook-boxed-style)
(provide (all-from-out racket))
(provide (all-from-out scribble/core scribble/manual scriblib/autobib scribble/example scribble/html-properties))
(provide (all-from-out "spec.rkt" "digitama/plural.rkt" "digitama/citation.rkt" "tongue.rkt" "system.rkt" "format.rkt" "echo.rkt"))

(require racket/hash)

(require scribble/core)
(require scribble/example)
(require scribble/manual)
(require scriblib/autobib)
(require scriblib/footnote)
(require scribble/html-properties)
(require scribble/latex-properties)

(require (for-syntax syntax/parse))
(require (for-syntax syntax/location))

(require (for-label racket))

(require "digitama/tamer.rkt")
(require "digitama/citation.rkt")
(require "digitama/plural.rkt")

(require "spec.rkt")
(require "echo.rkt")
(require "emoji.rkt")
(require "port.rkt")
(require "tongue.rkt")
(require "system.rkt")
(require "format.rkt")
(require "collection.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #%handbook (seclink "tamer-book" (italic "Handbook")))
(define noncontent-style (make-style #false '(unnumbered reverl no-index)))
(define subsub*toc-style (make-style #false '(toc)))

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

(define-syntax (tamer-taming-start! stx)
  (syntax-case stx [scribble +]
    [(_ scribble)
     #'(let ([modpath (quote-module-path)])
         (enter-digimon-zone!) ; Scribble modules are independent of each other

         (tamer-story
          (cond [(path? modpath) (tamer-story->modpath modpath)]
                [else (tamer-story->modpath (cadr modpath))]))
         
         (default-spec-handler tamer-record-story)
         (dynamic-require (tamer-story->module (tamer-story)) #false))]
    [(_)
     #'(begin (tamer-taming-start! scribble)
              (module+ main (call-as-normal-termination tamer-prove)))]))

(define-syntax (define-bib stx)
  (syntax-parse stx #:literals []
    [(_ id bib-args ...)
     #'(define id (in-bib (make-bib bib-args ...) (format ":~a" 'id)))]))

(define ~cite
  (lambda [bib #:same-author? [same? #false] . bibs]
    (if (not same?)
        (apply (tamer-cites) bib bibs)
        (apply (tamer-cite) bib bibs))))

(define ~subcite
  (lambda [bib #:same-author? [same? #false] . bibs]
    (subscript (apply ~cite bib #:same-author? same? bibs))))

(define subcite
  (lambda keys
    (subscript (apply cite keys))))

(define handbook-renderer?
  (lambda [get render]
    (and (memq render
               (cond [(procedure? get) (get 'scribble:current-render-mode null)]
                     [else (send get current-render-mode)]))
         #true)))

(define handbook-latex-renderer?
  (lambda [get]
    (handbook-renderer? get 'latex)))

(define handbook-prefab-style
  (lambda properties
    (make-style #false
                (filter symbol? properties))))

(define handbook-resolved-info-getter
  (lambda [infobase]
    (curry hash-ref (collect-info-fp (resolve-info-ci infobase)))))

(define handbook-register-finalizer
  (lambda [atexit/0]
    (void ((curry plumber-add-flush! (current-plumber))
           (λ [this] (with-handlers ([void void])
                       (plumber-flush-handle-remove! this)
                       (void (atexit/0))))))))

(define-syntax (handbook-title stx)
  (syntax-parse stx #:literals []
    [(_ pre-contents ...)
     #'(let* ([modname (path-replace-extension (file-name-from-path (quote-module-path)) #"")])
         (enter-digimon-zone!)
         (tamer-index-story (cons 0 (tamer-story) #| meanwhile the tamer story is #false |#))

         (list (title #:tag "tamer-book"
                      #:version (~a (#%info 'version (const "Baby")))
                      #:style (make-style #false
                                          (foldl (λ [resrcs properties]
                                                   (append properties
                                                           (map (car resrcs)
                                                                (tamer-resource-files modname (cdr resrcs)))))
                                                 null
                                                 (list (cons make-css-addition "tamer.css")
                                                       (cons make-tex-addition "tamer.tex")
                                                       (cons make-js-addition "tamer.js")
                                                       (cons make-css-style-addition "tamer-style.css")
                                                       (cons make-js-style-addition "tamer-style.js"))))
                      (let ([contents (list pre-contents ...)])
                        (cond [(pair? contents) contents]
                              [else (list (literal (speak 'handbook #:dialect 'tamer) ":") ~
                                          (current-digimon))])))
               (apply author
                      (map ~a (#%info 'pkg-authors
                                      (const (list (#%info 'pkg-idun
                                                           (const (string->symbol digimon-partner))))))))))]))

(define-syntax (handbook-title/pkg-desc stx)
  (syntax-parse stx #:literals []
    [(_ pre-contents ...)
     #'(handbook-title (#%info 'pkg-desc (const (current-digimon))))]))

(define-syntax (handbook-story stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:style style) #:defaults ([style #'#false]))
              (~optional (~seq #:index? index?) #:defaults ([index? #'#false])))
        ...
        contents ...)
     #`(begin (tamer-taming-start! scribble)

              (define-cite ~cite ~cites ~reference #:style number-style)
              (tamer-reference ~reference)
              (tamer-cites ~cites)
              (tamer-cite ~cite)

              (when (or index?)
                (tamer-index-story
                 (cons (add1 (car (tamer-index-story)))
                       (tamer-story))))

              (title #:tag (tamer-story->tag (tamer-story))
                     #:style style
                     (let ([story-literal (speak 'story #:dialect 'tamer)]
                           [input-contents (list contents ...)])
                       (cond [(string=? story-literal "") input-contents]
                             [else (list* (literal story-literal ":")) ~ input-contents]))))]))

(define-syntax (handbook-module-story stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:lang lang) #:defaults ([lang #'racket/base]))
              (~optional (~seq #:style style) #:defaults ([style #'#false]))
              (~optional (~seq #:index? index?) #:defaults ([index? #'#false]))
              (~optional (~seq #:requires extras) #:defaults ([extras #'()])))
        ...
        modpath:id contents ...)
     (with-syntax ([(reqs ...) (let ([maybe-extras (syntax-e #'extras)])
                                 (cond [(list? maybe-extras) maybe-extras]
                                       [else (list maybe-extras)]))])
       #'(begin (require (for-label modpath))
                
                (handbook-story #:style style #:index? index? contents ...)
                
                (declare-exporting modpath)
                (tamer-story-private-modules (list 'reqs ...))
                (cond [(eq? 'lang #true) (tamer-lang-module modpath)]
                      [else (tamer-module #:lang lang modpath)])))]))

(define-syntax (handbook-typed-module-story stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:lang lang) #:defaults ([lang #'typed/racket/base]))
              (~optional (~seq #:style style) #:defaults ([style #'#false]))
              (~optional (~seq #:index? index?) #:defaults ([index? #'#false]))
              (~optional (~seq #:requires extras) #:defaults ([extras #'()])))
        ...
        modpath:id contents ...)
     #'(handbook-module-story #:lang lang #:style style #:index? index? #:requires extras
                              modpath contents ...)]))

(define handbook-preface-title
  (lambda [#:tag [tag #false] . pre-contents]
    (title #:tag tag #:style noncontent-style
           (cond [(pair? pre-contents) pre-contents]
                 [else (literal (speak 'preface #:dialect 'tamer))]))))

(define handbook-preface-section
  (lambda [#:tag [tag #false] . pre-contents]
    (section #:tag tag #:style noncontent-style
             (cond [(pair? pre-contents) pre-contents]
                   [else (literal (speak 'preface #:dialect 'tamer))]))))

(define handbook-preface-subsection
  (lambda [#:tag [tag #false] . pre-contents]
    (subsection #:tag tag #:style noncontent-style
                pre-contents)))

(define handbook-scenario
  (lambda [#:tag [tag #false] #:style [style #false] . pre-contents]
    (define scenario-literal (speak 'scenario #:dialect 'tamer))
    
    (section #:tag (or tag (symbol->string (gensym 'scenario)))
             #:style style
             (cond [(string=? scenario-literal "") pre-contents]
                   [else (list* (literal scenario-literal ":")
                                ~ pre-contents)]))))

(define handbook-action
  (lambda [#:tag [tag #false] #:style [style #false] . pre-contents]
    (subsection #:tag (or tag (symbol->string (gensym 'action)))
                #:style style
                pre-contents)))

(define handbook-event
  (lambda [#:tag [tag #false] #:style [style #false] . pre-contents]
    (subsubsection #:tag (or tag (symbol->string (gensym 'event)))
                   #:style style
                   pre-contents)))

(define handbook-reference
  (lambda [#:auto-hide? [auto-hide? #true]]
    ;;; NOTE
    ; This section only contains references in the resulting `part` object,
    ; It is a good chance to hide other contents such as verbose Literate Chunks if they are moved after.

    (define references
      ((tamer-reference) #:tag (format "~a-reference" (path-replace-extension (tamer-story->tag (tamer-story)) ""))
                         #:sec-title (speak 'reference #:dialect 'tamer)))

    (tamer-story #false)

    (when (or (not auto-hide?)
              (pair? (table-blockss (car (part-blocks references)))))
      references)))

(define handbook-footnote note)

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
                                  #:url      "https://docs.racket-lang.org/scribble/index.html"))])
    (lambda [#:index? [index? #true] . bibentries]
      ((curry filter-not void?)
       (list (struct-copy part (apply bibliography #:tag "handbook-bibliography" (append entries bibentries))
                          [title-content (list (speak 'bibliography #:dialect 'tamer))]
                          [style noncontent-style]
                          [parts null])
             (unless (false? index?)
               (struct-copy part (index-section #:tag "handbook-index")
                            [title-content (list (speak 'index #:dialect 'tamer))]
                            [style noncontent-style])))))))
  
(define handbook-smart-table
  (lambda []
    (make-traverse-block
     (λ [get set!]
       (if (false? (handbook-renderer? get 'markdown))
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

(define handbook-latex-command0
  (lambda [cmd]
    (make-traverse-element
     (λ [get set!]
       (cond [(handbook-latex-renderer? get) (elem #:style cmd null)]
             [else null])))))

(define handbook-texbook-front
  (lambda []
    (handbook-latex-command0 "frontmatter")))

(define handbook-texbook-main
  (lambda []
    (handbook-latex-command0 "mainmatter")))

(define handbook-texbook-appendix
  (lambda []
    (handbook-latex-command0 "appendix")))

(define handbook-texbook-back
  (lambda []
    (handbook-latex-command0 "backmatter")))

(define handbook-latex-prefab-string
  (lambda [TeX]
    (case (string->symbol (string-downcase TeX))
      [(tex) (handbook-latex-command0 "TeX")]
      [(latex) (handbook-latex-command0 "LaTeX")]
      [(latexe) (handbook-latex-command0 "LaTeXe")]
      [else (handbook-latex-command0 TeX)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (tamer-module stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:lang lang:id) #:defaults ([lang #'racket/base])))
        ...
        modname modnames ...)
     #'(begin (unless (pair? (tamer-story-lang+modules))
                (tamer-story-lang+modules (list 'lang 'modname 'modnames ...)))
              
              (defmodule*/no-declare (modname modnames ...)))]))

(define-syntax (tamer-lang-module stx)
  (syntax-parse stx #:literals []
    [(_ lang extra-langs ...)
     #'(begin (unless (pair? (tamer-story-lang+modules))
                (tamer-story-lang+modules (list 'lang 'extra-langs ...)))
              (defmodule*/no-declare (lang extra-langs ...) #:lang))]))

(define-syntax (tamer-action stx)
  (syntax-parse stx #:literals []
    [(_ (~optional (~seq #:label label) #:defaults ([label #'#false]))
        s-exps ...)
     #'(let ([this-story (tamer-story)])
         (define example-label
           (cond [(symbol? label) (bold (speak label #:dialect 'tamer))]
                 [else label]))
         (tamer-zone-reference this-story (tamer-story-lang+modules))
         (make-traverse-block
          (λ [get set!]
            (define repl (examples #:label example-label #:eval (tamer-zone-ref this-story) s-exps ...))
            (tamer-zone-destory this-story #true)
            repl)))]))

(define-syntax (tamer-answer stx)
  (syntax-case stx []
    [(_ exprs ...)
     #'(tamer-action #:label 'answer exprs ...)]))

(define-syntax (tamer-solution stx)
  (syntax-case stx []
    [(_ exprs ...)
     #'(tamer-action #:label 'solution exprs ...)]))

(define tamer-story-space
  (lambda []
    (module->namespace (tamer-story))))

(define tamer-require
  (lambda [id]
    (namespace-variable-value id #true #false
                              (tamer-story-space))))

(define tamer-prove
  (lambda []
    (spec-prove
     (if (module-path? (tamer-story))
         (let ([htag (tamer-story->tag (tamer-story))])
           (make-spec-feature htag (or (and (dynamic-require (tamer-story) #false)
                                            (hash-has-key? handbook-stories htag)
                                            (reverse (hash-ref handbook-stories htag)))
                                       null)))
         (make-spec-feature "Features and Behaviors"
                            (cond [(zero? (hash-count handbook-stories)) null] ; no story ==> no :books:
                                  [else (let ([href (curry hash-ref handbook-stories)])
                                          (for/list ([htag (in-list (reverse (href books#)))])
                                            (make-spec-feature htag (reverse (href htag)))))]))))))

(define tamer-smart-summary
  (lambda []
    (define this-story (tamer-story))
    (define ~symbol (default-spec-issue-symbol))
    (define ~fgcolor (default-spec-issue-fgcolor))
    (define raco-setup-forget-my-digimon (current-digimon))
    
    (make-traverse-block
     (λ [get set!]
       (if (handbook-renderer? get 'markdown)
           (para (literal "---"))
           (make-delayed-block
            (λ [render% pthis infobase]
              (define get (handbook-resolved-info-getter infobase))
              (define scenarios (get tamer-scribble-story-id tamer-empty-issues))
              (define btimes (get tamer-scribble-story-times tamer-empty-issues))
              (define issues (get tamer-scribble-story-issues tamer-empty-issues))

              (parameterize ([current-digimon raco-setup-forget-my-digimon])
                (nested #:style handbook-boxed-style
                        (filebox (if (module-path? this-story)
                                     (italic (seclink "tamer-book" (string open-book#)) ~
                                             (~a "Behaviors in " (tamer-story->tag this-story)))
                                     (italic (string books#) ~
                                             (~a "Behaviors of " (current-digimon))))
                                 (let-values ([(features btimes metrics)
                                               (if (module-path? this-story)
                                                   (let ([htag (tamer-story->tag this-story)])
                                                     (values (tamer-feature-reverse-merge (hash-ref scenarios htag (λ [] null)))
                                                             (hash-ref btimes htag (λ [] tamer-empty-times))
                                                             (hash-ref issues htag (λ [] tamer-empty-issues))))
                                                   (values (for/list ([story (in-list (hash-ref handbook-stories books# null))])
                                                             (cons story (reverse (hash-ref scenarios story (λ [] null)))))
                                                           (apply map + tamer-empty-times (hash-values btimes))
                                                           (apply hash-union tamer-empty-issues (hash-values issues) #:combine +)))])
                                   (define bookmarks
                                     (for/list ([bookmark (in-list features)])
                                       ;;; also see (tamer-note)
                                       (define-values (local# brief)
                                         (cond [(string? (car bookmark)) (values bookmark# (car bookmark))] ;;; feature
                                               [else (values page# (unbox (car bookmark)))])) ;;; toplevel behavior
                                       
                                       (define issue-type
                                         (or (for/first ([t (in-list (list 'panic 'misbehaved 'todo 'skip))]
                                                         #:when (hash-has-key? metrics t)) t)
                                             'pass))
                                       
                                       (define symtype (~a (~symbol issue-type)))
                                       (if (module-path? this-story)
                                           (list (elem (italic (string local#)) ~ (elemref brief (racketkeywordfont (literal brief))))
                                                 (elemref brief symtype #:underline? #false))
                                           (let ([head (~a brief #:width 64 #:pad-string "." #:limit-marker "......")]
                                                 [stts (make-parameter issue-type)])
                                             (echof #:fgcolor 'lightyellow head)
                                             (echof #:fgcolor (~fgcolor issue-type) "~a~n" symtype)
                                             (list (elem (italic (string book#)) ~ (secref (car bookmark)))
                                                   (seclink (car bookmark) symtype #:underline? #false))))))

                                   (match-define-values ((list pass misbehaved panic skip todo) (list cpu real gc))
                                     (values (for/list ([meta (in-list (list 'pass 'misbehaved 'panic 'skip 'todo))])
                                               (hash-ref metrics meta (λ [] 0)))
                                             btimes))

                                   (define briefs
                                     (let ([population (+ pass misbehaved panic skip todo)])
                                       (if (zero? population)
                                           (list "No particular sample!")
                                           (list (format "~a% behaviors okay."
                                                   (~r #:precision '(= 2) (/ (* (+ pass skip) 100) population)))
                                                 (string-join (list (~w=n (length features) (if this-story "Scenario" "Story"))
                                                                    (~w=n population "Behavior") (~w=n misbehaved "Misbehavior") (~w=n panic "Panic")
                                                                    (~w=n skip "Skip") (~w=n todo "TODO"))
                                                              ", " #:after-last ".")
                                                 (apply format "~a wallclock seconds (~a task + ~a gc = ~a CPU)."
                                                        (map (λ [ms] (~r (* ms 0.001) #:precision '(= 3)))
                                                             (list real (- cpu gc) gc cpu)))))))

                                   (unless (module-path? this-story)
                                     (for ([brief (in-list (cons "" briefs))])
                                       (echof #:fgcolor 'lightcyan "~a~n" brief)))

                                   (let ([summaries (add-between (map racketoutput briefs) (linebreak))])
                                     (cond [(null? bookmarks) summaries]
                                           [else (cons (tabular bookmarks #:style 'boxed #:column-properties '(left right))
                                                       summaries)])))))))))))))

(define tamer-note
  (lambda [example . notes]
    (define this-story (tamer-story))
    (define ~symbol (default-spec-issue-symbol))
    (define raco-setup-forget-my-digimon (current-digimon))
    
    (make-traverse-block
     (λ [get set!]
       (parameterize ([current-digimon raco-setup-forget-my-digimon])
         (define htag (tamer-story->tag this-story))
         (define toplevel-indent 2)

         (define scenarios (traverse-ref! get set! tamer-scribble-story-id make-hash))
         (define btimes (traverse-ref! get set! tamer-scribble-story-times make-hash))
         (define issues (traverse-ref! get set! tamer-scribble-story-issues make-hash))
         
         (margin-note (unless (null? notes) (append notes (list (linebreak) (linebreak))))
                      (parameterize ([tamer-story this-story]
                                     [default-spec-issue-handler void])
                        ; seed : (Vector (Pairof (Listof (U Spec-Issue String tamer-feature)) (Listof tamer-feature)) (Listof scrible-flow))
                        (define (downfold-feature brief indent seed:info)
                          (define flows (vector-ref seed:info 1))

                          (vector (cons null (vector-ref seed:info 0))
                                  (cond [(= indent toplevel-indent)
                                         (cons (nonbreaking (racketmetafont (italic (string open-book#)) ~ (elemtag brief (literal brief)))) flows)]
                                        [(> indent toplevel-indent)
                                         (cons (nonbreaking (racketoutput (italic (string bookmark#)) ~ (larger (literal brief)))) flows)]
                                        [else flows])))
                        
                        (define (upfold-feature brief indent whocares children:info)
                          (define issues (vector-ref children:info 0))
                          (define this-issues (reverse (car issues)))

                          (vector (case indent ; never be 0
                                    [(1) (apply append (map tamer-feature-issues this-issues)) #| still (Listof tamer-feature) |#]
                                    [else (cons (cons (tamer-feature brief this-issues) (cadr issues)) (cddr issues))])
                                  (vector-ref children:info 1)))
                        
                        (define (fold-behavior brief issue indent cpu real gc seed:info)
                          (define issues (vector-ref seed:info 0))
                          (define idx (add1 (length (car issues))))
                          (define type (spec-issue-type issue))
                          (define flow (nonbreaking ((if (= indent toplevel-indent) (curry elemtag brief) elem)
                                                     (~a (~symbol type)) (racketkeywordfont ~ (italic (number->string idx)))
                                                     (racketcommentfont ~ (literal brief)))))

                          (vector (cons (cons (if (eq? type 'pass) brief issue) (car issues)) (cdr issues))
                                  (cons flow (vector-ref seed:info 1))))
                        
                        (match-define-values ((list (cons summary (vector features flows))) cpu real gc)
                          (time-apply (λ [] (spec-summary-fold (make-spec-feature htag (reverse (hash-ref handbook-stories htag null)))
                                                               (vector null null)
                                                               #:downfold downfold-feature #:upfold upfold-feature #:herefold fold-behavior
                                                               #:selector (list '* '* example)))
                                      null))
                        
                        (define population (apply + (hash-values summary)))

                        (hash-set! scenarios htag (append (hash-ref scenarios htag (λ [] null)) features))
                        (hash-set! btimes htag (map + (list cpu real gc) (hash-ref btimes htag (λ [] tamer-empty-times))))
                        (hash-set! issues htag (hash-union summary (hash-ref issues htag (λ [] (make-immutable-hasheq))) #:combine +))
                        
                        (let ([misbehavior (hash-ref summary 'misbehaved (λ [] 0))]
                              [panic (hash-ref summary 'panic (λ [] 0))])
                          (append (reverse (add-between flows (linebreak)))
                                  (list (linebreak) (linebreak)
                                        (nonbreaking (elem (string pin#)
                                                           ~ (if (= (+ misbehavior panic) 0)
                                                                 (racketresultfont (~a (~r (* real 0.001) #:precision '(= 3)) #\space "wallclock seconds"))
                                                                 (racketerror (~a (~n_w misbehavior "misbehavior") #\space (~n_w panic "panic"))))
                                                           ~ (seclink (tamer-story->tag (tamer-story)) ~ (string house-garden#))))))))))))))

(define tamer-racketbox
  (lambda [path #:line-start-with [line0 1]]
    (define this-story (tamer-story))
    (define raco-setup-forget-my-digimon (current-digimon))
    (make-traverse-block
     (λ [get set!]
       (parameterize ([tamer-story this-story]
                      [current-digimon raco-setup-forget-my-digimon])
         (define /path/file (simplify-path (if (symbol? path) (tamer-require path) path)))
         (handbook-nested-filebox (handbook-latex-renderer? get)
                                  /path/file
                                  (codeblock #:line-numbers line0 #:keep-lang-line? (> line0 0) ; make sure line number start from 1
                                             (string-trim (file->string /path/file) #:left? #false #:right? #true))))))))

(define tamer-racketbox/region
  (lambda [path #:pxstart [pxstart #px"\\S+"] #:pxstop [pxstop #false] #:greedy? [greedy? #false]]
    (define this-story (tamer-story))
    (define raco-setup-forget-my-digimon (current-digimon))
    (make-traverse-block
     (λ [get set!]
       (parameterize ([tamer-story this-story]
                      [current-digimon raco-setup-forget-my-digimon])
         (define /path/file (simplify-path (if (symbol? path) (tamer-require path) path)))
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
                       [(and (regexp? pxstop) (pair? contents) (regexp-match? pxstop line))
                        ; the stop line itself is excluded
                        (if (false? greedy?)
                            (values line0 (cons lang (reverse contents)))
                            (read-next lang line0 (cons line contents) (length contents)))]
                       [(regexp-match? #px"^#lang .+$" line)
                        (read-next line (add1 line0) contents end)]
                       [(and lang (null? contents) (regexp-match pxstart line))
                        (read-next lang line0 (list line) end)]
                       [(pair? contents) ; still search the end line greedily
                        (read-next lang line0 (cons line contents) end)]
                       [else ; still search the start line
                        (read-next lang (add1 line0) contents end)])))))
         (handbook-nested-filebox (handbook-latex-renderer? get)
                                  /path/file
                                  (codeblock #:line-numbers line0 #:keep-lang-line? #false
                                             (string-trim #:left? #false #:right? #true ; remove tail blank lines 
                                                          (string-join contents (string #\newline))))))))))

(define tamer-filebox/region
  (lambda [path #:pxstart [pxstart #px"\\S+"] #:pxstop [pxstop #false] #:greedy? [greedy? #false]]
    (define this-story (tamer-story))
    (define raco-setup-forget-my-digimon (current-digimon))
    (make-traverse-block
     (λ [get set!]
       (parameterize ([tamer-story this-story]
                      [current-digimon raco-setup-forget-my-digimon])
         (define /path/file (simplify-path (if (symbol? path) (tamer-require path) path)))
         (define-values (line0 contents)
           (call-with-input-file* /path/file
             (lambda [in.rkt]
               (let read-next ([line0 1] [contents null] [end 0])
                 (define line (read-line in.rkt))
                 ; if it does not work, please check whether your pxstart and pxend are pregexps first.
                 (cond [(eof-object? line)
                        (if (zero? end)
                            (values line0 (reverse contents))
                            (values line0 (take (reverse contents) end)))]
                       [(and (regexp? pxstop) (pair? contents) (regexp-match? pxstop line))
                        ; the stop line itself is excluded
                        (if (false? greedy?)
                            (values line0 (reverse contents))
                            (read-next line0 (cons line contents) (length contents)))]
                       [(and (null? contents) (regexp-match pxstart line))
                        (read-next line0 (list line) end)]
                       [(pair? contents) ; still search the end line greedily
                        (read-next line0 (cons line contents) end)]
                       [else ; still search the start line
                        (read-next (add1 line0) contents end)])))))
         (handbook-nested-filebox (handbook-latex-renderer? get)
                                  /path/file
                                  (codeblock #:line-numbers line0 #:keep-lang-line? #true
                                             (string-trim #:left? #false #:right? #true ; remove tail blank lines 
                                                          (string-join contents (string #\newline))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-tamer-indexed-traverse-block
  (lambda [traverse index-type]
    (define this-index-story (tamer-index-story))

    (make-traverse-block
     (λ [get set!]
       (parameterize ([tamer-index-story this-index-story])
         (define order (car this-index-story))
         (define this-story (cdr this-index-story))
         (define global-tags (traverse-indexed-tagbase get index-type))
         (define current-index (hash-ref global-tags this-story (λ [] 1)))
         (define-values (current-tag block) (traverse index-type order current-index))

         (hash-set! global-tags current-tag (cons order current-index))
         (hash-set! global-tags this-story (add1 current-index))
         (traverse-indexed-tagbase set! index-type global-tags get)

         block)))))

(define make-tamer-indexed-elemref
  (lambda [resolve index-type tag]
    (define this-index-story (tamer-index-story))
    (define sym:tag
      (cond [(symbol? tag) tag]
            [(string? tag) (string->symbol tag)]
            [else (string->symbol (~a tag))]))
    
    (make-delayed-element
     (λ [render% pthis infobase]
       (define get (handbook-resolved-info-getter infobase))
       (define global-tags (traverse-indexed-tagbase get index-type))
       (define target-info (hash-ref global-tags sym:tag (λ [] (cons (car this-index-story) #false))))
       (resolve index-type (car target-info) (cdr target-info)))
     (λ [] (content-width (resolve index-type (car this-index-story) #false)))
     (λ [] (content->string (resolve index-type (car this-index-story) #false))))))
