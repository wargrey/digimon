#lang racket

(provide (all-defined-out) tamer-story-disable-submodule tamer-story-submodule-name)
(provide tamer-boxed-style make-tamer-indexed-traverse-block make-tamer-indexed-block-ref)
(provide tamer-indexed-block-id->symbol tamer-indexed-block-elemtag tamer-block-chapter-label tamer-indexed-block-hide-chapter-index)
(provide tamer-block-label-separator tamer-block-label-tail tamer-block-label-style tamer-block-caption-style)
(provide tamer-center-block-style tamer-left-block-style tamer-right-block-style tamer-jfp-legend-style)
(provide graphviz graph.gv digraph.gv $ $$ $$* handbook-image tamer-image)
(provide fg:rgb bg:rgb fg-rgb bg-rgb type-rgb)

(provide (except-out (all-from-out racket) abstract))
(provide (except-out (all-from-out scribble/manual) title author))
(provide (all-from-out scribble/core scriblib/autobib scribble/example))
(provide (all-from-out scribble/html-properties scribble/latex-properties))
(provide (all-from-out "digitama/tamer/backend.rkt" "digitama/tamer/citation.rkt" "digitama/tamer/privacy.rkt"))
(provide (all-from-out "digitama/tamer/style.rkt" "digitama/tamer/texbook.rkt" "digitama/tamer/manual.rkt"))
(provide (all-from-out "digitama/plural.rkt"))
(provide (all-from-out "spec.rkt" "tongue.rkt" "system.rkt" "format.rkt" "echo.rkt"))

(provide (rename-out [note handbook-footnote]))
(provide (rename-out [tamer-deftech tamer-defterm]))
(provide (rename-out [tamer-deftech handbook-deftech]))
(provide (rename-out [tamer-deftech handbook-defterm]))
(provide (rename-out [handbook-acknowledgement handbook-acknowledgment]))

(provide (rename-out [dot? graphviz?]))
(provide (rename-out [graph.gv graph.dot]))
(provide (rename-out [digraph.gv digraph.dot]))

(require racket/hash)

(require scribble/lp2)
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

(require "digitama/tamer/style.rkt")
(require "digitama/tamer/backend.rkt")
(require "digitama/tamer/citation.rkt")
(require "digitama/tamer/manual.rkt")
(require "digitama/tamer/block.rkt")
(require "digitama/tamer/lstlisting.rkt")
(require "digitama/tamer/texbook.rkt")
(require "digitama/tamer/privacy.rkt")
(require "digitama/tamer/graphviz.rkt")
(require "digitama/tamer/color.rkt")
(require "digitama/tamer/image.rkt")

(require "digitama/tamer.rkt")
(require "digitama/plural.rkt")
(require "digitama/minimal/port.rkt")

(require "debug.rkt")
(require "spec.rkt")
(require "echo.rkt")
(require "emoji.rkt")
(require "tongue.rkt")
(require "system.rkt")
(require "format.rkt")
(require "collection.rkt")
(require "git.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #%handbook (seclink "tamer-book" (italic "Handbook")))
(define #%handbook-properties (make-parameter null))

(define-syntax (tamer-taming-start! stx)
  (syntax-case stx [scribble +]
    [(_ scribble)
     (syntax/loc stx
       (let ([modpath (quote-module-path)])
         (enter-digimon-zone!) ; Scribble modules are independent of each other

         (tamer-story
          (cond [(path? modpath) (tamer-story->modpath modpath)]
                [else (tamer-story->modpath (cadr modpath))]))
         
         (default-spec-handler tamer-record-story)
         (dynamic-require (tamer-story->module (tamer-story)) #false)))]
    [(_)
     (syntax/loc stx
       (begin (tamer-taming-start! scribble)
              (module+ main (call-as-normal-termination tamer-prove))))]))

(define-syntax (define-bib stx)
  (syntax-parse stx #:literals []
    [(_ id bib-args ...)
     (syntax/loc stx (define id (in-bib (make-bib bib-args ...) (format ":~a" 'id))))]))

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
    [(_ (~alt (~optional (~seq #:properties props:expr) #:defaults ([props #'null]))
              (~optional (~seq #:author pre-empty-author) #:defaults ([pre-empty-author #'#false]))
              (~optional (~seq #:hide-version? noversion?) #:defaults ([noversion? #'#false]))
              (~optional (~seq #:subtitle subtitle) #:defaults ([subtitle #'#false]))
              (~optional (~seq #:λtitle λtitle) #:defaults ([λtitle #'title]))) ...
        pre-contents ...)
     (syntax/loc stx
       (let* ([ext-properties (let ([mkprop (#%handbook-properties)]) (if (procedure? mkprop) (mkprop) mkprop))]
              [modname (path-replace-extension (file-name-from-path (quote-module-path)) #"")])
         (enter-digimon-zone!)
         (tamer-index-story (cons 0 (tamer-story) #| meanwhile the tamer story is #false |#))

         (list (λtitle #:tag "tamer-book"
                       #:version (and (not noversion?) (~a (#%info 'version (const "Baby"))))
                       #:style (handbook-title-style #false props ext-properties tamer-resource-files modname)
                       (let ([contents (list pre-contents ...)])
                         (append (cond [(pair? contents) contents]
                                       [else (list (literal (speak 'handbook #:dialect 'tamer) ":") ~
                                                   (current-digimon))])
                                 (cond [(not subtitle) null]
                                       [else (list (linebreak)
                                                   (elem #:style subtitle-style
                                                         subtitle))]))))
               (cond [(not pre-empty-author)
                      (apply author
                             (map ~a (#%info 'pkg-authors
                                             (const (list (#%info 'pkg-idun
                                                                  (const (string->symbol digimon-partner))))))))]
                     [(string? pre-empty-author) (author pre-empty-author)]
                     [else pre-empty-author]))))]))

(define-syntax (handbook-title/pkg-desc stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:properties props:expr) #:defaults ([props #'null]))
              (~optional (~seq #:author pre-empty-author) #:defaults ([pre-empty-author #'#false]))
              (~optional (~seq #:hide-version? noversion?) #:defaults ([noversion? #'#false]))
              (~optional (~seq #:subtitle subtitle) #:defaults ([subtitle #'#false]))
              (~optional (~seq #:λtitle λtitle) #:defaults ([λtitle #'title]))) ...
        pre-contents ...)
     (syntax/loc stx (handbook-title #:subtitle subtitle #:properties props
                                     #:author pre-empty-author #:hide-version? noversion?
                                     #:λtitle λtitle
                                     (#%info 'pkg-desc
                                             (const (let ([alt-contents (list pre-contents ...)])
                                                      (cond [(null? alt-contents) (current-digimon)]
                                                            [else alt-contents]))))))]))

(define-syntax (handbook-story stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:tag tag) #:defaults ([tag #'#false]))
              (~optional (~seq #:style style) #:defaults ([style #'#false]))
              (~optional (~seq #:counter-step? counter-step?) #:defaults ([counter-step? #'#false])))
        ...
        contents ...)
     (quasisyntax/loc stx
       (begin (tamer-taming-start! scribble)

              (define-cite ~cite ~cites ~reference #:style number-style)
              (tamer-reference ~reference)
              (tamer-cites ~cites)
              (tamer-cite ~cite)

              (define-footnote ~endnote ~endnote-section)
              (tamer-endnote ~endnote)
              (tamer-endnote-section ~endnote-section)

              (when (or counter-step?)
                (tamer-index-story
                 (cons (add1 (car (tamer-index-story)))
                       (tamer-story))))

              (title #:tag (or tag (tamer-story->tag (tamer-story)))
                     #:style style
                     (let ([story-literal (speak 'story #:dialect 'tamer)]
                           [input-contents (list contents ...)])
                       (cond [(string=? story-literal "") input-contents]
                             [else (list* (literal story-literal ":")) ~ input-contents])))))]))

(define-syntax (handbook-part stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:style style) #:defaults ([style #'#false]))
              (~optional (~seq #:counter-step? counter-step?) #:defaults ([counter-step? #'#false])))
        ...
        pre-contents ...)
     (syntax/loc stx
       ;;; NOTE
       ; In the body of part, the `tamer-index-story` is the same as the last chapter of this part,
       ; whereas the the `tamer-story` is different from the `tamer-index-story`
       (handbook-story #:counter-step? counter-step?
                       #:style (style-merge-property style grouper-style)
                       pre-contents ...))]))

(define-syntax (handbook-part-section stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:style style) #:defaults ([style #'#false]))
              (~optional (~seq #:counter-step? counter-step?) #:defaults ([counter-step? #'#false])))
        ...
        pre-contents ...)
     (syntax/loc stx
       (handbook-scenario #:style (style-merge-property style grouper-style)
                          pre-contents ...))]))

(define-syntax (handbook-root-story stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:style style) #:defaults ([style #'#false]))) ... contents ...)
     (syntax/loc stx (handbook-story #:style style #:counter-step? #true contents ...))]))

(define-syntax (handbook-appendix-story stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:style style) #:defaults ([style #'#false]))) ... contents ...)
     (syntax/loc stx
       (begin (handbook-story #:style style #:counter-step? #true contents ...)

              (unless (tamer-appendix-index)
                (tamer-appendix-index (car (tamer-index-story))))))]))

(define-syntax (handbook-module-story stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:lang lang) #:defaults ([lang #'racket/base]))
              (~optional (~seq #:style style) #:defaults ([style #'#false]))
              (~optional (~seq #:counter-step? counter-step?) #:defaults ([counter-step? #'#false]))
              (~optional (~seq #:requires extras) #:defaults ([extras #'()])))
        ...
        modpath:id contents ...)
     (with-syntax ([(reqs ...) (let ([maybe-extras (syntax-e #'extras)])
                                 (cond [(list? maybe-extras) maybe-extras]
                                       [else (list maybe-extras)]))])
       (syntax/loc stx
         (begin (require (for-label modpath))
                
                (handbook-story #:style style #:counter-step? counter-step? contents ...)
                
                (declare-exporting modpath)
                (tamer-story-private-modules (list 'reqs ...))
                (cond [(eq? 'lang #true) (tamer-lang-module modpath)]
                      [else (tamer-module #:lang lang modpath)]))))]))

(define-syntax (handbook-typed-module-story stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:lang lang) #:defaults ([lang #'typed/racket/base]))
              (~optional (~seq #:style style) #:defaults ([style #'#false]))
              (~optional (~seq #:counter-step? counter-step?) #:defaults ([counter-step? #'#false]))
              (~optional (~seq #:requires extras) #:defaults ([extras #'()])))
        ...
        modpath:id contents ...)
     (syntax/loc stx
       (handbook-module-story #:lang lang #:style style #:counter-step? counter-step? #:requires extras
                              modpath contents ...))]))

(define-syntax (handbook-chunk stx)
  (syntax-parse stx #:literals []
    [(_ id contents ...)
     (syntax/loc stx (begin ($tex:phantomsection)
                            (chunk id contents ...)))]))

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
    
    (section #:tag (or tag (generate-immutable-string 'scenario))
             #:style style
             (cond [(string=? scenario-literal "") pre-contents]
                   [else (list* (literal scenario-literal ":")
                                ~ pre-contents)]))))

(define handbook-action
  (lambda [#:tag [tag #false] #:style [style #false] . pre-contents]
    (subsection #:tag (or tag (generate-immutable-string 'action))
                #:style style
                pre-contents)))

(define handbook-event
  (lambda [#:tag [tag #false] #:style [style #false] . pre-contents]
    (subsubsection #:tag (or tag (generate-immutable-string 'event))
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

(define handbook-endnote
  (lambda body
    (apply (tamer-endnote) body)))

(define handbook-endnotes
  (lambda []
    ((tamer-endnote-section))))

(define handbook-acknowledgement
  (lambda [#:numbered? [numbered? #false] . pre-contents]
    (section #:tag "handbook-acknowledgment"
             #:style (if (not numbered?) noncontent-style #false)
             (cond [(pair? pre-contents) pre-contents]
                   [else (literal (speak 'acknowledgment #:dialect 'tamer))]))))

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
    ;;; NOTE that the unnumbered sections might be hard to be located in resulting PDF
    (lambda [#:index-section? [index? #true] #:numbered? [numbered? #false] #:racket-bibentries? [racket? #true]
             #:title-localization? [title-l18n? #false] . bibentries]
      (define all-bibentries
        (cond [(not racket?) (flatten bibentries)]
              [else (append entries (flatten bibentries))]))
      
      (list (if (pair? all-bibentries)
                (let ([bibliography-self (apply bibliography #:tag "handbook-bibliography" all-bibentries)])
                  (list (cond [(and title-l18n?)
                               (struct-copy part bibliography-self
                                            [title-content (list (speak 'bibliography #:dialect 'tamer))]
                                            [style (if numbered? plain (part-style bibliography-self))])]
                              [(and numbered?) (struct-copy part bibliography-self [style plain])]
                              [else bibliography-self])))
                 null)
            
            (if (and index?)
                (let* ([index-origin (index-section #:tag "handbook-index")]
                       [index-self (struct-copy part index-origin 
                                                [style (if numbered? plain (part-style index-origin))]
                                                [blocks (append (list (texbook-twocolumn))
                                                                (part-blocks index-origin)
                                                                (list (texbook-onecolumn)))])])
                  (list (if (and title-l18n?)
                            (struct-copy part index-self [title-content (list (speak 'index #:dialect 'tamer))])
                            index-self)))
                null)))))

(define handbook-smart-table
  (lambda []
    (make-traverse-block
     (λ [get set!]
       (if (false? (handbook-markdown-renderer? get))
           (table-of-contents)
           (make-delayed-block
            (λ [render% pthis _]
              (define-values (/dev/tamer/stdin /dev/tamer/stdout) (make-pipe #false '/dev/tamer/stdin '/dev/tamer/stdout))
              (parameterize ([current-input-port /dev/tamer/stdin]
                             [current-error-port /dev/tamer/stdout]
                             [current-output-port /dev/tamer/stdout]
                             [tamer-story #false])
                (define summary? (make-parameter #false))
                (thread (thunk (dynamic-wind collect-garbage*
                                             tamer-prove
                                             (thunk (close-output-port /dev/tamer/stdout)))))
                (para (filter-map (λ [line] (and (not (void? line)) (map ~markdown (if (list? line) line (list line)))))
                                  (for/list ([line (in-lines)])
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

(define handbook-statistics
  (lambda [#:gitstat-width [git-width #false] #:gitstat-radius [git-radius #false] #:recursive? [recursive? #true]
           #:ignore [exclude-submodules null] #:filter [filter null] #:subgroups [subgroups git-default-subgroups]
           #:altcolors [altcolors null] #:since [since #false] #:date-delta [date-delta (* 3600 24 31)]
           #:lang-delta-only? [lang-delta-only? #false]
           #:file-header-style [file-header-style 'tt] #:file-color [file-color #x586069]
           #:insertion-color [insertion-color #x20A745] #:deletion-color [deletion-color #xCB2431]
           #:ring-name [ring-name "lang-ring"] #:loc-name [loc-name "line-of-code"]
           ring-chart loc-series]
    (make-traverse-block
     (λ [get set!]      
       (define all-files (git-list-tree #:recursive? recursive? #:ignore-submodule exclude-submodules #:filter filter))
       (define all-numstats (git-numstat #:recursive? recursive? #:ignore-submodule exclude-submodules #:since since #:filter filter))
       (define lang-files (git-files->langfiles all-files null subgroups))
       (define lang-sizes (git-files->langsizes all-files null subgroups))
       (define lang-stats (git-numstats->langstats all-numstats null subgroups))
       
       (define src-file
         (for/fold ([count 0])
                   ([lf (in-hash-values lang-files)])
           (+ count (length (git-language-content lf)))))
       
       (define-values (insertions deletions)
         (if (not lang-delta-only?)
             (git-numstats->additions+deletions* all-numstats)
             (git-langstats->additions+deletions* lang-stats)))
       
       (define langstats
         (for/list ([(id lang) (in-hash lang-stats)]
                    #:when (hash-has-key? lang-sizes id))
           lang))
       
       (nested (filebox (elem #:style (fg-rgb file-color file-header-style) (~integer src-file) (subscript "files")
                              ~ (elem #:style (fg-rgb insertion-color file-header-style) (~integer insertions) (subscript "++"))
                              ~ (elem #:style (fg-rgb deletion-color file-header-style) (~integer deletions) (subscript (literal "--"))))
                        (tabular #:sep (hspace 1) #:column-properties '(left right)
                                 (list (let* ([pie-radius (or git-radius 75)]
                                              [series-height (* (or git-radius pie-radius) 2)]
                                              [series-width (or git-width 380)])
                                         (list (handbook-image #:name ring-name
                                                               (ring-chart pie-radius lang-sizes altcolors))
                                               (handbook-image #:name loc-name
                                                               (loc-series series-width series-height langstats altcolors date-delta))))))))))))
    
(define handbook-appendix-tabular/2
  (lambda [table-head table-rows [gap 1] [empty-cols (list "")]]
    (define col-size (length table-head))
    (define col-properties (make-list col-size 'left))
    (define cel-properties (make-list col-size '()))
     
    (tabular #:sep (hspace gap)
             #:style 'centered
             #:row-properties '(bottom-border ())
             #:column-properties (append col-properties '(center) col-properties)
             #:cell-properties (list (append cel-properties '(left-border) cel-properties))
             (cons (append table-head empty-cols table-head)
                   (let make-table ([rows table-rows]
                                    [swor null])
                     (cond [(null? rows) (reverse swor)]
                           [(null? (cdr rows)) (reverse (cons (append (car rows) empty-cols (make-list col-size 'cont)) swor))]
                           [else (make-table (cddr rows) (cons (append (car rows) empty-cols (cadr rows)) swor))]))))))

(define handbook-seclink
  (lambda [#:doc [modpath #false] #:tag-prefixes [tps #false]
           #:underline? [underline? #true] #:captalized? [captalized? #true]
           #:mode [mode #false]
           tag . label]
    (define style (and mode (link-render-style mode)))

    (cond [(pair? label) (apply seclink #:doc modpath #:tag-prefixes tps #:underline? underline? #:link-render-style style tag label)]
          [(not captalized?) (secref #:doc modpath #:tag-prefixes tps #:underline? underline? #:link-render-style style tag)]
          [else (Secref #:doc modpath #:tag-prefixes tps #:underline? underline? #:link-render-style style tag)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (tamer-module stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:lang lang:id) #:defaults ([lang #'racket/base])))
        ...
        modname modnames ...)
     (syntax/loc stx
       (begin (unless (pair? (tamer-story-lang+modules))
                (tamer-story-lang+modules (list 'lang 'modname 'modnames ...)))
              
              (defmodule*/no-declare (modname modnames ...))))]))

(define-syntax (tamer-lang-module stx)
  (syntax-parse stx #:literals []
    [(_ lang extra-langs ...)
     (syntax/loc stx
       (begin (unless (pair? (tamer-story-lang+modules))
                (tamer-story-lang+modules (list 'lang 'extra-langs ...)))
              (defmodule*/no-declare (lang extra-langs ...) #:lang)))]))

(define-syntax (tamer-repl stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:label label) #:defaults ([label #'#false]))
              (~optional (~seq #:requires hidden-requires) #:defaults ([hidden-requires #'()]))
              (~optional (~seq #:style style) #:defaults ([style #''inset]))) ...
        s-exps ...)
     (with-syntax ([(hidden-mods ...) #'hidden-requires])
       (syntax/loc stx
         (let ([this-story (tamer-story)])
           (define example-label
             (cond [(symbol? label) (bold (speak label #:dialect 'tamer))]
                   [else label]))
           (tamer-zone-reference this-story (tamer-story-lang+modules))
           (make-traverse-block
            (λ [get set!]
              (let ([zeval (tamer-zone-ref this-story)])
                (examples #:label #false #:eval zeval #:hidden (require hidden-mods)) ...
                (begin0 (if (not style)
                            (examples #:no-inset #:label example-label #:eval zeval s-exps ...)
                            (nested #:style style
                                    (examples #:no-inset #:label example-label #:eval zeval s-exps ...)))
                        (tamer-zone-destory this-story #true))))))))]))

(define-syntax (tamer-answer stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:requires hidden-requires) #:defaults ([hidden-requires #'()]))) ...
        exprs ...)
     (syntax/loc stx (tamer-repl #:label 'answer #:requires hidden-requires exprs ...))]))

(define-syntax (tamer-solution stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:requires hidden-requires) #:defaults ([hidden-requires #'()]))) ...
        exprs ...)
     (syntax/loc stx (tamer-repl #:label 'solution #:requires hidden-requires exprs ...))]))

(define tamer-story-space
  (lambda []
    (module->namespace (tamer-story))))

(define tamer-require
  (lambda [id]
    (namespace-variable-value id #true #false
                              (tamer-story-space))))

(define tamer-apply
  (lambda [id . argv]
    (define f (tamer-require id))

    (apply f argv)))

(define tamer-datum
  (lambda [id . argv]
    (define v (tamer-require id))
    (define datum
      (cond [(procedure? v) (apply v argv)]
            [else v]))

    (racket #,datum)))

(define tamer-prove
  (lambda []
    (spec-prove
     (if (module-path? (tamer-story))

         (let ([htag (tamer-story->tag (tamer-story))])
           (make-spec-feature htag
                              (or (and (dynamic-require (tamer-story) #false)
                                       (hash-has-key? handbook-stories htag)
                                       (hash-ref handbook-stories htag))
                                  null)))

         (make-spec-feature "Features and Behaviors"
                            (if (> (hash-count handbook-stories) 0)
                                (for/list ([htag (in-list (reverse (hash-ref handbook-stories books#)))])
                                  (make-spec-feature htag (hash-ref handbook-stories htag)))

                                ; no story ==> no :books:
                                null))))))

(define tamer-deftech
  (lambda [#:key [key #false] #:normalize? [normalize? #true] #:origin [origin #false] #:abbr [abbr #false] . body]
    (define term
      (list ($tex:phantomsection)
            (apply deftech #:key key #:normalize? normalize? #:style? #true body)))

    (cond [(and origin abbr) (list term "(" (tamer-deftech abbr) "," ~ (tamer-deftech origin) ")")]
          [(or origin abbr) (list term "(" (tamer-deftech (or origin abbr)) ")")]
          [else term])))

(define tamer-elemtag
  (lambda [tag #:style [style #false] #:type [type 'tamer] . body]
    (make-target-element style body `(,type ,tag))))

(define tamer-elemref
  (lambda [tag #:style [style #false] #:type [type 'tamer] . body]
    (make-link-element style body `(,type ,tag))))

(define tamer-smart-summary
  (lambda []
    (define this-story (tamer-story))
    (define ~symbol (default-spec-issue-symbol))
    (define ~fgcolor (default-spec-issue-fgcolor))
    (define raco-setup-forget-my-digimon (current-digimon))
    
    (make-traverse-block
     (λ [get set!]
       (if (handbook-markdown-renderer? get)
           (para (literal "---"))
           (make-delayed-block
            (λ [render% pthis infobase]
              (define get (handbook-resolved-info-getter infobase))
              (define scenarios (get tamer-scribble-story-id tamer-empty-issues))
              (define btimes (get tamer-scribble-story-times tamer-empty-issues))
              (define issues (get tamer-scribble-story-issues tamer-empty-issues))

              (parameterize ([current-digimon raco-setup-forget-my-digimon])
                (nested #:style tamer-boxed-style
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
                                                             (cons story (hash-ref scenarios story (λ [] null))))
                                                           (apply map + tamer-empty-times (hash-values btimes))
                                                           (apply hash-union tamer-empty-issues (hash-values issues) #:combine +)))])
                                   (define bookmarks
                                     (for/list ([bookmark (in-list (reverse features))])
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
                                           (list (elem (italic (string local#)) ~ (tamer-elemref brief (racketkeywordfont (literal brief))))
                                                 (tamer-elemref brief symtype #:style "plainlink"))
                                           (let ([head (~a brief #:width 64 #:pad-string "." #:limit-marker "......")]
                                                 [stts (make-parameter issue-type)])
                                             (echof #:fgcolor 'lightyellow head)
                                             (echof #:fgcolor (~fgcolor issue-type) "~a~n" symtype)
                                             (list (elem (italic (string book#)) ~ (secref (car bookmark)))
                                                   (seclink (car bookmark) symtype #:underline? #false))))))

                                   (match-define-values ((list pass misbehaved panic skip todo) (list memory cpu real gc))
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
  (lambda [example #:note [note margin-note] . notes]
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
         
         ((if (procedure? note) note (λ body (nested #:style note body)))
          (unless (null? notes) (append notes (list (linebreak) (linebreak))))
          (parameterize ([tamer-story this-story]
                         [default-spec-issue-handler void])
            ; seed : (Vector (Pairof (Listof (U Spec-Issue String tamer-feature)) (Listof tamer-feature)) (Listof scrible-flow))
            (define (downfold-feature brief indent seed:info)
              (define flows (vector-ref seed:info 1))
              
              (vector (cons null (vector-ref seed:info 0))
                      (cond [(= indent toplevel-indent)
                             (cons (nonbreaking (racketmetafont (italic (string open-book#)) ~ (tamer-elemtag brief (literal brief)))) flows)]
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
            
            (define (fold-behavior brief issue indent memory cpu real gc seed:info)
              (define issues (vector-ref seed:info 0))
              (define idx (add1 (length (car issues))))
              (define type (spec-issue-type issue))
              (define flow (nonbreaking ((if (= indent toplevel-indent) (curry tamer-elemtag brief) elem)
                                         (~a (~symbol type)) (racketkeywordfont ~ (italic (number->string idx)))
                                         (racketcommentfont ~ (literal brief)))))
              
              (vector (cons (cons (if (eq? type 'pass) brief issue) (car issues)) (cdr issues))
                      (cons flow (vector-ref seed:info 1))))
            
            (match-define-values ((cons summary (vector features flows)) memory cpu real gc)
              (time-apply* (λ [] (spec-summary-fold (make-spec-feature htag (hash-ref handbook-stories htag null))
                                                    (vector null null)
                                                    #:downfold downfold-feature #:upfold upfold-feature #:herefold fold-behavior
                                                    #:selector (list '* '* example)))))
            
            (define population (apply + (hash-values summary)))
            
            (hash-set! scenarios htag (append (hash-ref scenarios htag (λ [] null)) features))
            (hash-set! btimes htag (map + (list memory cpu real gc) (hash-ref btimes htag (λ [] tamer-empty-times))))
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
                 (define line (read-line in.rkt 'any))
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
(define-tamer-indexed-figure figure #:anchor #false
  [#:style [align-style tamer-center-block-style]] #:with [legend pre-flows]
  #:λ (make-figure-block legend align-style pre-flows))

(define tamer-figure-margin
  (lambda [id caption #:style [align-style margin-figure-style] #:legend-style [legend-style chia-legend-style] . pre-flows]
    (tamer-indexed-block id tamer-figure-type
                         (tamer-default-figure-label) (tamer-default-figure-label-separator) (tamer-default-figure-label-tail) caption
                         marginfigure-style legend-style (tamer-default-figure-label-style) (tamer-default-figure-caption-style) #false
                         (λ [] (make-figure-block align-style pre-flows)) #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-tamer-indexed-list code #:anchor #false
  [srcpath #:language [language #false] #:rootdir [rootdir #false] #:style [align-style tamer-left-block-style] #:oc-ness [ocness 'close]]
  #:with [legend maybe-range]
  #:λ (make-code-block legend align-style language rootdir srcpath ocness maybe-range))

(define tamer-code-class
  (lambda [#:alignment [alignment 'here] #:alt-pxend [alt-pxend #false]
           #:language [alt-lang #false] #:rootdir [rootdir #false]
           id caption srcpath]
    (define ext (path-get-extension srcpath))
    (define language (or alt-lang (source->language srcpath)))
    (define-values (pxstart pxdefend) (lang->class-range language id))
    (define full-id (merge-ext+id language ext id))
    (define pxend (or alt-pxend pxdefend))
    
    (case alignment
      [(here) (tamer-code! #:language language #:rootdir rootdir full-id caption srcpath pxstart pxend)]
      [(wide) (tamer-code* #:language language #:rootdir rootdir full-id caption srcpath pxstart pxend)]
      [else   (tamer-code  #:language language #:rootdir rootdir full-id caption srcpath pxstart pxend)])))

(define tamer-code-function
  (lambda [#:language [alt-lang #false] #:rootdir [rootdir #false] #:alignment [alignment 'here]
           #:class [cls-name #false] #:ns [ns #false]
           #:subpattern [subpattern #false] #:alt-pxend [alt-pxend #false]
           id caption srcpath]
    (define ext (path-get-extension srcpath))
    (define language (or alt-lang (source->language srcpath)))
    (define-values (pxdefstart pxdefend) (lang->function-range language id ns))
    (define full-id (merge-ext+id language ext id))
    (define pxstart (cons pxdefstart subpattern))
    (define pxend (or alt-pxend pxdefend))

    (case alignment
      [(here) (tamer-code! #:language language #:rootdir rootdir full-id caption srcpath pxstart pxend)]
      [(wide) (tamer-code* #:language language #:rootdir rootdir full-id caption srcpath pxstart pxend)]
      [else   (tamer-code  #:language language #:rootdir rootdir full-id caption srcpath pxstart pxend)])))
