#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "../../../digitama/latex.rkt")
(require "../../../digitama/exec.rkt")
(require "../../../digitama/system.rkt")
(require "../../../filesystem.rkt")
(require "../../../dtrace.rkt")
(require "../../../predicate.rkt")

(require "../parameter.rkt")
(require "../native.rkt")
(require "../phony.rkt")
(require "../spec.rkt")
(require "../path.rkt")
(require "../racket.rkt")

(require "cc.rkt")

(require/typed
 "../../../digitama/tamer.rkt"
 [handbook-metainfo (-> Path-String String (Values String String))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct tex-info
  ([path : Path]
   [engine : (Option Symbol)]
   [name : (Option String)]
   [dependencies : (Listof (U Regexp Byte-Regexp))]
   [options : (Listof Keyword)]
   [extra-argv : (Vectorof String)])
  #:type-name Tex-Info
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-digimon-typesettings : (->* (Info-Ref)
                                         ((Option (-> (Listof Symbol))) #:info-id Symbol)
                                         (Listof Tex-Info))
  (lambda [#:info-id [symid 'typesettings] info-ref [list-engines #false]]
    (define maybe-typesettings (info-ref symid (λ [] null)))
    
    (unless (list? maybe-typesettings)
      (raise-user-error 'info.rkt "malformed `~a`: ~a" symid maybe-typesettings))
    
    ((inst filter-map Tex-Info Any)
     (λ [typesetting]
       (if (and (pair? typesetting) (path-string? (car typesetting)))
           (let ([setting.scrbl (build-path (current-directory) (path-normalize/system (car typesetting)))])
             (typeset-filter-texinfo setting.scrbl (cdr typesetting) (or list-engines tex-list-engines)))
           (raise-user-error 'info.rkt "malformed `~a`: ~a" symid typesetting)))
     maybe-typesettings)))

(define digimon-scribbles->typesettings : (->* ((Listof Tex-Info) (Listof Path))
                                               ((Option (-> (Listof Symbol))))
                                               (Listof Tex-Info))
  (lambda [info-targets targets [list-engines #false]]
    (for/fold ([required-typesets : (Listof Tex-Info) null])
              ([p (in-list targets)])
      (let ([info-p (findf (λ [[ti : Tex-Info]] (equal? p (tex-info-path ti))) info-targets)])
        (if (not info-p)
            (let ([typeset/fly (typeset-filter-texinfo p null (or list-engines tex-list-engines))])
              (if (not typeset/fly) required-typesets (cons typeset/fly required-typesets)))
            (cons info-p required-typesets))))))

(define make-typesetting-specs : (-> (Listof Tex-Info) (Values (Listof Path) (Listof Path) Wisemon-Specification))
  (lambda [typesettings]
    (define local-rootdir : Path (digimon-path 'zone))
    (define local-info.rkt : Path (digimon-path 'info))
    (define local-stone : Path (digimon-path 'stone))
    (define local-tamer.tex (build-path local-stone "tamer.tex"))
    (define typeset-subdir : String "tex")

    (for/fold ([always-files : (Listof Path) null]
               [ignored-files : (Listof Path) null]
               [specs : Wisemon-Specification null])
              ([typesetting (in-list typesettings)])
      (define-values (TEXNAME.scrbl engine) (values (tex-info-path typesetting) (or (tex-info-engine typesetting) tex-fallback-engine)))
      (define-values (maybe-name dependencies) (values (tex-info-name typesetting) (tex-info-dependencies typesetting)))
      (define raw-tex? (regexp-match? #px"\\.tex$" TEXNAME.scrbl))
      (define RENAMED.scrbl (or (and maybe-name (path-replace-filename TEXNAME.scrbl maybe-name)) TEXNAME.scrbl))
      (define TEXNAME.ext (assert (tex-document-destination RENAMED.scrbl #true #:extension (tex-document-extension engine #:fallback tex-fallback-engine))))
      (define TEXNAME.sub (build-path (assert (path-only TEXNAME.ext)) typeset-subdir (assert (file-name-from-path TEXNAME.ext))))
      (define TEXNAME.tex (path-replace-extension TEXNAME.sub #".tex"))
      (define this-stone (build-path local-stone (assert (file-name-from-path (path-replace-extension TEXNAME.scrbl #"")))))
      (define pdfinfo.tex (path-replace-extension TEXNAME.sub #".hyperref.tex"))
      (define docmentclass.tex (build-path this-stone "documentclass.tex"))
      (define style.tex (build-path this-stone "style.tex"))
      (define load.tex (build-path this-stone "load.tex"))
      (define this-tamer.tex (build-path this-stone "tamer.tex"))
      (define scrbl-deps (scribble-smart-dependencies TEXNAME.scrbl))
      (define stone-deps (if (pair? dependencies) (find-digimon-files (make-regexps-filter dependencies) local-rootdir) null))
      (define tex-deps (list docmentclass.tex style.tex load.tex this-tamer.tex local-tamer.tex))
      (define options : (Listof Keyword) (tex-info-options typesetting))

      (unless (tex-info-engine typesetting)
        (dtrace-note #:topic the-name #:prefix? #false
                     "~a ~a: no suitable engine is found, use `~a` instead"
                     the-name (current-make-phony-goal) tex-fallback-engine))

      (define real-target? : Boolean
        (and (or (member TEXNAME.scrbl (current-make-real-targets))
                 (member TEXNAME.ext (current-make-real-targets)))
             #true))

      (values
       (if (or (memq '#:always-make options)
               (and real-target? (memq '#:explicitly-make options)))
           (list* TEXNAME.ext TEXNAME.tex pdfinfo.tex always-files)
           always-files)

       (if (and (memq '#:explicitly-make options)
                (not real-target?))
           (list* TEXNAME.ext TEXNAME.tex pdfinfo.tex ignored-files)
           ignored-files)

       ;;; NOTE: order matters
       (append specs
               (if (and raw-tex?)
                   (list (wisemon-spec TEXNAME.ext #:^ (filter file-exists? (tex-smart-dependencies TEXNAME.scrbl)) #:-
                                       (define dest-dir : Path (assert (path-only TEXNAME.ext)))
                                       (define pwd : Path (assert (path-only TEXNAME.scrbl)))
                                       
                                       (typeset-note engine maybe-name TEXNAME.scrbl)
                                       (tex-render #:fallback tex-fallback-engine #:enable-filter #false
                                                   engine TEXNAME.scrbl dest-dir)))
                   
                   (list (wisemon-spec TEXNAME.ext #:^ (list TEXNAME.tex) #:-
                                       (tex-render #:dest-subdir typeset-subdir #:fallback tex-fallback-engine #:enable-filter #true
                                                   engine TEXNAME.tex (assert (path-only TEXNAME.ext))))

                         (wisemon-spec TEXNAME.tex #:^ (cons pdfinfo.tex (filter file-exists? (append tex-deps scrbl-deps stone-deps))) #:-
                                       (define dest-dir : Path (assert (path-only TEXNAME.tex)))
                                       (define pwd : Path (assert (path-only TEXNAME.scrbl)))
                                       (define ./TEXNAME.scrbl (find-relative-path pwd TEXNAME.scrbl))
                                       
                                       (typeset-note engine maybe-name TEXNAME.scrbl)
                                       
                                       (let ([src.tex (path-replace-extension TEXNAME.ext #".tex")]
                                             [hook.rktl (path-replace-extension TEXNAME.scrbl #".rktl")])
                                         (parameterize ([current-directory pwd]
                                                        [current-command-line-arguments (tex-info-extra-argv typesetting)]
                                                        [current-namespace (make-base-namespace)]
                                                        [exit-handler (λ _ (error the-name "~a ~a: [fatal] ~a needs a proper `exit-handler`!"
                                                                                  the-name (current-make-phony-goal) ./TEXNAME.scrbl))])
                                           (eval '(require (prefix-in tex: scribble/latex-render) setup/xref scribble/render))
                                           
                                           (when (file-exists? load.tex)
                                             (dtrace-debug "~a ~a: load hook: ~a" the-name engine load.tex)
                                             
                                             (eval '(require scribble/core scribble/latex-properties))
                                             (eval `(define (tex:replace-property p)
                                                      (cond [(not (latex-defaults? p)) p]
                                                            [else (make-latex-defaults+replacements
                                                                   (latex-defaults-prefix p)
                                                                   (latex-defaults-style p)
                                                                   (latex-defaults-extra-files p)
                                                                   (hash "scribble-load-replace.tex" ,load.tex))])))
                                                    (eval '(define (tex:replace doc)
                                                             (define tex:style (part-style doc))
                                                             (struct-copy part doc
                                                                          [style (make-style (style-name tex:style)
                                                                                             (map tex:replace-property
                                                                                                  (style-properties tex:style)))]))))
                                           
                                           (eval `(define (tex:render TEXNAME.scrbl #:dest-dir dest-dir)
                                                    (define TEXNAME.doc (dynamic-require TEXNAME.scrbl 'doc))
                                                    (render (list (if (file-exists? ,load.tex) (tex:replace TEXNAME.doc) TEXNAME.doc)) (list ,src.tex)
                                                            #:render-mixin tex:render-mixin #:dest-dir dest-dir
                                                            #:prefix-file (and (file-exists? ,docmentclass.tex) ,docmentclass.tex)
                                                            #:style-file (and (file-exists? ,style.tex) ,style.tex) #:style-extra-files (list ,pdfinfo.tex)
                                                            #:redirect "/~:/" #:redirect-main "/~:/" #:xrefs (list (load-collections-xref)))))
                                           
                                           (when (file-exists? hook.rktl)
                                             (eval `(define (dynamic-load-character-conversions hook.rktl)
                                                      (let ([ecc (dynamic-require hook.rktl 'extra-character-conversions (λ [] #false))])
                                                        (when (procedure? ecc) (tex:extra-character-conversions ecc)))))
                                             (fg-recon-eval engine `(dynamic-load-character-conversions ,hook.rktl)))
                                           
                                           (fg-recon-eval engine `(tex:render ,TEXNAME.scrbl #:dest-dir ,dest-dir)))))
                         
                         (wisemon-spec pdfinfo.tex #:^ (filter file-exists? (list local-info.rkt TEXNAME.scrbl)) #:-
                                       (define-values (title authors) (handbook-metainfo TEXNAME.scrbl "; "))
                                       (define dest-dir : Path (assert (path-only pdfinfo.tex)))
                                       
                                       (define (hypersetup [/dev/stdout : Output-Port]) : Void
                                         (displayln "\\hypersetup{" /dev/stdout)
                                         (dtrace-debug "~a ~a: title: ~a" the-name engine title)
                                         (fprintf /dev/stdout "  pdftitle={~a},~n" title)
                                         (dtrace-debug "~a ~a: authors: ~a" the-name engine authors)
                                         (fprintf /dev/stdout "  pdfauthor={~a},~n" authors)
                                         (displayln "}" /dev/stdout)
                                         (newline /dev/stdout))
                                       
                                       (unless (directory-exists? dest-dir)
                                         (fg-recon-mkdir engine dest-dir))
                                       
                                       (fg-recon-save-file engine pdfinfo.tex hypersetup)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-typeset-specs+targets : (-> (Option Info-Ref) (Values (Listof Path) (Listof Path) Wisemon-Specification (Listof Path)))
  (lambda [info-ref]
    (define all-typesettings (if (not info-ref) null (find-digimon-typesettings info-ref)))
    (define real-goals : (Listof Path) (current-make-real-targets))

    (define-values (always-files ignored-files specs)
      (make-typesetting-specs
       (cond [(null? real-goals) all-typesettings]
             [else (digimon-scribbles->typesettings all-typesettings real-goals)])))

    (values always-files ignored-files specs (wisemon-targets-flatten specs))))

(define make-typeset-prepare : (-> String Info-Ref Any)
  (lambda [digimon info-ref]
    (define natives (map (inst car Path CC-Launcher-Info) (find-digimon-native-launcher-names info-ref #false)))
    
    (wisemon-make (make-native-library-specs info-ref natives) px.so)
    (wisemon-compile (current-directory) digimon info-ref)))

(define make-typeset : (-> Wisemon-Specification (Listof Path) (Listof Path) (Listof Path) Void)
  (lambda [specs always-files ignored-files targets]
    (parameterize ([make-assumed-oldfiles (append always-files (make-assumed-oldfiles))]
                   [make-assumed-newfiles (append ignored-files (make-assumed-newfiles))])
      (wisemon-make specs targets))))

(define make~typeset : Make-Info-Phony
  (lambda [digimon info-ref]
    (define-values (always-files ignored-files specs targets) (make-typeset-specs+targets info-ref))
    
    (make-typeset-prepare digimon info-ref)
    (make-typeset specs always-files ignored-files targets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define typeset-filter-texinfo : (-> Path Any (-> (Listof Symbol)) (Option Tex-Info))
  (lambda [setting.scrbl argv list-engines]
    (define candidates : (Listof Symbol) (list-engines))
    (let*-values ([(maybe-engines rest) (partition symbol? (if (list? argv) argv (list argv)))]
                  [(tags rest) (partition keyword? rest)]
                  [(maybe-names rest) (partition string? rest)]
                  [(dependencies rest) (partition bs-regexp? rest)])
      (tex-info setting.scrbl
                (let check : (Option Symbol) ([engines : (Listof Symbol) maybe-engines])
                  (and (pair? engines)
                       (cond [(memq (car engines) candidates) (car engines)]
                             [else (check (cdr engines))])))
                (and (pair? maybe-names) (car maybe-names))
                dependencies
                tags
                (list->vector
                 (for/fold ([argv : (Listof String) null])
                           ([arg (in-list rest)])
                   (cond [(list? arg) (append argv (for/list : (Listof String) ([a (in-list arg)]) (format "~a" a)))]
                         [(vector? arg) (append argv (for/list : (Listof String) ([a (in-vector arg)]) (format "~a" a)))]
                         [else argv])))))))

(define typeset-note : (-> Symbol (Option String) Path Void)
  (lambda [engine maybe-name TEXNAME.scrbl]
    (if (not maybe-name)
        (dtrace-note "~a ~a: ~a" the-name engine TEXNAME.scrbl)
        (dtrace-note "~a ~a: ~a [~a]" the-name engine TEXNAME.scrbl maybe-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-fallback-engine : Symbol 'latex)

(define tex-smart-dependencies : (->* (Path-String) ((Listof Path)) (Listof Path))
  (lambda [entry [memory null]]
    (foldl (λ [[subpath : Bytes] [memory : (Listof Path)]] : (Listof Path)
             (define subsrc (simplify-path (build-path (assert (path-only entry) path?) (bytes->string/utf-8 subpath))))
             (cond [(member subsrc memory) memory]
                   [else (tex-smart-dependencies subsrc memory)]))
           (append memory (list (if (string? entry) (string->path entry) entry)))
           (call-with-input-file* entry
             (λ [[texin : Input-Port]]
               (regexp-match* #px"(?<=\\\\(input|include(only)?)[{]).+?.(tex)(?=[}])"
                              texin))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define typeset-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'typeset #:phony make~typeset #:desc "Typeset writting publication in PDF via LaTex"))
