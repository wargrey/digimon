#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/list)

(require "../../../digitama/latex.rkt")
(require "../../../digitama/exec.rkt")
(require "../../../digitama/system.rkt")
(require "../../../dtrace.rkt")

(require "../parameter.rkt")
(require "../native.rkt")
(require "../phony.rkt")
(require "../spec.rkt")
(require "../path.rkt")
(require "../racket.rkt")

(require/typed
 "../../../digitama/tamer.rkt"
 [handbook-metainfo (-> Path-String String (Values String String))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Tex-Info (Pairof Path (List Symbol (Option String) (Listof (U Regexp Byte-Regexp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-digimon-typesettings : (->* (Info-Ref) (Boolean) (Listof Tex-Info))
  (lambda [info-ref [silent #false]]
    (define maybe-typesettings (info-ref 'typesettings (λ [] null)))
    (cond [(not (list? maybe-typesettings)) (raise-user-error 'info.rkt "malformed `typesettings`: ~a" maybe-typesettings)]
          [else ((inst filter-map Tex-Info Any)
                 (λ [typesetting]
                   (if (and (pair? typesetting) (path-string? (car typesetting)))
                       (let ([setting.scrbl (build-path (current-directory) (car typesetting))])
                         (and (file-exists? setting.scrbl)
                              (cons setting.scrbl (filter-typesetting-renderer (cdr typesetting) silent))))
                       (raise-user-error 'info.rkt "malformed `typesetting`: ~a" typesetting)))
                 maybe-typesettings)])))

(define make-typesetting-specs : (-> Info-Ref Wisemon-Specification)
  (lambda [info-ref]
    (define local-info.rkt : Path (digimon-path 'info))
    (define local-stone : Path (digimon-path 'stone))
    (define local-tamer.tex (build-path local-stone "tamer.tex"))

    (for/fold ([specs : Wisemon-Specification null])
              ([typesetting (in-list (find-digimon-typesettings info-ref))])
      (define-values (TEXNAME.scrbl renderer maybe-name regexps) (values (car typesetting) (cadr typesetting) (caddr typesetting) (cadddr typesetting)))
      (define raw-tex? (regexp-match? #px"\\.tex$" TEXNAME.scrbl))
      (define TEXNAME.ext (assert (tex-document-destination TEXNAME.scrbl #true #:extension (tex-document-extension renderer #:fallback tex-fallback-renderer))))
      (define this-stone (build-path local-stone (assert (file-name-from-path (path-replace-extension TEXNAME.scrbl #"")))))
      (define pdfinfo.tex (path-replace-extension TEXNAME.ext #".hyperref.tex"))
      (define docmentclass.tex (build-path this-stone "documentclass.tex"))
      (define style.tex (build-path this-stone "style.tex"))
      (define load.tex (build-path this-stone "load.tex"))
      (define this-tamer.tex (build-path this-stone "tamer.tex"))
      (define static-deps (if (not raw-tex?) (list pdfinfo.tex local-info.rkt) (list local-info.rkt)))
      (define scrbl-deps (if (not raw-tex?) (racket-smart-dependencies TEXNAME.scrbl) (tex-smart-dependencies TEXNAME.scrbl)))
      (define stone-deps (if (pair? regexps) (find-digimon-files (make-regexps-filter regexps) local-stone) null))
      (define tex-deps (list docmentclass.tex style.tex load.tex this-tamer.tex local-tamer.tex))
      
      (append specs
              (list (wisemon-spec TEXNAME.ext #:^ (append static-deps (filter file-exists? (append tex-deps scrbl-deps stone-deps))) #:-
                                  (define dest-dir : (Option Path) (path-only TEXNAME.ext))
                                  (define pwd : (Option Path) (path-only TEXNAME.scrbl))
                                  
                                  (when (and (path? dest-dir) (path? pwd))
                                    (define ./TEXNAME.scrbl (find-relative-path pwd TEXNAME.scrbl))
                                    
                                    (if (not maybe-name)
                                        (dtrace-note "~a ~a: ~a" the-name renderer TEXNAME.scrbl)
                                        (dtrace-note "~a ~a: ~a [~a]" the-name renderer TEXNAME.scrbl maybe-name))
                                    
                                    (if (and raw-tex?)
                                        (let ([TEXNAME.ext (tex-render renderer TEXNAME.scrbl dest-dir #:fallback tex-fallback-renderer #:enable-filter #false)])
                                          (unless (not maybe-name)
                                            (let* ([ext (path-get-extension TEXNAME.ext)]
                                                   [target.ext (build-path dest-dir (if (bytes? ext) (path-replace-extension maybe-name ext) maybe-name))])
                                              (fg-recon-mv renderer TEXNAME.ext target.ext))))
                                        (let ([src.tex (path-replace-extension TEXNAME.ext #".tex")]
                                              [hook.rktl (path-replace-extension TEXNAME.scrbl #".rktl")])
                                          (parameterize ([current-directory pwd]
                                                         [current-namespace (make-base-namespace)]
                                                         [exit-handler (λ _ (error the-name " typeset: [fatal] ~a needs a proper `exit-handler`!" ./TEXNAME.scrbl))])
                                            (eval '(require (prefix-in tex: scribble/latex-render) setup/xref scribble/render))

                                            (when (file-exists? load.tex)
                                              (dtrace-debug "~a ~a: load hook: ~a" the-name renderer load.tex)
                                              
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
                                              (fg-recon-eval renderer `(dynamic-load-character-conversions ,hook.rktl)))
                                            
                                            (fg-recon-eval renderer `(tex:render ,TEXNAME.scrbl #:dest-dir ,dest-dir))
                                            (tex-render renderer src.tex dest-dir #:fallback tex-fallback-renderer #:enable-filter #true)))))))

              (cond [(and raw-tex?) null]
                    [else (list (wisemon-spec pdfinfo.tex #:^ (list local-info.rkt TEXNAME.scrbl) #:-
                                              (define-values (title authors) (handbook-metainfo TEXNAME.scrbl "; "))
                                              (define (hypersetup [/dev/stdout : Output-Port]) : Void
                                                (displayln "\\hypersetup{" /dev/stdout)
                                                (dtrace-debug "~a ~a: title: ~a" the-name renderer title)
                                                (fprintf /dev/stdout "  pdftitle={~a},~n" title)
                                                (dtrace-debug "~a ~a: authors: ~a" the-name renderer authors)
                                                (fprintf /dev/stdout "  pdfauthor={~a},~n" authors)
                                                (displayln "}" /dev/stdout)
                                                (newline /dev/stdout))
                                              (fg-recon-mkdir renderer (assert (path-only pdfinfo.tex)))
                                              (fg-recon-save-file renderer pdfinfo.tex hypersetup)))])))))
    
(define make~typeset : Make-Phony
  (lambda [digimon info-ref]
    (wisemon-make (make-native-library-specs info-ref))
    (wisemon-compile (current-directory) digimon info-ref)

    (wisemon-make (make-typesetting-specs info-ref) (current-make-real-targets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define filter-typesetting-renderer : (-> Any Boolean (List Symbol (Option String) (Listof (U Regexp Byte-Regexp))))
  (lambda [argv silent]
    (define candidates : (Listof Symbol) (tex-list-renderers))
    (define-values (maybe-renderers rest) (partition symbol? (if (list? argv) argv (list argv))))
    (define maybe-names (filter string? rest))
    (list (let check : Symbol ([renderers : (Listof Symbol) maybe-renderers])
            (cond [(null? renderers)
                   (when (not silent)
                     (dtrace-note "~a typeset: no suitable renderer is found, use `~a` instead"
                                  the-name tex-fallback-renderer #:topic the-name #:prefix? #false))
                   tex-fallback-renderer]
                  [(memq (car renderers) candidates) (car renderers)]
                  [else (check (cdr renderers))]))
          (and (pair? maybe-names) (car maybe-names))
          (filter typeset-regexp? rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-fallback-renderer 'latex)

(define typeset-regexp? : (-> Any Boolean : (U Regexp Byte-Regexp))
  (lambda [v]
    (or (byte-regexp? v)
        (regexp? v))))

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
  (wisemon-make-phony #:name 'typeset #:phony make~typeset #:desc "Typeset writting publication in PDF via LaTex"))
