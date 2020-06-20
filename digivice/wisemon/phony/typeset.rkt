#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/list)

(require "../../../digitama/latex.rkt")
(require "../../../digitama/exec.rkt")
(require "../../../dtrace.rkt")

(require "../parameter.rkt")
(require "../native.rkt")
(require "../phony.rkt")
(require "../spec.rkt")
(require "../racket.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Tex-Info (Pairof Path (Pairof Symbol (Option String))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-digimon-typesettings : (->* (Info-Ref) (Boolean) (Listof Tex-Info))
  (lambda [info-ref [silent #false]]
    (define maybe-typesettings (info-ref 'typesettings (λ [] null)))
    (cond [(not (list? maybe-typesettings)) (raise-user-error 'info.rkt "malformed `typesettings`: ~a" maybe-typesettings)]
          [else (filter-map (λ [typesetting]
                              (if (and (pair? typesetting) (path-string? (car typesetting)))
                                  (let ([setting.scrbl (build-path (current-directory) (car typesetting))])
                                    (and (file-exists? setting.scrbl)
                                         (cons setting.scrbl (filter-typesetting-renderer (cdr typesetting) silent))))
                                  (raise-user-error 'info.rkt "malformed `typesetting`: ~a" typesetting)))
                            maybe-typesettings)])))

(define make-typesetting-specs : (-> Info-Ref Wisemon-Specification)
  (lambda [info-ref]
    (for/list ([typesetting (in-list (find-digimon-typesettings info-ref))])
      (define-values (TEXNAME.scrbl renderer maybe-name) (values (car typesetting) (cadr typesetting) (cddr typesetting)))
      (define raw-tex? (regexp-match? #px"\\.tex$" TEXNAME.scrbl))
      (define TEXNAME.ext (assert (tex-document-destination TEXNAME.scrbl #true #:extension (tex-document-extension renderer #:fallback tex-fallback-renderer))))
      
      (wisemon-spec TEXNAME.ext #:^ (filter file-exists? (if (not raw-tex?) (racket-smart-dependencies TEXNAME.scrbl) (tex-smart-dependencies TEXNAME.scrbl))) #:-
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
                              (eval `(define (tex:render TEXNAME.scrbl #:dest-dir dest-dir)
                                       (render (list (dynamic-require TEXNAME.scrbl 'doc)) (list ,src.tex)
                                               #:render-mixin tex:render-mixin #:dest-dir dest-dir
                                               #:redirect "/~:/" #:redirect-main "/~:/" #:xrefs (list (load-collections-xref)))))
                              
                              (when (file-exists? hook.rktl)
                                (eval `(define (dynamic-load-character-conversions hook.rktl)
                                         (let ([ecc (dynamic-require hook.rktl 'extra-character-conversions (λ [] #false))])
                                           (when (procedure? ecc) (tex:extra-character-conversions ecc)))))
                                (fg-recon-eval renderer `(dynamic-load-character-conversions ,hook.rktl)))
                              
                              (fg-recon-eval renderer `(tex:render ,TEXNAME.scrbl #:dest-dir ,dest-dir))
                              (tex-render renderer src.tex dest-dir #:fallback tex-fallback-renderer #:enable-filter #true)))))))))

(define make~typeset : Make-Phony
  (lambda [digimon info-ref]
    (wisemon-make (make-native-library-specs info-ref))
    (wisemon-compile (current-directory) digimon info-ref)

    (wisemon-make (make-typesetting-specs info-ref) (current-make-real-targets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define filter-typesetting-renderer : (-> Any Boolean (Pairof Symbol (Option String)))
  (lambda [argv silent]
    (define candidates : (Listof Symbol) (tex-list-renderers))
    (define-values (maybe-renderers rest) (partition symbol? (if (list? argv) argv (list argv))))
    (define maybe-names (filter string? rest))
    (cons (let check : Symbol ([renderers : (Listof Symbol) maybe-renderers])
            (cond [(null? renderers)
                   (when (not silent)
                     (dtrace-note "~a typeset: no suitable renderer is found, use `~a` instead"
                                  the-name tex-fallback-renderer #:topic the-name #:prefix? #false))
                   tex-fallback-renderer]
                  [(memq (car renderers) candidates) (car renderers)]
                  [else (check (cdr renderers))]))
          (and (pair? maybe-names) (car maybe-names)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-fallback-renderer 'latex)

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
