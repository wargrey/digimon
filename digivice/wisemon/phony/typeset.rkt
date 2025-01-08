#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/string)

(require "../../../digitama/tamer/typed.rkt")
(require "../../../digitama/tamer/scrbl.rkt")
(require "../../../digitama/tamer/stat.rkt")

(require "../../../digitama/minimal/dtrace.rkt")
(require "../../../digitama/minimal/string.rkt")

(require "../../../digitama/latex.rkt")
(require "../../../digitama/exec.rkt")
(require "../../../digitama/system.rkt")
(require "../../../filesystem.rkt")
(require "../../../predicate.rkt")

(require "../parameter.rkt")
(require "../ffi.rkt")
(require "../phony.rkt")
(require "../spec.rkt")
(require "../path.rkt")
(require "../racket.rkt")

(require "cc.rkt")

(require/typed
 "../../../digitama/tamer/documentclass.rkt"
 [handbook-tex-inspect (-> Path Path Path (-> Symbol String Any * Any) (Values (Option Part) (-> Path Void)))])

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

(define make-typesetting-specs : (-> Symbol (Listof Tex-Info) Boolean (Values (Listof Path) (Listof Path) Wisemon-Specification))
  (lambda [topic-name typesettings halt-on-error?]
    (define local-rootdir : Path (digimon-path 'zone))
    (define local-info.rkt : Path (digimon-path 'info))
    (define typeset-subdir : String "tex")

    (for/fold ([always-files : (Listof Path) null]
               [ignored-files : (Listof Path) null]
               [specs : Wisemon-Specification null])
              ([typesetting (in-list typesettings)])
      (define-values (TEXNAME.scrbl engine) (values (tex-info-path typesetting) (or (tex-info-engine typesetting) tex-fallback-engine)))
      (define-values (maybe-name dependencies) (values (tex-info-name typesetting) (tex-info-dependencies typesetting)))
      (define pwd : Path (assert (path-only TEXNAME.scrbl)))

      (define RENAMED.scrbl (or (and maybe-name (path-replace-filename TEXNAME.scrbl maybe-name)) TEXNAME.scrbl))
      (define TEXNAME.ext (assert (tex-document-destination RENAMED.scrbl #true #:extension (tex-document-extension engine #:fallback tex-fallback-engine))))
      (define TEXNAME.sub (build-path (assert (path-only TEXNAME.ext)) typeset-subdir (assert (file-name-from-path TEXNAME.ext))))
      (define TEXNAME.tex (path-replace-extension TEXNAME.sub #".tex"))
      (define this-name (assert (file-name-from-path TEXNAME.scrbl)))
      (define pdfinfo.tex (path-replace-extension TEXNAME.sub #".pdfinfo.tex"))
      (define dtrace-msg (typeset-dtrace topic-name engine this-name))
      
      (unless (tex-info-engine typesetting)
        (dtrace-warning #:topic topic-name #:prefix? #false
                        "~a ~a: ~a: no suitable engine is found, use `~a` instead"
                        topic-name (current-make-phony-goal) this-name
                        tex-fallback-engine))
      
      (parameterize ([current-directory pwd]
                     [current-command-line-arguments (tex-info-extra-argv typesetting)]
                     [exit-handler (λ _ (error topic-name "~a ~a: [fatal] ~a needs a proper `exit-handler`!"
                                               topic-name (current-make-phony-goal) (find-relative-path local-rootdir TEXNAME.scrbl)))])
        (define-values (scribble.doc self->tex) (handbook-tex-inspect TEXNAME.scrbl TEXNAME.tex pdfinfo.tex dtrace-msg))
        (define foreign-deps  (if (not scribble.doc) null (handbook-scripts scribble.doc 'latex)))
        (define scrbl-deps (filter file-exists? #| <- say, commented out (require)s |# (scribble-smart-dependencies TEXNAME.scrbl)))
        (define regexp-deps (if (pair? dependencies) (find-digimon-files (make-regexps-filter dependencies) local-rootdir) null))
        (define options : (Listof Keyword) (tex-info-options typesetting))

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
                 (if (not scribble.doc)
                     (list (wisemon-spec TEXNAME.ext #:^ (filter file-exists? (tex-smart-dependencies TEXNAME.scrbl)) #:-
                                         (define dest-dir : Path (assert (path-only TEXNAME.ext)))
                                         
                                         (typeset-note topic-name engine maybe-name TEXNAME.scrbl)
                                         (tex-render #:fallback tex-fallback-engine #:enable-filter #false
                                                     #:halt-on-error? halt-on-error? #:shell-escape? #false #:dest-copy? #true
                                                     engine TEXNAME.scrbl dest-dir)))
                     
                     (list (wisemon-spec TEXNAME.ext #:^ (list TEXNAME.tex) #:-
                                         (tex-render #:dest-subdir typeset-subdir #:fallback tex-fallback-engine #:enable-filter #true
                                                     #:halt-on-error? halt-on-error? #:shell-escape? #false #:dest-copy? #true
                                                     engine TEXNAME.tex (assert (path-only TEXNAME.ext)))

                                         (handbook-display-metrics dtrace-msg 'note
                                                                   (handbook-stats scribble.doc 'latex)))
                           
                           (wisemon-spec TEXNAME.tex #:^ (list* pdfinfo.tex (append scrbl-deps foreign-deps regexp-deps)) #:-
                                         (define hook.rktl (path-replace-extension TEXNAME.scrbl #".rktl"))
                                         
                                         (typeset-note topic-name engine maybe-name TEXNAME.scrbl)
                                         (self->tex hook.rktl))
                           
                           (wisemon-spec pdfinfo.tex #:^ (filter file-exists? (list TEXNAME.scrbl local-info.rkt)) #:-
                                         (define-values (title authors) (handbook-metainfo scribble.doc))
                                         (define dest-dir : Path (assert (path-only pdfinfo.tex)))
                                         
                                         (define (hypersetup [/dev/stdout : Output-Port]) : Void
                                           (displayln "\\hypersetup{" /dev/stdout)
                                           
                                           (when (non-empty-string? title)
                                             (define title-lines (~string-lines title))
                                             
                                             (dtrace-msg 'debug "title: ~a" (car title-lines))
                                             
                                             (for ([subtitle (in-list (cdr title-lines))])
                                               (unless (regexp-match? #px"^\\s*$" subtitle)
                                                 (dtrace-msg 'debug "subtitle: ~a" subtitle)))
                                             
                                             (fprintf /dev/stdout "  pdftitle={~a},~n" title))
                                           
                                           (when (pair? authors)
                                             (dtrace-msg 'debug "authors: ~a" authors)
                                             (fprintf /dev/stdout "  pdfauthor={~a},~n" (string-join authors "; ")))
                                           
                                           (displayln "}" /dev/stdout)
                                           (newline /dev/stdout))
                                         
                                         (unless (directory-exists? dest-dir)
                                           (fg-recon-mkdir engine dest-dir))
                                         
                                         (fg-recon-save-file engine pdfinfo.tex hypersetup))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-typeset-specs+targets : (-> Symbol (Listof Tex-Info) Boolean (Values (Listof Path) (Listof Path) Wisemon-Specification (Listof Path)))
  (lambda [topic-name typesettings halt-on-error?]
    (define-values (always-files ignored-files specs) (make-typesetting-specs topic-name typesettings halt-on-error?))

    (values always-files ignored-files specs (wisemon-targets-flatten specs))))

(define make-typeset-prepare : (-> String (Option Info-Ref) (Listof Tex-Info))
  (lambda [digimon info-ref]
    (define all-typesettings (if (not info-ref) null (find-digimon-typesettings info-ref)))
    (define real-goals : (Listof Path) (current-make-real-targets))
    (define texinfos : (Listof Tex-Info)
      (cond [(null? real-goals) all-typesettings]
            [else (digimon-scribbles->typesettings all-typesettings real-goals)]))

    (when (and info-ref)
      (define natives (map (inst car Path CC-Launcher-Info) (find-digimon-native-launcher-names info-ref #false)))
    
      (wisemon-make (make-ffi-library-specs info-ref natives) px.so)
      (wisemon-compile (current-directory) digimon info-ref))

    (compile-scribble (map tex-info-path texinfos) (current-directory))
    texinfos))

(define make-typeset : (-> Wisemon-Specification (Listof Path) (Listof Path) (Listof Path) Boolean Void)
  (lambda [specs always-files ignored-files targets always-run?]
    (parameterize ([make-assumed-oldfiles (append always-files (make-assumed-oldfiles))]
                   [make-assumed-newfiles (append ignored-files (make-assumed-newfiles))])
      (wisemon-make specs targets always-run?))))

(define make~typeset : Make-Info-Phony
  (lambda [digimon info-ref]
    (define typesettings : (Listof Tex-Info) (make-typeset-prepare digimon info-ref))

    (when (pair? typesettings)
      (define-values (always-files ignored-files specs targets)
        (make-typeset-specs+targets the-name typesettings (make-verbose)))
      
      (make-typeset specs always-files ignored-files targets (make-always-run)))))

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

(define typeset-note : (-> Symbol Symbol (Option String) Path Void)
  (lambda [topic-name engine maybe-name TEXNAME.scrbl]
    (if (not maybe-name)
        (dtrace-note "~a ~a: ~a" topic-name engine TEXNAME.scrbl)
        (dtrace-note "~a ~a: ~a [~a]" topic-name engine TEXNAME.scrbl maybe-name))))

(define typeset-dtrace : (-> Symbol Symbol Path (-> Symbol String Any * Any))
  (lambda [topic-name engine TEXNAME.scrbl]
    (define scrptdb : (HashTable Path Boolean) (make-hash))
    
    (values
     #;(λ [level title]
       (when (<= 0 level 1)
         (dtrace-debug "~a ~a: ~a: ~a~a"
                       topic-name engine TEXNAME.scrbl
                       (~space (* level 2)) title)))
     #;(λ [scrpt]
       (unless (hash-has-key? scrptdb scrpt)
         (hash-set! scrptdb scrpt #true)
         (dtrace-debug "~a ~a: ~a: [depend on ~a]"
                       topic-name engine TEXNAME.scrbl scrpt)))
     (λ [level fmt . argl]
       (apply dtrace-message
              level (string-append "~a ~a: ~a: " fmt)
              topic-name engine TEXNAME.scrbl
              argl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-fallback-engine : Symbol 'xelatex)

(define tex-smart-dependencies : (->* (Path) ((Listof Path)) (Listof Path))
  (lambda [entry [memory null]]
    (foldl (λ [[subpath : Bytes] [memory : (Listof Path)]] : (Listof Path)
             (define subsrc (simplify-path (build-path (assert (path-only entry) path?) (bytes->string/utf-8 subpath))))
             (cond [(member subsrc memory) memory]
                   [else (tex-smart-dependencies subsrc memory)]))
           (append memory (list entry))
           (call-with-input-file* entry
             (λ [[texin : Input-Port]]
               (regexp-match* #px"(?<=\\\\(input|include(only)?)[{]).+?.(tex)(?=[}])"
                              texin))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define typeset-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'typeset #:phony make~typeset #:desc "Typeset writting publication in PDF via LaTex"))
