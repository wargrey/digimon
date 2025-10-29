#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/symbol)

(require "../../../digitama/tamer/stat.rkt")
(require "../../../digitama/tamer/scrbl.rkt")
(require "../../../digitama/tamer/render.rkt")
(require "../../../digitama/tamer/selector.rkt")

(require "../../../digitama/minimal/dtrace.rkt")
(require "../../../digitama/minimal/regexp.rkt")

(require "../../../digitama/latex.rkt")
(require "../../../digitama/system.rkt")
(require "../../../filesystem.rkt")
(require "../../../scribble.rkt")

(require "../../wisemon/display.rkt")
(require "../parameter.rkt")
(require "../ffi.rkt")
(require "../phony.rkt")
(require "../spec.rkt")
(require "../path.rkt")
(require "../racket.rkt")

(require "cc.rkt")

(require typed/racket/unsafe)

(unsafe-require/typed
 "../../../digitama/tamer/documentclass.rkt"
 [handbook-tex-inspect (-> Path Scribble-Message Boolean (Option Part))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Wisemon-Scribble->Extension (-> Symbol Bytes))
(define-type Wisemon-Scribble->Specification
  (-> Symbol Part Tex-Desc (Listof Path) Scribble-Message
      (U Wisemon-Spec (Listof Wisemon-Spec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct tex-info
  ([path : Path]
   [engine : (Option Symbol)]
   [alias : (Option String)]
   [dependencies : (Listof (U Regexp Byte-Regexp))]
   [options : (Listof Keyword)]
   [selector : (Option Handbook-Selector)]
   [extra-argv : (Vectorof String)])
  #:type-name Tex-Info
  #:transparent)

(struct tex-desc
  ([volume.scrbl : Path]
   [alias : (Option String)]
   [subdir : Path]
   [self.out : Path]
   [self.tex : Path])
  #:type-name Tex-Desc
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-digimon-typesettings : (->* (Info-Ref)
                                         ((Option (-> (Listof Symbol))) #:force-engine (Option Symbol) #:info-id Symbol)
                                         (Listof Tex-Info))
  (lambda [#:info-id [symid 'typesettings] #:force-engine [force-engine #false] info-ref [list-engines #false]]
    (define maybe-typesettings (info-ref symid (λ [] null)))
    
    (unless (list? maybe-typesettings)
      (raise-user-error 'info.rkt "malformed `~a`: ~a" symid maybe-typesettings))
    
    ((inst filter-map Tex-Info Any)
     (λ [typesetting]
       (if (and (pair? typesetting) (path-string? (car typesetting)))
           (let ([setting.scrbl (build-path (current-directory) (path-normalize/system (car typesetting)))])
             (typeset-filter-texinfo setting.scrbl (cdr typesetting) (or list-engines tex-list-engines) force-engine))
           (raise-user-error 'info.rkt "malformed `~a`: ~a" symid typesetting)))
     maybe-typesettings)))

(define digimon-scribbles->typesettings : (->* ((Listof Tex-Info) (Listof Path))
                                               ((Option (-> (Listof Symbol))) #:force-engine (Option Symbol))
                                               (Listof Tex-Info))
  (lambda [#:force-engine [force-engine #false] info-targets targets [list-engines #false]]
    (for/fold ([required-typesets : (Listof Tex-Info) null])
              ([p (in-list targets)])
      (let ([info-p (findf (λ [[ti : Tex-Info]] (equal? p (tex-info-path ti))) info-targets)])
        (if (not info-p)
            (let ([typeset/fly (typeset-filter-texinfo p null (or list-engines tex-list-engines) force-engine)])
              (if (not typeset/fly) required-typesets (cons typeset/fly required-typesets)))
            (cons info-p required-typesets))))))

(define digimon-typeset-files : (-> Tex-Info (U (Listof (Pairof (Option Part) Tex-Desc)) Tex-Desc) (Values (Listof Path) (Listof Path)))
  (lambda [typesetting all-typesets]
    (define options : (Listof Keyword) (tex-info-options typesetting))
    
    (let ([always-make? (memq '#:always-make options)]
          [explicit? (memq '#:explicitly-make options)]
          [real-targets (current-make-real-targets)])
      (for/fold ([always-selves : (Listof Path) null]
                 [ignored-selves : (Listof Path) null])
                ([scrbl.desc (if (list? all-typesets) (in-list all-typesets) (in-value (cons #false all-typesets)))])
        (define desc (cdr scrbl.desc))
        (define selves (list (tex-desc-self.out desc) (tex-desc-self.tex desc)))
        (define real-target?
          (or (member (tex-desc-volume.scrbl desc) real-targets)
              (member (tex-desc-self.out desc) real-targets)))
        
        (cond [(or always-make? (and real-target? explicit?)) (values (append selves always-selves) ignored-selves)]
              [(and explicit? (not real-target?)) (values always-selves (append selves ignored-selves))]
              [else (values always-selves ignored-selves)])))))

(define digimon-typeset-desc : (-> Path (Option String) (Option String) Bytes Tex-Desc)
  (lambda [self.scrbl alias destdir extension]
    (define TEXNAME.scrbl (or (and alias (path-replace-filename self.scrbl (string-append alias ".hack-for-dotted-name"))) self.scrbl))
    (define TEXNAME.out : Path (assert (tex-document-destination #:extension extension #:dest-dirname destdir TEXNAME.scrbl #true)))
    
    (define basename (assert (file-name-from-path TEXNAME.out)))
    (define subdir (path-replace-extension basename #""))
    (define TEXNAME.sub (build-path (assert (path-only TEXNAME.out)) subdir basename))
    (define TEXNAME.tex (path-replace-extension TEXNAME.sub #".tex"))

    (tex-desc self.scrbl alias subdir
              TEXNAME.out TEXNAME.tex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-typesetting-scrbl-specs : (-> Path Part Tex-Desc Tex-Info Symbol Scribble-Message Wisemon-Scribble->Specification
                                           (Values (Listof Path) (Listof Path) Wisemon-Specification))
  (lambda [VOLUME.scrbl scrbl.doc volume-desc typesetting engine dtrace-msg make-specs]
    (define zonedir : Path (digimon-path 'zone))
    (define info.rkt : Path (digimon-path 'info))
    
    (define op.cfg (or (current-user-specified-selector) (tex-info-selector typesetting)))
    (define dependencies (tex-info-dependencies typesetting))

    (define scrbl-deps (filter file-exists? #| <- say, commented out (require)s |# (scribble-smart-dependencies VOLUME.scrbl)))
    (define regexp-deps (if (pair? dependencies) (find-digimon-files (make-regexps-filter dependencies) zonedir) null))
    (define all-deps (append (if (file-exists? info.rkt) (list info.rkt) null) scrbl-deps regexp-deps))
    
    (define offprints : (Listof (Pairof Part Tex-Desc))
      (if (and op.cfg)
          (let*-values ([(preface offprints hooks bonus) (handbook-offprint scrbl.doc op.cfg dtrace-msg)]
                        [(volume-name) (path->string (tex-desc-subdir volume-desc))]
                        [(rootdir) (path->string (build-path "offprint" volume-name))])
            (for/list : (Listof (Pairof Part Tex-Desc)) ([offprint (in-list offprints)])
              (define name-tag (cadar (part-tags (car offprint))))
              (define chapter-seq : Handbook-Chapter-Index (cdr offprint))
              (define chapter-name : String
                (string-append (cond [(char? chapter-seq) (string chapter-seq)]
                                     [else (number->string chapter-seq)])
                               "-"
                               (cond [(string? name-tag) (tex-chapter-name zonedir name-tag)]
                                     [else volume-name])))
              (define body+back-parts : (Listof Part)
                (if (exact-integer? chapter-seq)
                    (cons (car offprint) (append hooks bonus))
                    (append hooks (cons (car offprint) bonus))))
              
              (cons (cond [(list? preface)         (struct-copy part scrbl.doc [parts (append preface body+back-parts)])]
                          [(null? (unbox preface)) (struct-copy part scrbl.doc [parts body+back-parts])]
                          [else (struct-copy part scrbl.doc
                                             [blocks (append (part-blocks scrbl.doc) (unbox preface))]
                                             [parts body+back-parts])])
                    (digimon-typeset-desc VOLUME.scrbl chapter-name rootdir
                                          (assert (path-get-extension (tex-desc-self.out volume-desc)))))))
          null))
    
    (define all-typesets
      (cond [(current-user-request-no-volume?) offprints]
            [(memq '#:no-volume (tex-info-options typesetting)) offprints]
            [(not scrbl.doc) offprints]
            [else (cons (cons scrbl.doc volume-desc) offprints)]))
    
    (define-values (always-files ignored-files) (digimon-typeset-files typesetting all-typesets))
    
    (values always-files ignored-files
            (apply append
                   (for/list : (Listof Wisemon-Specification) ([typeset (in-list all-typesets)])
                     (define specs : (U Wisemon-Spec Wisemon-Specification)
                       (make-specs engine (car typeset) (cdr typeset)
                                   (append all-deps (handbook-extract-scripts (car typeset) 'latex dtrace-msg))
                                   dtrace-msg))

                     (cond [(list? specs) specs]
                           [else (list specs)]))))))

(define make-typesetting-specs : (-> (Listof Tex-Info) Boolean Wisemon-Scribble->Specification Wisemon-Scribble->Extension
                                     (Values (Listof Path) (Listof Path) Wisemon-Specification))
  (lambda [typesettings adjust? make-specs engine-extension]
    (define zonedir : Path (digimon-path 'zone))

    (for/fold ([always-files : (Listof Path) null]
               [ignored-files : (Listof Path) null]
               [specs : Wisemon-Specification null])
              ([typesetting (in-list typesettings)])
      (define-values (TEXNAME.scrbl alias) (values (tex-info-path typesetting) (tex-info-alias typesetting)))
      (define engine (or (tex-info-engine typesetting) tex-fallback-engine))
      (define volume-desc : Tex-Desc (digimon-typeset-desc TEXNAME.scrbl alias #false (engine-extension engine)))
      (define this-name (assert (file-name-from-path TEXNAME.scrbl)))
      (define dtrace-msg (make-wisemon-dtrace engine this-name))
      
      (unless (tex-info-engine typesetting)
        (dtrace-warning #:topic (the-cmd-name) #:prefix? #false
                        "~a ~a: ~a: no suitable engine is found, use `~a` instead"
                        (the-cmd-name) (current-make-phony-goal) this-name
                        tex-fallback-engine))
      
      (parameterize ([current-directory (assert (path-only TEXNAME.scrbl))]
                     [current-command-line-arguments (tex-info-extra-argv typesetting)]
                     [exit-handler (λ _ (error (the-cmd-name) "~a: [fatal] ~a needs a proper `exit-handler`!"
                                               (current-make-phony-goal) (find-relative-path zonedir TEXNAME.scrbl)))])
        (define scrbl.doc (handbook-tex-inspect TEXNAME.scrbl dtrace-msg adjust?))
        (define-values (this-always this-ignored this-specs)
          (if (or scrbl.doc)
              (make-typesetting-scrbl-specs TEXNAME.scrbl scrbl.doc volume-desc typesetting engine dtrace-msg make-specs)
              (make-typesetting-raw-tex-specs TEXNAME.scrbl volume-desc typesetting engine dtrace-msg)))
        
        (values (append always-files this-always)
                (append ignored-files this-ignored)
                (append specs this-specs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-typeset-prepare : (-> String (Option Info-Ref) (Option Symbol) (Listof Tex-Info))
  (lambda [digimon info-ref force-engine]
    (define all-typesettings (if (not info-ref) null (find-digimon-typesettings info-ref #:force-engine force-engine)))
    (define real-goals : (Listof Path) (current-make-real-targets))
    (define texinfos : (Listof Tex-Info)
      (cond [(null? real-goals) all-typesettings]
            [else (digimon-scribbles->typesettings all-typesettings real-goals #:force-engine force-engine)]))

    (when (and info-ref)
      (define natives (map (inst car Path CC-Launcher-Info) (find-digimon-native-launcher-names info-ref #false)))
    
      (wisemon-make (make-ffi-library-specs info-ref natives) px.so)
      (wisemon-compile (current-directory) digimon info-ref))

    (compile-scribble (map tex-info-path texinfos) (current-directory))
    texinfos))

(define make-typeset : (->* ((Pairof Tex-Info (Listof Tex-Info)) Boolean)
                            (Boolean Wisemon-Scribble->Specification Wisemon-Scribble->Extension)
                            (Listof Path))
  (lambda [typesettings always-run? [adjust? #true] [make-specs typeset-scrbl-specs] [engine-ext typeset-engine-extension]]
    (define-values (always-files ignored-files specs) (make-typesetting-specs typesettings adjust? make-specs engine-ext))
    (define targets (wisemon-targets-flatten specs))
    
    (parameterize ([make-assumed-oldfiles (append always-files (make-assumed-oldfiles))]
                   [make-assumed-newfiles (append ignored-files (make-assumed-newfiles))])
      (wisemon-make specs targets always-run?)
      targets)))

(define make~typeset : Make-Info-Phony
  (lambda [digimon info-ref]
    (define typesettings : (Listof Tex-Info) (make-typeset-prepare digimon info-ref #false))
      
    (when (pair? typesettings)
      (void (make-typeset typesettings (make-always-run))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-typesetting-raw-tex-specs : (-> Path Tex-Desc Tex-Info Symbol Scribble-Message (Values (Listof Path) (Listof Path) (List Wisemon-Spec)))
  (lambda [VOLUME.tex volume-desc typesetting engine dtrace-msg]
    (define dependencies (tex-info-dependencies typesetting))
    (define scrbl-deps (filter file-exists? #| <- say, commented out includes |# (tex-smart-dependencies VOLUME.tex)))
    (define regexp-deps (if (pair? dependencies) (find-digimon-files (make-regexps-filter dependencies) (digimon-path 'zone)) null))
    (define all-deps (append scrbl-deps regexp-deps))
    
    (define-values (always-files ignored-files) (digimon-typeset-files typesetting volume-desc))
    
    (values always-files ignored-files
            (list (wisemon-spec (tex-desc-self.out volume-desc) #:^ all-deps #:-
                                (define dest-dir : Path (assert (path-only (tex-desc-self.out volume-desc))))

                                (tex-render #:dest-subdir (tex-desc-subdir volume-desc) #:fallback tex-fallback-engine #:enable-filter #false
                                            #:halt-on-error? (make-trace-log) #:shell-escape? #false
                                            #:dest-copy? #false
                                            engine VOLUME.tex dest-dir))))))

(define typeset-scrbl-specs : Wisemon-Scribble->Specification
  (lambda [engine scrbl.doc volume-desc all-deps dtrace-msg]
    (define hook.rktl : Path (path-replace-extension (tex-desc-volume.scrbl volume-desc) #".rktl"))
    
    (list (wisemon-spec (tex-desc-self.out volume-desc) #:^ (list (tex-desc-self.tex volume-desc)) #:-
                        (tex-render #:dest-subdir (tex-desc-subdir volume-desc) #:fallback tex-fallback-engine #:enable-filter #true
                                    #:halt-on-error? (make-trace-log) #:shell-escape? #false
                                    #:dest-copy? #false
                                    engine (tex-desc-self.tex volume-desc) (assert (path-only (tex-desc-self.out volume-desc))))
                        
                        (handbook-display-metrics dtrace-msg 'note (handbook-stats scrbl.doc 'latex)))
          
          (wisemon-spec (tex-desc-self.tex volume-desc) #:^ (append (if (file-exists? hook.rktl) (list hook.rktl) null) all-deps) #:-
                        (handbook-tex-render (tex-desc-volume.scrbl volume-desc) scrbl.doc
                                             (tex-desc-self.tex volume-desc) hook.rktl
                                             dtrace-msg)))))

(define typeset-engine-extension : Wisemon-Scribble->Extension
  (lambda [engine]
    (tex-document-extension engine #:fallback tex-fallback-engine)))

(define typeset-default-extension : Wisemon-Scribble->Extension
  (lambda [engine]
    (bytes-append #"." (string->bytes/utf-8 (symbol->immutable-string engine)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Typeset-Offprint-Datum (U '#:offprint (Pairof (U 'offprint '#:offprint) Any)))

(define typeset-offprint? : (-> Any Boolean : #:+ Typeset-Offprint-Datum)
  (lambda [option]
    (or (eq? option '#:offprint)
        (and (pair? option)
             (eq? (car option) '#:offprint)))))

(define typeset-offprint-option : (-> (Pairof Typeset-Offprint-Datum (Listof Typeset-Offprint-Datum)) (Option Handbook-Selector))
  (lambda [options]
    (handbook-selector-flatten
     (for/list : (Listof Any) ([opt (in-list options)])
       (if (pair? opt)
           (cdr opt)
           (values 'all))))))

(define typeset-filter-texinfo : (-> Path Any (-> (Listof Symbol)) (Option Symbol) (Option Tex-Info))
  (lambda [setting.scrbl argv list-engines force-engine]
    (define candidates : (Listof Symbol) (list-engines))
    (let*-values ([(maybe-offprints rest) (partition typeset-offprint? (if (list? argv) argv (list argv)))]
                  [(maybe-engines rest) (partition symbol? rest)]
                  [(maybe-options rest) (partition keyword? rest)]
                  [(aliass rest) (partition string? rest)]
                  [(dependencies rest) (partition bs-regexp? rest)])
      (tex-info setting.scrbl
                (or force-engine
                    (let check : (Option Symbol) ([engines : (Listof Symbol) maybe-engines])
                      (and (pair? engines)
                           (cond [(memq (car engines) candidates) (car engines)]
                                 [else (check (cdr engines))]))))
                (and (pair? aliass) (car aliass))
                dependencies
                maybe-options
                (and (pair? maybe-offprints)
                     (typeset-offprint-option maybe-offprints))
                (list->vector
                 (for/fold ([argv : (Listof String) null])
                           ([arg (in-list rest)])
                   (cond [(list? arg) (append argv (for/list : (Listof String) ([a (in-list arg)]) (format "~a" a)))]
                         [(vector? arg) (append argv (for/list : (Listof String) ([a (in-vector arg)]) (format "~a" a)))]
                         [else argv])))))))

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

(define tex-chapter-name : (-> Path String String)
  (lambda [zonedir name]
    (define maybe-subpath (path-normalize/system name))
    
    (if (or (regexp-match? #px"[.](scrbl|rkt)$" name)
            (file-exists? (build-path zonedir maybe-subpath))
            (file-exists? (build-path (current-directory) maybe-subpath)))
        (path->string (path-replace-extension (assert (file-name-from-path maybe-subpath)) #""))
        (list->string (for/list : (Listof Char) ([ch (in-string name)])
                        (cond [(char-alphabetic? ch) ch]
                              [(char-numeric? ch) ch]
                              [(char-blank? ch) #\_]
                              [(char-symbolic? ch) #\-]
                              [(char-punctuation? ch) #\-]
                              [(char-iso-control? ch) #\-]
                              [else ch]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define typeset-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'typeset #:phony make~typeset #:desc "Typeset publications in PDF via LaTex"))
