#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/string)

(require "../../../digitama/tamer/typed.rkt")
(require "../../../digitama/tamer/scrbl.rkt")
(require "../../../digitama/tamer/stat.rkt")
(require "../../../digitama/tamer/selector.rkt")

(require "../../../digitama/minimal/dtrace.rkt")
(require "../../../digitama/minimal/regexp.rkt")

(require "../../../digitama/latex.rkt")
(require "../../../digitama/system.rkt")
(require "../../../filesystem.rkt")

(require "../parameter.rkt")
(require "../ffi.rkt")
(require "../phony.rkt")
(require "../spec.rkt")
(require "../path.rkt")
(require "../racket.rkt")

(require "cc.rkt")

(require/typed
 "../../../digitama/tamer/documentclass.rkt"
 [handbook-tex-inspect (-> Path Scribble-Message (Option Part))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct tex-info
  ([path : Path]
   [engine : (Option Symbol)]
   [name : (Option String)]
   [dependencies : (Listof (U Regexp Byte-Regexp))]
   [options : (Listof Keyword)]
   [selector : (Option Handbook-Selector)]
   [extra-argv : (Vectorof String)])
  #:type-name Tex-Info
  #:transparent)

(struct tex-desc
  ([scrbl : Path]
   [alt-name : (Option String)]
   [subdir : Path]
   [self.out : Path]
   [self.tex : Path])
  #:type-name Tex-Desc
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

(define digimon-typeset-desc : (-> Path (Option String) Symbol (Option String) Tex-Desc)
  (lambda [self.scrbl alt-name engine destdir]
    (define TEXNAME.scrbl (or (and alt-name (path-replace-filename self.scrbl (string-append alt-name ".hack-for-dotted-name"))) self.scrbl))
    (define TEXNAME.out : Path
      (assert (tex-document-destination #:extension (tex-document-extension engine #:fallback tex-fallback-engine)
                                        #:dest-dirname destdir
                                        TEXNAME.scrbl #true)))
    
    (define basename (assert (file-name-from-path TEXNAME.out)))
    (define subdir (path-replace-extension basename #""))
    (define TEXNAME.sub (build-path (assert (path-only TEXNAME.out)) subdir basename))
    (define TEXNAME.tex (path-replace-extension TEXNAME.sub #".tex"))

    (tex-desc self.scrbl alt-name subdir
              TEXNAME.out TEXNAME.tex)))

(define digimon-typeset-specs : (-> Part Tex-Desc Path Path
                                    Symbol (Listof Path) Symbol Boolean Scribble-Message
                                    Wisemon-Specification)
  (lambda [scrbl.doc self hook.rktl local-info.rkt engine all-deps topic-name halt-on-error? dtrace-msg]
    (list (wisemon-spec (tex-desc-self.out self) #:^ (list (tex-desc-self.tex self)) #:-
                        (tex-render #:dest-subdir (tex-desc-subdir self) #:fallback tex-fallback-engine #:enable-filter #true
                                    #:halt-on-error? halt-on-error? #:shell-escape? #false
                                    #:dest-copy? #false
                                    engine (tex-desc-self.tex self) (assert (path-only (tex-desc-self.out self))))
                        
                        (handbook-display-metrics dtrace-msg 'note (handbook-stats scrbl.doc 'latex)))
          
          (wisemon-spec (tex-desc-self.tex self) #:^ all-deps #:-
                        (typeset-note topic-name engine (tex-desc-alt-name self) (tex-desc-scrbl self))
                        (handbook-tex-render (tex-desc-scrbl self) scrbl.doc (tex-desc-self.tex self) hook.rktl dtrace-msg)))))

(define make-typesetting-specs : (-> Symbol (Listof Tex-Info) Boolean (Values (Listof Path) (Listof Path) Wisemon-Specification))
  (lambda [topic-name typesettings halt-on-error?]
    (define local-rootdir : Path (digimon-path 'zone))
    (define local-info.rkt : Path (digimon-path 'info))

    (for/fold ([always-files : (Listof Path) null]
               [ignored-files : (Listof Path) null]
               [specs : Wisemon-Specification null])
              ([typesetting (in-list typesettings)])
      (define-values (TEXNAME.scrbl engine) (values (tex-info-path typesetting) (or (tex-info-engine typesetting) tex-fallback-engine)))
      (define-values (maybe-name dependencies) (values (tex-info-name typesetting) (tex-info-dependencies typesetting)))
      (define pwd : Path (assert (path-only TEXNAME.scrbl)))

      (define selector (or (current-user-specified-selector) (tex-info-selector typesetting)))
      (define hook.rktl (path-replace-extension TEXNAME.scrbl #".rktl"))
      (define main-volume : Tex-Desc (digimon-typeset-desc TEXNAME.scrbl maybe-name engine #false))
      (define this-name (assert (file-name-from-path TEXNAME.scrbl)))
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
        (define scrbl.doc (handbook-tex-inspect TEXNAME.scrbl dtrace-msg))
        (define foreign-deps (if (not scrbl.doc) null (handbook-extract-scripts scrbl.doc 'latex dtrace-msg)))
        (define scrbl-deps (filter file-exists? #| <- say, commented out (require)s |# (scribble-smart-dependencies TEXNAME.scrbl)))
        (define regexp-deps (if (pair? dependencies) (find-digimon-files (make-regexps-filter dependencies) local-rootdir) null))
        (define all-deps (append scrbl-deps foreign-deps regexp-deps))
        (define options : (Listof Keyword) (tex-info-options typesetting))

        (define offprints : (Listof (Pairof Part Tex-Desc))
          (if (and scrbl.doc selector)
              (let*-values ([(preface offprints bonus) (handbook-offprint scrbl.doc selector dtrace-msg)]
                            [(volume-name) (path->string (tex-desc-subdir main-volume))]
                            [(rootdir) (path->string (build-path "offprint" volume-name))])
                (for/list : (Listof (Pairof Part Tex-Desc)) ([offprint (in-list offprints)])
                  (define name-tag (cadar (part-tags (car offprint))))
                  (define chapter-name : String
                    (string-append (let ([seq (cdr offprint)])
                                     (cond [(char? seq) (string seq)]
                                           [else (number->string (cdr offprint))]))
                                   "-"
                                   (cond [(string? name-tag) (tex-chapter-name TEXNAME.scrbl name-tag)]
                                         [else volume-name])))
                  
                  (cons (cond [(list? preface)         (struct-copy part scrbl.doc [parts (append preface (cons (car offprint) bonus))])]
                              [(null? (unbox preface)) (struct-copy part scrbl.doc [parts (cons (car offprint) bonus)])]
                              [else (struct-copy part scrbl.doc
                                                 [blocks (append (part-blocks scrbl.doc) (unbox preface))]
                                                 [parts (cons (car offprint) bonus)])])
                        (digimon-typeset-desc TEXNAME.scrbl chapter-name engine rootdir))))
              null))

        (define all-typesets
          (cond [(current-user-request-no-volume?) offprints]
                [(memq '#:no-volume options) offprints]
                [(not scrbl.doc) offprints]
                [else (cons (cons scrbl.doc main-volume) offprints)]))

        (define-values (always-files++ ignored-files++)
          (let ([always-make? (memq '#:always-make options)]
                [explicit? (memq '#:explicitly-make options)]
                [real-targets (current-make-real-targets)])
            (for/fold ([always-selves : (Listof Path) always-files]
                       [ignored-selves : (Listof Path) ignored-files])
                      ([scrbl.desc (in-list all-typesets)])
              (define desc (cdr scrbl.desc))
              (define selves (list (tex-desc-self.out desc) (tex-desc-self.tex desc)))
              (define real-target?
                (or (member (tex-desc-scrbl desc) real-targets)
                    (member (tex-desc-self.out desc) real-targets)))
              
              (cond [(or always-make? (and real-target? explicit?)) (values (append selves always-selves) ignored-files)]
                    [(and explicit? (not real-target?)) (values always-selves (append selves ignored-selves))]
                    [else (values always-selves ignored-selves)]))))
        
        (values
         always-files++
         ignored-files++
         
         ;;; NOTE: order matters
         (if (not scrbl.doc) ; raw tex document
             (append specs
                     (list (wisemon-spec (tex-desc-self.out main-volume) #:^ (filter file-exists? (tex-smart-dependencies TEXNAME.scrbl)) #:-
                                         (define dest-dir : Path (assert (path-only (tex-desc-self.out main-volume))))
                                         
                                         (typeset-note topic-name engine maybe-name TEXNAME.scrbl)
                                         (tex-render #:dest-subdir (tex-desc-subdir main-volume) #:fallback tex-fallback-engine #:enable-filter #false
                                                     #:halt-on-error? halt-on-error? #:shell-escape? #false
                                                     #:dest-copy? #false
                                                     engine TEXNAME.scrbl dest-dir))))
             (apply append specs
                    (for/list : (Listof Wisemon-Specification) ([typeset (in-list all-typesets)])
                      (digimon-typeset-specs (car typeset) (cdr typeset) hook.rktl local-info.rkt
                                             engine all-deps topic-name halt-on-error? dtrace-msg)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-typeset-specs+targets : (-> Symbol (Listof Tex-Info) Boolean (Values (Listof Path) (Listof Path) Wisemon-Specification (Listof Path)))
  (lambda [topic-name typesettings halt-on-error?]
    (define-values (always-files ignored-files specs)
      (make-typesetting-specs topic-name typesettings halt-on-error?))

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

(define typeset-filter-texinfo : (-> Path Any (-> (Listof Symbol)) (Option Tex-Info))
  (lambda [setting.scrbl argv list-engines]
    (define candidates : (Listof Symbol) (list-engines))
    (let*-values ([(maybe-offprints rest) (partition typeset-offprint? (if (list? argv) argv (list argv)))]
                  [(maybe-engines rest) (partition symbol? rest)]
                  [(maybe-options rest) (partition keyword? rest)]
                  [(maybe-names rest) (partition string? rest)]
                  [(dependencies rest) (partition bs-regexp? rest)])
      (tex-info setting.scrbl
                (let check : (Option Symbol) ([engines : (Listof Symbol) maybe-engines])
                  (and (pair? engines)
                       (cond [(memq (car engines) candidates) (car engines)]
                             [else (check (cdr engines))])))
                (and (pair? maybe-names) (car maybe-names))
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

(define typeset-note : (-> Symbol Symbol (Option String) Path Void)
  (lambda [topic-name engine maybe-name TEXNAME.scrbl]
    (if (not maybe-name)
        (dtrace-note "~a ~a: ~a" topic-name engine TEXNAME.scrbl)
        (dtrace-note "~a ~a: ~a [~a]" topic-name engine TEXNAME.scrbl maybe-name))))

(define typeset-dtrace : (-> Symbol Symbol Path Scribble-Message)
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

(define tex-chapter-name : (-> Path String String)
  (lambda [TEXNAME.scrbl name]
    (if (file-exists? (build-path (assert (path-only TEXNAME.scrbl)) name))
        (path->string (path-replace-extension (assert (file-name-from-path name)) #""))
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
  (wisemon-make-info-phony #:name 'typeset #:phony make~typeset #:desc "Typeset writting publication in PDF via LaTex"))
