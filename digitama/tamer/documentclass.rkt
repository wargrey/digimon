#lang racket/base

(provide (all-defined-out))

(require scribble/core)
(require scribble/latex-properties)

(require setup/collects)

(require racket/path)
(require racket/file)
(require racket/string)
(require file/convertible)

(require (submod "scrbl.rkt" unsafe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-tex-inspect
  (lambda [scrbl dtrace adjust?]
    (if (regexp-match? #px"\\.scrbl$" scrbl)
        (let ([doc (dynamic-require scrbl 'doc)])
          (dtrace 'note "Inspecting ~a" scrbl)
          (cond [(not adjust?) doc]
                [else (let ([new-style (handbook-style-adjust doc dtrace)])
                        (struct-copy part doc [style new-style]))]))
        #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct tex-config (documentclass options cjk load style extra-files)
  #:constructor-name make-tex-config
  #:transparent
  #:property prop:convertible
  (λ [self mime fallback]
    (case mime
      [(script-path src-path) (tex-config-paths self)]
      [else fallback])))

(define default-tex-config
  (make-tex-config #false null #false
                   #false #false
                   null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-tex-config
  (lambda [doclass options CJK? load style extra-files]
    (define enc (and CJK? #"\\usepackage{xeCJK}\n"))
    (define load.tex (tex-segment load))
    (define style.tex (tex-segment style))

    (and (or doclass options enc load.tex style.tex (pair? extra-files))
         (make-tex-config (tex-documentclass doclass) options
                          enc load.tex style.tex
                          (if (pair? extra-files)
                              (map tex-segment extra-files)
                              null)))))

(define handbook-style-adjust
  (lambda [self dtrace]
    (define s (part-style self))
    (define s:props (style-properties s))
    (define h:tconf (memf tex-config? s:props))

    (define-values (props setup)
      (let adjust ([rest s:props]
                   [sporp null]
                   [setup #false]
                   [done? #false])
        (cond [(pair? rest)
               (let-values ([(self tail) (values (car rest) (cdr rest))])
                 (cond [(latex-defaults? self)
                        (let* ([tconf (if (pair? h:tconf) (car h:tconf) default-tex-config)]
                               [tex-config (handbook-documentclass-adjust tconf self dtrace)])
                          (adjust tail (cons tex-config sporp) setup #true))]
                       [(tex-config? self) (adjust tail (cons self sporp) setup done?)] ; for collecting dependencies
                       [(and (hash? self) (immutable? self) (hash-has-key? self 'hypersetup)) (adjust tail (cons self sporp) self done?)]
                       [else (adjust tail (cons self sporp) setup done?)]))]
              [(not done?)
               (values (if (pair? h:tconf)
                           (let ([tex-prop (handbook-documentclass-adjust (car h:tconf) #false dtrace)])
                             (cons tex-prop (reverse sporp)))
                           (reverse sporp))
                       setup)]
              [else (values (reverse sporp) setup)])))

    (let ([/dev/pdfout (open-output-bytes '/dev/pdfout)]
          [/dev/cfgout (open-output-bytes '/dev/cfgout)])
      (tex-hypersetup-pdfinfo self /dev/pdfout dtrace)
      (tex-hypersetup-config setup /dev/cfgout dtrace)
      (make-style (style-name s)
                  (list* (tex-addition (get-output-bytes /dev/pdfout))
                         (tex-addition (get-output-bytes /dev/cfgout))
                         props)))))

(define handbook-documentclass-adjust
  (lambda [texconf self dtrace]
    (define alt-load "scribble-load-replace.tex")
    (define doclass (tex-config-documentclass texconf))

    (define replacements
      (let ([load.tex (tex-config-load texconf)]
            [replaces (and (latex-defaults+replacements? self) (latex-defaults+replacements-replacements self))])
        (cond [(not replaces) (and load.tex (hash alt-load load.tex))]
              [(not (immutable? replaces))
               (unless (not load.tex)
                 (hash-set! replaces alt-load load.tex))
               replaces]
              [else (if (not load.tex) replaces (hash-set replaces alt-load load.tex))])))

    (define prefix
      (tex-documentclass-option-replace
       (let ([enc (tex-config-cjk texconf)])
         (cond [(or doclass) (tex-unicode-append enc doclass)]
               [(or self) (tex-unicode-filter enc (latex-defaults-prefix self))]
               [else (tex-unicode-append enc (tex-documentclass 'article))]))
       (tex-config-options texconf)))

    (define style.tex
      (or (tex-config-style texconf)
          (cond [(not self) #""]
                [else (latex-defaults-style self)])))
    
    (define extra-files
      (append (if (or doclass) (tex-config-extra-files texconf) null)
              (if (or self) (latex-defaults-extra-files self) null)))

    (let-values ([(dclass doptions) (tex-documentclass-info prefix #true)])
      (dtrace 'note "documentclass: ~a" dclass)
      (when (bytes? doptions)
        (dtrace 'note "options: ~a" doptions))
      
      (dtrace 'debug "embed '~a' for style" (tex-desc style.tex))

      (when (hash? replacements)
        (for ([(target tex) (in-hash replacements)])
          (dtrace 'debug "embed '~a' as '~a'" (tex-desc tex) target)))

      (for ([tex (in-list extra-files)])
        (dtrace 'debug "copy `~a`" (tex-desc tex))))

    (if (hash? replacements)
        (make-latex-defaults+replacements prefix style.tex extra-files replacements)
        (make-latex-defaults prefix style.tex extra-files))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tex-config-paths
  (lambda [tinfo]
    (filter path?
            (list* (tex-config-documentclass tinfo)
                   (tex-config-load tinfo)
                   (tex-config-style tinfo)
                   (tex-config-extra-files tinfo)))))

(define tex-hypersetup-pdfinfo
  (lambda [self /dev/pdfout dtrace]
    (define-values (title subtitles) (handbook-extract-title+subtitles self))
    (define-values (authors keywords) (handbook-extract-metainfo self))
    
    (displayln "\\hypersetup{" /dev/pdfout)
    
    (when (and title)
      (dtrace 'note "title: ~a" title)
      
      (for ([subtitle (in-list subtitles)])
        (dtrace 'note "subtitle: ~a" subtitle))
      
      (if (null? subtitles)
          (fprintf /dev/pdfout "    pdftitle={~a},~n" title)
          (fprintf /dev/pdfout "    pdftitle={~a: ~a},~n" title (car subtitles))))
    
    (when (pair? authors)
      (dtrace 'note "authors: ~a" authors)
      (fprintf /dev/pdfout "    pdfauthor={~a},~n" (string-join authors "; ")))

    (when (pair? keywords)
      (dtrace 'note "keywords: ~a" keywords)
      (fprintf /dev/pdfout "    pdfkeywords={~a},~n" (string-join authors ", ")))

    (fprintf /dev/pdfout "    pdfcreator={wisemon over Scribble},~n")
    
    (displayln "}" /dev/pdfout)
    (newline /dev/pdfout)))

(define tex-hypersetup-config
  (lambda [cfg /dev/pdfout dtrace]
    (displayln "\\hypersetup{" /dev/pdfout)
    
    (fprintf /dev/pdfout "    pdffitwindow=true,~n") ; window fit to page when opened
    (fprintf /dev/pdfout "    colorlinks=true,~n")   ; false: boxed links; true: colored links

    (when (and cfg)
      (for ([(key color) (in-immutable-hash (hash-remove cfg 'hypersetup))])
        (dtrace 'debug "~a: ~a" key color)
        (fprintf /dev/pdfout "    ~a=~a,~n" key color)))
    
    (displayln "}" /dev/pdfout)
    (newline /dev/pdfout)))

(define tex-documentclass
  (lambda [doclass]
    (cond [(or (string? doclass) (path? doclass)) (simple-form-path doclass)]
          [(symbol? doclass) (string->bytes/utf-8 (format "\\documentclass{~a}~n" doclass))]
          [else doclass])))

(define tex-documentclass-info
  (lambda [prefix maybe-comments]    
    (define portions (regexp-match #px"^\\s*\\\\documentclass(\\[(.+)\\])?[{](\\w+)[}]\\s*" prefix))

    (cond [(and portions) (values (cadddr portions) (caddr portions))]
          [else (when (not maybe-comments)
                  (raise-user-error "syntax error: \\documentclass: " prefix))

                (let skip-comment ([/dev/texin (open-input-bytes prefix)])
                  (define line (read-bytes-line /dev/texin 'any))

                  (unless (bytes? line)
                    (raise-user-error "no \\documentclass found: " prefix))

                  (if (regexp-match? #px#"^\\s*\\\\documentclass" line)
                      (tex-documentclass-info line #false)
                      (skip-comment /dev/texin)))])))

(define tex-documentclass-option-append
  (lambda [option /dev/bytout no-pair?]
    (cond [(symbol? option) (write option /dev/bytout)]
          [(string? option) (write-string option /dev/bytout)]
          [(eq? option #true) (write 'true /dev/bytout)]
          [(eq? option #false) (write 'false /dev/bytout)]
          [(and no-pair?) (write option /dev/bytout)]
          [(pair? option)
           (tex-documentclass-option-append (car option) /dev/bytout #true)
           (unless (null? (cdr option))
             (write '= /dev/bytout)
             (if (list? (cdr option))
                 (tex-documentclass-option-append (cadr option) /dev/bytout #true)
                 (tex-documentclass-option-append (cdr option) /dev/bytout #true)))]
          [else (write option /dev/bytout)])))

(define tex-documentclass-option-join
  (lambda [options sep [pre #false] [post #false]]
    (define /dev/bytout (open-output-bytes))

    (when (bytes? pre)
      (write-bytes pre /dev/bytout))
    
    (tex-documentclass-option-append (car options) /dev/bytout #false)
    (for ([opt (in-list (cdr options))])
      (write-bytes sep /dev/bytout)
      (tex-documentclass-option-append opt /dev/bytout #false))

    (when (bytes? post)
      (write-bytes post /dev/bytout))

    (get-output-bytes /dev/bytout #true)))

(define tex-documentclass-option-replace
  (lambda [prefix opts]
    (if (list? opts)
        (if (null? opts)
            (regexp-replace #px#"documentclass(\\[.+\\])?" prefix #"documentclass")
            (regexp-replace #px#"documentclass(\\[.+\\])?" prefix
                            (tex-documentclass-option-join opts #"," #"documentclass[" #"]")))
        prefix)))

(define tex-unicode-append
  (lambda [enc documentclass]
    (define body
      (cond [(bytes? documentclass) documentclass]
            [else (file->bytes (collects-relative->path documentclass))]))

    (cond [(not enc) body]
          [else (bytes-append body enc)])))

(define tex-unicode-filter
  (lambda [enc prefix]
    (define px:encs #px#"\\\\usepackage\\[(utf8|T1)\\][{](input|font)enc[}][\n\r]*")

    (define body
      (regexp-replace* px:encs
                       (cond [(bytes? prefix) prefix]
                             [else (file->bytes (collects-relative->path prefix))])
                       ""))

    (cond [(not enc) body]
          [else (bytes-append body enc)])))

(define tex-segment
  (lambda [res]
    (and res
         (cond [(bytes? res) res]
               [(equal? res "") #""]
               [else (simple-form-path res)]))))

(define tex-desc
  (lambda [segment]
    (cond [(bytes? segment) segment]
          [else (collects-relative->path segment)])))
