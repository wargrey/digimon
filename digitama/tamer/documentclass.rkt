#lang racket/base

(provide (all-defined-out))

(require scribble/core)
(require scribble/latex-properties)

(require setup/collects)

(require racket/path)
(require racket/file)
(require racket/symbol)
(require file/convertible)

(require "../minimal/dtrace.rkt")
(require "../minimal/string.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct tex-config (documentclass options cjk load style extra-files tex.bib)
  #:constructor-name make-tex-config
  #:transparent
  #:property prop:convertible
  (λ [self mime fallback]
    (case mime
      [(script-path src-path) (tex-config-paths self)]
      [else fallback])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-tex-config
  (lambda [doclass options CJK? load style extra-files pbib]
    (define enc (and CJK? #"\\usepackage{xeCJK}\n"))
    (define load.tex (tex-segment load))
    (define style.tex (tex-segment style))

    (and (or doclass options enc load.tex style.tex (pair? extra-files))
         (make-tex-config (tex-documentclass doclass) options
                          enc load.tex style.tex
                          (if (pair? extra-files)
                              (map tex-segment extra-files)
                              null)
                          (tex-segment pbib)))))

(define handbook-style-adjust
  (lambda [s pdfinfo.tex engine]
    (define s:props (style-properties s))
    (define h:texdoc (memf tex-config? s:props))

    (if (pair? h:texdoc)
        (let adjust ([rest s:props]
                     [sporp null])
          (cond [(null? rest) (make-style (style-name s) (reverse sporp))]
                [else (let-values ([(self tail) (values (car rest) (cdr rest))])
                        (cond [(tex-config? self) (adjust tail sporp)]
                              [(not (latex-defaults? self)) (adjust tail (cons self sporp))]
                              [else (let ([tex-config (handbook-documentclass-adjust (car h:texdoc) self pdfinfo.tex engine)])
                                      (adjust tail (cons tex-config sporp)))]))]))
        s)))

(define handbook-documentclass-adjust
  (lambda [texdoc self pdfinfo.tex engine]
    (define alt-load "scribble-load-replace.tex")
    (define doclass (tex-config-documentclass texdoc))

    (define replacements
      (let ([load.tex (tex-config-load texdoc)]
            [replaces (and (latex-defaults+replacements? self) (latex-defaults+replacements-replacements self))])
        (cond [(not replaces) (and load.tex (hash alt-load load.tex))]
              [(not (immutable? replaces))
               (unless (not load.tex)
                 (hash-set! replaces alt-load load.tex))
               replaces]
              [else (if (not load.tex) replaces (hash-set replaces alt-load load.tex))])))

    (define prefix
      (tex-documentclass-option-replace
       (let ([enc (tex-config-cjk texdoc)])
         (if (not doclass)
             (tex-unicode-filter enc (latex-defaults-prefix self))
             (tex-unicode-append enc doclass)))
       (tex-config-options texdoc)))

    (define style.tex (or (tex-config-style texdoc) (latex-defaults-style self)))
    (define extra-files
      (cons pdfinfo.tex
            (cond [(not doclass) (latex-defaults-extra-files self)]
                  [else (tex-config-extra-files texdoc)])))

    (let-values ([(dclass doptions) (tex-documentclass-info prefix #true)])
      (dtrace-debug "documentclass: ~a" #:topic engine dclass)
      (when (bytes? doptions)
        (dtrace-debug "options: ~a" #:topic engine doptions))
      
      (dtrace-debug "embeds `~a` for style" #:topic engine (tex-desc style.tex))

      (when (hash? replacements)
        (for ([(target tex) (in-hash replacements)])
          (dtrace-debug "embeds `~a` as `~a`" #:topic engine (tex-desc tex) target)))

      (for ([tex (in-list extra-files)])
        (dtrace-debug "copied `~a`" #:topic engine (tex-desc tex))))

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
                   (tex-config-tex.bib tinfo)
                   (tex-config-extra-files tinfo)))))

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
