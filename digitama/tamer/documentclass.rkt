#lang racket/base

(provide (all-defined-out))

(require scribble/core)
(require scribble/latex-properties)

(require setup/collects)

(require racket/path)
(require racket/file)
(require racket/string)
(require racket/symbol)

(require "../minimal/dtrace.rkt")
(require "../minimal/string.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct tex-config (prefix cjk load style extra-files)
  #:constructor-name make-tex-config
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-tex-config
  (lambda [doclass CJK? load style extra-files]
    (define prefix (tex-documentclass doclass))
    (define enc (and CJK? #"\\usepackage{xeCJK}\n"))
    (define load.tex (tex-segment load))
    (define style.tex (tex-segment style))

    (and (or prefix enc load.tex style.tex (pair? extra-files))
         (make-tex-config prefix enc load.tex style.tex
                          (if (pair? extra-files)
                              (map tex-segment extra-files)
                              null)))))

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
    (define doclass (tex-config-prefix texdoc))
    (define alt-load "scribble-load-replace.tex")
    
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
      (let ([enc (tex-config-cjk texdoc)])
        (cond [(not enc) (or doclass (latex-defaults-prefix self))]
              [(not doclass) (tex-unicode-filter enc (latex-defaults-prefix self))]
              [else (tex-unicode-append enc doclass)])))

    (define style.tex (or (tex-config-style texdoc) (latex-defaults-style self)))
    (define extra-files
      (cons pdfinfo.tex
            (cond [(not doclass) (latex-defaults-extra-files self)]
                  [else (tex-config-extra-files texdoc)])))

    (let-values ([(dclass doptions) (tex-documentclass-info prefix)])
      (dtrace-debug "documentclass: ~a" #:topic engine dclass)
      (when (string? doptions)
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
            (list* (tex-config-prefix tinfo)
                   (tex-config-load tinfo)
                   (tex-config-style tinfo)
                   (tex-config-extra-files tinfo)))))

(define tex-documentclass-option
  (lambda [option]
    (cond [(symbol? option) (symbol->immutable-string option)]
          [(string? option) option]
          [(pair? option)
           (format "~a=~a" (car option)
             (case (cdr option)
               [(#true) 'true]
               [(#false) 'false]
               [else (cdr option)]))]
          [else (format "~a" option)])))

(define tex-documentclass
  (lambda [doclass]
    (cond [(or (not doclass) (null? doclass)) #false]
          [(list? doclass)
           (cond [(null? (cdr doclass)) (tex-documentclass (car doclass))]
                 [else (string->bytes/utf-8 (format "\\documentclass[~a]{~a}\n"
                                              (string-join (map tex-documentclass-option (cdr doclass)) ",")
                                              (car doclass)))])]
          [(or (path? doclass) (path-string? doclass)) (simple-form-path doclass)]
          [else (string->bytes/utf-8 (format "\\documentclass{~a}\n" doclass))])))

(define tex-documentclass-info
  (lambda [prefix]
    (define doclass (car (~string-lines prefix)))
    (define portions (regexp-match #px"^.documentclass(.(.+).)?[{](\\w+)[}]\\s*$" doclass))

    (when (not portions)
      (raise-user-error "syntax error: \\documentclass: " doclass))

    (values (cadddr portions) (caddr portions))))

(define tex-unicode-append
  (lambda [enc prefix]
    (bytes-append
     (cond [(bytes? prefix) prefix]
           [else (file->bytes (collects-relative->path prefix))])
     enc)))

(define tex-unicode-filter
  (lambda [enc prefix]
    (define px:encs
      '([#px#"\\\\usepackage\\[utf8\\][{]inputenc[}][\n\r]*" ""]
        [#px#"\\\\usepackage\\[T1\\][{]fontenc[}][\n\r]*" ""]))

    (and prefix
         (bytes-append
          (if (bytes? prefix)
              (regexp-replaces prefix px:encs)
              (call-with-input-file* (collects-relative->path prefix)
                (λ [/dev/texin] (regexp-replaces /dev/texin px:encs))))
          enc))))


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
