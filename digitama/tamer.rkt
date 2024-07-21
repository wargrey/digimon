#lang racket

(provide (all-defined-out) quote-module-path)

(require racket/sandbox)
(require racket/symbol)
(require syntax/location)

(require setup/xref)
(require setup/dirs)
(require scribble/core)
(require scribble/xref)
(require scribble/manual)
(require scribble/example)

(require "../emoji.rkt")
(require "../system.rkt")
(require "../tongue.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-story (make-parameter #false))
(define tamer-story-disable-submodule (make-parameter #false))
(define tamer-story-submodule-name (make-parameter 'tamer))

(define tamer-story-propagate-exceptions (make-parameter #true))
(define tamer-story-lang+modules (make-parameter null))
(define tamer-story-private-modules (make-parameter null))
(define tamer-index-story (make-parameter (cons 0 #false)))
(define tamer-appendix-index (make-parameter #false))

(define tamer-endnote (make-parameter void))
(define tamer-endnote-section (make-parameter void))

(define tamer-filebox-line-number-space (make-parameter 2))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (tamer-story->tag stx)
  (syntax-case stx []
    [(_ story-sexp)
     (syntax/loc stx
       (path->string
        (find-relative-path (digimon-path 'zone)
                            (tamer-story->file-path story-sexp))))]))

(define tamer-story->modpath
  (lambda [story-path]
    (cond [(tamer-story-disable-submodule) story-path]
          [else `(submod ,story-path ,(tamer-story-submodule-name))])))

(define tamer-story->file-path
  (lambda [story-modpath]
    (if (pair? story-modpath) (cadr story-modpath) story-modpath)))

(define tamer-story->tamer-modpath
  (lambda [story-modpath]
    (define htag (tamer-story->tag story-modpath))
    (define modpath
      (cond [(string-prefix? htag "literacy") (digimon-path 'literacy "literacy.rkt")]
            [(string-prefix? htag "tamer") (digimon-path 'tamer "tamer.rkt")]
            [else #false]))

    (or (and modpath (file-exists? modpath) modpath)
        (collection-file-path "tamer.rkt" "digimon"))))

(define tamer-story->module
  (lambda [story [use-tamer-modpath? #true]]
    (cond [(module-declared? story #true) story]
          [(or use-tamer-modpath?) (tamer-story->tamer-modpath story)]
          [else #false])))

(define tamer-resource-files
  (lambda [dirname basename .res]
    (define tamer.res (string-append "tamer" .res))
    (define literacy.res (string-append "literacy" .res))
    (define self.res (string-append basename .res))
    
    (remove-duplicates (list (collection-file-path literacy.res "digimon" "stone" "typeset")
                             (collection-file-path tamer.res "digimon" "stone" "typeset")
                             (build-path dirname literacy.res)
                             (build-path dirname tamer.res)
                             (build-path dirname self.res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For Scribble and Racket
(define handbook-stories #;(HashTable Symbol (U (Listof (-> Spec-Feature)) (Listof String))) (make-hash))

(define tamer-record-story
  (lambda [name unit]
    (define htag (tamer-story->tag (tamer-story)))
    
    (hash-set! handbook-stories htag
               (append (hash-ref handbook-stories htag null)
                       (list (λ [] unit))))
    
    (let ([books (hash-ref handbook-stories books# null)])
      (unless (member htag books)
        ;;; keep the order of stories for indexing
        (hash-set! handbook-stories books#
                   (cons htag books))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For Examples
(define tamer-zones #;(HastTable Any (Pairof Integer Evaluation)) (make-hash))

(define make-tamer-zone
  (lambda [story lang+modules private-modules]
    (define tamer-module (tamer-story->module story (not (pair? lang+modules))))

    (parameterize ([sandbox-propagate-exceptions (tamer-story-propagate-exceptions)]
                   [exit-handler (λ [retcode]
                                   (error 'tamer-repl "[fatal] ~a unexpectedly escaped from the Racket Virtual Machine!"
                                          (tamer-story->tag story)))])
      (if (and tamer-module)
          (parameterize ([sandbox-namespace-specs (cons (thunk (module->namespace tamer-module)) null)])
            (make-base-eval #:pretty-print? #true))
          (apply make-base-eval #:pretty-print? #true #:lang (car lang+modules)
                 (map (λ [mod] `(require ,mod))
                      (append (cdr lang+modules) private-modules)))))))

(define tamer-zone-reference
  (lambda [story [lang+modules null] [private-modules null]]
    (define z (hash-ref tamer-zones story (λ [] (cons 0 (make-tamer-zone story lang+modules private-modules)))))
    (hash-set! tamer-zones story (cons (add1 (car z)) (cdr z)))))

(define tamer-zone-ref ; see `tamer-repl`
  (lambda [story [lang+modules null] [private-modules null]]
    (cdr (hash-ref! tamer-zones story (λ [] (cons 1 (make-tamer-zone story lang+modules private-modules)))))))

(define tamer-zone-destory
  (lambda [story]
    (define z (hash-ref tamer-zones story (λ [] #false)))

    (when (pair? z)
      (cond [(= (car z) 1)
             (when (cdr z) (close-eval (cdr z)))
             (hash-remove! tamer-zones story)]
            [else (hash-set! tamer-zones story (cons (sub1 (car z)) (cdr z)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For Summaries, the `compiled specification`, all features and behaviors have been proved.
(struct tamer-feature (brief issues #;(Listof (U Spec-Issue String tamer-features))) #:transparent)

(define tamer-scribble-story-id 'digimon:tamer:story)
(define tamer-scribble-story-times 'digimon:tamer:story:times)
(define tamer-scribble-story-issues 'digimon:tamer:story:issues)

(define tamer-empty-times (list 0 0 0 0))
(define tamer-empty-issues (make-immutable-hash))

(define tamer-feature-reverse-merge
  (lambda [srutaef]
    (define features (make-hash))
    
    (let merge ([briefs null]
                [rest srutaef])
      (if (null? rest)
          (for/list ([brief (in-list briefs)])
            (cons brief (hash-ref features brief)))
          (let ([brief (tamer-feature-brief (car rest))])
            (define maybe-existed (hash-ref features brief (λ [] null)))
            (define-values (briefs++ behaviors)
              (if (null? maybe-existed)
                  (values (cons brief briefs) (tamer-feature-issues (car rest)))
                  (values briefs (append (tamer-feature-issues (car rest)) maybe-existed))))
            
            (hash-set! features brief behaviors)
            (merge briefs++ (cdr rest)))))))

(define tamer-tag-value
  (lambda [val]
    (cond [(or (procedure? val) (symbol? val))
           (let*-values ([(modpath) (build-path (digimon-path 'tamer) "tamer.rkt")]
                         [(export) (or (object-name val) val)]
                         [(xref) (load-collections-xref)]
                         [(tag) (xref-binding->definition-tag xref (list modpath export) #false)]
                         [(path anchor) (with-handlers ([exn? (λ _ (values #false #false))])
                                          (xref-tag->path+anchor xref tag #:external-root-url #false))])
             (or (and path anchor
                      (racketvalfont (hyperlink (format "/~~:/~a#~a" (find-relative-path (find-doc-dir) path) anchor)
                                                (symbol->immutable-string export))))
                 val))]
          [(vector? val)
           (vector-map tamer-tag-value val)]
          [(pair? val) #| also for lists |#
           (cons (tamer-tag-value (car val)) (tamer-tag-value (cdr val)))]
          [else val])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For indexed traverse blocks and elements
(define tamer-scribble-story-index 'digimon:tamer:story:index)

(define traverse-indexed-tagbase
  (case-lambda
    [(get index-type) ; (HashTable (U Symbol Tamer-Story String) Natural)
     (define indices (or (get tamer-scribble-story-index #false) (make-hasheq)))
     (hash-ref indices index-type (λ [] (make-hash)))]
    [(set! index-type local-tags get)
     (define indices (traverse-ref! get set! tamer-scribble-story-index make-hasheq))
     (hash-set! indices index-type local-tags)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-boxed-style
  (let* ([props (list 'multicommand 'never-indents)]
         [boxed (make-style "boxed" props)])
    (case-lambda
      [() boxed]
      [(path) (make-style "boxed" (cons path props))])))
  
(define handbook-nested-filebox
  (lambda [latex? /path/file block]
    (centered
     (if (not latex?)
         (nested #:style (handbook-boxed-style /path/file)
                 (filebox (italic (string memo#) ~ (path->string (tr-if-path /path/file)))
                          block))
         (nested #:style (handbook-boxed-style /path/file)
                 (filebox (smaller (italic (path->string (tr-if-path /path/file))))
                          block))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define section-title
  (lambda [title tongue]
    (cond [(symbol? title) (speak title #:in tongue #:dialect 'tamer)]
          [else (~a title)])))

(define pkg-author-contents
  (lambda []
    (define authors
      (#%info 'pkg-authors
              (λ [] (list (#%info 'pkg-idun
                                   (λ [] digimon-partner))))))
    
    (if (list? authors)
        (map pkg-author-content authors)
        (list (pkg-author-content authors)))))

(define pkg-author-content
  (lambda [author]
    (cond [(symbol? author) (symbol->immutable-string author)]
          [(pair? author) (author+email (~a (car author)) (~a (cdr author)))]
          [else author])))

(define traverse-ref!
  (lambda [get set! symbol make-default]
    (unless (get symbol #false)
      (set! symbol (make-default)))

    (get symbol #false)))

(define ~markdown
  (lambda [line]
    (define padding (λ [line] (make-string (- 72 (remainder (string-length (format "~a" line)) 72)) #\space)))
    (cond [(string? line) (literal (format "~a~a" line (padding line)))]
          [else (list line (literal (padding (car (element-content line)))))])))

(define ~url
  (lambda [digimon]
    (format "http://~a/~~~a:~a" (#%info 'pkg-domain) (#%info 'pkg-idun) digimon)))

(define ~github
  (lambda [projname username]
    (format "https://github.com/~a/~a" username projname)))

(define tr-d (λ [p] (string-replace p (path->string (digimon-path 'zone)) "")))
(define tr-if-path (λ [p] (if (path? p) (build-path (tr-d (format "~a" p))) p)))
