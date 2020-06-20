#lang racket

(provide (all-defined-out) quote-module-path)

(require racket/sandbox)
(require syntax/location)

(require setup/xref)
(require setup/dirs)
(require scribble/core)
(require scribble/xref)
(require scribble/manual)
(require scribble/example)

(require "../emoji.rkt")
(require "../system.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-story (make-parameter #false))
(define tamer-index-story (make-parameter (cons 1 #false)))

(define tamer-cite (make-parameter void))
(define tamer-cites (make-parameter void))
(define tamer-reference (make-parameter void))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (tamer-story->tag stx)
  (syntax-case stx []
    [(_ story-sexp)
     #'(let ([modpath (path->string (with-handlers ([exn? (λ [e] story-sexp)]) (cadr story-sexp)))]
             [literacy (path->string (digimon-path 'literacy))]
             [tamer (path->string (digimon-path 'tamer))])
         (cond [(string-prefix? modpath literacy) (substring modpath (add1 (string-length literacy)))]
               [(string-prefix? modpath tamer) (substring modpath (add1 (string-length tamer)))]
               [else (path->string (find-relative-path literacy modpath))]))]))

(define tamer-story->modpath
  (lambda [story-path]
    `(submod ,story-path tamer)))

(define tamer-story->module
  (lambda [story]
    (cond [(module-declared? story #true) story]
          [(let ([tamer.rkt (build-path (digimon-path 'tamer) "tamer.rkt")])
             (and (file-exists? tamer.rkt) tamer.rkt)) => values]
          [else (collection-file-path "tamer.rkt" "digimon")])))

(define make-tamer-zone
  (lambda [story]
    (define tamer-module (tamer-story->module story))
    (parameterize ([sandbox-namespace-specs (cons (thunk (module->namespace tamer-module)) null)])
      (make-base-eval #:pretty-print? #true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For Scribble and Racket
(define handbook-stories #;(HashTable Symbol (U Spec-Feature (Listof String))) (make-hash))

(define tamer-record-story
  (lambda [name unit]
    (define htag (tamer-story->tag (tamer-story)))
    (define units (hash-ref handbook-stories htag null))

    (unless (dict-has-key? units name)
      (hash-set! handbook-stories htag (cons unit units)))
    
    (let ([books (hash-ref handbook-stories books# null)])  ;;; keep the order of stories for indexing
      (unless (member htag books)
        (hash-set! handbook-stories books#
                   (cons htag books))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For Examples
(define tamer-zones #;(HastTable Any (Pairof Integer Evaluation)) (make-hash))

(define tamer-zone-reference
  (lambda [story]
    (define z (hash-ref tamer-zones story (λ [] (cons 0 (make-tamer-zone story)))))
    (hash-set! tamer-zones story (cons (add1 (car z)) (cdr z)))))

(define tamer-zone-ref
  (lambda [story]
    (cdr (hash-ref tamer-zones story (λ [] (cons 0 #false))))))

(define tamer-zone-destory
  (lambda [story]
    (define z (hash-ref tamer-zones story (λ [] #false)))

    (when (pair? z)
      (cond [(<= (car z) 1) (close-eval (cdr z)) (hash-remove! tamer-zones story)]
            [else (hash-set! tamer-zones story (cons (sub1 (car z)) (cdr z)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For Summaries, the `compiled specification`, all features and behaviors have been proved.
(struct tamer-feature (brief issues #;(Listof (U Spec-Issue String tamer-features))) #:transparent)

(define tamer-scribble-story-id 'digimon:tamer:story)
(define tamer-scribble-story-times 'digimon:tamer:story:times)
(define tamer-scribble-story-issues 'digimon:tamer:story:issues)

(define tamer-empty-times (list 0 0 0))
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
                                                (symbol->string export))))
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
(define handbook-boxed-style (make-style "boxed" null))

(define smart-nested-filebox
  (lambda [latex? /path/file block]
    (nested #:style handbook-boxed-style
            (filebox (if (not latex?)
                         (italic (string memo#) ~ (path->string (tr-if-path /path/file)))
                         (italic (path->string (tr-if-path /path/file))))
                     block))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
