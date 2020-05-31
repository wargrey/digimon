#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/path)
(require racket/match)

(require "prove.rkt")
(require "typeset.rkt")

(require "../spec.rkt")
(require "../phony.rkt")
(require "../racket.rkt")
(require "../parameter.rkt")

(require "../../../digitama/system.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Tex-Sample-Info (Pairof (Pairof Path Path) (Pairof Index (Option Index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-implicit-dist-specs : (-> Info-Ref Wisemon-Specification)
  (lambda [info-ref]
    (define root-readmes : (Listof Tex-Sample-Info)
      (append (map (λ [[readme : Path]] (cons (cons readme (current-directory)) (cons 0 0)))
                   (find-digimon-handbooks info-ref))
              (filter-map (λ [[readme : Tex-Info]]
                            (and (regexp-match? #px"\\.scrbl$" (car readme))
                                 (cons (cons (car readme) (current-directory)) (cons 0 1))))
                          (find-digimon-typesettings info-ref #true))))

    (define readmes : (Listof Tex-Sample-Info)
      (append (if (null? root-readmes) null (list (car root-readmes)))
              (find-digimon-typeseting-samples info-ref)))

    (cond [(string=? digimon-partner "root") null]
          [else (for/list : (Listof Wisemon-Spec) ([readme (in-list readmes)])
                  (define-values (readme.scrbl start endp1) (values (caar readme) (cadr readme) (cddr readme)))
                  (wisemon-spec (build-path (cdar readme) "README.md")
                                (filter file-exists? (list* (build-path (digimon-path 'zone) "info.rkt") (racket-smart-dependencies readme.scrbl)))
                                (λ [[target : Path]]
                                  (parameterize ([current-namespace (make-base-namespace)]
                                                 [current-input-port /dev/eof] ; tell scribble this is rendering to markdown
                                                 [exit-handler (λ _ (error the-name "[fatal] ~a needs a proper `exit-handler`!"
                                                                           (find-relative-path (current-directory) readme.scrbl)))])
                                    (eval `(require (prefix-in markdown: scribble/markdown-render) scribble/core scribble/render racket/list))
                                    (eval `(render (let* ([readme (dynamic-require ,readme.scrbl 'doc)]
                                                          [subparts (part-parts readme)]
                                                          [size (length subparts)]
                                                          [span (- (if (not ,endp1) size (min ,endp1 size)) ,start)])
                                                     (list (cond [(null? subparts) readme]
                                                                 [(or (<= span 0) (>= ,start size)) (struct-copy part readme [parts null])]
                                                                 [(= ,start 0) (struct-copy part readme [parts (take subparts span)])]
                                                                 [else (struct-copy part readme [parts (take (list-tail subparts start) span)])])))
                                                   (list ,target)
                                                   #:dest-dir ,(path-only target) #:render-mixin markdown:render-mixin
                                                   #:quiet? #false #:warn-undefined? #false))
                                    (void)))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-digimon-typeseting-samples : (-> Info-Ref (Listof Tex-Sample-Info))
  (lambda [info-ref]
    (define maybe-samples (info-ref 'samples (λ [] null)))
    (cond [(not (list? maybe-samples)) (raise-user-error 'info.rkt "malformed `samples`: ~a" maybe-samples)]
          [else (filter-map (λ [sample]
                              (if (and (pair? sample) (path-string? (car sample)))
                                  (let* ([sample.scrbl (build-path (current-directory) (car sample))]
                                         [dest-dir (path-only sample.scrbl)])
                                    (and (file-exists? sample.scrbl) (path? dest-dir)
                                         (cons (cons sample.scrbl dest-dir)
                                               (match (cdr sample)
                                                 [(list) (cons 0 #false)]
                                                 [(list (? index? endp1)) (cons 0 endp1)]
                                                 [(list (? index? start) (? index? endp1)) (cons start endp1)]
                                                 [(list (? index? start) '_) (cons start #false)]
                                                 [_ (raise-user-error 'info.rkt "malformed `sample`: ~a" sample)]))))
                                  (raise-user-error 'info.rkt "malformed `sample`: ~a" sample)))
                            maybe-samples)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(define dist-phony-goal : Wisemon-Phony
    (wisemon-make-phony #:name 'dist #:phony make~dist #:desc "Create a distribution file of the source files"))
