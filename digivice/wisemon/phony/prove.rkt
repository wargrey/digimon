#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

(require "../parameter.rkt")
(require "../phony.rkt")

(require "../spec.rkt")
(require "../native.rkt")
(require "../racket.rkt")

(require "../../../dtrace.rkt")
(require "../../../format.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-digimon-handbooks : (-> Info-Ref (Listof Path))
  (lambda [info-ref]
    (define maybe-handbooks (info-ref 'scribblings (λ [] null)))
    (cond [(not (list? maybe-handbooks)) (raise-user-error 'info.rkt "malformed `scribblings`: ~a" maybe-handbooks)]
          [else (filter file-exists?
                        (for/list : (Listof Path) ([handbook (in-list maybe-handbooks)])
                          (cond [(and (pair? handbook) (path-string? (car handbook))) (build-path (current-directory) (car handbook))]
                                [else (raise-user-error 'info.rkt "malformed `scribbling`: ~a" handbook)])))])))

(define make~prove : Make-Phony
  (lambda [digimon info-ref]
    (wisemon-make (make-native-library-specs info-ref))
    (wisemon-compile (current-directory) digimon info-ref)

    (for ([handbook (in-list (if (null? (current-make-real-targets)) (find-digimon-handbooks info-ref) (current-make-real-targets)))])
      (define pwd : (Option Path) (path-only handbook))
      (when (and pwd (directory-exists? pwd))
        (define ./handbook : Path-For-Some-System (find-relative-path (current-directory) handbook))
        (dtrace-debug "~a prove: ~a" the-name ./handbook)
        
        (parameterize ([current-directory pwd]
                       [current-namespace (make-base-namespace)])
          (if (equal? (path-get-extension ./handbook) #".rkt")
              (parameterize ([exit-handler (λ [[retcode : Any]]
                                             (when (and (exact-integer? retcode) (<= 1 retcode 255))
                                               (error the-name "prove: [error] ~a breaks ~a!" ./handbook (~n_w retcode "sample"))))])
                (define modpath `(submod ,handbook main))
                (when (module-declared? modpath #true)
                  (dynamic-require `(submod ,handbook main) #false)))
              (parameterize ([exit-handler (λ _ (error the-name "prove: [fatal] ~a needs a proper `exit-handler`!" ./handbook))])
                (eval '(require (prefix-in html: scribble/html-render) setup/xref scribble/render))
                (eval `(render (list ,(dynamic-require handbook 'doc)) (list ,handbook)
                               #:render-mixin (λ [%] (html:render-multi-mixin (html:render-mixin %)))
                               #:dest-dir ,(build-path pwd (car (use-compiled-file-paths)))
                               #:redirect "/~:/" #:redirect-main "/~:/" #:xrefs (list (load-collections-xref))
                               #:quiet? #false #:warn-undefined? #false)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define prove-phony-goal : Wisemon-Phony
  (wisemon-make-phony #:name 'prove #:phony make~prove #:desc "Verify and generate test report along with documentation"))
