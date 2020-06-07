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
(require "../../../digitama/exec.rkt")

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

    (wisemon-make
     (for/list : Wisemon-Specification ([handbook (in-list (append (find-digimon-handbooks info-ref) (current-make-real-targets)))])
       (wisemon-spec handbook #:-
                     (define pwd : (Option Path) (path-only handbook))
                     (when (and pwd (directory-exists? pwd))
                       (define ./handbook : Path-For-Some-System (find-relative-path (current-directory) handbook))
         
                       (dtrace-note "~a prove: ~a" the-name ./handbook)
         
                       (parameterize ([current-directory pwd]
                                      [current-namespace (make-base-namespace)])
                         (if (equal? (path-get-extension ./handbook) #".rkt")
                             (parameterize ([exit-handler (λ [[retcode : Any]]
                                                            (when (and (exact-integer? retcode) (<= 1 retcode 255))
                                                              (error the-name "~a prove: [error] ~a breaks ~a!" the-name ./handbook (~n_w retcode "sample"))))])
                               (define modpath `(submod ,handbook main))
                               (when (module-declared? modpath #true)
                                 (dynamic-require `(submod ,handbook main) #false)))
                             (parameterize ([exit-handler (λ _ (error the-name "~a prove: [fatal] ~a needs a proper `exit-handler`!" the-name ./handbook))])
                               (eval '(require (prefix-in html: scribble/html-render) setup/xref scribble/render))
                               (fg-recon-eval 'prove `(render (list (dynamic-require ,handbook 'doc)) (list ,handbook)
                                                              #:render-mixin (λ [%] (html:render-multi-mixin (html:render-mixin %)))
                                                              #:dest-dir ,(build-path pwd (car (use-compiled-file-paths)))
                                                              #:redirect "/~:/" #:redirect-main "/~:/" #:xrefs (list (load-collections-xref))))))))))
     (current-make-real-targets) #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define prove-phony-goal : Wisemon-Phony
  (wisemon-make-phony #:name 'prove #:phony make~prove #:desc "Verify and generate test report along with documentation"))
