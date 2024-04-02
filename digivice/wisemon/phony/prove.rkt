#lang typed/racket/base

(provide (all-defined-out))

(require "../parameter.rkt")
(require "../phony.rkt")

(require "../spec.rkt")
(require "../ffi.rkt")
(require "../racket.rkt")

(require "../../../filesystem.rkt")
(require "../../../digitama/exec.rkt")

(require "../../../digitama/minimal/format.rkt")
(require "../../../digitama/minimal/dtrace.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-digimon-handbooks : (-> Info-Ref (Listof Path))
  (lambda [info-ref]
    (define maybe-handbooks (info-ref 'scribblings (λ [] null)))
    (unless (list? maybe-handbooks)
      (raise-user-error 'info.rkt "malformed `scribblings`: ~a" maybe-handbooks))
    
    (filter file-exists?
            (for/list : (Listof Path) ([handbook (in-list maybe-handbooks)])
              (cond [(and (pair? handbook) (path-string? (car handbook))) (build-path (current-directory) (path-normalize/system (car handbook)))]
                    [else (raise-user-error 'info.rkt "malformed `scribbling`: ~a" handbook)])))))

(define make-prove-specs : (-> (Option Info-Ref) Wisemon-Specification)
  (lambda [info-ref]
    (define handbooks : (Listof Path)
      (let ([info-targets (if (not info-ref) null (find-digimon-handbooks info-ref))]
            [real-targets (current-make-real-targets)])
        (append info-targets real-targets)))
    
    (for/list : Wisemon-Specification ([handbook.scrbl (in-list handbooks)])
      (wisemon-spec handbook.scrbl #:-
                    (define pwd : (Option Path) (path-only handbook.scrbl))
                    (when (and pwd (directory-exists? pwd))
                      (define ./handbook : Path-For-Some-System (find-relative-path (current-directory) handbook.scrbl))
                      
                      (dtrace-note "~a ~a: ~a" the-name (current-make-phony-goal) ./handbook)
                      
                      (parameterize ([current-directory pwd]
                                     [current-namespace (make-base-namespace)])
                        (if (equal? (path-get-extension ./handbook) #".rkt")
                            (parameterize ([exit-handler (λ [[retcode : Any]]
                                                           (when (and (exact-integer? retcode) (<= 1 retcode 255))
                                                             (error the-name "~a ~a: [error] ~a breaks ~a!"
                                                                    the-name (current-make-phony-goal) ./handbook (~n_w retcode "sample"))))])
                              (define modpath `(submod ,handbook.scrbl main))
                              (when (module-declared? modpath #true)
                                (dynamic-require `(submod ,handbook.scrbl main) #false)))
                            (parameterize ([exit-handler (λ _ (error the-name "~a ~a: [fatal] ~a needs a proper `exit-handler`!"
                                                                     the-name (current-make-phony-goal) ./handbook))])
                              (eval '(require (prefix-in html: scribble/html-render) setup/xref scribble/render))
                              (eval `(define (multi-html:render handbook.scrbl #:dest-dir dest-dir)
                                       (render (list (dynamic-require handbook.scrbl 'doc)) (list ,handbook.scrbl)
                                               #:render-mixin (λ [%] (html:render-multi-mixin (html:render-mixin %))) #:dest-dir dest-dir
                                               #:redirect "/~:/" #:redirect-main "/~:/" #:xrefs (list (load-collections-xref)))))
                              (fg-recon-eval 'prove `(multi-html:render ,handbook.scrbl #:dest-dir ,(build-path pwd (car (use-compiled-file-paths)))))))))))))

(define make~prove : Make-Free-Phony
  (lambda [digimon info-ref]
    (unless (not info-ref)
      (wisemon-make (make-ffi-library-specs info-ref) px.so)
      (wisemon-compile (current-directory) digimon info-ref))

    (wisemon-make (make-prove-specs info-ref) (current-make-real-targets) #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define prove-phony-goal : Wisemon-Phony
  (wisemon-make-free-phony #:name 'prove #:phony make~prove #:desc "Verify and generate test report along with documentation"))
