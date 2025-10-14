#lang typed/racket/base

(provide (all-defined-out))

(require "../parameter.rkt")
(require "../cmdname.rkt")
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
              (if (and (pair? handbook)
                       (path-string? (car handbook)))
                  (build-path (current-directory) (path-normalize/system (car handbook)))
                  (raise-user-error 'info.rkt "malformed `scribbling`: ~a" handbook))))))

(define make-weave-specs : (-> (Listof Path) Wisemon-Specification)
  (lambda [handbooks]
    (for/list : Wisemon-Specification ([handbook.scrbl (in-list handbooks)])
      (wisemon-spec handbook.scrbl #:-
                    (define pwd : (Option Path) (path-only handbook.scrbl))
                    (when (and pwd (directory-exists? pwd))
                      (define ./handbook : Path-For-Some-System (find-relative-path (current-directory) handbook.scrbl))
                      
                      (dtrace-note "~a ~a: ~a" (the-cmd-name) (current-make-phony-goal) ./handbook)
                      
                      (parameterize ([current-directory pwd]
                                     [current-namespace (make-base-namespace)])
                        (if (equal? (path-get-extension ./handbook) #".rkt")
                            (parameterize ([exit-handler (λ [[retcode : Any]]
                                                           (when (and (exact-integer? retcode) (<= 1 retcode 255))
                                                             (error (the-cmd-name) "~a: [error] ~a breaks ~a!"
                                                                    (current-make-phony-goal) ./handbook (~n_w retcode "sample"))))])
                              (define modpath `(submod ,handbook.scrbl main))
                              (when (module-declared? modpath #true)
                                (dynamic-require `(submod ,handbook.scrbl main) #false)))
                            (parameterize ([exit-handler (λ _ (error (the-cmd-name) "~a: [fatal] ~a needs a proper `exit-handler`!"
                                                                     (current-make-phony-goal) ./handbook))])
                              (eval '(require (prefix-in html: scribble/html-render) setup/xref scribble/render))
                              (eval `(define (multi-html:render handbook.scrbl #:dest-dir dest-dir)
                                       (define scribble.doc (dynamic-require handbook.scrbl 'doc))
                                       
                                       (render (list scribble.doc) (list ,handbook.scrbl)
                                               #:render-mixin (λ [%] (html:render-multi-mixin (html:render-mixin %))) #:dest-dir dest-dir
                                               #:redirect "/~:/" #:redirect-main "/~:/" #:xrefs (list (load-collections-xref))
                                               #:image-preferences '(svg png gif pdf))))
                              (fg-recon-eval 'weave `(multi-html:render ,handbook.scrbl #:dest-dir ,(build-path pwd (car (use-compiled-file-paths)))))))))))))

(define make~weave : Make-Free-Phony
  (lambda [digimon info-ref]
    (unless (not info-ref)
      (wisemon-make (make-ffi-library-specs info-ref) px.so)
      (wisemon-compile (current-directory) digimon info-ref))

    (define handbooks : (Listof Path)
      (let ([info-targets (if (not info-ref) null (find-digimon-handbooks info-ref))]
            [real-targets (current-make-real-targets)])
        (append info-targets real-targets)))
    
    (compile-scribble handbooks (current-directory))
    (wisemon-make (make-weave-specs handbooks) (current-make-real-targets) #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define weave-phony-goal : Wisemon-Phony
  (wisemon-make-free-phony #:name 'weave #:phony make~weave #:desc "Craft living documentation from narratives and executable specs."))
