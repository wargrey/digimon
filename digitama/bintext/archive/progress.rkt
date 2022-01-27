#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Archive-Progress-Handler (-> Symbol Natural Natural Void))
(define-type Archive-Entry-Progress-Handler (-> Symbol String Natural Natural Void))

(define-type Archive-Port (U Input-Port Output-Port))

(define archive-resolve-progress-topic : (-> Archive-Port Symbol)
  (lambda [/dev/aio]
    (define name : Any (object-name /dev/aio))
    (cond [(symbol? name) name]
          [(path? name) (string->symbol (format "~a" (file-name-from-path name)))]
          [(string? name) (string->symbol name)]
          [else 'topic:archive:progress])))

(define default-archive-progress-handler : (Parameterof Archive-Progress-Handler) (make-parameter void))
(define default-archive-entry-progress-handler : (Parameterof Archive-Entry-Progress-Handler) (make-parameter void))

(define default-archive-progress-topic-resolver : (Parameterof (Option (-> Archive-Port Symbol)) (-> Archive-Port Symbol))
  (make-parameter archive-resolve-progress-topic
                  (λ [[resolver : (Option (-> Archive-Port Symbol))]]
                    (or resolver archive-resolve-progress-topic))))
