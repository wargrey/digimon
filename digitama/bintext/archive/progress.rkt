#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

(require "../../evt.rkt")
(require "../../../port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Archive-Progress-Handler (-> Symbol Natural Natural Void))
(define-type Archive-Entry-Progress-Handler (-> Symbol String Natural Natural Boolean Void))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-progress-input-port : (-> Input-Port Symbol String Archive-Entry-Progress-Handler Natural Input-Port)
  (lambda [/dev/zipin topic entry-name progress-handler total]
    (define consumed : Natural 0)

    (progress-handler topic entry-name 0 total #false)
    
    (define (do-read [str : Bytes]) : Port-Reader-Datum
      (define size : (U Natural EOF) (read-bytes! str /dev/zipin))

      (cond [(eof-object? size) (progress-handler topic entry-name consumed total #true)]
            [else (set! consumed (+ consumed size))
                  (progress-handler topic entry-name consumed total #false)])
      
      size)

    (define (do-peek [str : Bytes] [skip : Natural] [progress-evt : (Option EvtSelf)]) : (Option Port-Reader-Datum)
      (peek-bytes! str skip /dev/zipin))

    (cond [(eq? progress-handler void) /dev/zipin]
          [else (make-input-port (object-name /dev/zipin)
                                 do-read do-peek
                                 (λ [] (close-input-port /dev/zipin))
                                 #false #false #false
                                 void /dev/zipin)])))
