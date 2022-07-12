#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

(require "../../evt.rkt")
(require "../../../port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Archive-Progress-Handler (-> Symbol Natural Natural Void))
(define-type Archive-Entry-Progress-Handler (-> Symbol String Natural Natural Boolean Void))
(define-type Archive-Entry-Shadow-Handler (U Output-Port (-> Symbol String Bytes Natural (U Natural EOF) Void)))

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
(define default-archive-entry-shadow-handler : (Parameterof Archive-Entry-Shadow-Handler) (make-parameter void))

(define default-archive-progress-topic-resolver : (Parameterof (Option (-> Archive-Port Symbol)) (-> Archive-Port Symbol))
  (make-parameter archive-resolve-progress-topic
                  (λ [[resolver : (Option (-> Archive-Port Symbol))]]
                    (or resolver archive-resolve-progress-topic))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-progress-input-port : (-> Input-Port Symbol String Archive-Entry-Progress-Handler Archive-Entry-Shadow-Handler Natural Input-Port)
  (lambda [/dev/zipin topic entry-name progress-handler shadow-handler total]
    (define consumed : Natural 0)

    (progress-handler topic entry-name 0 total #false)
    
    (define (do-read [str : Bytes]) : (U Natural EOF)
      (define size : (U Natural EOF) (read-bytes! str /dev/zipin))

      (cond [(eof-object? size) (progress-handler topic entry-name consumed total #true)]
            [else (set! consumed (+ consumed size))
                  (progress-handler topic entry-name consumed total #false)])
      
      size)

    (define (do-read/shadow [str : Bytes]) : (U Natural EOF)
      (define position : Natural consumed)
      (define size : (U Natural EOF) (do-read str))

      (cond [(not (output-port? shadow-handler)) (shadow-handler topic entry-name str position size)]
            [(exact-integer? size) (write-bytes str shadow-handler 0 size)])
      
      size)

    (define (do-peek [str : Bytes] [skip : Natural] [progress-evt : (Option EvtSelf)]) : (Option Port-Reader-Datum)
      (peek-bytes! str skip /dev/zipin))

    (cond [(and (eq? progress-handler void) (eq? shadow-handler void)) /dev/zipin]
          [else (make-input-port (object-name /dev/zipin)
                                 (if (eq? shadow-handler void) do-read do-read/shadow) do-peek
                                 (λ [] (close-input-port /dev/zipin))
                                 #false #false #false
                                 void /dev/zipin)])))
