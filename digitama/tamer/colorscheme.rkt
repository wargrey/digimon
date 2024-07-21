#lang typed/racket/base

(provide (all-defined-out))

(require typed/setup/getinfo)

(require racket/list)
(require racket/match)
(require racket/symbol)

(require "../predicate.rkt")
(require "../../filesystem.rkt")
(require "../../function.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; @docs: color-prefs:register-info-based-color-schemes

(define-type Color-Scheme-Info (List* (Option Color-Datum) (Option Color-Datum) (Listof Symbol)))

(struct color-datum
  ([rgb : (List Byte Byte Byte)]
   [alpha : Nonnegative-Flonum])
  #:type-name Color-Datum
  #:transparent)

(struct fg:rgba color-datum () #:transparent)
(struct bg:rgba color-datum () #:transparent)

(struct color-scheme
  ([name : String]
   [inverted-scheme : (Option Symbol)]
   [entries : (Immutable-HashTable Symbol Color-Scheme-Info)]
   [white-on-black? : Boolean]
   [info-dir : Path])
  #:type-name Color-Scheme
  #:transparent)

(define make-color-scheme : (->* (Any) (Path) (Option Color-Scheme))
  (let ([empty-entries : (Immutable-HashTable Symbol Color-Scheme-Info) (hasheq)])
    (lambda [maybe-scheme [srcdir (current-directory)]]
      (and (hash? maybe-scheme)
           (let ([name (hash-ref maybe-scheme 'name (λ [] (dirname srcdir)))]
                 [opposite-name (hash-ref maybe-scheme 'inverted-base-name λfalse)]
                 [inverted? (hash-ref maybe-scheme 'white-on-black-base? λfalse)]
                 [colors (hash-ref maybe-scheme 'colors λfalse)])
             (color-scheme (cond [(string? name) name]
                                 [(symbol? name) (color-scheme-name->string name)]
                                 [else (format "~a" name)])
                           (and (symbol? opposite-name) opposite-name)
                           (for/fold ([entries : (Immutable-HashTable Symbol Color-Scheme-Info) empty-entries])
                                     ([e:info (in-list (if (list? colors) colors null))])
                             (or (and (list? e:info)
                                      (let*-values ([(names rgbas) (partition symbol? (filter-map color-entry-element->color-datum e:info))]
                                                    [(fgs rest) (partition fg:rgba? rgbas)]
                                                    [(bgs rest) (partition bg:rgba? rest)])
                                        (and (pair? names)
                                             (hash-set entries (car names)
                                                       (list* (and (pair? fgs) (car fgs))
                                                              (and (pair? bgs) (car bgs))
                                                              (cdr names))))))
                                 entries))
                           (and (boolean? inverted?) inverted?)
                           srcdir))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define color-load-scheme : (->* () (Path) (Listof Color-Scheme))
  (let ([color-scheme-idname 'framework:color-schemes])
    (case-lambda
      [() (apply append (map color-load-scheme (find-relevant-directories (list color-scheme-idname))))]
      [(dir)
       (let* ([info-ref (get-info/full dir #:bootstrap? #true)]
              [maybe-schemes (and info-ref (info-ref color-scheme-idname λfalse))])
         (cond [(not (list? maybe-schemes)) null]
               [else (filter-map (λ [maybe-scheme] (make-color-scheme maybe-scheme dir))
                                 maybe-schemes)]))])))

(define color-entry-element->color-datum : (-> Any (U Symbol Color-Datum False))
  (lambda [e]
    (when (struct? e)
      (displayln (struct->vector e)))
    (match (if (struct? e) (struct->vector e) e)
      [(vector (? byte? r) (? byte? g) (? byte? b)) (fg:rgba (list r g b) 1.0)]
      [(vector (? byte? r) (? byte? g) (? byte? b) (? normalized-flonum? a)) (fg:rgba (list r g b) a)]
      [(vector 'struct:background (vector (? byte? r) (? byte? g) (? byte? b))) (bg:rgba (list r g b) 1.0)]
      [(vector 'struct:background (vector (? byte? r) (? byte? g) (? byte? b) (? normalized-flonum? a))) (bg:rgba (list r g b) a)]
      [(? symbol? style) style]
      [_ #false])))

(define color-scheme-name->string : (-> Symbol String)
  (lambda [name]
    (case name
      [(modern-color-scheme) "Modern"]
      [(classic-color-scheme) "Classic"]
      [else (symbol->immutable-string name)])))


(color-load-scheme)