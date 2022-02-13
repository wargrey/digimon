#lang typed/racket/base

(provide (all-defined-out))

(require typed/setup/getinfo)

(require racket/fixnum)
(require racket/string)
(require racket/symbol)
(require racket/path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct place-message ([stream : Any]) #:prefab)

(define current-digimon : (Parameterof String) (make-parameter "digimon"))
(define current-free-zone : (Parameterof (Option Path-String)) (make-parameter #false))
(define current-digivice : (Parameterof String)
  (make-parameter (let ([run-file (file-name-from-path (find-system-path 'run-file))])
                    (cond [(path? run-file) (path->string (path-replace-extension (assert run-file path?) ""))]
                          [else "wisemon"]))))

(define-values (digimon-waketime digimon-partner digimon-system)
  (values (current-milliseconds)
          (or (getenv "USER") (getenv "LOGNAME") #| daemon |# "root")
          (let ([libpath (path->string (system-library-subpath #false))])
            (cond [(regexp-match? #px"solaris" libpath) 'illumos]
                  [(regexp-match? #px"linux" libpath) 'linux]
                  [else (system-type 'os)]))))

(define #%info : (->* (Symbol) ((-> Any)) Any)
  (let ([cache : (HashTable String (Option Info-Ref)) (make-hash)])
    (lambda [id [mkdefval (λ [] #false)]]
      (define digimon : String (current-digimon))
      (define info-ref : (Option Info-Ref) (hash-ref! cache digimon (λ [] (get-info/full (digimon-path 'zone)))))
      (cond [(not info-ref) (mkdefval)]
            [else (info-ref id mkdefval)]))))

(define digimon-uptime : (-> [#:now Integer] Fixnum)
  (lambda [#:now [now (current-milliseconds)]]
    (fx- now digimon-waketime)))

(define digimon-path : (-> (U Symbol Path-String) Path-String * Path)
  (let ([cache : (HashTable (Listof (U Path-String Symbol)) Path) (make-hash)])
    (lambda [path . paths]
      (define (map-path [digimon-zone : Path-String] [path : Symbol]) : Path
        (case path
          [(digivice digitama stone tamer literacy village) (build-path digimon-zone (symbol->immutable-string path))]
          [(info) (build-path digimon-zone "info.rkt")]
          [else (build-path digimon-zone "stone" (symbol->immutable-string path))]))
      (define (prefab-path [digimon-zone : Path-String] [path : Symbol]) : Path-String
        (define fullpath : Symbol (string->symbol (string-append "digimon-" (symbol->immutable-string path))))
        (define info-ref : (Option Info-Ref) (get-info/full digimon-zone))
        (cond [(eq? path 'zone) digimon-zone]
              [(not info-ref) (map-path digimon-zone path)]
              [else (let ([tail (info-ref fullpath (λ [] #false))])
                      (cond [(not tail) (map-path digimon-zone path)]
                            [(string? tail) (build-path digimon-zone tail)]
                            [else (raise-user-error 'digimon-path "not a path value in info.rkt: ~a" tail)]))]))
      (define digimon : String (current-digimon))
      (define (get-zone) : Path-String
        (simplify-path ((inst collection-file-path Path-String) "." digimon
                        #:fail (λ [[errmsg : String]] : Path-String
                                 (or (current-free-zone) (current-directory))))
                       #false))
      (hash-ref! cache (list* digimon path paths)
                 (λ [] (let ([zone : Path-String (hash-ref cache (list digimon 'digimon-zone) get-zone)])
                         (cond [(symbol? path) (apply build-path (prefab-path zone path) paths)]
                               [else (apply build-path zone path paths)])))))))

(define digivice-path : (-> (U String Bytes) (U Symbol Path-String) Path-String * Path)
  (lambda [suffix path . paths]
    (build-path (apply digimon-path path paths) (path-add-extension (current-digivice) suffix))))

(define digimon-stone-path? : (-> Path-String Boolean)
  (lambda [path]
    (string-prefix? (path->string (simple-form-path path))
                    (path->string (digimon-path 'stone)))))
