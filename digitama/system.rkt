#lang typed/racket/base

(provide (all-defined-out))

(require typed/setup/getinfo)

(require racket/fixnum)
(require racket/port)
(require racket/path)
(require racket/match)

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/base
 [collection-file-path (->* (Path-String #:fail (-> String Path-String)) (#:check-compiled? Boolean) #:rest Path-String Path)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct place-message ([stream : Any]) #:prefab)

(define /dev/stdin : Input-Port (current-input-port))
(define /dev/stdout : Output-Port (current-output-port))
(define /dev/stderr : Output-Port (current-error-port))
(define /dev/eof : Input-Port (open-input-bytes #"" '/dev/null))
(define /dev/null : Output-Port (open-output-nowhere '/dev/null))

(define current-digimon : (Parameterof String) (make-parameter "digimon"))
(define current-free-zone : (Parameterof (Option Path-String)) (make-parameter #false))
(define current-digivice : (Parameterof String)
  (make-parameter (let ([run-file (file-name-from-path (find-system-path 'run-file))])
                    (cond [(path? run-file) (path->string (path-replace-extension (assert run-file path?) ""))]
                          [else "wisemon"]))))

(define-values (digimon-waketime digimon-partner digimon-system)
  (values (current-milliseconds)
          (or (getenv "USER") (getenv "LOGNAME") #| daemon |# "root")
          (match (path->string (system-library-subpath #false))
            [(pregexp #px"solaris") 'illumos]
            [(pregexp #px"linux") 'linux]
            [_ (system-type 'os)])))

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
          [(digivice digitama stone tamer literacy) (build-path digimon-zone (symbol->string path))]
          [else (build-path digimon-zone "stone" (symbol->string path))]))
      (define (prefab-path [digimon-zone : Path-String] [path : Symbol]) : Path-String
        (define fullpath : Symbol (string->symbol (string-append "digimon-" (symbol->string path))))
        (define info-ref : (Option Info-Ref) (get-info/full digimon-zone))
        (cond [(eq? path 'zone) digimon-zone]
              [(not info-ref) (map-path digimon-zone path)]
              [else (let ([tail (info-ref fullpath (λ [] #false))])
                      (cond [(not tail) (map-path digimon-zone path)]
                            [(string? tail) (build-path digimon-zone tail)]
                            [else (raise-user-error 'digimon-path "not a path value in info.rkt: ~a" tail)]))]))
      (define digimon : String (current-digimon))
      (define (get-zone) : Path-String
        (simplify-path (collection-file-path #:fail (λ [[errmsg : String]] (or (current-free-zone) (current-directory)))
                                             "." digimon)
                       #false))
      (hash-ref! cache (list* digimon path paths)
                 (λ [] (let ([zone : Path-String (hash-ref cache (list digimon 'digimon-zone) get-zone)])
                         (cond [(symbol? path) (apply build-path (prefab-path zone path) paths)]
                               [else (apply build-path zone path paths)])))))))

(define digivice-path : (-> (U String Bytes) (U Symbol Path-String) Path-String * Path)
  (lambda [suffix path . paths]
    (build-path (apply digimon-path path paths) (path-add-extension (current-digivice) suffix))))
