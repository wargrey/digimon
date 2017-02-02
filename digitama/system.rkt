#lang typed/racket

(provide (all-defined-out))

(require typed/setup/getinfo)
(require racket/fixnum)

(struct place-message ([stream : Any]) #:prefab)

(define /dev/stdin : Input-Port (current-input-port))
(define /dev/stdout : Output-Port (current-output-port))
(define /dev/stderr : Output-Port (current-error-port))
(define /dev/eof : Input-Port (open-input-bytes #"" '/dev/null))
(define /dev/null : Output-Port (open-output-nowhere '/dev/null))

(define current-digimon : (Parameterof String) (make-parameter "digimon"))
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
    (lambda [id [mkdefval (thunk #false)]]
      (define digimon : String (current-digimon))
      (define info-ref : (Option Info-Ref) (hash-ref! cache digimon (thunk (get-info/full (digimon-path 'zone)))))
      (cond [(false? info-ref) (mkdefval)]
            [else (info-ref id mkdefval)]))))

(define digimon-uptime : (-> [#:now Integer] Fixnum)
  (lambda [#:now [now (current-milliseconds)]]
    (fx- now digimon-waketime)))

(define digimon-path : (-> (U Symbol Path-String) Path-String * Path)
  (let ([cache : (HashTable (Listof (U Path-String Symbol)) Path) (make-hash)])
    (lambda [path . paths]
      (define (get-zone) : Path (simplify-path (collection-file-path "." digimon) #false))
      (define (map-path [digimon-zone : Path] [path : Symbol]) : Path
        (case path
          [(digivice digitama stone tamer) (build-path digimon-zone (symbol->string path))]
          [else (build-path digimon-zone "stone" (symbol->string path))]))
      (define (prefab-path [digimon-zone : Path] [path : Symbol]) : Path
        (define fullpath : Symbol (string->symbol (string-append "digimon-" (symbol->string path))))
        (define info-ref : (Option Info-Ref) (get-info/full digimon-zone))
        (cond [(eq? path 'zone) digimon-zone]
              [(false? info-ref) (map-path digimon-zone path)]
              [else (let ([tail (info-ref fullpath (thunk #false))])
                      (cond [(false? tail) (map-path digimon-zone path)]
                            [(string? tail) (build-path digimon-zone tail)]
                            [else (raise-user-error 'digimon-path "not a path value in info.rkt: ~a" tail)]))]))
      (define digimon : String (current-digimon))
      (hash-ref! cache (list* digimon path paths)
                 (thunk (let ([zone : Path (hash-ref cache (list digimon 'digimon-zone) get-zone)])
                          (cond [(symbol? path) (apply build-path (prefab-path zone path) paths)]
                                [else (apply build-path zone path paths)])))))))

(define digivice-path : (-> (U String Bytes) (U Symbol Path-String) Path-String * Path)
  (lambda [suffix path . paths]
    (build-path (apply digimon-path path paths) (path-add-extension (current-digivice) suffix))))
