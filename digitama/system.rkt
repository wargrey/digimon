#lang typed/racket

(provide (all-defined-out) Info-Ref)

(require typed/setup/getinfo)
(require/typed/provide racket [vector-set-performance-stats! Vector-Set-Performance-Stats!])

(define-type Term-Color (Option (U String Symbol Byte)))
(define-type Racket-Place-Status (Vector Fixnum Fixnum Fixnum Natural Natural Natural Natural Natural Fixnum Fixnum Natural Natural))
(define-type Racket-Thread-Status (Vector Boolean Boolean Boolean Natural))
(define-type Vector-Set-Performance-Stats! (case-> [Racket-Place-Status -> Void]
                                                   [Racket-Thread-Status Thread -> Void]))

(struct place-message ([stream : Any]) #:prefab)

(define /dev/stdin : Input-Port (current-input-port))
(define /dev/stdout : Output-Port (current-output-port))
(define /dev/stderr : Output-Port (current-error-port))
(define /dev/eof : Input-Port (open-input-bytes #"" '/dev/null))
(define /dev/null : Output-Port (open-output-nowhere '/dev/null))

(define current-digimon : (Parameterof String) (make-parameter "digimon"))
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
    (define run-file : (Option Path) (file-name-from-path (find-system-path 'run-file)))
    (build-path (apply digimon-path path paths) (path-replace-extension (assert run-file path?) suffix))))

(define vim-colors : (HashTable String Byte)
  #hash(("black" . 0) ("darkgray" . 8) ("darkgrey" . 8) ("lightgray" . 7) ("lightgrey" . 7) ("gray" . 7) ("grey" . 7) ("white" . 15)
                      ("darkred" . 1) ("darkgreen" . 2) ("darkyellow" . 3) ("darkblue" . 4) ("brown" . 5) ("darkmagenta" . 5)
                      ("darkcyan" . 6) ("red" . 9) ("lightred" . 9) ("green" . 10) ("lightgreen" . 10) ("yellow" . 11) ("lightyellow" . 11)
                      ("blue" . 12) ("lightblue" . 12) ("magenta" . 13) ("lightmagenta" . 13) ("cyan" . 14) ("lightcyan" . 14)))

(define term-colorize : (-> Term-Color Term-Color (Listof Symbol) String String)
  (lambda [fg bg attrs content]
    (define color-code : (-> String [#:bgcolor? Boolean] String)
      (lambda [color #:bgcolor? [bg? #false]]
        (format "~a8;5;~a" (if bg? 4 3) (if (regexp-match? #px"\\d+" color) color (hash-ref vim-colors color)))))
    (regexp-replace #px"^(\\s*)(.+?)(\\s*)$" content
                    (format "\\1\033[~a;~a;~am\\2\033[0m\\3"
                            (string-replace (for/fold : String ([effects ""]) ([attr : Symbol (in-list attrs)])
                                              (case (string-downcase (format "~a" attr))
                                                [{"bold" "bright"} (string-append effects ";1")]
                                                [{"dim"} (string-append effects ";2")]
                                                [{"underline" "undercurl"} (string-append effects ";4")]
                                                [{"blink"} (string-append effects ";5")]
                                                [{"reverse" "inverse"} (string-append effects ";7")]
                                                [{"hidden" "password"} (string-append effects ";8")]
                                                [else (error 'tarminal-colorize "Unsupported Terminal Attribute: ~a" attr)]))
                                            "^;" "" #:all? #false)
                            (if (false? fg) 39 (color-code (string-downcase (format "~a" fg))))
                            (if (false? bg) 49 (color-code (string-downcase (format "~a" bg)) #:bgcolor? #true))))))

(define echof : (-> String [#:fgcolor Term-Color] [#:bgcolor Term-Color] [#:attributes (Listof Symbol)] Any * Void)
  (lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (printf "~a" (if (terminal-port? (current-output-port)) (term-colorize fg bg attrs rawmsg) rawmsg))))

(define eechof : (-> String [#:fgcolor Term-Color] [#:bgcolor Term-Color] [#:attributes (Listof Symbol)] Any * Void)
  (lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (eprintf "~a" (if (terminal-port? (current-error-port)) (term-colorize fg bg attrs rawmsg) rawmsg))))
