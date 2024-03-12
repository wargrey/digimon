#lang typed/racket/base

(provide (all-defined-out))

(require racket/fixnum)
(require racket/path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define digimon-uptime : (-> [#:now Integer] Fixnum)
  (lambda [#:now [now (current-milliseconds)]]
    (fx- now digimon-waketime)))
