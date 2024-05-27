#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/path)

(require "../exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type D2-Theme-Datum (U Symbol Natural))
(define-type D2-Theme (U D2-Theme-Datum (Pairof D2-Theme-Datum D2-Theme-Datum)))

(define d2-default-interval : (Parameterof Index) (make-parameter 1200))
(define d2-default-inset-pixels : (Parameterof Index) (make-parameter 8))

(define d2-default-layout : (Parameterof Symbol) (make-parameter 'elk))
(define d2-default-theme : (Parameterof D2-Theme) (make-parameter 0))

(define d2-default-sketch? : (Parameterof Boolean) (make-parameter #false))
(define d2-default-appendix? : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define d2-render : (->* ((U Bytes Path-String) (Option Path-String))
                         (Symbol #:target (Option String) #:scale (Option Real) #:theme (Option D2-Theme) #:inset (Option Integer)
                                 #:interval (Option Integer) #:timeout (Option Integer)
                                 #:bundle? Boolean #:appendix? Boolean #:sketch? Boolean #:center? Boolean
                                 #:debug? Boolean #:stdin-log-level (Option Symbol) #:outfile (Option (U Path-String Output-Port)))
                         Bytes)
  (lambda [#:outfile [outfile #false] #:target [target #false] #:scale [scale #false] #:theme [theme #false] #:inset [inset #false]
           #:interval [interval #false] #:timeout [timeout #false]
           #:sketch? [sketch? (d2-default-sketch?)] #:appendix? [appendix? (d2-default-appendix?)]
           #:center? [center? #true] #:bundle? [bundle? #true] #:stdin-log-level [log-level #false] #:debug? [debug? #false]
           src.d2 d2.out [layout (d2-default-layout)]]
    (define d2 (find-executable-path "d2"))
   
    (cond [(not d2) (error layout "D2 not found")]
          [(and (not (bytes? src.d2)) (not (file-exists? src.d2))) (error layout "no such D2 script file: ~a" src.d2)]
          [else (let ([operation 'd2])
                  (define options : (Listof (Listof String))
                    (list (d2-options
                           (list (cons 'layout layout)
                                 (cons 'scale scale)
                                 (cons 'pad (or inset (d2-default-inset-pixels)))
                                 (cons 'animate-interval (d2-interval interval (and d2.out (path-get-extension d2.out)))) ; SVG and GIF only, and stops others
                                 (cons 'timeout timeout)))
                          (d2-flags
                           (list (cons 'bundle bundle?)
                                 (cons 'sketch sketch?)
                                 (cons 'center center?)
                                 (cons 'force-appendix appendix?) ; for forcing SVG adding appendix
                                 (cons 'debug debug?)))
                          (d2-themes (or theme (d2-default-theme)))
                          (if (not target) null (list "--target" (format "'~a'" target)))
                          (d2-iofile src.d2)
                          (d2-iofile d2.out)))
                  
                  (when (and d2.out)
                    (define dir (path-only d2.out))
                    (when (and dir (not (directory-exists? dir)))
                      (fg-recon-mkdir operation dir)))

                  (let ([/dev/stdin (and (bytes? src.d2) (open-input-bytes src.d2))])
                    (if (or (path? outfile) (string? outfile))
                        (call-with-output-file* outfile #:exists 'truncate/replace
                          (λ [[/dev/d2out : Output-Port]] : Bytes
                            (fg-recon-exec/pipe #:stdin-log-level log-level
                                                #:/dev/stdin /dev/stdin
                                                #:/dev/stdout /dev/d2out
                                                operation d2 options)))
                        
                        (fg-recon-exec/pipe #:stdin-log-level log-level
                                            #:/dev/stdin /dev/stdin
                                            #:/dev/stdout outfile
                                            operation d2 options))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define d2-smart-dependencies : (->* (Path) ((Listof Path)) (Listof Path))
  (lambda [entry [memory null]]
    (foldl (λ [[subpath : Bytes] [memory : (Listof Path)]] : (Listof Path)
             (define subsrc (simplify-path (build-path (assert (path-only entry) path?) (d2-path-from-bytes subpath))))
             (cond [(member subsrc memory) memory]
                   [else (d2-smart-dependencies subsrc memory)]))
           (append memory (list entry))
           (call-with-input-file* entry
             (λ [[texin : Input-Port]]
               (regexp-match* #px"(?<=@)(\"[^\"]+\"|([.]{1,2}/|[^.\\s]+)+)"
                              texin))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define d2-options : (-> (Listof (Pairof Symbol (U Symbol Real False))) (Listof String))
  (lambda [opts]
    (for/list ([opt (in-list opts)] #:when (cdr opt))
      (format "--~a=~a" (car opt) (cdr opt)))))

(define d2-flags : (-> (Listof (Pairof Symbol Boolean)) (Listof String))
  (lambda [opts]
    (for/list ([opt (in-list opts)])
      (format "--~a=~a" (car opt) (if (cdr opt) 'true 'false)))))

(define d2-iofile : (-> (U Bytes False Path-String) (Listof String))
  (lambda [d2]
    (list (cond [(or (bytes? d2) (not d2)) "-"]
                [(path? d2) (path->string d2)]
                [else d2]))))

(define d2-interval : (-> (Option Integer) (Option Bytes) (Option Integer))
  (lambda [usr-arg ext]
    (and (member ext '(#".svg" #".gif" #".SVG" #".GIF"))
         (cond [(not usr-arg) (d2-default-interval)]
               [else usr-arg]))))

(define d2-script-destination : (->* (Path-String Bytes) (Boolean #:dest-dirname String) (Option Path))
  (lambda [src ext [contained-in-package? #true] #:dest-dirname [rootdir "diagram"]]
    (define dirname : (Option Path) (path-only src))
    (define basename : (Option Path) (file-name-from-path src))

    (and (path? dirname) (path? basename)
         (build-path dirname (car (use-compiled-file-paths))
                     rootdir (path-replace-extension basename ext)))))

(define d2-path-from-bytes : (-> Bytes String)
  (lambda [raw]
    (define size (bytes-length raw))

    (define subpath : String
      (if (and (> size 2) (eq? (bytes-ref raw 0) #x22))
          (bytes->string/utf-8 raw #false 1 (- size 1))
          (bytes->string/utf-8 raw)))

    (cond [(string-suffix? subpath ".d2") subpath]
          [else (string-append subpath ".d2")])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define d2-theme : (-> Symbol D2-Theme-Datum (Immutable-HashTable Symbol Index) (Listof String))
  (lambda [option theme names]
    (cond [(index? theme) (list (format "--~a=~a" option theme))]
          [(hash-has-key? names theme) (list (format "--~a=~a" option (hash-ref names theme)))]
          [else null])))

(define d2-themes : (-> (Option D2-Theme) (Listof String))
  (lambda [themes]
    (cond [(not themes) null]
          [(pair? themes) (append (d2-theme 'theme (car themes) d2-light-themes)
                                  (d2-theme 'dark-theme (cdr themes) d2-dark-themes))]
          [else (d2-theme 'theme themes d2-light-themes)])))

(define d2-light-themes : (Immutable-HashTable Symbol Index)
  #hash((grey . 1)
        (flagship . 3)
        (classics . 4)
        (mixed-berry-blue . 5)
        (grape-soda . 6)
        (aubergine . 7)
        (colorblind-clear . 8)
        (vanilla-nitro-cola . 100)
        (orange-creamsicle . 101)
        (shirley-temple . 102)
        (earth-tones . 103)
        (everglade-green . 104)
        (buttered-toast . 105)
        (terminal . 300)
        (terminal-grayscale . 301)
        (origami . 302)))

(define d2-dark-themes : (Immutable-HashTable Symbol Index)
  #hash((mauve . 200)
        (flagship . 201)))
