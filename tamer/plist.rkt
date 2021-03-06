#lang typed/racket

(require digimon/plist)
(require digimon/system)
(require digimon/collection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bplist-prove : (-> Path-String Any Any Boolean)
  (lambda [name plist display?]
    (define Info.plist (build-path (find-system-path 'temp-dir) (path-replace-extension name #".plist")))

    (printf "~n>> ~a~n" Info.plist)

    (write-plist plist Info.plist)
    (bplist-dissect Info.plist)

    (let ([bplist (read-plist Info.plist)])
      (unless (not display?)
        (pretty-print plist)
        (pretty-print bplist))
      
      (equal? plist bplist))))

(module+ main
  (enter-digimon-zone!)
  
  (for/and ([info.rkt (in-list (directory-list (build-path (digimon-path 'tamer) "bplist") #:build? #true))]
            #:when (equal? (path-get-extension info.rkt) #".rkt"))
    (define name : (Option Path) (file-name-from-path info.rkt))
    
    (and (path? name)
         (dynamic-require info.rkt #false)
         (let ([ns (module->namespace info.rkt)])
           (define result
             (bplist-prove name
                           (namespace-variable-value 'plist #false (λ [] (box 'bplist-fill)) ns)
                           (namespace-variable-value 'display? #false (λ [] #false) ns)))
           
           (printf "~n~a~n" (if result 'OK 'Bad))
           result))))
