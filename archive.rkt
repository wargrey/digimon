#lang typed/racket/base

(provide (all-defined-out))
(provide Archive-Entry Archive-Entries archive-entry?)
(provide Archive-Directory-Configure Archive-Entry-Config make-archive-directory-entries)
(provide make-archive-chained-configure make-archive-ignore-configure defualt-archive-ignore-configure)
(provide make-archive-file-entry make-archive-ascii-entry make-archive-binary-entry)
(provide ZIP-Strategy zip-strategy? zip-normal-preference zip-lazy-preference zip-special-preference)
(provide zip-identity-preference zip-plain-preference zip-backward-preference zip-run-preference)
(provide ZIP-Entry zip-entry? sizeof-zip-entry)
(provide ZIP-Directory zip-directory? sizeof-zip-directory)

(require "digitama/bintext/archive.rkt")
(require "digitama/bintext/zipinfo.rkt")
(require "digitama/bintext/zipconfig.rkt")
(require "digitama/bintext/zip.rkt")
(require "digitama/ioexn.rkt")

(require "filesystem.rkt")
(require "dtrace.rkt")
(require "format.rkt")
(require "stdio.rkt")
(require "echo.rkt")
(require "port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Archive-Entry-Readerof** a b) (-> Input-Port String Boolean Natural a b))
(define-type (Archive-Entry-Readerof* a) (Archive-Entry-Readerof** (U Void a) a))
(define-type (Archive-Entry-Readerof a) (Archive-Entry-Readerof** a a))
(define-type Archive-Entry-Reader (Archive-Entry-Readerof** Void Void))
(define-type Archive-Entry-Resolve-Conflict (-> Path String Natural (U Symbol Path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define archive-no-compression-suffixes : (Parameterof (Listof Symbol)) (make-parameter null))

(define archive-skip-existing-entry : Archive-Entry-Resolve-Conflict (λ _ 'skip))
(define archive-replace-existing-entry : Archive-Entry-Resolve-Conflict (λ _ 'replace))

(define archive-rename-outdated-entry : Archive-Entry-Resolve-Conflict
  (λ [target entry entry-timestamp]
    (define target-timestamp : Nonnegative-Fixnum (file-or-directory-modify-seconds target))
    
    (cond [(> entry-timestamp target-timestamp)
           (let ([newpath (assert (path-add-timestamp target target-timestamp #true))])
             (rename-file-or-directory target newpath #true)
             'replace)]
          [(= entry-timestamp target-timestamp) 'replace] ; ensure to perform the extracting 
          [else (assert (path-add-timestamp target entry-timestamp #true))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-archive-hexdump-reader : (->* ()
                                           (Output-Port Boolean #:width Byte #:add-gap-line? Boolean #:binary? Boolean #:decimal-cursor? Boolean #:metainfo? Boolean)
                                           (Archive-Entry-Readerof* Natural))
  (lambda [#:width [width 32] #:add-gap-line? [addline? #true] #:binary? [binary? #false] #:decimal-cursor? [dec-cursor? #false] #:metainfo? [metainfo? #true]
           [/dev/zipout (current-output-port)] [check? #true]]
    (λ [/dev/zipin entry folder? timestamp idx]
      (define /dev/hexout : Output-Port (open-output-hexdump /dev/zipout #:width width #:binary? binary? #:decimal-cursor? dec-cursor?))
      
      (unless (not addline?)
        (unless (void? idx)
          (newline /dev/zipout)))
      
      (let ([cdir (assert (current-zip-entry))])
        (define-values (_ CRC32 rSize csize) (zip-directory-data-descriptor cdir))
        
        (display (object-name /dev/zipin) /dev/zipout)

        (cond [(or folder? (not cdir)) (newline /dev/zipout)]
              [else (let*-values ([(cfactor) (- 1 (if (= rSize 0) 1 (/ csize rSize)))])
                      (fprintf /dev/zipout " [~a]~n" (~% cfactor #:precision `(= 2))))])
        
        (when (zip-directory? cdir)
          (let ([comment (zip-directory-comment cdir)])
            (unless (string=? comment "")
              (displayln comment /dev/zipout)))

          (unless (not metainfo?)
            (for ([info (in-list (read-zip-metainfos (zip-directory-metainfo cdir)))])
              (displayln info /dev/zipout))))

        (cond [(not check?) (copy-port /dev/zipin /dev/hexout)]
              [else (let ([errmsg (zip-entry-copy/trap /dev/zipin /dev/hexout rSize CRC32)])
                      (when (string? errmsg)
                        (eechof "~a~n" errmsg #:fgcolor 'red)))]))

      (cond [(void? idx) 1]
            [else (add1 idx)]))))

(define make-archive-verification-reader : (->* () (Index #:dtrace-topic Any) (Archive-Entry-Readerof* (Listof (Pairof String (U True String)))))
  (lambda [[pool-size 4096] #:dtrace-topic [topic #false]]
    (λ [/dev/zipin entry folder? timestamp result-set]
      (define cdir (current-zip-entry))
      
      (cond [(and cdir)
             (let*-values ([(_ CRC32 rSize cSize) (zip-directory-data-descriptor cdir)]
                           [(result) (or folder? (zip-entry-copy/trap /dev/zipin /dev/null rSize CRC32 (max 1 pool-size)))]
                           [(entry-result) (cons entry result)])
               (when (symbol? topic)
                 (if (string? result)
                     (dtrace-error "~a: ~a" entry result #:topic topic #:urgent entry-result)
                     (dtrace-info "~a: OK" entry #:topic topic #:urgent entry-result)))
               
               (cond [(void? result-set) (list entry-result)]
                     [else (cons entry-result result-set)]))]
            [(void? result-set) null]
            [else result-set]))))

(define make-archive-filesystem-reader : (->* ()
                                              (#:strip Integer #:on-conflict Archive-Entry-Resolve-Conflict #:checksum? Boolean
                                               #:preserve-timestamps? Boolean #:always-mkdir? Boolean #:permissive? Boolean
                                               (U Path-String Symbol (Pairof Symbol (Listof Path-String)) False) Index)
                                              Archive-Entry-Reader)
  (lambda [#:strip [strip 0] #:on-conflict [resolve-conflict archive-rename-outdated-entry] #:checksum? [checksum? #true]
           #:permissive? [permissive? #false] #:preserve-timestamps? [preserve-timestamps? #false] #:always-mkdir? [mkdir? #true]
           [root #false] [pool-size 4096]]
    (define rootdir : Path
      (cond [(path? root) root]
            [(string? root) (string->path root)]
            [(symbol? root) (with-handlers ([exn? (λ _ (current-directory))]) (find-system-path root))]
            [(pair? root) (apply build-path (with-handlers ([exn? (λ _ (current-directory))]) (find-system-path (car root))) (cdr root))]
            [else (current-directory)]))
    
    (define (redirect-file [/dev/zipin : Input-Port] [entry : String] [target0 : Path] [timestamp : Natural]) : Any
      (define solution : (U Symbol Path)
        (cond [(not (file-exists? target0)) 'replace]
              [else (resolve-conflict target0 entry timestamp)]))

      (define-values (target operation)
        (if (path? solution)
            (values solution 'replace)
            (values target0 solution)))

      (and (not (eq? operation 'skip))

           (make-parent-directory* target)

           (let ([/dev/zipout (open-output-file target #:exists (case operation [(error append) operation] [else 'truncate/replace]))])
             (cond [(not checksum?) (copy-port /dev/zipin /dev/zipout)]
                   [else (let ([cdir (assert (current-zip-entry))])      
                           (define CRC32 : Index (zip-directory-crc32 cdir))
                           (define-values (rsize crc32) (zip-entry-copy /dev/zipin /dev/zipout (max 1 pool-size)))

                           (unless (= CRC32 crc32)
                             (throw-check-error /dev/zipin '|| "Bad CRC ~a (should be ~a)"
                                                (~hexstring crc32) (~hexstring CRC32))))])

             ; some system may limit the maximum number of open files
             ; but, if exception is threw, the open file would be closed by the custodian
             (close-output-port /dev/zipout)

             ; TODO: deal with file permission and more
           
             (cond [(path? solution)
                    (file-or-directory-modify-seconds target timestamp)
                    #false #| don't `touch` existing file |#]
                   [else #true]))))
    
    (λ [/dev/zipin entry folder? timestamp0 _]
      (when (absolute-path? entry)
        (throw-check-error /dev/zipin '|| "entry is an absolute path: ~a" entry))

      (define subpaths : (Listof (U 'same 'up Path-For-Some-System)) (explode-path/cleanse entry #:strip strip))

      (when (and (pair? subpaths) (not (eq? subpaths 'same)))
        (when (and (not permissive?) (eq? (car subpaths) 'up))
          (throw-check-error /dev/zipin '|| "up-directory indicator is disallowed: ~a" entry))

        (let ([timestamp (if (not preserve-timestamps?) timestamp0 0)]
              [target (apply build-path rootdir subpaths)])
          (when (and (path? target)
                     (cond [(not folder?) (redirect-file /dev/zipin entry target timestamp)]
                           [(and mkdir?) (make-directory* target)]
                           [else #false #| we don't know if certain directory already created because of its children |#]))
            (file-or-directory-modify-seconds target timestamp void)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-file-reader zip-list-directories #:+ (Listof ZIP-Directory) #:binary
  (lambda [/dev/zipin src]
    (define-values (cdir-offset cdir-end _) (zip-seek-central-directory-section /dev/zipin zip-list-directories))

    (let ls ([sridc : (Listof ZIP-Directory) null]
             [cdir-pos : Natural (port-seek /dev/zipin cdir-offset)])
      (cond [(>= cdir-pos cdir-end) (reverse sridc)]
            [else (let ([cdir (read-zip-directory /dev/zipin)])
                    (ls (cons cdir sridc)
                        (+ cdir-pos (sizeof-zip-directory cdir))))]))))

(define-file-reader zip-list-entries #:+ (Listof ZIP-Entry) #:binary
  (lambda [/dev/zipin src]
    (define-values (cdir-offset cdir-end _) (zip-seek-central-directory-section /dev/zipin zip-list-entries))

    (let ls ([seirtne : (Listof ZIP-Entry) null]
             [cdir-pos : Natural cdir-offset])
      (cond [(>= cdir-pos cdir-end) (reverse seirtne)]
            [else (let ([cdir (read-zip-directory /dev/zipin cdir-pos)])
                    (ls (cons (read-zip-entry* /dev/zipin (zip-directory-relative-offset cdir)) seirtne)
                        (+ cdir-pos (sizeof-zip-directory cdir))))]))))

(define-file-reader zip-list-local-entries #:+ (Listof (Pairof ZIP-Entry Natural)) #:binary
  (lambda [/dev/zipin src]
    (zip-seek-signature* /dev/zipin zip-list-local-entries)
    (port-seek /dev/zipin 0)

    (let ls ([seirtne : (Listof (Pairof ZIP-Entry Natural)) null])
      (define maybe-pos : (Option Natural) (zip-seek-local-file-signature /dev/zipin))
      (cond [(not maybe-pos) (reverse seirtne)]
            [else (let-values ([(lfheader dr-size) (read-zip-entry** /dev/zipin)])
                    (when (= dr-size 0)
                      (port-skip /dev/zipin (zip-entry-csize lfheader)))
                    (ls (cons (cons lfheader maybe-pos) seirtne)))]))))

(define-file-reader zip-list-comments #:+ (Pairof String (Listof (Pairof String String)))
  (lambda [/dev/zipin src]
    (define-values (cdir-offset cdir-end zcomment) (zip-seek-central-directory-section /dev/zipin zip-list-comments))

    (let ls ([seirtne : (Listof (Pairof String String)) null]
             [cdir-pos : Natural cdir-offset])
      (cond [(>= cdir-pos cdir-end) (cons zcomment (reverse seirtne))]
            [else (let ([cdir (read-zip-directory /dev/zipin cdir-pos)])
                    (ls (cons (cons (zip-directory-filename cdir) (zip-directory-comment cdir)) seirtne)
                        (+ cdir-pos (sizeof-zip-directory cdir))))]))))

(define zip-list : (-> (U Input-Port Path-String (Listof (U ZIP-Directory ZIP-Entry))) (Listof String))
  (lambda [/dev/zipin]
    (for/list ([lst (in-list (zip-list* /dev/zipin))])
      (car lst))))

(define zip-list* : (-> (U Input-Port Path-String (Listof (U ZIP-Directory ZIP-Entry))) (Listof (List String Natural Natural)))
  (lambda [/dev/zipin]
    (define entries : (Listof (U ZIP-Directory ZIP-Entry))
      (cond [(input-port? /dev/zipin) (zip-list-directories /dev/zipin)]
            [(not (list? /dev/zipin)) (zip-list-directories* /dev/zipin)]
            [else /dev/zipin]))
    
    (for/list ([e (in-list entries)])
      (define-values (fname rsize csize) (zip-entry-metrics e))
      (list fname csize rsize))))

(define zip-content-size : (-> (U Input-Port Path-String (Listof (U ZIP-Directory ZIP-Entry))) Natural)
  (lambda [/dev/zipin]
    (define-values (csize rsize) (zip-content-size* /dev/zipin))
    rsize))

(define zip-content-size* : (-> (U Input-Port Path-String (Listof (U ZIP-Directory ZIP-Entry))) (Values Natural Natural))
  (lambda [/dev/zipin]
    (define entries : (Listof (U ZIP-Directory ZIP-Entry))
      (cond [(input-port? /dev/zipin) (zip-list-directories /dev/zipin)]
            [(not (list? /dev/zipin)) (zip-list-directories* /dev/zipin)]
            [else /dev/zipin]))
    
    (for/fold ([csize : Natural 0] [rsize : Natural 0])
              ([e (in-list entries)])
      (let-values ([(_ r c) (zip-entry-metrics e)])
        (values (+ csize c) (+ rsize r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-create : (->* ((U Path-String Output-Port) Archive-Entries)
                          (#:root (Option Path-String) #:zip-root (Option Path-String) #:suffixes (Listof Symbol)
                           #:strategy PKZIP-Strategy #:memory-level Positive-Byte #:force-zip64? Boolean #:disable-seeking? Boolean
                           String)
                          Void)
  (lambda [#:root [root (current-directory)] #:zip-root [zip-root #false] #:suffixes [suffixes (archive-no-compression-suffixes)]
           #:strategy [strategy #false] #:memory-level [memlevel 8] #:force-zip64? [force-zip64? #false] #:disable-seeking? [disable-seeking? #false]
           out.zip entries [comment "packed by λsh - https://github.com/wargrey/lambda-shell"]]
    (parameterize ([current-custodian (make-custodian)]
                   [default-stdout-all-fields? #false])
      (define /dev/zipout : Output-Port
        (cond [(output-port? out.zip) out.zip]
              [else (begin (make-parent-directory* out.zip)
                           (open-output-file out.zip #:exists 'truncate/replace))]))
      
      (define px:suffix : Regexp (archive-suffix-regexp (append suffixes pkzip-default-suffixes)))
      (define seekable? : Boolean (if (not disable-seeking?) (port-random-access? /dev/zipout) #false))
      (define crc-pool : Bytes (make-bytes 4096))

      (define (write-entries [entries : (Rec aes (Listof (U Archive-Entry aes)))]) : (Rec zds (Listof (U ZIP-Directory False zds)))
        (for/fold ([cdirs : (Rec zds (Listof (U ZIP-Directory False zds))) null])
                  ([entry : (Rec aes (U Archive-Entry (Listof aes))) (in-list entries)])
          (cons (cond [(list? entry) (write-entries entry)]
                      [else (zip-write-entry /dev/zipout entry
                                             root zip-root px:suffix seekable?
                                             strategy memlevel force-zip64? crc-pool)])
                cdirs)))

      (dynamic-wind void
                    (λ [] (zip-write-directories /dev/zipout comment (write-entries entries) force-zip64?))
                    (λ [] (custodian-shutdown-all (current-custodian)))))))

(define zip-update : (->* (Path-String Archive-Entries)
                          (#:root (Option Path-String) #:zip-root (Option Path-String) #:suffixes (Listof Symbol) #:freshen? Boolean
                           #:strategy PKZIP-Strategy #:memory-level Positive-Byte #:force-zip64? Boolean #:disable-seeking? Boolean
                           String)
                          Void)
  (lambda [#:root [root (current-directory)] #:zip-root [zip-root #false] #:suffixes [suffixes (archive-no-compression-suffixes)] #:freshen? [freshen? #false]
           #:strategy [strategy #false] #:memory-level [memlevel 8] #:force-zip64? [force-zip64? #false] #:disable-seeking? [disable-seeking? #false]
           src.zip entries [comment "packed by λsh - https://github.com/wargrey/lambda-shell"]]
    (if (file-exists? src.zip)
        (void)
        (zip-create #:root root #:zip-root zip-root #:suffixes suffixes
                    #:strategy strategy #:memory-level memlevel #:force-zip64? force-zip64? #:disable-seeking? disable-seeking?
                    src.zip entries))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-directory-partition : (-> (U Input-Port Path-String (Listof ZIP-Directory)) (U Path-String Regexp (Listof (U Path-String Regexp)))
                                      (Values (Listof ZIP-Directory) (Listof ZIP-Directory) (Listof String)))
  (lambda [/dev/zipin entries]
    (define cdirectories : (Listof ZIP-Directory)
      (cond [(list? /dev/zipin) /dev/zipin]
            [(input-port? /dev/zipin) (zip-list-directories /dev/zipin)]
            [else (zip-list-directories* /dev/zipin)]))
    (define targets : (Listof (U String Regexp))
      (cond [(list? entries) (for/list ([e (in-list entries)]) (if (regexp? e) e (zip-path-normalize e)))]
            [(regexp? entries) (list entries)]
            [else (list (zip-path-normalize entries))]))

    (let partition ([cdirectories : (Listof ZIP-Directory) cdirectories]
                    [paths : (Listof (U String Regexp)) targets]
                    [seirotceridc : (Listof ZIP-Directory) null]
                    [shtap : (Listof String) null])
      (cond [(null? paths) (values (reverse seirotceridc) cdirectories (reverse shtap))]
            [else (let-values ([(self-path rest-path) (values (car paths) (cdr paths))])
                    (let search ([cdirs : (Listof ZIP-Directory) cdirectories]
                                 [sridc : (Listof ZIP-Directory) null])
                      (cond [(null? cdirs) (partition cdirectories rest-path seirotceridc (if (string? self-path) (cons self-path shtap) shtap))]
                            [else (let-values ([(self-cdir rest-cdirs) (values (car cdirs) (cdr cdirs))])
                                    (if (let ([entry-name (zip-directory-filename self-cdir)])
                                          (if (string? self-path)
                                              (string=? self-path entry-name)
                                              (regexp-match? self-path entry-name)))
                                        (partition (append (reverse sridc) rest-cdirs) rest-path (cons self-cdir seirotceridc) shtap)
                                        (search rest-cdirs (cons self-cdir sridc))))])))]))))

(define #:forall (seed) zip-extract : (case-> [(U Input-Port Path-String) -> Void]
                                              [(U Input-Port Path-String) (Archive-Entry-Readerof* seed) -> Void]
                                              [(U Input-Port Path-String) (Archive-Entry-Readerof seed) seed -> seed])
  (case-lambda
    [(/dev/zipin) (zip-extract /dev/zipin (make-archive-hexdump-reader))]
    [(/dev/zipin read-entry) (void (zip-extract /dev/zipin read-entry (void)))]
    [(/dev/zipin read-entry datum0)
     (if (input-port? /dev/zipin)
         (let-values ([(cdir-offset cdir-end _) (zip-seek-central-directory-section /dev/zipin zip-extract)])
           (let extract ([datum : seed datum0]
                         [cdir-pos : Natural cdir-offset])
             (cond [(>= cdir-pos cdir-end) datum]
                   [else (let ([cdir (read-zip-directory /dev/zipin cdir-pos)])
                           (extract (or (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false) #false)])
                                          (zip-extract-entry /dev/zipin cdir read-entry datum))
                                        datum)
                                    (+ cdir-pos (sizeof-zip-directory cdir))))])))
         (call-with-input-file* /dev/zipin
           (λ [[/dev/zipin : Input-Port]]
             (zip-extract /dev/zipin read-entry datum0))))]))

(define #:forall (seed) zip-extract*
  : (case-> [(U Input-Port Path-String) (U String (Listof String)) -> (Values Void (Listof ZIP-Directory) (Listof String))]
            [(U Input-Port Path-String) (U String (Listof String)) (Archive-Entry-Readerof* seed) -> (Values Void (Listof ZIP-Directory) (Listof String))]
            [(U Input-Port Path-String) (U String (Listof String)) (Archive-Entry-Readerof seed) seed -> (Values seed (Listof ZIP-Directory) (Listof String))])
  (case-lambda
    [(/dev/zipin entry) (zip-extract* /dev/zipin entry (make-archive-hexdump-reader))]
    [(/dev/zipin entry read-entry) (let-values ([(_ rdirs ??) (zip-extract* /dev/zipin entry read-entry (void))]) (values (void) rdirs ??))]
    [(/dev/zipin entry read-entry datum0)
     (if (input-port? /dev/zipin)
         (let-values ([(requested-dirs rest-dirs unknowns) (zip-directory-partition /dev/zipin entry)])
           (values (zip-extract-directories /dev/zipin requested-dirs read-entry datum0) rest-dirs unknowns))
         (apply values
                (call-with-input-file* /dev/zipin
                  (λ [[/dev/zipin : Input-Port]]
                    (let-values ([(datum rsts ??) (zip-extract* /dev/zipin entry read-entry datum0)])
                      (list datum rsts ??))))))]))

(define #:forall (seed) zip-extract-directories : (case-> [(U Input-Port Path-String) (Listof ZIP-Directory) -> Void]
                                                          [(U Input-Port Path-String) (Listof ZIP-Directory) (Archive-Entry-Readerof* seed) -> Void]
                                                          [(U Input-Port Path-String) (Listof ZIP-Directory) (Archive-Entry-Readerof seed) seed -> seed])
  (case-lambda
    [(/dev/zipin cdirs) (zip-extract-directories /dev/zipin cdirs (make-archive-hexdump-reader))]
    [(/dev/zipin cdirs read-entry) (void (zip-extract-directories /dev/zipin cdirs read-entry (void)))]
    [(/dev/zipin cdirs read-entry datum0)
     (if (input-port? /dev/zipin)
         (for/fold ([datum : seed datum0])
                   ([cdir (in-list cdirs)])
           (or (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false) #false)])
                 (zip-extract-entry /dev/zipin cdir read-entry datum))
               datum))
         (call-with-input-file* /dev/zipin
           (λ [[/dev/zipin : Input-Port]]
             (zip-extract-directories /dev/zipin cdirs read-entry datum0))))]))

(define #:forall (seed) zip-extract-directories*
  : (case-> [(U Input-Port Path-String) (Listof ZIP-Directory) (U String (Listof String))
                                        -> (Values Void (Listof ZIP-Directory) (Listof String))]
            [(U Input-Port Path-String) (Listof ZIP-Directory) (U String (Listof String)) (Archive-Entry-Readerof* seed)
                                        -> (Values Void (Listof ZIP-Directory) (Listof String))]
            [(U Input-Port Path-String) (Listof ZIP-Directory) (U String (Listof String)) (Archive-Entry-Readerof seed) seed
                                        -> (Values seed (Listof ZIP-Directory) (Listof String))])
  (case-lambda
    [(/dev/zipin cdirs entry)
     (zip-extract-directories* /dev/zipin cdirs entry (make-archive-hexdump-reader))]
    [(/dev/zipin cdirs entry read-entry)
     (let-values ([(_ rdirs ??) (zip-extract-directories* /dev/zipin cdirs entry read-entry (void))])
       (values (void) rdirs ??))]
    [(/dev/zipin cdirs entry read-entry datum0)
     (let-values ([(requested-dirs rest-dirs unknowns) (zip-directory-partition cdirs entry)])
       (values (zip-extract-directories /dev/zipin requested-dirs read-entry datum0) rest-dirs unknowns))]))

(define #:forall (a b) zip-extract-entry
  : (case-> #| arity 2 |# [(U Input-Port Path-String) ZIP-Directory -> Boolean]
            #| arity 3 |# [(U Input-Port Path-String) ZIP-Directory (Archive-Entry-Readerof** (U Void a) b) -> Boolean]
            #| arity 4 |# [(U Input-Port Path-String) ZIP-Directory (Archive-Entry-Readerof** a b) a -> (Option b)]
            #| arity 3 |# [(U Input-Port Path-String) (Listof ZIP-Directory) String -> Boolean]
            #| arity 4 |# [(U Input-Port Path-String) (Listof ZIP-Directory) String (Archive-Entry-Readerof** (U Void a) b) -> Boolean]
            #| arity 5 |# [(U Input-Port Path-String) (Listof ZIP-Directory) String (Archive-Entry-Readerof** a b) b -> (Option b)])
  (case-lambda
    [(/dev/zipin entry)
     (zip-extract-entry /dev/zipin entry (make-archive-hexdump-reader))]
    [(/dev/zipin entry/cdirs read-entry/entry)
     (if (list? entry/cdirs)
         (zip-extract-entry /dev/zipin entry/cdirs read-entry/entry (make-archive-hexdump-reader))
         (and (zip-extract-entry /dev/zipin entry/cdirs read-entry/entry (void)) #true))]
    [(/dev/zipin cdirs entry read-entry datum0)
     (let ([name (zip-path-normalize entry)])
       (let search ([cdirs : (Listof ZIP-Directory) cdirs])
         (and (null? cdirs)
              (let ([cdir (car cdirs)])
                (if (string=? (zip-directory-filename cdir) name)
                    (zip-extract-entry /dev/zipin cdir read-entry datum0)
                    (search (cdr cdirs)))))))]
    [(/dev/zipin cdir read-entry datum0)
     (cond [(not (input-port? /dev/zipin))
            (call-with-input-file* /dev/zipin
              (λ [[/dev/zipin : Input-Port]]
                (zip-extract-entry /dev/zipin cdir read-entry datum0)))]
           [(list? cdir) (and (zip-extract-entry /dev/zipin cdir read-entry datum0 (void)) #true)]
           [else (parameterize ([current-custodian (make-custodian)]
                                [current-zip-entry cdir])
                   (dynamic-wind
                    void
                    (λ [] (read-entry (open-input-zip-entry /dev/zipin cdir #:verify? #false)
                                      (zip-directory-filename cdir)
                                      (zip-folder-entry? cdir)
                                      (zip-entry-modify-seconds (zip-directory-mdate cdir) (zip-directory-mtime cdir))
                                      datum0))
                    (λ [] (custodian-shutdown-all (current-custodian)))))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (seed) zip-verify : (-> (U Input-Port Path-String) Natural)
  (lambda [/dev/zipin]
    (if (input-port? /dev/zipin)
        (let-values ([(cdir-offset cdir-end _) (zip-seek-central-directory-section /dev/zipin zip-verify)])
          (let verify ([failures : Natural 0]
                       [cdir-pos : Natural cdir-offset])
            (cond [(>= cdir-pos cdir-end) failures]
                  [else (let ([cdir (read-zip-directory /dev/zipin cdir-pos)])
                          (verify (+ failures
                                     (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false) 1)])
                                       (zip-verify-entry /dev/zipin cdir)))
                                  (+ cdir-pos (sizeof-zip-directory cdir))))])))
        (call-with-input-file* /dev/zipin
          (λ [[/dev/zipin : Input-Port]]
            (zip-verify /dev/zipin))))))

(define #:forall (seed) zip-verify* : (-> (U Input-Port Path-String) (U String (Listof String)) (Values Natural (Listof ZIP-Directory) (Listof String)))
  (lambda [/dev/zipin entry]
    (if (input-port? /dev/zipin)
        (let-values ([(requested-dirs rest-dirs unknowns) (zip-directory-partition /dev/zipin entry)])
          (values (zip-verify-directories /dev/zipin requested-dirs) rest-dirs unknowns))
        (apply values
               (call-with-input-file* /dev/zipin
                 (λ [[/dev/zipin : Input-Port]]
                   (let-values ([(datum rsts ??) (zip-verify* /dev/zipin entry)])
                     (list datum rsts ??))))))))

(define #:forall (seed) zip-verify-directories : (-> (U Input-Port Path-String) (Listof ZIP-Directory) Natural)
  (lambda [/dev/zipin cdirs]
    (if (input-port? /dev/zipin)
        (for/fold ([failures : Natural 0])
                  ([cdir (in-list cdirs)])
          (+ failures
             (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:brief? #false) 1)])
               (zip-verify-entry /dev/zipin cdir))))
        (call-with-input-file* /dev/zipin
          (λ [[/dev/zipin : Input-Port]]
            (zip-verify-directories /dev/zipin cdirs))))))

(define #:forall (seed) zip-verify-directories* : (-> (U Input-Port Path-String) (Listof ZIP-Directory) (U String (Listof String))
                                                      (Values Natural (Listof ZIP-Directory) (Listof String)))
  (lambda [/dev/zipin cdirs entry]
    (let-values ([(requested-dirs rest-dirs unknowns) (zip-directory-partition cdirs entry)])
      (values (zip-verify-directories /dev/zipin requested-dirs) rest-dirs unknowns))))

(define #:forall (a b) zip-verify-entry : (case-> [(U Input-Port Path-String) ZIP-Directory -> Natural]
                                                  [(U Input-Port Path-String) (Listof ZIP-Directory) String -> Natural])
  (case-lambda
    [(/dev/zipin cdirs entry)
     (let ([name (zip-path-normalize entry)])
       (let search ([cdirs : (Listof ZIP-Directory) cdirs])
         (cond [(null? cdirs) 0]
               [else (let ([cdir (car cdirs)])
                       (if (string=? (zip-directory-filename cdir) name)
                           (zip-verify-entry /dev/zipin cdir)
                           (search (cdr cdirs))))])))]
    [(/dev/zipin cdir)
     (if (input-port? /dev/zipin)
         (let ([verify-entry (make-archive-verification-reader #:dtrace-topic 'zip-verify)])
           (parameterize ([current-custodian (make-custodian)]
                          [current-zip-entry cdir])
             (dynamic-wind
              void
              (λ [] (let ([result (cdar (verify-entry (open-input-zip-entry /dev/zipin cdir #:verify? #true)
                                                      (zip-directory-filename cdir)
                                                      (zip-folder-entry? cdir)
                                                      (zip-entry-modify-seconds (zip-directory-mdate cdir) (zip-directory-mtime cdir))
                                                      null))])
                      (if (eq? result #true) 0 1)))
              (λ [] (custodian-shutdown-all (current-custodian))))))
         (call-with-input-file* /dev/zipin
           (λ [[/dev/zipin : Input-Port]]
             (zip-verify-entry /dev/zipin cdir))))]))
