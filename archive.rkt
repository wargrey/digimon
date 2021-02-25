#lang typed/racket/base

(provide (all-defined-out))
(provide Archive-Entry Archive-Entries archive-entry?)
(provide Archive-Directory-Configure Archive-Entry-Config make-archive-directory-entries)
(provide make-archive-chained-configure make-archive-ignore-configure defualt-archive-ignore-configure)
(provide make-archive-file-entry make-archive-ascii-entry make-archive-binary-entry)
(provide ZIP-Strategy zip-strategy? zip-normal-preference zip-lazy-preference)
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
                                           (Output-Port #:width Byte #:add-gap-line? Boolean #:binary? Boolean #:decimal-cursor? Boolean #:metainfo? Boolean)
                                           (Archive-Entry-Readerof* Natural))
  (lambda [#:width [width 32] #:add-gap-line? [addline? #true] #:binary? [binary? #false] #:decimal-cursor? [dec-cursor? #true] #:metainfo? [metainfo? #true]
           [/dev/zipout (current-output-port)]]
    (λ [/dev/zipin entry directory? timestamp idx]
      (define /dev/hexout : Output-Port (open-output-hexdump /dev/zipout #:width width #:binary? binary? #:decimal-cursor? dec-cursor?))
      
      (unless (not addline?)
        (unless (void? idx)
          (newline /dev/zipout)))
      
      (let ([cdir (current-zip-entry)])
        (display (object-name /dev/zipin) /dev/zipout)

        (cond [(or directory? (not cdir)) (newline /dev/zipout)]
              [else (let*-values ([(csize rsize) (zip-content-size* (list cdir))]
                                  [(cfactor) (- 1 (if (= rsize 0) 1 (/ csize rsize)))])
                      (fprintf /dev/zipout " [~a]~n" (~% cfactor #:precision `(= 2))))])
        
        (when (zip-directory? cdir)
          (let ([comment (zip-directory-comment cdir)])
            (unless (string=? comment "")
              (displayln comment /dev/zipout)))

          (unless (not metainfo?)
            (for ([info (in-list (read-zip-metainfos (zip-directory-metainfo cdir)))])
              (displayln info)))))
      
      (copy-port /dev/zipin /dev/hexout)

      (cond [(void? idx) 1]
            [else (add1 idx)]))))

(define make-archive-verification-reader : (->* () (Index #:dtrace Any) (Archive-Entry-Readerof* (Listof (List String (U True String)))))
  (lambda [[pool-size 4096] #:dtrace [topic #false]]
    (λ [/dev/zipin entry directory? timestamp result-set]
      (define cdir (current-zip-entry))
      
      (cond [(and cdir (not directory?))
             (define-values (CRC32 rSize)
               (if (zip-directory? cdir)
                   (values (zip-directory-crc32 cdir) (zip-directory-rsize cdir))
                   (values (zip-entry-crc32 cdir) (zip-entry-rsize cdir))))
             
             (define result : (U True String)
               (with-handlers ([exn:fail? exn-message])
                 (let-values ([(rsize crc32) (zip-entry-copy /dev/zipin /dev/null (max 1 pool-size))])
                   (cond [(not (= rSize rsize)) (format "Bad size ~a (should be ~a)" rsize rSize)]
                         [(not (= CRC32 crc32)) (format "Bad CRC ~a (should be ~a)" (~hexstring crc32) (~hexstring CRC32))]
                         [else #true]))))

             (let ([entry-result (list entry result)])
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

           ; archive extractor manages the custodian, which controls the file resource.
           ; TODO: deal with file permission and more
           (let ([/dev/zipout (open-output-file target #:exists (case operation [(error append) operation] [else 'truncate/replace]))])
             (cond [(not checksum?) (copy-port /dev/zipin /dev/zipout)]
                   [else (let ([cdir (assert (current-zip-entry))])      
                           (define CRC32 : Index (if (zip-directory? cdir) (zip-directory-crc32 cdir) (zip-entry-crc32 cdir)))
                           (define-values (rsize crc32) (zip-entry-copy /dev/zipin /dev/zipout (max 1 pool-size)))

                           (define-values (r c p) (port-next-location /dev/zipin))
                           (displayln (list r c p (file-position /dev/zipin)))
                           
                           (unless (= CRC32 crc32)
                             (throw-check-error /dev/zipin '|| "Bad CRC ~a (should be ~a)"
                                                (~hexstring crc32) (~hexstring CRC32))))])


             (cond [(path? solution)
                    (file-or-directory-modify-seconds target timestamp)
                    #false #| don't `touch` existing file |#]
                   [else #true]))))
    
    (λ [/dev/zipin entry directory? timestamp0 _]
      (when (absolute-path? entry)
        (throw-check-error /dev/zipin '|| "entry is an absolute path: ~a" entry))

      (define subpaths : (Listof (U 'same 'up Path-For-Some-System)) (explode-path/cleanse entry #:strip strip))

      (when (and (pair? subpaths) (not (eq? subpaths 'same)))
        (when (and (not permissive?) (eq? (car subpaths) 'up))
          (throw-check-error /dev/zipin '|| "up-directory indicator is disallowed: ~a" entry))

        (let ([timestamp (if (not preserve-timestamps?) timestamp0 0)]
              [target (apply build-path rootdir subpaths)])
          (when (and (path? target)
                     (cond [(not directory?) (redirect-file /dev/zipin entry target timestamp)]
                           [(and mkdir?) (make-directory* target)]
                           [else #false #| we don't know if certain directory already created because of its children |#]))
            (file-or-directory-modify-seconds target timestamp void)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-file-reader zip-list-directories #:+ (Listof ZIP-Directory) #:binary
  (lambda [/dev/zipin src]
    (define maybe-sigoff (zip-seek-signature /dev/zipin))

    (cond [(not maybe-sigoff) (throw-signature-error /dev/zipin 'zip-directory-list "not a ZIP file")]
          [else (let* ([eocdr (read-zip-end-of-central-directory /dev/zipin)]
                       [cdir-offset (zip-end-of-central-directory-cdir-offset eocdr)])
                  (let ls ([sridc : (Listof ZIP-Directory) null]
                           [pos : Natural (port-seek /dev/zipin cdir-offset)])
                    (cond [(>= pos maybe-sigoff) (reverse sridc)]
                          [else (let ([cdir (read-zip-directory /dev/zipin)])
                                  (ls (cons cdir sridc)
                                      (file-position /dev/zipin)))])))])))

(define-file-reader zip-list-entries #:+ (Listof ZIP-Entry) #:binary
  (lambda [/dev/zipin src]
    (define maybe-sigoff (zip-seek-signature /dev/zipin))

    (cond [(not maybe-sigoff) (throw-signature-error /dev/zipin 'zip-entry-list "not a ZIP file")]
          [else (let* ([eocdr (read-zip-end-of-central-directory /dev/zipin)])
                  (let ls ([seirtne : (Listof ZIP-Entry) null]
                           [cdir-pos : Natural (zip-end-of-central-directory-cdir-offset eocdr)])
                    (cond [(>= cdir-pos maybe-sigoff) (reverse seirtne)]
                          [else (let ([cdir (read-zip-directory /dev/zipin cdir-pos)])
                                  (ls (cons (read-zip-entry* /dev/zipin (zip-directory-relative-offset cdir)) seirtne)
                                      (+ cdir-pos (sizeof-zip-directory cdir))))])))])))

(define-file-reader zip-list-local-entries #:+ (Listof ZIP-Entry) #:binary
  (lambda [/dev/zipin src]
    (when (not (zip-seek-signature /dev/zipin))
      (throw-signature-error /dev/zipin 'zip-entry-list "not a ZIP file"))

    (port-seek /dev/zipin 0)
    (let ls ([seirtne : (Listof ZIP-Entry) null])
      (cond [(not (zip-seek-local-file-signature /dev/zipin)) (reverse seirtne)]
            [else (let-values ([(lfheader dr-size) (read-zip-entry** /dev/zipin)])
                    (when (= dr-size 0)
                      (port-skip /dev/zipin (zip-entry-csize lfheader)))
                    (ls (cons lfheader seirtne)))]))))

(define-file-reader zip-list-comments #:+ (Pairof String (Listof (Pairof String String)))
  (lambda [/dev/zipin src]
    (define maybe-sigoff (zip-seek-signature /dev/zipin))

    (cond [(not maybe-sigoff) (throw-signature-error /dev/zipin 'zip-entry-list "not a ZIP file")]
          [else (let* ([eocdr (read-zip-end-of-central-directory /dev/zipin)]
                       [zcomment (zip-end-of-central-directory-comment eocdr)])
                  (let ls ([seirtne : (Listof (Pairof String String)) null]
                           [cdir-pos : Natural (zip-end-of-central-directory-cdir-offset eocdr)])
                    (cond [(>= cdir-pos maybe-sigoff) (cons zcomment (reverse seirtne))]
                          [else (let ([cdir (read-zip-directory /dev/zipin cdir-pos)])
                                  (ls (cons (cons (zip-directory-filename cdir) (zip-directory-comment cdir)) seirtne)
                                      (+ cdir-pos (sizeof-zip-directory cdir))))])))])))

(define zip-list : (-> (U Input-Port Path-String (Listof (U ZIP-Directory ZIP-Entry))) (Listof String))
  (lambda [/dev/zipin]
    (for/list ([lst (in-list (zip-list* /dev/zipin))])
      (car lst))))

(define zip-list* : (-> (U Input-Port Path-String (Listof (U ZIP-Directory ZIP-Entry))) (Listof (List String Index Index)))
  (lambda [/dev/zipin]
    (define entries : (Listof (U ZIP-Directory ZIP-Entry))
      (cond [(input-port? /dev/zipin) (zip-list-directories /dev/zipin)]
            [(not (list? /dev/zipin)) (zip-list-directories* /dev/zipin)]
            [else /dev/zipin]))
    
    (for/list ([e (in-list entries)])
      (if (zip-directory? e)
          (list (zip-directory-filename e)
                (zip-directory-csize e) (zip-directory-rsize e))
          (list (zip-entry-filename e)
                (zip-entry-csize e) (zip-entry-rsize e))))))

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
      (if (zip-directory? e)
          (values (+ csize (zip-directory-csize e)) (+ rsize (zip-directory-rsize e)))
          (values (+ csize (zip-entry-csize e)) (+ rsize (zip-entry-rsize e)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-create : (->* ((U Path-String Output-Port) Archive-Entries)
                          (#:root (Option Path-String) #:zip-root (Option Path-String) #:suffixes (Listof Symbol)
                           #:strategy PKZIP-Strategy #:memory-level Positive-Byte String)
                          Void)
  (lambda [#:root [root (current-directory)] #:zip-root [zip-root #false] #:suffixes [suffixes (archive-no-compression-suffixes)]
           #:strategy [strategy #false] #:memory-level [memlevel 8]
           out.zip entries [comment "packed by λsh - https://github.com/wargrey/lambda-shell"]]
    (parameterize ([current-custodian (make-custodian)])
      (define /dev/zipout : Output-Port (if (output-port? out.zip) out.zip (open-output-file out.zip)))
      (define px:suffix : Regexp (archive-suffix-regexp (append suffixes pkzip-default-suffixes)))
      (define seekable? : Boolean (port-random-access? /dev/zipout))
      (define crc-pool : Bytes (make-bytes 4096))

      (define (write-entries [entries : (Rec aes (Listof (U Archive-Entry aes)))]) : (Rec zds (Listof (U ZIP-Directory False zds)))
        (for/fold ([cdirs : (Rec zds (Listof (U ZIP-Directory False zds))) null])
                  ([entry : (Rec aes (U Archive-Entry (Listof aes))) (in-list entries)])
          (cons (cond [(list? entry) (write-entries entry)]
                      [else (zip-write-entry /dev/zipout entry
                                             root zip-root px:suffix seekable?
                                             strategy memlevel crc-pool)])
                cdirs)))

      (dynamic-wind void
                    (λ [] (zip-write-directories /dev/zipout comment (write-entries entries)))
                    (λ [] (custodian-shutdown-all (current-custodian)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-directory-partition : (-> (U Input-Port Path-String (Listof ZIP-Directory)) (U Path-String (Listof Path-String))
                                      (Values (Listof ZIP-Directory) (Listof ZIP-Directory) (Listof String)))
  (lambda [/dev/zipin entries]
    (define cdirectories : (Listof ZIP-Directory)
      (cond [(list? /dev/zipin) /dev/zipin]
            [(input-port? /dev/zipin) (zip-list-directories /dev/zipin)]
            [else (zip-list-directories* /dev/zipin)]))
    (define targets : (Listof String)
      (cond [(list? entries) (map zip-path-normalize entries)]
            [else (list (zip-path-normalize entries))]))

    (let partition ([cdirectories : (Listof ZIP-Directory) cdirectories]
                    [paths : (Listof String) targets]
                    [seirotceridc : (Listof ZIP-Directory) null]
                    [shtap : (Listof String) null])
      (cond [(null? paths) (values (reverse seirotceridc) cdirectories (reverse shtap))]
            [else (let-values ([(self-path rest-path) (values (car paths) (cdr paths))])
                    (let search ([cdirs : (Listof ZIP-Directory) cdirectories]
                                 [sridc : (Listof ZIP-Directory) null])
                      (cond [(null? cdirs) (partition cdirectories rest-path seirotceridc (cons self-path shtap))]
                            [else (let-values ([(self-cdir rest-cdirs) (values (car cdirs) (cdr cdirs))])
                                    (cond [(not (string=? self-path (zip-directory-filename self-cdir))) (search rest-cdirs (cons self-cdir sridc))]
                                          [else (partition (append (reverse sridc) rest-cdirs) rest-path (cons self-cdir seirotceridc) shtap)]))])))]))))

(define #:forall (seed) zip-extract : (case-> [(U Input-Port Path-String) -> Void]
                                              [(U Input-Port Path-String) (Archive-Entry-Readerof* seed) -> Void]
                                              [(U Input-Port Path-String) (Archive-Entry-Readerof seed) seed -> seed])
  (case-lambda
    [(/dev/zipin) (zip-extract /dev/zipin (make-archive-hexdump-reader))]
    [(/dev/zipin read-entry) (void (zip-extract /dev/zipin read-entry (void)))]
    [(/dev/zipin read-entry datum0)
     (if (input-port? /dev/zipin)
         (let ([maybe-sigoff (zip-seek-signature /dev/zipin)])
           (cond [(not maybe-sigoff) (throw-signature-error /dev/zipin 'zip-extract "not a ZIP file")]
                 [else (let* ([eocdr (read-zip-end-of-central-directory /dev/zipin)]
                              [cdir-offset (zip-end-of-central-directory-cdir-offset eocdr)])
                         (let extract ([datum : seed datum0]
                                       [pos : Natural (port-seek /dev/zipin cdir-offset)])
                           (cond [(>= pos maybe-sigoff) datum]
                                 [else (let ([cdir (read-zip-directory /dev/zipin)]
                                             [pos++ (file-position /dev/zipin)])
                                         (extract (or (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e) #false)])
                                                        (zip-extract-entry /dev/zipin cdir read-entry datum))
                                                      datum)
                                                  (port-seek /dev/zipin pos++)))])))]))
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
                   ([cdir (in-list (zip-list-directories /dev/zipin))])
           (or (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e) #false)])
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
                    (λ [] (read-entry (open-input-zip-entry /dev/zipin cdir)
                                      (zip-directory-filename cdir)
                                      (zip-folder-entry? cdir)
                                      (zip-entry-modify-seconds (zip-directory-mdate cdir) (zip-directory-mtime cdir))
                                      datum0))
                    (λ [] (custodian-shutdown-all (current-custodian)))))])]))
