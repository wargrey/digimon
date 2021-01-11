#lang typed/racket/base

(provide (all-defined-out))
(provide Archive-Entry archive-entry?)
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
(require "date.rkt")
(require "port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Archive-Entry-Readerof* a b) (-> Input-Port String Boolean Natural a b))
(define-type (Archive-Entry-Readerof a) (Archive-Entry-Readerof* a a))
(define-type Archive-Entry-Reader (Archive-Entry-Readerof* Void Void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define archive-no-compression-suffixes : (Parameterof (Listof Symbol)) (make-parameter null))

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

(define zip-list : (-> (U Input-Port Path-String (Listof ZIP-Directory) (Listof ZIP-Entry)) (Listof String))
  (lambda [/dev/zipin]
    (for/list ([lst (in-list (zip-list* /dev/zipin))])
      (car lst))))

(define zip-list* : (-> (U Input-Port Path-String (Listof ZIP-Directory) (Listof ZIP-Entry)) (Listof (List String Index Index)))
  (lambda [/dev/zipin]
    (define entries : (U (Listof ZIP-Directory) (Listof ZIP-Entry))
      (cond [(input-port? /dev/zipin) (zip-list-directories /dev/zipin)]
            [(not (list? /dev/zipin)) (zip-list-directories* /dev/zipin)]
            [else /dev/zipin]))
    
    (for/list ([e (in-list entries)])
      (if (zip-directory? e)
          (list (zip-directory-filename e)
                (zip-directory-csize e) (zip-directory-rsize e))
          (list (zip-entry-filename e)
                (zip-entry-csize e) (zip-entry-rsize e))))))

(define zip-content-size : (-> (U Input-Port Path-String (Listof ZIP-Directory) (Listof ZIP-Entry)) Natural)
  (lambda [/dev/zipin]
    (define-values (csize rsize) (zip-content-size* /dev/zipin))
    rsize))

(define zip-content-size* : (-> (U Input-Port Path-String (Listof ZIP-Directory) (Listof ZIP-Entry)) (Values Natural Natural))
  (lambda [/dev/zipin]
    (define entries : (U (Listof ZIP-Directory) (Listof ZIP-Entry))
      (cond [(input-port? /dev/zipin) (zip-list-directories /dev/zipin)]
            [(not (list? /dev/zipin)) (zip-list-directories* /dev/zipin)]
            [else /dev/zipin]))
    
    (for/fold ([csize : Natural 0] [rsize : Natural 0])
              ([e (in-list entries)])
      (if (zip-directory? e)
          (values (+ csize (zip-directory-csize e)) (+ rsize (zip-directory-rsize e)))
          (values (+ csize (zip-entry-csize e)) (+ rsize (zip-entry-rsize e)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-create : (->* ((U Path-String Output-Port) (Listof Archive-Entry))
                          (#:root (Option Path-String) #:zip-root (Option Path-String) #:suffixes (Listof Symbol)
                           #:strategy PKZIP-Strategy #:memory-level Positive-Byte
                           String)
                          Void)
  (lambda [#:root [root (current-directory)] #:zip-root [zip-root #false] #:suffixes [suffixes (archive-no-compression-suffixes)]
           #:strategy [strategy #false] #:memory-level [memlevel 8]
           out.zip entries [comment "packed by λsh - https://github.com/wargrey/lambda-shell"]]
    (parameterize ([current-custodian (make-custodian)])
      (define /dev/zipout : Output-Port (if (output-port? out.zip) out.zip (open-output-file out.zip)))
      (define px:suffix : Regexp (archive-suffix-regexp (append suffixes pkzip-default-suffixes)))
      (define seekable? : Boolean (port-random-access? /dev/zipout))

      (dynamic-wind void
                    (λ [] (zip-write-directories /dev/zipout comment
                                                 (for/list ([e (in-list entries)])
                                                   (zip-write-entry /dev/zipout e
                                                                    root zip-root px:suffix seekable?
                                                                    strategy memlevel))))
                    (λ [] (custodian-shutdown-all (current-custodian))))
      (void))))

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
                                              [(U Input-Port Path-String) (Archive-Entry-Readerof (U Void seed)) -> Void]
                                              [(U Input-Port Path-String) (Archive-Entry-Readerof seed) seed -> seed])
  (case-lambda
    [(/dev/zipin) (zip-extract /dev/zipin (make-archive-hexdump-entry-reader))]
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
            [(U Input-Port Path-String) (U String (Listof String)) (Archive-Entry-Readerof (U Void seed)) -> (Values Void (Listof ZIP-Directory) (Listof String))]
            [(U Input-Port Path-String) (U String (Listof String)) (Archive-Entry-Readerof seed) seed -> (Values seed (Listof ZIP-Directory) (Listof String))])
  (case-lambda
    [(/dev/zipin entry) (zip-extract* /dev/zipin entry (make-archive-hexdump-entry-reader))]
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
                                                          [(U Input-Port Path-String) (Listof ZIP-Directory) (Archive-Entry-Readerof (U Void seed)) -> Void]
                                                          [(U Input-Port Path-String) (Listof ZIP-Directory) (Archive-Entry-Readerof seed) seed -> seed])
  (case-lambda
    [(/dev/zipin cdirs) (zip-extract-directories /dev/zipin cdirs (make-archive-hexdump-entry-reader))]
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
            [(U Input-Port Path-String) (Listof ZIP-Directory) (U String (Listof String)) (Archive-Entry-Readerof (U Void seed))
                                        -> (Values Void (Listof ZIP-Directory) (Listof String))]
            [(U Input-Port Path-String) (Listof ZIP-Directory) (U String (Listof String)) (Archive-Entry-Readerof seed) seed
                                        -> (Values seed (Listof ZIP-Directory) (Listof String))])
  (case-lambda
    [(/dev/zipin cdirs entry)
     (zip-extract-directories* /dev/zipin cdirs entry (make-archive-hexdump-entry-reader))]
    [(/dev/zipin cdirs entry read-entry)
     (let-values ([(_ rdirs ??) (zip-extract-directories* /dev/zipin cdirs entry read-entry (void))])
       (values (void) rdirs ??))]
    [(/dev/zipin cdirs entry read-entry datum0)
     (let-values ([(requested-dirs rest-dirs unknowns) (zip-directory-partition cdirs entry)])
       (values (zip-extract-directories /dev/zipin requested-dirs read-entry datum0) rest-dirs unknowns))]))

(define #:forall (a b) zip-extract-entry
  : (case-> #| arity 2 |# [(U Input-Port Path-String) ZIP-Directory -> Boolean]
            #| arity 3 |# [(U Input-Port Path-String) ZIP-Directory (Archive-Entry-Readerof* (U Void a) b) -> Boolean]
            #| arity 4 |# [(U Input-Port Path-String) ZIP-Directory (Archive-Entry-Readerof* a b) a -> (Option b)]
            #| arity 3 |# [(U Input-Port Path-String) (Listof ZIP-Directory) String -> Boolean]
            #| arity 4 |# [(U Input-Port Path-String) (Listof ZIP-Directory) String (Archive-Entry-Readerof* (U Void a) b) -> Boolean]
            #| arity 5 |# [(U Input-Port Path-String) (Listof ZIP-Directory) String (Archive-Entry-Readerof* a b) b -> (Option b)])
  (case-lambda
    [(/dev/zipin entry)
     (zip-extract-entry /dev/zipin entry (make-archive-hexdump-entry-reader))]
    [(/dev/zipin entry/cdirs read-entry/entry)
     (if (list? entry/cdirs)
         (zip-extract-entry /dev/zipin entry/cdirs read-entry/entry (make-archive-hexdump-entry-reader))
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
                   (begin0 (read-entry (open-input-zip-entry /dev/zipin cdir)
                                       (zip-directory-filename cdir)
                                       (zip-folder-entry? cdir)
                                       (msdos-datetime->utc-seconds (zip-directory-mdate cdir)
                                                                    (zip-directory-mtime cdir))
                                       datum0)
                           (custodian-shutdown-all (current-custodian))))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-archive-hexdump-entry-reader : (->* ()
                                                 (Output-Port #:width Byte #:add-gap-line? Boolean #:binary? Boolean #:metainfo? Boolean)
                                                 (Archive-Entry-Readerof (U Void Natural)))
  (lambda [[/dev/zipout (current-output-port)] #:width [width 32] #:add-gap-line? [addline? #true] #:binary? [binary? #false] #:metainfo? [metainfo? #true]]
    (define magazine : Bytes (make-bytes width))
    
    (λ [/dev/zipin entry directory? timestamp idx]
      (unless (not addline?)
        (unless (void? idx)
          (newline /dev/zipout)))
      
      (displayln (object-name /dev/zipin) /dev/zipout)
      
      (let ([cdir (current-zip-entry)])
        (when (zip-directory? cdir)
          (let ([comment (zip-directory-comment cdir)])
            (unless (string=? comment "")
              (displayln comment /dev/zipout)))

          (unless (not metainfo?)
            (for ([info (in-list (read-zip-metainfos (zip-directory-metainfo cdir)))])
              (displayln info)))))
      
      (let hexdump ([pos : Natural 0])
        (define size (read-bytes! magazine /dev/zipin))

        (unless (eof-object? size)
          (display (~r pos #:min-width 8 #:base 16 #:pad-string "0") /dev/zipout)
          (display #\space /dev/zipout)

          (for ([b (in-bytes magazine 0 size)])
            (if (not binary?)
                (display (byte->hex-string b) /dev/zipout)
                (display (byte->bin-string b) /dev/zipout))
            (display #\space /dev/zipout))

          (when (< size width)
            (display (~space (* (assert (- width size) byte?)
                                (if (not binary?) 3 9)))
                     /dev/zipout))
          
          (for ([b (in-bytes magazine 0 size)])
            (define ch (integer->char b))
            (display (if (char-graphic? ch) ch ".") /dev/zipout))
          
          (newline /dev/zipout)
          (hexdump (+ pos size))))

      (cond [(void? idx) 1]
            [else (add1 idx)]))))
