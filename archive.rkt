#lang typed/racket/base

(provide (all-defined-out))
(provide ZIP-Entry zip-entry? sizeof-zip-entry)
(provide ZIP-Directory zip-directory? sizeof-zip-directory)

(require racket/port)

(require "digitama/bintext/zipinfo.rkt")
(require "digitama/bintext/zip.rkt")
(require "digitama/ioexn.rkt")

(require "filesystem.rkt")
(require "date.rkt")
(require "port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Archive-Entry-Readerof a) (-> Input-Port String Boolean Natural a a))
(define-type Archive-Entry-Reader (Archive-Entry-Readerof Void))

(define archive-make-write-entry-reader : (->* () (Output-Port #:add-gap-line? Boolean) (Archive-Entry-Readerof (U Void Natural)))
  (lambda [[/dev/zipout (current-output-port)] #:add-gap-line? [addline? #true]]
    (λ [[/dev/zipin : Input-Port] [entry : String] [directory? : Boolean] [timestamp : Natural] [idx : (U Void Natural)]] : (U Void Natural)
      (unless (not addline?)
        (unless (void? idx)
          (newline /dev/zipout)))

      (copy-port /dev/zipin /dev/zipout)
      (newline /dev/zipout)

      (cond [(void? idx) 1]
            [else (add1 idx)]))))

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

(define zip-directory-partition : (->* ((U Input-Port Path-String (Listof ZIP-Directory)) (U Path-String (Listof Path-String))) (Boolean)
                                       (Values (Listof ZIP-Directory) (Listof ZIP-Directory) (Listof String)))
  (lambda [/dev/zipin entries [case-sensitive? #true]]
    (define path=? : (-> String String Boolean) (if (not case-sensitive?) string-ci=? string=?))
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
                                    (cond [(not (path=? self-path (zip-directory-filename self-cdir))) (search rest-cdirs (cons self-cdir sridc))]
                                          [else (partition (append (reverse sridc) rest-cdirs) rest-path (cons self-cdir seirotceridc) shtap)]))])))]))))

(define #:forall (seed) zip-extract : (case-> [(U Input-Port Path-String) -> Void]
                                              [(U Input-Port Path-String) (Archive-Entry-Readerof (U Void seed)) -> Void]
                                              [(U Input-Port Path-String) (Archive-Entry-Readerof seed) seed -> seed])
  (case-lambda
    [(/dev/zipin) (zip-extract /dev/zipin (archive-make-write-entry-reader))]
    [(/dev/zipin read-entry) (void (zip-extract /dev/zipin read-entry (void)))]
    [(/dev/zipin read-entry datum0)
     (cond [(input-port? /dev/zipin) (zip-extract-entry /dev/zipin (zip-list-directories /dev/zipin) read-entry datum0)]
           [else (call-with-input-file* /dev/zipin
                   (λ [[/dev/zipin : Input-Port]]
                     (zip-extract /dev/zipin read-entry datum0)))])]))

(define #:forall (seed) zip-extract-entry : (case-> [Input-Port (Listof ZIP-Directory) -> Void]
                                                    [Input-Port (Listof ZIP-Directory) (Archive-Entry-Readerof (U Void seed)) -> Void]
                                                    [Input-Port (Listof ZIP-Directory) (Archive-Entry-Readerof seed) seed -> seed])
  (case-lambda
    [(/dev/zipin zdirs) (zip-extract-entry /dev/zipin zdirs (archive-make-write-entry-reader))]
    [(/dev/zipin zdirs read-entry) (void (zip-extract-entry /dev/zipin zdirs read-entry (void)))]
    [(/dev/zipin zdirs read-entry datum0)
     (for/fold ([datum : seed datum0])
               ([cdir (in-list (zip-list-directories /dev/zipin))])
       (parameterize ([current-custodian (make-custodian)])
         (begin0 (read-entry (open-input-zip-entry /dev/zipin cdir)
                             (zip-directory-filename cdir)
                             (zip-folder-entry? cdir)
                             (msdos-datetime->utc-seconds (zip-directory-lmdate cdir)
                                                          (zip-directory-lmtime cdir))
                             datum)
                 (custodian-shutdown-all (current-custodian)))))]))

(define #:forall (seed) zip-extract-entry*
  : (case-> [Input-Port (Listof ZIP-Directory) (U String (Listof String)) -> (Values Void (Listof ZIP-Directory) (Listof String))]
            [Input-Port (Listof ZIP-Directory) (U String (Listof String)) (Archive-Entry-Readerof (U Void seed)) -> (Values Void (Listof ZIP-Directory) (Listof String))]
            [Input-Port (Listof ZIP-Directory) (U String (Listof String)) (Archive-Entry-Readerof seed) seed -> (Values seed (Listof ZIP-Directory) (Listof String))])
  (case-lambda
    [(/dev/zipin zdirs entry) (zip-extract-entry* /dev/zipin zdirs entry (archive-make-write-entry-reader))]
    [(/dev/zipin zdirs entry read-entry) (let-values ([(_ rdirs ??) (zip-extract-entry* /dev/zipin zdirs entry read-entry (void))]) (values (void) rdirs ??))]
    [(/dev/zipin zdirs entry read-entry datum0)
     (let-values ([(requested-dirs rest-dirs unknowns) (zip-directory-partition zdirs entry)])
       (values (zip-extract-entry /dev/zipin requested-dirs read-entry datum0) rest-dirs unknowns))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
