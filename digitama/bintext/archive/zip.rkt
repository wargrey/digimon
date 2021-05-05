#lang typed/racket/base

(provide (all-defined-out))

(require "../zipinfo.rkt")
(require "../archive.rkt")
(require "../zip.rkt")

(require "../../../port.rkt")
(require "../../../stdio.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct zip-archive-entry
  ([cdir : ZIP-Directory]
   [/dev/zipin : Input-Port])
  #:type-name ZIP-Archive-Entry
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-directory-filename<=>? : (-> ZIP-Directory ZIP-Directory Integer)
  (lambda [ldir rdir]
    (define lname (zip-directory-filename ldir))
    (define rname (zip-directory-filename rdir))
    
    (cond [(string=? lname rname) 00]
          [(string<? lname rname) -1]
          [else                   01])))

(define zip-directory-timestamp<=? : (-> ZIP-Directory ZIP-Directory Boolean)
  (lambda [ldir rdir]
    (<= (zip-entry-modify-seconds (zip-directory-mdate ldir) (zip-directory-mtime ldir))
        (zip-entry-modify-seconds (zip-directory-mdate rdir) (zip-directory-mtime rdir)))))

(define zip-directory-sort : (All (a) (-> (Listof ZIP-Directory) (-> ZIP-Directory a) (-> a a Boolean) (Listof ZIP-Directory)))
  (lambda [cdirs field-ref <]
    ((inst sort ZIP-Directory a) cdirs < #:key field-ref)))

(define zip-directory-sort/filename : (-> (Listof ZIP-Directory) (Listof ZIP-Directory))
  (lambda [cdirs]
    (zip-directory-sort cdirs zip-directory-filename string<?)))

(define zip-directory-sort/position : (-> (Listof ZIP-Directory) (Listof ZIP-Directory))
  (lambda [cdirs]
    (zip-directory-sort cdirs zip-directory-relative-offset <)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-archive-fragments : (-> Input-Port (Listof ZIP-Directory) (Values Archive-Fragments Natural))
  (lambda [/dev/zipin cdirs]
    (let seek-fragment ([ocdirs : (Listof ZIP-Directory) (zip-directory-sort/position cdirs)]
                        [fragments : Archive-Fragments null]
                        [last-end : Natural 0])
      (cond [(null? ocdirs) (values (reverse fragments) last-end)]
            [else (let-values ([(self rest) (values (car ocdirs) (cdr ocdirs))])
                    (define-values (offset offend) (zip-directory-entry-section /dev/zipin self))
                    (define fragment-size : Integer (- offset last-end))
                    (cond [(<= fragment-size 0) (seek-fragment rest fragments offend)]
                          [else (seek-fragment rest (cons (cons last-end fragment-size) fragments) offend)]))]))))

(define zip-copy-directory : (-> Input-Port ZIP-Directory Output-Port Archive-Fragments (Values ZIP-Directory Archive-Fragments))
  (lambda [/dev/zipin srcdir /dev/zipout fragments]
    (define here-position : Natural (file-position /dev/zipout))  
    (define-values (here-cdir in-offset in-csize) (zip-directory-relocate srcdir here-position #:clear-data-descriptor-flag? #true))
    (define here-file : ZIP-File (zip-directory->local-file-entry here-cdir))
    (define-values (?fragment ordered-fragments) (zip-select-fragment fragments (+ (sizeof-zip-file here-file) in-csize)))

    (define-values (cdir self-file)
      (cond [(not ?fragment) (values here-cdir here-file)]
            [else (let*-values ([(frag-pos) (car ?fragment)]
                                [(cdir _ __) (zip-directory-relocate srcdir frag-pos #:clear-data-descriptor-flag? #true)])
                    (file-position /dev/zipout frag-pos)
                    (values cdir (zip-directory->local-file-entry cdir)))]))
    
    (file-position /dev/zipin in-offset)
    (read-zip-file /dev/zipin) ; useless because we need the chance to remove the data descriptor following the content
    
    (let ([/dev/entin (open-input-block /dev/zipin in-csize #false)])
      (write-zip-file self-file /dev/zipout)
      (copy-port /dev/entin /dev/zipout)
      (close-input-port /dev/entin)
      (flush-output /dev/zipout))
    
    (values cdir
            (cond [(not ?fragment) ordered-fragments]
                  [else (let ([subfrag-position (file-position /dev/zipout)])
                          ; move to the current position of `/dev/zipout`
                          (file-position /dev/zipout here-position)
                          (zip-fragments-update ordered-fragments ?fragment subfrag-position))]))))

(define zip-copy-directories : (-> Input-Port (Listof ZIP-Directory) Output-Port Archive-Fragments (Values (Listof ZIP-Directory) Archive-Fragments))
  (lambda [/dev/zipin cdirs /dev/zipout fragment0]
    (define-values (#{cdirs++ : (Listof ZIP-Directory)} #{fragments : Archive-Fragments})
      (for/fold ([updated-cdirs : (Listof ZIP-Directory) null] [fragments++ : Archive-Fragments fragment0])
                ([cdir (in-list cdirs)])
        (let-values ([(updirs frag++) (zip-copy-directory /dev/zipin cdir /dev/zipout fragments++)])
          (values (cons updirs updated-cdirs) frag++))))
    
    (values cdirs++ fragments)))

(define zip-archive-entry-partition : (-> (Listof ZIP-Directory) Archive-Entries (Option Path-String) (Option Path-String)
                                          (Values (Listof ZIP-Directory) (Listof ZIP-Directory) (Listof Archive-Entry)))
  (lambda [cdirectories entries root zip-root]
    (define stegrat : (Listof (Pairof String Archive-Entry))
      (let flatten-entry : (Listof (Pairof String Archive-Entry)) ([es : Archive-Entries entries])
        (for/fold ([stegrat : (Listof (Pairof String Archive-Entry)) null])
                  ([e (in-list entries)])
          (if (archive-entry? e)
              (let* ([entry-source (archive-entry-source e)]
                     [regular-file? (or (bytes? entry-source) (file-exists? entry-source))]
                     [entry-name (zip-path-normalize (archive-entry-reroot (archive-entry-name e) root zip-root 'stdin) regular-file?)])
                (cons (cons entry-name e) stegrat))
              (append (flatten-entry e) stegrat)))))

    (let partition ([cdirectories : (Listof ZIP-Directory) cdirectories]
                    [named-entries : (Listof (Pairof String Archive-Entry)) (reverse stegrat)]
                    [seirotceridc : (Listof ZIP-Directory) null]
                    [seirtne : (Listof Archive-Entry) null])
      (cond [(null? named-entries) (values (reverse seirotceridc) cdirectories (reverse seirtne))]
            [else (let-values ([(self-named-entry rest-named-entries) (values (car named-entries) (cdr named-entries))])
                    (let search ([cdirs : (Listof ZIP-Directory) cdirectories]
                                 [sridc : (Listof ZIP-Directory) null])
                      (cond [(null? cdirs) (partition cdirectories rest-named-entries seirotceridc (cons (cdr self-named-entry) seirtne))]
                            [else (let-values ([(self-cdir rest-cdirs) (values (car cdirs) (cdr cdirs))])
                                    (if (string=? (zip-directory-filename self-cdir) (car self-named-entry))
                                        (partition (append (reverse sridc) rest-cdirs) rest-named-entries (cons self-cdir seirotceridc) seirtne)
                                        (search rest-cdirs (cons self-cdir sridc))))])))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define zip-select-fragment : (-> Archive-Fragments Natural (Values (Option Archive-Fragment) Archive-Fragments))
  (lambda [fragments request-size]
    (define ordered-fragments ((inst sort Archive-Fragment) fragments > #:key cdr))

    (values (and (pair? ordered-fragments)
                 (let ([fragment (car ordered-fragments)])
                   (and (<= request-size (cdr fragment))
                        fragment)))
            ordered-fragments)))

(define zip-fragments-update : (-> Archive-Fragments Archive-Fragment Natural Archive-Fragments)
  (lambda [fragments fragment subfrag-start]
    (let update ([fs : Archive-Fragments fragments]
                 [sf : Archive-Fragments null])
      (cond [(null? fs) (reverse sf)]
            [else (let-values ([(self rest) (values (car fs) (cdr fs))])
                    (cond [(not (= (car self) (car fragment))) (update rest (cons self sf))]
                          [else (let ([size (- (+ (car self) (cdr self)) subfrag-start)])
                                  
                                  (append (reverse sf)
                                          (cond [(<= size 0) rest]
                                                [else (cons (cons subfrag-start size) rest)])))]))]))))

(define zip-fragments-add-entry : (-> Archive-Fragments Input-Port ZIP-Directory Archive-Fragments)
  (lambda [fragments /dev/zipin cdir]
    (define-values (offset offend) (zip-directory-entry-section /dev/zipin cdir))
    (define fragment-size : Integer (- offend offset))

    (cond [(<= fragment-size 0) fragments]
          [else (let insert ([fs : Archive-Fragments fragments]
                             [sf : Archive-Fragments null])
                  (cond [(null? fs) (cons (cons offset fragment-size) sf)]
                        [else (let*-values ([(self rest) (values (car fs) (cdr fs))]
                                            [(self-start self-size) (values (car self) (cdr self))]
                                            [(self-end) (+ self-start self-size)])
                                (if (= self-end offset)
                                    (append rest (cons (cons self-start (+ self-size fragment-size)) sf))
                                    (insert rest (cons self sf))))]))])))
