#lang typed/racket/base

(provide (all-defined-out))

(require "../zipinfo.rkt")
(require "../archive.rkt")
(require "../zip.rkt")

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

(define zip-directory-timestamp<? : (-> ZIP-Directory ZIP-Directory Boolean)
  (lambda [ldir rdir]
    (< (zip-entry-modify-seconds (zip-directory-mdate ldir) (zip-directory-mtime ldir))
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

(define zip-copy-directory : (-> Input-Port ZIP-Directory Output-Port Archive-Fragments Archive-Fragments)
  (lambda [/dev/zipin cdir /dev/zipout fragments]
     fragments))

(define zip-copy-directories : (-> Input-Port (Listof ZIP-Directory) Output-Port Archive-Fragments Archive-Fragments)
  (lambda [/dev/zipin cdirs /dev/zipout fragments]
    (for/fold ([fragments++ : Archive-Fragments fragments])
              ([cdir (in-list cdirs)])
      (zip-copy-directory /dev/zipin cdir /dev/zipout fragments++))))

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
