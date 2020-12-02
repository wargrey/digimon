#lang racket/base

(provide (all-defined-out))

(require racket/format)

(require scribble/manual)
(require scriblib/autobib)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://tools.ietf.org/html/draft-carpenter-rfc-citation-recs-01#section-5.1

(define-syntax (define-rfc-bib stx)
  (syntax-parse stx #:datum-literals []
    [(_ key number title
        (~alt (~optional (~seq #:author author) #:defaults ([author #'#false]))
              (~optional (~seq #:date date) #:defaults ([date #'#false]))
              (~optional (~seq #:note note) #:defaults ([note #'#false])))
        ...)
     (syntax/loc stx
       (define key
         (let ([request-for-comments (number->string number)])
           (in-bib (make-bib #:title title
                             #:author author
                             #:location (techrpt-location #:institution "RFC Editor" #:number request-for-comments)
                             #:url (format "https://www.rfc-editor.org/rfc/rfc~a.txt" request-for-comments)
                             #:date date
                             #:note note)
                   (format ":~a" 'key)))))]))

(define rfc-bib-entry
  (lambda [number title #:author [author #false] #:date [date #false] #:key [key #false] #:note [note #false]]
    (define request-for-comments (number->string number))
    
    (bib-entry #:key      (bib-entry~key key (λ [] (string-append "RFC" request-for-comments)))
               #:title    title
               #:author   author
               #:date     (bib-entry~date date)
               #:location (techrpt-location #:institution "RFC Editor" #:number request-for-comments)
               #:url      (format "https://www.rfc-editor.org/rfc/rfc~a.txt" request-for-comments)
               #:note     note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-book-bib stx)
  (syntax-parse stx #:datum-literals []
    [(_ key title author publisher
        (~alt (~optional (~seq #:date date) #:defaults ([date #'#false]))
              (~optional (~seq #:url url) #:defaults ([url #'#false]))
              (~optional (~seq #:note note) #:defaults ([note #'#false]))
              (~optional (~seq #:edition edition) #:defaults ([edition #'#false]))
              (~optional (~seq #:chapter chapter) #:defaults ([chapter #'#false]))
              (~optional (~seq #:series series) #:defaults ([series #'#false]))
              (~optional (~seq #:volume volume) #:defaults ([volume #'#false]))
              (~optional (~seq #:pages pages) #:defaults ([pages #'#false])))
        ...)
     (syntax/loc stx
       (define key
         (in-bib (make-bib #:title title
                           #:author author
                           #:location (cond [(not chapter) (book-location #:edition edition #:publisher publisher)]
                                            [else (book-chapter-location chapter #:publisher publisher #:series series #:pages pages #:volume volume)])
                           #:date date
                           #:url url
                           #:note note
                           #:is-book? #true)
                 (format ":~a" 'key))))]))

(define book-bib-entry
  (lambda [key title author publisher
               #:date [date #false] #:note [note #false] #:url [url #false]
               #:edition [edition #false] #:chapter [chapter #false] #:series [series #false] #:pages [pages #false] #:volume [volume #false]]
    (bib-entry #:key      (bib-entry~key key)
               #:title    title
               #:author   author
               #:location (cond [(not chapter) (book-location #:edition edition #:publisher publisher)]
                                [else (book-chapter-location chapter #:publisher publisher #:series series #:pages pages #:volume volume)])
               #:date     (bib-entry~date date)
               #:url      url
               #:note     note
               #:is-book? #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-proceedings-bib stx)
  (syntax-parse stx #:datum-literals []
    [(_ key title author location
        (~alt (~optional (~seq #:date date) #:defaults ([date #'#false]))
              (~optional (~seq #:url url) #:defaults ([url #'#false]))
              (~optional (~seq #:note note) #:defaults ([note #'#false]))
              (~optional (~seq #:series series) #:defaults ([series #'#false]))
              (~optional (~seq #:volume volume) #:defaults ([volume #'#false]))
              (~optional (~seq #:pages pages) #:defaults ([pages #'#false])))
        ...)
     (syntax/loc stx
       (define key
         (in-bib (make-bib #:title title
                           #:author author
                           #:location (proceedings-location location #:pages pages #:series series #:volume volume)
                           #:date date
                           #:url url
                           #:note note)
                 (format ":~a" 'key))))]))

(define proceedings-bib-entry
  (lambda [key title author location
               #:date [date #false] #:note [note #false] #:url [url #false]
               #:number [number #false] #:pages [pages #false] #:volume [volume #false]]
    (bib-entry #:key      (bib-entry~key key)
               #:title    title
               #:author   author
               #:location (proceedings-location location #:pages pages #:number number #:volume volume)
               #:date     (bib-entry~date date)
               #:url      url
               #:note     note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-journal-bib stx)
  (syntax-parse stx #:datum-literals []
    [(_ key title author location
        (~alt (~optional (~seq #:date date) #:defaults ([date #'#false]))
              (~optional (~seq #:url url) #:defaults ([url #'#false]))
              (~optional (~seq #:note note) #:defaults ([note #'#false]))
              (~optional (~seq #:volume volume) #:defaults ([volume #'#false]))
              (~optional (~seq #:number number) #:defaults ([number #'#false]))
              (~optional (~seq #:pages pages) #:defaults ([pages #'#false])))
        ...)
     (syntax/loc stx
       (define key
         (in-bib (make-bib #:title title
                           #:author author
                           #:location (journal-location location #:pages pages #:number number #:volume volume)
                           #:date date
                           #:url url
                           #:note note)
                 (format ":~a" 'key))))]))

(define journal-bib-entry
  (lambda [key title author location
               #:date [date #false] #:note [note #false] #:url [url #false]
               #:volume [volume #false] #:number [number #false] #:pages [pages #false]]
    (bib-entry #:key      (bib-entry~key key)
               #:title    title
               #:author   author
               #:location (journal-location location #:pages pages #:number number #:volume volume)
               #:date     (bib-entry~date date)
               #:url      url
               #:note     note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-dissertation-bib stx)
  (syntax-parse stx #:datum-literals []
    [(_ key title author institution
        (~alt (~optional (~seq #:date date) #:defaults ([date #'#false]))
              (~optional (~seq #:url url) #:defaults ([url #'#false]))
              (~optional (~seq #:note note) #:defaults ([note #'#false]))
              (~optional (~seq #:degree degree) #:defaults ([degree #'"PhD"])))
        ...)
     (syntax/loc stx
       (define key
         (in-bib (make-bib #:title title
                           #:author author
                           #:location (dissertation-location #:institution institution #:degree degree)
                           #:date date
                           #:url url
                           #:note note)
                 (format ":~a" 'key))))]))

(define dissertation-bib-entry
  (lambda [key title author institution #:degree [degree "PhD"]
               #:date [date #false] #:note [note #false] #:url [url #false]]
    (bib-entry #:key      (bib-entry~key key)
               #:title    title
               #:author   author
               #:location (dissertation-location #:institution institution #:degree degree)
               #:date     (bib-entry~date date)
               #:url      url
               #:note     note)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-report-bib stx)
  (syntax-parse stx #:datum-literals []
    [(_ key title author institution number
        (~alt (~optional (~seq #:date date) #:defaults ([date #'#false]))
              (~optional (~seq #:url url) #:defaults ([url #'#false]))
              (~optional (~seq #:note note) #:defaults ([note #'#false])))
        ...)
     (syntax/loc stx
       (define key
         (in-bib (make-bib #:title title
                           #:author author
                           #:location (techrpt-location #:institution institution #:number number)
                           #:date date
                           #:url url
                           #:note note)
                 (format ":~a" 'key))))]))

(define report-bib-entry
  (lambda [key title author institution number
               #:date [date #false] #:note [note #false] #:url [url #false]]
    (bib-entry #:key      (bib-entry~key key)
               #:title    title
               #:author   author
               #:location (techrpt-location #:institution institution #:number number)
               #:date     (bib-entry~date date)
               #:url      url
               #:note     note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-url-bib stx)
  (syntax-parse stx #:datum-literals []
    [(_ key title url
        (~alt (~optional (~seq #:author author) #:defaults ([author #'#false]))
              (~optional (~seq #:date date) #:defaults ([date #'#false]))
              (~optional (~seq #:note note) #:defaults ([note #'#false])))
        ...)
     (syntax/loc stx
       (define key
         (in-bib (make-bib #:title title
                           #:author author
                           #:url url
                           #:date date
                           #:note note)
                 (format ":~a" 'key))))]))

(define url-bib-entry
  (lambda [key title url #:author [author #false] #:date [date #false] #:note [note #false]]
    (bib-entry #:key      (bib-entry~key key)
               #:title    title
               #:author   author
               #:date     (bib-entry~date date)
               #:url      url
               #:note     note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bib-entry~key
  (lambda [key [~key #false]]
    (cond [(string? key) key]
          [(symbol? key) (symbol->string key)]
          [(procedure? ~key) (~key)]
          [else (~a key)])))

(define bib-entry~date
  (lambda [date]
    (cond [(string? date) date]
          [(number? date) (number->string date)]
          [else #false])))
