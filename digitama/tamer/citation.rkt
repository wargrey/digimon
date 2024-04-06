#lang racket/base

(provide (all-defined-out))

(require racket/format)
(require racket/symbol)
(require racket/string)

(require scribble/manual)
(require scriblib/autobib)

(require file/convertible)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-citation-style (make-parameter number-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-bib stx)
  (syntax-parse stx #:literals []
    [(_ id bib-args ...)
     (with-syntax* ([id* (format-id #'id "~a*" (syntax-e #'id))])
       (syntax/loc stx
         (begin (define id (make-bib bib-args ...))
                (define id* (in-bib id (format ":~a" 'id))))))]))

; https://tools.ietf.org/html/draft-carpenter-rfc-citation-recs-01#section-5.1
(define-syntax (define-rfc-bib stx)
  (syntax-parse stx #:datum-literals []
    [(_ key number title
        (~alt (~optional (~seq #:author author) #:defaults ([author #'#false]))
              (~optional (~seq #:date date) #:defaults ([date #'#false]))
              (~optional (~seq #:note note) #:defaults ([note #'#false]))
              (~optional (~seq #:doi doi) #:defaults ([doi #'#false])))
        ...)
     (syntax/loc stx
       (define-bib key
         #:title title
         #:author (bib-entry~author author)
         #:location (techrpt-location #:institution "RFC Editor" #:number (number->string number))
         #:url (format "https://www.rfc-editor.org/rfc/rfc~a.html" (number->string number))
         #:date date
         #:note note))]))

(define rfc-bib-entry
  (lambda [number title #:author [author #false] #:date [date #false] #:key [key #false] #:note [note #false]]
    (define request-for-comments (number->string number))
    
    (bib-entry #:key      (bib-entry~key key (λ [] (string-append "RFC" request-for-comments)))
               #:title    title
               #:author   (bib-entry~author author)
               #:date     (bib-entry~date date)
               #:location (techrpt-location #:institution "RFC Editor" #:number request-for-comments)
               #:url      (format "https://www.rfc-editor.org/rfc/rfc~a.html" request-for-comments)
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
              (~optional (~seq #:pages pages) #:defaults ([pages #'#false]))
              (~optional (~seq #:doi doi) #:defaults ([doi #'#false])))
        ...)
     (syntax/loc stx
       (define-bib key
         #:title title
         #:author (bib-entry~author author)
         #:location (and (or edition publisher chapter)
                         (cond [(not chapter) (book-location #:edition edition #:publisher publisher)]
                               [else (book-chapter-location chapter #:publisher publisher #:series series #:pages pages #:volume volume)]))
         #:date date
         #:url url
         #:note note
         #:doi doi
         #:is-book? #true))]))

(define book-bib-entry
  (lambda [key title author publisher
               #:date [date #false] #:note [note #false] #:url [url #false] #:doi [doi #false]
               #:edition [edition #false] #:chapter [chapter #false] #:series [series #false] #:pages [pages #false] #:volume [volume #false]]
    (bib-entry #:key      (bib-entry~key key)
               #:title    title
               #:author   (bib-entry~author author)
               #:location (and (or edition publisher chapter)
                               (cond [(not chapter) (book-location #:edition edition #:publisher publisher)]
                                     [else (book-chapter-location chapter #:publisher publisher #:series series #:pages pages #:volume volume)]))
               #:date     (bib-entry~date date)
               #:url      (bib-entry-uri url doi)
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
              (~optional (~seq #:pages pages) #:defaults ([pages #'#false]))
              (~optional (~seq #:doi doi) #:defaults ([doi #'#false])))
        ...)
     (syntax/loc stx
       (define-bib key
         #:title title
         #:author (bib-entry~author author)
         #:location (proceedings-location location #:pages pages #:series series #:volume volume)
         #:date date
         #:url url
         #:note note
         #:doi doi))]))

(define proceedings-bib-entry
  (lambda [key title author location
               #:date [date #false] #:note [note #false] #:url [url #false] #:doi [doi #false]
               #:number [number #false] #:pages [pages #false] #:volume [volume #false]]
    (bib-entry #:key      (bib-entry~key key)
               #:title    title
               #:author   (bib-entry~author author)
               #:location (proceedings-location location #:pages pages #:number number #:volume volume)
               #:date     (bib-entry~date date)
               #:url      (bib-entry-uri url doi)
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
              (~optional (~seq #:pages pages) #:defaults ([pages #'#false]))
              (~optional (~seq #:doi doi) #:defaults ([doi #'#false])))
        ...)
     (syntax/loc stx
       (define-bib key
         #:title title
         #:author (bib-entry~author author)
         #:location (journal-location location #:pages pages #:number number #:volume volume)
         #:date date
         #:url url
         #:note note
         #:doi doi))]))

(define journal-bib-entry
  (lambda [key title author location
               #:date [date #false] #:note [note #false] #:url [url #false] #:doi [doi #false]
               #:volume [volume #false] #:number [number #false] #:pages [pages #false]]
    (bib-entry #:key      (bib-entry~key key)
               #:title    title
               #:author   (bib-entry~author author)
               #:location (journal-location location #:pages pages #:number number #:volume volume)
               #:date     (bib-entry~date date)
               #:url      (bib-entry-uri url doi)
               #:note     note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-dissertation-bib stx)
  (syntax-parse stx #:datum-literals []
    [(_ key title author institution
        (~alt (~optional (~seq #:date date) #:defaults ([date #'#false]))
              (~optional (~seq #:url url) #:defaults ([url #'#false]))
              (~optional (~seq #:note note) #:defaults ([note #'#false]))
              (~optional (~seq #:degree degree) #:defaults ([degree #'"PhD"]))
              (~optional (~seq #:doi doi) #:defaults ([doi #'#false])))
        ...)
     (syntax/loc stx
       (define-bib key
         #:title title
         #:author (bib-entry~author author)
         #:location (dissertation-location #:institution institution #:degree degree)
         #:date date
         #:url url
         #:note note
         #:doi doi))]))

(define dissertation-bib-entry
  (lambda [key title author institution #:degree [degree "PhD"]
               #:date [date #false] #:note [note #false] #:url [url #false] #:doi [doi #false]]
    (bib-entry #:key      (bib-entry~key key)
               #:title    title
               #:author   (bib-entry~author author)
               #:location (dissertation-location #:institution institution #:degree degree)
               #:date     (bib-entry~date date)
               #:url      (bib-entry-uri url doi)
               #:note     note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-report-bib stx)
  (syntax-parse stx #:datum-literals []
    [(_ key title author institution number
        (~alt (~optional (~seq #:date date) #:defaults ([date #'#false]))
              (~optional (~seq #:url url) #:defaults ([url #'#false]))
              (~optional (~seq #:note note) #:defaults ([note #'#false]))
              (~optional (~seq #:doi doi) #:defaults ([doi #'#false])))
        ...)
     (syntax/loc stx
       (define-bib key
         #:title title
         #:author (bib-entry~author author)
         #:location (techrpt-location #:institution institution #:number number)
         #:date date
         #:url url
         #:note note
         #:doi doi))]))

(define report-bib-entry
  (lambda [key title author institution number
               #:date [date #false] #:note [note #false] #:url [url #false] #:doi [doi #false]]
    (bib-entry #:key      (bib-entry~key key)
               #:title    title
               #:author   (bib-entry~author author)
               #:location (techrpt-location #:institution institution #:number number)
               #:date     (bib-entry~date date)
               #:url      (bib-entry-uri url doi)
               #:note     note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-url-bib stx)
  (syntax-parse stx #:datum-literals []
    [(_ key title url
        (~alt (~optional (~seq #:author author) #:defaults ([author #'#false]))
              (~optional (~seq #:date date) #:defaults ([date #'#false]))
              (~optional (~seq #:note note) #:defaults ([note #'#false]))
              (~optional (~seq #:doi doi) #:defaults ([doi #'#false]))
              (~optional (~seq #:accessed accessed) #:defaults ([accessed #'#false])))
        ...)
     (syntax/loc stx
       (define-bib key
         #:title title
         #:author (bib-entry~author author)
         #:location (if (not accessed) (webpage-location url) (webpage-location url #:accessed accessed))
         #:date date
         #:note note
         #:doi doi))]))

(define url-bib-entry
  (lambda [key title url #:author [author #false] #:date [date #false] #:note [note #false] #:accessed [accessed #false]]
    (bib-entry #:key      (bib-entry~key key)
               #:title    title
               #:location (if (not accessed) (webpage-location url) (webpage-location url #:accessed accessed))
               #:author   (bib-entry~author author)
               #:date     (bib-entry~date date)
               #:url      url
               #:note     note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bibtex-cite
  (lambda [key-cite bib-cite bib . bibs]
    (cond [(bib? bib) (apply (bib-cite) bib bibs)]
          [else ((key-cite) (string-join (map ~a (cons bib bibs)) " "))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bib-entry~key
  (lambda [key [~key #false]]
    (cond [(string? key) key]
          [(symbol? key) (symbol->immutable-string key)]
          [(procedure? ~key) (~key)]
          [else (~a key)])))

(define bib-entry~date
  (lambda [date]
    (cond [(string? date) date]
          [(number? date) (number->string date)]
          [else #false])))

(define bib-entry~author
  (lambda [author]
    (cond [(list? author) (string-join author ", " #:before-last " and ")]
          [else author])))

(define bib-entry-uri
  (lambda [url doi]
    (or doi url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plt-tr1
  (make-bib #:title    "Reference: Racket"
            #:author   (authors "Matthew Flatt" "PLT")
            #:date     "2010"
            #:location (techrpt-location #:institution "PLT Design Inc." #:number "PLT-TR-2010-1")
            #:url      "https://racket-lang.org/tr1"))

(define #%racket.bib
  (bib-entry #:key      "Racket"
             #:title    "Reference: Racket"
             #:author   (authors "Matthew Flatt" "PLT")
             #:date     "2010"
             #:location (techrpt-location #:institution "PLT Design Inc." #:number "PLT-TR-2010-1")
             #:url      "https://racket-lang.org/tr1"))

(define #%scribble.bib
  (bib-entry #:key      "Scribble"
             #:title    "The Racket Documentation Tool"
             #:author   (authors "Matthew Flatt" "Eli Barzilay")
             #:url      "https://docs.racket-lang.org/scribble/index.html"))
