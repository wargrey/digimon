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
        (~alt (~optional (~seq #:date date) #:defaults ([date #'#false]))
              (~optional (~seq #:note note) #:defaults ([note #'#false])))
        ...)
     #'(define key
         (let ([request-for-comments (number->string number)])
           (in-bib (make-bib #:title title
                             #:location (techrpt-location #:institution "RFC Editor" #:number request-for-comments)
                             #:url (format "https://www.rfc-editor.org/rfc/rfc~a.txt" request-for-comments)
                             #:date date
                             #:note note)
                   (format ":~a" 'key))))]))

(define rfc-bib-entry
  (lambda [number title #:author [author #false] #:date [date #false] #:key [key #false] #:note [note #false]]
    (define request-for-comments (number->string number))
    
    (bib-entry #:key      (cond [(string? key) key] [(symbol? key) (symbol->string key)] [else (string-append "RFC" request-for-comments)])
               #:title    title
               #:author   author
               #:date     (cond [(string? date) date] [(number? date) (number->string date)] [else #false])
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
     #'(define key
         (in-bib (make-bib #:title title
                           #:author author
                           #:location (cond [(not chapter) (book-location #:edition edition #:publisher publisher)]
                                            [else (book-chapter-location chapter #:publisher publisher #:series series #:pages pages #:volume volume)])
                           #:date date
                           #:url url
                           #:note note
                           #:is-book? #true)
                 (format ":~a" 'key)))]))

(define book-bib-entry
  (lambda [key title author publisher
               #:date [date #false] #:note [note #false] #:url [url #false]
               #:edition [edition #false] #:chapter [chapter #false] #:series [series #false] #:pages [pages #false] #:volume [volume #false]]
    (bib-entry #:key      (cond [(string? key) key] [(symbol? key) (symbol->string key)] [else (~a key)])
               #:title    title
               #:author   author
               #:location (cond [(not chapter) (book-location #:edition edition #:publisher publisher)]
                                [else (book-chapter-location chapter #:publisher publisher #:series series #:pages pages #:volume volume)])
               #:date     (cond [(string? date) date] [(number? date) (number->string date)] [else #false])
               #:url      url
               #:note     note
               #:is-book? #true)))
