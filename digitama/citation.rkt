#lang racket/base

; https://tools.ietf.org/html/draft-carpenter-rfc-citation-recs-01#section-5.1

(provide (all-defined-out))

(require scribble/manual)
(require scriblib/autobib)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-rfc-bib stx)
  (syntax-case stx []
    [(_ key number title addition ...)
     #'(define key
         (let ([request-for-comments (number->string number)])
           (in-bib (make-bib #:title title
                             #:location (techrpt-location #:institution "RFC Editor" #:number request-for-comments)
                             #:url (format "https://www.rfc-editor.org/rfc/rfc~a.txt" request-for-comments)
                             addition ...)
                   (format ":~a" 'key))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
