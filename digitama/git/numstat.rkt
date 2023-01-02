#lang typed/racket/base

(provide (all-defined-out) s/day)

(require racket/string)
(require racket/match)

(require "parameter.rkt")

(require "../../number.rkt")
(require "../../date.rkt")

(require "../exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Git-Numstat-Line (Immutable-Vector Natural Natural (U String (Pairof String String))))
(define-type Git-Numstat (Pairof Natural (Listof Git-Numstat-Line)))

(define-type Git-Date-Datum (U Integer String date))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numstat-exec : (-> Path (Listof (Listof String)) (-> String (Listof Git-Numstat) (Listof Git-Numstat)) (Listof Git-Numstat) (Listof Git-Numstat))
  (lambda [git options numstat-fold initial]
    ((inst fg-recon-exec* (Listof Git-Numstat))
     #:silent git-silents (current-git-procedure)
     git options numstat-fold initial)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numstat-make-fold : (-> (Option Natural) (Option String) Git-Match-Datum (-> String (Listof Git-Numstat) (Listof Git-Numstat)))
  (lambda [group-span:s subpath/ filter]
    (define adjust-timestamp : (case-> [Natural -> Natural]
                                       [(Option Natural) -> (Option Natural)])
      (λ [timestamp]
        (and timestamp
             (cond [(not group-span:s) timestamp]
                   [else (floor-seconds timestamp group-span:s)]))))

    (define (filter-path? [path : (U String (Pairof String String))]) : (Option (U String (Pairof String String)))
      (cond [(string? path) (and (git-path-match? path filter #:match-for-empty? #true) path)]
            [else (and (git-path-match? (cdr path) filter #:match-for-empty? #true) path)]))

    (define filter-map-path : (-> (U String (Pairof String String)) (Option (U String (Pairof String String))))
      (cond [(not subpath/) filter-path?]
            [else (λ [path] (filter-path? (cond [(string? path) (string-append subpath/ path)]
                                                [else (cons (string-append subpath/ (car path))
                                                            (string-append subpath/ (cdr path)))])))]))

    (define (make-now-numstat) : Git-Numstat
      (cons (adjust-timestamp (max (current-seconds) 0)) null))
    
    (define (make-empty-numstat [timestamp : Natural]) : Git-Numstat
      (cons timestamp null))

    (λ [line stats]
      (define-values (self rest)
        (cond [(pair? stats) (values (car stats) (cdr stats))]
              [else (values (make-now-numstat) null)]))
      
      (let ([tokens (git-numstat-line-split line)])
        (cond [(list? tokens) ; normal
               (let ([insertion (string->natural (car tokens))]
                     [deletion (string->natural (cadr tokens))]
                     [pathname (filter-map-path (caddr tokens))])
                 (cond [(and insertion deletion pathname)
                        (let ([num-line (vector-immutable insertion deletion pathname)])
                          (cons ((inst cons Natural (Listof Git-Numstat-Line)) (car self) (cons num-line (cdr self))) rest))]
                       [else (cons self rest)]))]
              [(string? tokens)
               ;;; another timestamp follows closely, rarely happens. or,
               ;;; `git log` always provides timestamps whereas `git diff` doesn't
               (let ([timestamp (adjust-timestamp (string->natural tokens))])
                 (cond [(not timestamp) '#:deadcode (cons self rest)]
                       [(= timestamp (car self)) (cons self rest)]
                       [(null? (cdr self)) (cons (make-empty-numstat timestamp) rest)]
                       [else (cons (make-empty-numstat timestamp) (cons self rest))]))]
              [(vector? tokens) ; rename
               (let ([insertion (string->natural (vector-ref tokens 0))]
                     [deletion (string->natural (vector-ref tokens 1))]
                     [pathname (filter-map-path (vector-ref tokens 2))])
                 (cond [(and insertion deletion pathname)
                        (let ([rename-num-line (vector-immutable insertion deletion pathname)])
                          (cons ((inst cons Natural (Listof Git-Numstat-Line)) (car self) (cons rename-num-line (cdr self))) rest))]
                       [else (cons self rest)]))]
              [else (cons ((inst cons Natural (Listof Git-Numstat-Line)) (car self) (reverse (cdr self))) rest)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numatat-merge : (-> (Listof Git-Numstat) (Listof Git-Numstat) Boolean (Listof Git-Numstat))
  (lambda [lstats rstats reverse?]
    (define lt? (if reverse? > <))
    (let merge ([lstats : (Listof Git-Numstat) lstats]
                [rstats : (Listof Git-Numstat) rstats]
                [stats : (Listof Git-Numstat) null])
      (cond [(null? lstats) (append (reverse stats) rstats)]
            [(null? rstats) (append (reverse stats) lstats)]
            [else (let*-values ([(lstat lrest) (values (car lstats) (cdr lstats))]
                                [(rstat rrest) (values (car rstats) (cdr rstats))]
                                [(lself rself) (values (car lstat) (car rstat))])
                    (cond [(lt? lself rself) (merge lrest rstats (cons lstat stats))]
                          [(not (= lself rself)) (merge lstats rrest (cons rstat stats))]
                          [else ; NOTE: the numstat list is currently in reverse order since `numstat-fold` doesn't have chance to reverse them
                           (let ([self ((inst cons Natural (Listof Git-Numstat-Line)) lself (append (cdr rstat) (cdr lstat)))])
                             (merge lrest rrest (cons self stats)))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numstat-line-split : (-> String (U False String (List String String String) (Vector String String (Pairof String String))))
  (lambda [line]
    (match (string-split line)
      [(list a d f) (list a d f)]
      [(list ts) ts]
      [(list a d f "=>" rest ...)
       (let ([raw (string-join (cons f (cons "=>" rest)))])
         (cond [(regexp-match? px:rename:normal raw) (vector a d (cons (regexp-replace px:rename:normal raw "\\1") (regexp-replace px:rename:normal raw "\\2")))]
               [(regexp-match? px:rename:sub raw) (vector a d (cons (regexp-replace px:rename:sub raw "") (regexp-replace px:rename:sub raw "/\\1")))]
               [(regexp-match? px:rename:sup raw) (vector a d (cons (regexp-replace px:rename:sup raw "\\1/") (regexp-replace px:rename:sup raw "")))]
               [else #false]))]
      [(list a d f ...) (list a d (regexp-replace px:spaced-fname:numstat line "\\1"))]
      [_ #false])))

(define git-numstat-date-option : (-> String Git-Date-Datum Boolean (Listof String))
  (lambda [opt d local?]
    (list (string-append "--" opt "="
                         (cond [(string? d) d]
                               [(date? d) (date->string d local?)]
                               [(> d 0) (date->string (seconds->date d local?) local?)]
                               [else (string-append "'" (number->string (- d)) " days ago'")])))))
