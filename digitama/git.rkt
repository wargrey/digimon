#lang typed/racket/base

(provide (all-defined-out) s/day)

(require racket/string)

(require "../number.rkt")
(require "../date.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Git-Numstat-Line (List Natural Natural String))
(define-type Git-Numstat (Pairof Natural (Listof Git-Numstat-Line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git:%at : String "--pretty=format:%at")

(define git-numstat-make-fold : (-> (Option Natural) (-> String (Listof Git-Numstat) (Listof Git-Numstat)))
  (lambda [group-span:s]
    (define make-empty-numstat : (-> Natural Git-Numstat)
      (lambda [timestamp]
        (cons (cond [(not group-span:s) timestamp]
                    [else (floor-seconds timestamp group-span:s)])
              null)))
    
    (λ [line stats]
      (define-values (#{self : Git-Numstat} rest)
        (cond [(pair? stats) (values (car stats) (cdr stats))]
              [else (values (make-empty-numstat (max (current-seconds) 0)) null)]))
      
      (define tokens : (Listof String) (git-numstat-line-split line))
      
      (cond [(= (length tokens) 3)
             (let ([insertion (string->natural (car tokens))]
                   [deletion (string->natural (cadr tokens))])
               (cond [(and insertion deletion)
                      (let ([self++ ((inst cons Natural (Listof Git-Numstat-Line)) (car self) (cons (list insertion deletion (caddr tokens)) (cdr self)))])
                        (cons self++ rest))]
                     [else (cons self rest)]))]
            [(pair? tokens)
             ;;; another timestamp follows closely, rarely happens. or,
             ;;; `git log` always provides timestamps whereas `git diff` doesn't
             (let ([timestamp (string->natural line)])
               (cond [(not timestamp) '#:deadcode (cons self rest)]
                     [(null? (cdr self)) (cons (make-empty-numstat timestamp) rest)]
                     [else (cons (make-empty-numstat timestamp) (cons self rest))]))]
            [else (cons ((inst cons Natural (Listof Git-Numstat-Line)) (car self) (reverse (cdr self))) rest)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-numstat-line-split : (-> String (Listof String))
  (lambda [line]
    ; Note: lines like `59 53 {src.ext => renamed.ext}` are only produced by `git diff`
    ; TODO: seriously deal with spaces in filename
    (define tokens : (Listof String) (string-split line))
    (cond [(<= (length tokens) 3) tokens]
          [(regexp-match? #px" => " line) (list (car tokens) (cadr tokens) (caddr tokens))] ; TODO: correct?
          [else (list (car tokens) (cadr tokens) (string-join (cddr tokens) " "))])))

