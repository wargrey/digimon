#lang racket/base

(provide (all-defined-out))

(require scribble/core)
(require scribble/decode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-content-filter
  (lambda [c]
    (cond [(content? c) c]
          [else (format "~a" c)])))

(define handbook-block-filter
  (lambda [c]
    (cond [(block? c) c]
          [else (make-paragraph plain c)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define handbook-newline-element?
  (lambda [v]
    (and (element? v)
         (eq? (element-style v)
              'newline))))

(define handbook-meta-paragraph?
  (lambda [v]
    (and (paragraph? v)
         (let ([s (paragraph-style v)])
           (and (style? s)
                (eq? (style-name s)
                     'pretitle))))))

(define handbook-decode-lines
  (lambda [body #:each-line [line-wrap values] #:finalize [finalize values] #:nested? [nested? #false] #:filter [satisfy? #false]]
    (let partition ([senil null]
                    [enil null]
                    [rest body])
      (cond [(pair? rest)
             (let-values ([(self tail) (values (car rest) (cdr rest))])
               (cond [(list? self) (partition senil enil (append self body))]
                     [(splice? self) (partition senil enil (append (splice-run self) body))]
                     [(handbook-newline-element? self)
                      (if (null? senil)
                          (partition (list (line-wrap (decode-content (reverse enil)))) null tail)
                          (partition (cons (line-wrap (decode-content (reverse enil))) senil) null tail))]
                     [(and (element? self) (if (procedure? nested?) (nested? self) nested?))
                      (partition senil enil (append (decode-content (list (element-content self))) tail))]
                     [(and satisfy? (not (satisfy? self))) (partition senil enil tail)]
                     [else (partition senil (cons self enil) tail)]))]
            [else (finalize (reverse (cons (line-wrap (decode-content (reverse enil))) senil)))]))))
