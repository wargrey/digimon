#lang typed/racket/base

(provide (all-defined-out))

(require "unsafe/release/ops.rkt")
(require "../character.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct word-statistics
  ([asian : Natural]
   [letter-word : Natural]
   [whitespace : Natural]
   [newline : Natural]
   [emoji : Natural]
   [char : Natural]
   [states : (Immutable-Vector Boolean Boolean)])
  #:constructor-name make-word-statistics
  #:type-name Word-Statistics
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define string-word-count-start : (-> Word-Statistics)
  (let ([wstat0 (make-word-statistics 0 0 0 0 0 0 (vector-immutable #false #false))])
    (lambda []
      wstat0)))

(define string-word-count-iterate : (-> String Word-Statistics Word-Statistics)
  (lambda [text pstats]
    (define pstate : (Immutable-Vector Boolean Boolean) (word-statistics-states pstats))
    (define size : Index (string-length text))
    
    (let wc ([as : Index 0]
             [ls : Index 0]
             [ws : Index 0]
             [nl : Index 0]
             [es : Index 0]
             [prev-ls? : Boolean (vector-ref pstate 0)]
             [prev-cr? : Boolean (vector-ref pstate 1)]
             [idx : Nonnegative-Fixnum 0])
      (if (< idx size)

          (let ([c (string-ref text idx)]
                [idx++ (+ idx 1)])
            (cond [(char-asian? c) (wc (unsafe-idx+ as 1) ls ws nl es #false #false idx++)]
                  [(char-emoji? c) (wc as ls ws nl (unsafe-idx+ es 1) #false #false idx++)]
                  [(char-whitespace? c)
                   (let ([ls++ (if (not prev-ls?) ls (unsafe-idx+ ls 1))]
                         [ws++ (unsafe-idx+ ws 1)])
                     (cond [(eq? c #\return) (wc as ls++ ws++ (unsafe-idx+ nl 1) es #false #true idx++)]
                           [(not (eq? c #\linefeed)) (wc as ls++ ws++ nl es #false #false idx++)]
                           [(and prev-cr?) (wc as ls++ ws nl es #false #false idx++)] ; CRLF
                           [else (wc as ls++ ws++ (unsafe-idx+ nl 1) es #false #false idx++)]))]
                  [else (wc as ls ws nl es #true #false idx++)]))

          (make-word-statistics (+ (word-statistics-asian pstats) as)
                                (+ (word-statistics-letter-word pstats) ls)
                                (+ (word-statistics-whitespace pstats) ws)
                                (+ (word-statistics-newline pstats) nl)
                                (+ (word-statistics-emoji pstats) es)
                                (+ (word-statistics-char pstats) size)
                                (vector-immutable prev-ls? prev-cr?))))))

(define string-word-count-done : (-> Word-Statistics Word-Statistics)
  (lambda [pstats]
    (let ([prev-ls? : Boolean (vector-ref (word-statistics-states pstats) 0)])
      (if (vector-ref (word-statistics-states pstats) 0) ; prev-ls?
          (make-word-statistics (word-statistics-asian pstats)
                                (+ (word-statistics-letter-word pstats) 1)
                                (word-statistics-whitespace pstats)
                                (word-statistics-newline pstats)
                                (word-statistics-emoji pstats)
                                (word-statistics-char pstats)
                                (vector-immutable #false #false))
          pstats))))

(define string-word-count : (-> (U String (Listof String)) Word-Statistics)
  (lambda [text]
    (define wstats0 : Word-Statistics (string-word-count-start))
    
    (if (string? text)

        (string-word-count-done
         (string-word-count-iterate text wstats0))

        (let wc ([wstats : Word-Statistics wstats0]
                 [texts : (Listof String) text])
          (if (pair? texts)
              (wc (string-word-count-iterate (car texts) wstats) (cdr texts))
              (string-word-count-done wstats))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define string-asian-split : (-> String (Listof String))
  (lambda [s]
    (regexp-match* #px"(\\p{Lo}+)|(\\p{^Lo}+)" s)))

(define string-asian-partition : (-> String (Values (Listof String) (Listof String)))
  (lambda [s]
    (define-values (as ls)
      (for/fold ([asian : (Listof String) null]
                 [other : (Listof String) null])
                ([token (in-list (string-asian-split s))])
        (if (regexp-match? #px"^\\p{Lo}" token)
            (values (cons token asian) other)
            (values asian (cons token other)))))

    (values (reverse as) (reverse ls))))
