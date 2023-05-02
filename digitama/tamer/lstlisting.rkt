#lang racket/base

(provide (all-defined-out))

(require scribble/core)
(require scribble/decode)
(require scribble/latex-properties)

(require "misc.rkt")
(require "block.rkt")
(require "../tamer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-code-block
  (lambda [content-style language filepath? srcpath ex-lastline? maybe-range]
    (define source (if (path? srcpath) (path->string srcpath) srcpath))
    (define lang (value->block language))
    (define code (value->block source))
    (define range (code-range srcpath maybe-range ex-lastline?))
    (define rel-srcpath (tr-d source))

    (define-values (body title)
      (cond [(pair? range)
             (values (make-nested-flow code-chunk-style
                                       (list lang
                                             (value->block (number->string (car range)))
                                             (value->block (number->string (cdr range)))
                                             code))
                     (format "~a:~a" rel-srcpath (car range)))]
            [(exact-integer? range)
             (values (make-nested-flow code-tail-style (list lang (value->block (number->string range)) code))
                     (format "~a:~a" rel-srcpath range))]
            [else (values (make-nested-flow code-file-style (list lang code)) rel-srcpath)]))
    (make-nested-flow content-style
                      (cond [(not filepath?) (list body)]
                            [else (list (make-paragraph code-title-style title)
                                        body)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define code-range
  (lambda [srcpath hints ex-lastline?]
    (define (do-range /dev/srcin hint line0)
      (cond [(exact-positive-integer? hint) hint]
            [(or (regexp? hint) (byte-regexp? hint)) (search-linenumber /dev/srcin hint line0)]
            [else #false]))

    (and (pair? hints)
         (call-with-input-file srcpath
           (λ [/dev/srcin]
             (define maybe-start (do-range /dev/srcin (car hints) 1))
             (cond [(null? (cdr hints)) maybe-start]
                   [else (let ([maybe-end (and maybe-start (do-range /dev/srcin (cadr hints) (+ maybe-start 1)))])
                           (and maybe-start maybe-end
                                (< maybe-start maybe-end)
                                (cons maybe-start
                                      (if (not ex-lastline?)
                                          maybe-end (- maybe-end 1)))))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define code-style-extras
  (list 'multicommand
        (make-tex-addition
         (tamer-block-source "lstlisting.tex"))))

(define code-title-style (make-style "lstCodeTitle" code-style-extras))
(define code-file-style (make-style "lstFile" code-style-extras))
(define code-tail-style (make-style "lstTail" code-style-extras))
(define code-chunk-style  (make-style "lstChunk" code-style-extras))

(define (value->block v) (make-paragraph placeholder-style v))

(define (search-linenumber /dev/srcin pattern line0)
  (let search ([nl line0])
    (define line (read-line /dev/srcin))
    (and (string? line)
         (cond [(regexp-match? pattern line) nl]
               [else (search (+ nl 1))]))))
