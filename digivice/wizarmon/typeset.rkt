#lang typed/racket/base

(provide (all-defined-out))

(require "../../digitama/tamer/selector.rkt")
(require "../../cmdopt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-scrbl-name : Symbol '|wizarmon scrbl|)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cmdopt-string->chapter-index : (-> Symbol String (U Positive-Index Char))
  (lambda [option s]
    (if (= (string-length s) 1)
        (let ([idx (string-ref s 0)])
          (cond [(char<=? #\A idx #\Z) idx]
                [(char<=? #\a idx #\z) (char-upcase idx)]
                [else (cmdopt-string+>index option s)]))
        (cmdopt-string+>index option s))))

(define-cmdlet-option scrbl-flags #: Scrbl-Flags
  #:program the-scrbl-name
  #:args args

  #:usage-help "set and overload the offprint configuration"
  #:once-each
  [[(#\f flatten) "perform a granular offprinting"]
   [(#\s strip) "remove prefaces and bonus appendices"]]
  #:multi
  [[(chapter seq) #:=> cmdopt-string->chapter-index id #: Handbook-Chapter-Index  "build the part or chapter whose number is ~1"]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-typeset/flags : (->* (Path Symbol (-> Path Symbol Any)) (Symbol) Any)
  (lambda [path.scrbl lang-name do-typeset [alt-name the-scrbl-name]]
    (define-values (options λargv) (parse-scrbl-flags #:program alt-name))

    (cond [(scrbl-flags-help? options) (display-scrbl-flags #:program alt-name)]
          [(pair? (scrbl-flags-chapter options))
           (let ([selector (make-user-specified-selector (scrbl-flags-chapter options)
                                                         (scrbl-flags-flatten options)
                                                         (scrbl-flags-strip options))])
             (parameterize ([current-user-specified-selector selector]
                            [current-user-request-no-volume? #true])
               (do-typeset path.scrbl lang-name)))]
          [else (do-typeset path.scrbl lang-name)])))
