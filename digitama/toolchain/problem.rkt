#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "../../filesystem.rkt")
(require "../../string.rkt")
(require "../../dtrace.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct problem-spec
  ([brief : String]
   [input : String]
   [output : (Option (Listof (U String Regexp)))]
   [timeout : (Option Natural)])
  #:type-name Problem-Spec
  #:constructor-name make-problem-spec
  #:transparent)

(struct problem-info
  ([title : (Option String)]
   [description : (Listof String)]
   [arguments : (Listof String)]
   [result : (Option String)]
   [specs : (Listof Problem-Spec)]
   [attachment : Any])
  #:constructor-name make-problem-info
  #:type-name Problem-Info
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define problem-topic-name : Symbol 'spec)

(define dtrace-problem-info : (-> Problem-Info Void)
  (lambda [pinfo]
    (when (pair? (problem-info-description pinfo))
      (dtrace-debug "@~a:" 'problem)
      (for ([brief (in-list (problem-info-description pinfo))])
        (dtrace-note "~a" brief)))
    (when (pair? (problem-info-arguments pinfo))
      (dtrace-debug "@~a:" 'input)
      (for ([input (in-list (problem-info-arguments pinfo))])
        (dtrace-note "~a" input)))
    (when (problem-info-result pinfo)
      (dtrace-debug "@~a:" 'output)
      (when (problem-info-result pinfo)
        (dtrace-note "~a" (problem-info-result pinfo))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define problem-description-split-input-output : (-> (Listof String) (Values (Listof String) (Option String) (Listof String)))
  (lambda [lines]
    (let split ([src : (Listof String) lines]
                [sgra : (Listof String) null]
                [result : String ""]
                [rest : (Listof String) null])
      (if (pair? src)
          (let-values ([(self tail) (values (car src) (cdr src))])
            (cond [(regexp-match #px"^(@|\\\\)(arg|param)" self) (split tail (cons (string-trim self #px"(^.\\w+\\s*)|(\\s*$)") sgra) result rest)]
                  [(regexp-match #px"^(@|\\\\)(returns?|result)" self) (split tail sgra (string-trim self #px"(^.\\w+\\s*)|(\\s*$)") rest)]
                  [else (split tail sgra result (cons self rest))]))
          (values (string-list-normalize-blanks (reverse sgra))
                  (if (string-blank? result) #false result)
                  (reverse rest))))))

(define problem-description-split-spec : (-> Path (Listof String) (Values (Listof Problem-Spec) (Listof String)))
  (lambda [main.cpp lines]
    (let split ([ids : (Listof Index) (indexes-where lines problem-spec-predicate)]
                [src : (Listof String) lines]
                [ss : (Listof Problem-Spec) null]
                [body : (Option (Listof String)) #false]
                [offset : Index 0])
      (define idx : Nonnegative-Fixnum (add1 (length ss)))
      
      (if (pair? ids)
          (let*-values ([(pos ids--) (values (car ids) (cdr ids))]
                        [(head tail) (split-at src (- pos offset))])
            (cond [(not body) (split ids-- tail ss head pos)]
                  [(null? head) (split ids-- tail ss head pos)]
                  [else (split ids-- tail (cons (problem-description->spec main.cpp (car head) (cdr head) idx) ss) body pos)]))
          (cond [(not body) (values null (string-list-normalize-blanks src))]
                [(null? src) (values (reverse ss) (string-list-normalize-blanks body))]
                [else (values (reverse (cons (problem-description->spec main.cpp (car src) (cdr src) idx) ss))
                              (string-list-normalize-blanks body))])))))

(define problem-description->spec : (-> Path String (Listof String) Nonnegative-Fixnum Problem-Spec)
  (lambda [main.cpp head lines idx]
    (define brief : String (problem-spec-description head idx))

    (dtrace-debug #:topic problem-topic-name "parsing ~a" brief)
    
    (let parse ([src : (Listof String) lines]
                [is : (Listof String) null]
                [os : (Listof (Listof String)) null]
                [timeout : (Option Natural) #false]
                [ydob : (Listof String) null]
                [dir : (Option Symbol) #false]
                [defout? : Boolean #false])
      (cond [(pair? src)
             (let-values ([(self rest) (values (car src) (cdr src))])
               (cond [(regexp-match? "^(input:?|[>]{2})" self) (parse rest (problem-input-prepare is self) os timeout ydob 'input defout?)]
                     [(regexp-match? "^(output:?|[<]{2})" self) (parse rest is (problem-output-prepare os self) timeout ydob 'output #true)]
                     [(regexp-match? #px"^(@|\\\\)(file|include)\\s+" self) (parse (problem-spec-include main.cpp self rest) is os timeout ydob dir defout?)]
                     [(regexp-match? "^(timeout:?)" self) (parse rest is os (problem-spec-extract-timeout self) ydob dir defout?)]
                     [(eq? dir 'input) (parse rest (cons self is) os timeout ydob dir defout?)]
                     [(eq? dir 'output) (parse rest is (cons (cons self (car os)) (cdr os)) timeout ydob dir defout?)]
                     [else (parse rest is os timeout (cons self ydob) dir defout?)]))]

            [(not dir) ; no (#:input ior #:output)
             (let-values ([(is os) (splitf-at (string-list-trim-blanks (reverse ydob)) string!blank?)])
               (make-problem-spec brief (problem-spec-join is)
                                  (let ([output (problem-spec-join os)])
                                    (and (not (string-blank? output)) (list output)))
                                  timeout))]

            [else
             (make-problem-spec brief
                                (problem-spec-join (reverse is))
                                (and defout? (map problem-spec-rjoin (reverse os)))
                                timeout)]))))

(define problem-spec-description : (-> String Nonnegative-Fixnum String)
  (lambda [headline idx]
    (define maybe-brief (substring headline 5))

    (if (string-blank? maybe-brief)
        (format "case ~a" idx)
        (format "case ~a: ~a" idx (string-trim maybe-brief)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define problem-spec-one-line-datum : (-> String (Option String))
  (lambda [self]
    (define content : String (string-trim self #px"(^\\S+\\s*)|(\\s*$)"))

    (if (string-blank? content) #false content)))

(define problem-spec-include : (-> Path String (Listof String) (Listof String))
  (lambda [src self rest]
    (define file : (Option String) (problem-spec-one-line-datum self))

    (if (string? file)
        (let ([path (build-path (assert (path-only src)) file)])
          (if (file-exists? path)
              (append (file->lines path) rest)
              (and (dtrace-warning #:topic problem-topic-name "ignored `~a` due to not found" file)
                   rest)))
        (and (dtrace-warning #:topic problem-topic-name "invalid ~a" self)
             rest))))

(define problem-spec-join : (-> (Listof String) String)
  (lambda [lines]
    (string-join (string-list-normalize-blanks lines) "\n")))

(define problem-spec-rjoin : (-> (Listof String) String)
  (lambda [lines]
    (string-join (string-list-normalize-blanks (reverse lines)) "\n")))

(define problem-spec-predicate : (-> String Any)
  (lambda [self]
    (regexp-match #px"^(@|\\\\)(test)" self)))

(define problem-spec-extract-timeout : (-> String (Option Natural))
  (lambda [self]
    (define content : (Option String) (problem-spec-one-line-datum self))

    (or (and content
             (let ([to (string->number content)])
               (cond [(exact-nonnegative-integer? to) to]
                     [else #false])))
        (and (dtrace-warning #:topic problem-topic-name "invalid ~a" self)
             #false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define problem-input-prepare : (-> (Listof String) String (Listof String))
  (lambda [is self]
    (define one-line-datum : (Option String) (problem-spec-one-line-datum self))

    (if (not one-line-datum) is (cons one-line-datum is))))

(define problem-output-prepare : (-> (Listof (Listof String)) String (Listof (Listof String)))
  (lambda [os self]
    (define one-line-datum : (Option String) (problem-spec-one-line-datum self))

    (if (not one-line-datum)
        (cond [(null? os) (list null)]
              [else (cons null os)])
        (cond [(null? os) (list (list one-line-datum))]
              [else (cons (list one-line-datum) os)]))))
