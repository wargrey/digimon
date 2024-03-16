#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/list)

(require "../../filesystem.rkt")
(require "../../token.rkt")
(require "../../string.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct problem-testcase
  ([input : Bytes]
   [output : Bytes])
  #:type-name Problem-Testcase
  #:constructor-name make-problem-testcase
  #:transparent)

(struct problem-info
  ([description : (Listof String)]
   [arguments : (Listof String)]
   [result : (Option String)]
   [testcases : (Listof Problem-Testcase)])
  #:constructor-name make-problem-info
  #:type-name Problem-Info
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-cpp-problem-info : (-> Path (Option Problem-Info))
  (lambda [main.cpp]
    (call-with-input-file* main.cpp
      (λ [[/dev/stdin : Input-Port]]
        (syn-token-skip-whitespace /dev/stdin)

        (and (regexp-match #px"^[/][*][*]\\s*" /dev/stdin)
             (let*-values ([(body) (read-cpp-problem-description /dev/stdin)]
                           [(args result rest) (problem-description-split-input-output body)]
                           [(testcases description) (problem-description-split-testcase rest)])
               (make-problem-info description args result testcases)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-cpp-problem-description : (-> Input-Port (Listof String))
  (lambda [/dev/stdin]
    (let read-desc ([senil : (Listof String) null])
      (define self (read-line /dev/stdin))

      (if (string? self)
          (if (not (regexp-match #px"[*]+[/].*" self))
              (read-desc (cons (string-trim self #px"(^\\s*[*]\\s)|(\\s*$)") senil))
              (let ([last (string-trim self #px"(^\\s*[*]?\\s*)|(\\s*[*]+[/].*$)")])
                (if (string-blank? last)
                    (reverse senil)
                    (reverse (cons last senil)))))
          (reverse senil)))))

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
          (values (reverse sgra)
                  (if (string-blank? result) #false result)
                  (reverse rest))))))

(define problem-description-split-testcase : (-> (Listof String) (Values (Listof Problem-Testcase) (Listof String)))
  (lambda [lines]
    (let split ([ids : (Listof Index) (indexes-where lines problem-testcase-predicate)]
                [src : (Listof String) lines]
                [sct : (Listof Problem-Testcase) null]
                [body : (Option (Listof String)) #false]
                [offset : Index 0])

      (if (pair? ids)
          (let*-values ([(pos ids--) (values (car ids) (cdr ids))]
                        [(head tail) (split-at src (- pos offset))])
            (cond [(not body) (split ids-- tail sct head pos)]
                  [else (split ids-- tail (cons (problem-description->testcase (cdr head)) sct) body pos)]))
          (cond [(not body) (values null (string-list-normalize-empties src))]
                [(null? src) (values (reverse sct) (string-list-normalize-empties body))]
                [else (values (reverse (cons (problem-description->testcase (cdr src)) sct))
                              (string-list-normalize-empties body))])))))

(define problem-description->testcase : (-> (Listof String) Problem-Testcase)
  (lambda [lines]
    (let partition ([src : (Listof String) lines]
                    [is : (Listof String) null]
                    [os : (Listof String) null]
                    [dir : (Option Symbol) #false])
      (cond [(pair? src)
             (let-values ([(self rest) (values (car src) (cdr src))])
               (cond [(string-prefix? self "input") (partition rest is os 'input)]
                     [(string-prefix? self "output") (partition rest is os 'output)]
                     [(eq? dir 'input) (partition rest (cons self is) os dir)]
                     [(eq? dir 'output) (partition rest is (cons self os) dir)]
                     [else (partition rest is os dir)]))]
            [(or (pair? is) (pair? os))
             (make-problem-testcase (problem-testcase-join (reverse is)) (problem-testcase-join (reverse os)))]
            [else (make-problem-testcase (problem-testcase-join lines) #"")]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define problem-testcase-join : (-> (Listof String) Bytes)
  (lambda [lines]
    (string->bytes/utf-8 (string-join (string-list-normalize-empties lines) "\n"))))

(define problem-testcase-predicate : (-> String Any)
  (lambda [self]
    (regexp-match #px"^(@|\\\\)(test)" self)))
