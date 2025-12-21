#lang typed/racket/base

(provide (all-defined-out))

(require racket/vector)

(require digimon/token)
(require digimon/string)
(require digimon/spec)

(require digimon/digitama/spec/dsl)
(require digimon/digitama/spec/behavior)

(require "problem.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct python-problem-attachment
  ([doctests : (Listof String)])
  #:constructor-name make-python-problem-attachment
  #:type-name Python-Problem-Attachment
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-python-problem-info : (-> Path (Option Problem-Info))
  (lambda [mod.py]
    (call-with-input-file* mod.py
      (λ [[/dev/stdin : Input-Port]]
        (syn-token-skip-shebang-line /dev/stdin)
        
        (let try-next-docstring : (Option Problem-Info) ()
          (syn-token-skip-whitespace /dev/stdin)

          (define px:docstring-end : (Option Regexp)
            (cond [(regexp-try-match #px"^\"{3}" /dev/stdin) #px"\"{3}"]
                  [(regexp-try-match #px"^'{3}" /dev/stdin) #px"'{3}"]
                  [else #false]))
          
          (cond [(or px:docstring-end)
                 (let*-values ([(head continue?) (read-python-problem-title /dev/stdin px:docstring-end)]
                               [(body doctests) (if (not continue?) (values null null) (read-python-problem-description /dev/stdin px:docstring-end))]
                               [(args results rest) (problem-description-split-input-output body)]
                               [(specs description) (problem-description-split-spec mod.py rest)])
                   (if (or (pair? specs) (pair? doctests))
                       (make-problem-info head description args results specs
                                          (make-python-problem-attachment doctests))
                       (try-next-docstring)))]
                [(regexp-try-match #px"^\\s*#" /dev/stdin)
                 (read-line /dev/stdin 'any)
                 (try-next-docstring)]
                [else #false]))))))

(define python-problem->feature : (-> Problem-Info Path String (Vectorof String) (-> Problem-Spec Spec-Exec.Cfg) Boolean Spec-Feature)
  (lambda [problem-info python mod.py shell-argv spec->config verbose?]
    (describe ["~a" (or (problem-info-title problem-info) mod.py)]
      #:do (for/spec ([t (in-list (problem-info-specs problem-info))])
             (define-values (usr-args result) (values (problem-spec-input t) (problem-spec-output t)))
             (define brief (problem-spec-brief t))
             (define timeout (or (problem-spec-timeout t) 0))
             (cond [(problem-spec-ignore? t)
                    (if (eq? (problem-spec-ignore? t) 'skip)
                        (if (problem-spec-reason t)
                            (it brief #:do (ignore "~a" (problem-spec-reason t)))
                            (it brief #:do (collapse "skipped test requires a reason")))
                        (if (problem-spec-reason t)
                            (it brief #:do (pending "~a" (problem-spec-reason t)))
                            (it brief #:do #;(pending))))]
                   [(and (string-blank? usr-args) (not result))
                    (it brief #:do #;(pending))]
                   [(list? result) ; a complete test spec overrides the doctests
                    (it brief #:do #:millisecond timeout
                      #:do (expect-stdout python
                                          (python-cmd-args mod.py (cond [(> (vector-length shell-argv) 0) shell-argv]
                                                                        [else (or (problem-spec-argv t) shell-argv)])
                                                           #false verbose?)
                                          usr-args result (spec->config t)))]
                   [else ; let doctest do its job
                    (it brief #:do #:millisecond timeout
                      #:do (let* ([attachment (assert (problem-info-attachment problem-info) python-problem-attachment?)]
                                  [doctest? (pair? (python-problem-attachment-doctests attachment))])
                             (expect-stdout python (python-cmd-args mod.py shell-argv doctest? verbose?)
                                            usr-args null (spec->config t))))])))))

(define python-cmd-args : (-> String (Vectorof String) Boolean Boolean (Vectorof String))
  (lambda [mod.py cmd-argv doctest? verbose?]
    (define pyargv : (Vectorof String) (vector "-X" "utf-8"))
    
    (cond [(not doctest?) (vector-append pyargv (vector mod.py) cmd-argv)]
          [(and verbose?) (vector-append pyargv (vector "-m" "doctest" "-v" mod.py) cmd-argv)]
          [else (vector-append pyargv (vector "-m" "doctest" mod.py) cmd-argv)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-python-problem-title : (-> Input-Port Regexp (Values (Option String) Boolean))
  (lambda [/dev/stdin px:docstring]
    (define self (read-line /dev/stdin 'any))
    
    (cond [(or (eof-object? self) (regexp-match? px:docstring self)) (values #false #false)]
          [(string-blank? self) (values #false #true)]
          [else (values (string-trim self) #true)])))

(define read-python-problem-description : (-> Input-Port Regexp (Values (Listof String) (Listof String)))
  (lambda [/dev/stdin px:docstring]
    (let read-desc ([senil : (Listof String) null]
                    [stsetcod : (Listof String) null]
                    [doctest? : Boolean #false])
      (define self (read-line /dev/stdin 'any))

      (if (string? self)
          (cond [(regexp-match? px:docstring self) (values (reverse senil) (reverse stsetcod))]
                [(regexp-match? #px"^\\s*[#]" self) (read-desc senil stsetcod doctest?)]
                [(regexp-match? #px"^\\s*[>]{3}\\s+\\S+" self) (read-desc senil (cons self stsetcod) #true)]
                [(or doctest?) (if (string-blank? self) (read-desc senil stsetcod #false) (read-desc senil (cons self stsetcod) doctest?))]
                [else (read-desc (cons self senil) stsetcod doctest?)])
          (values (reverse senil) (reverse stsetcod))))))
