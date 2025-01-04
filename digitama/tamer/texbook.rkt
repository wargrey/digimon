#lang at-exp racket/base

(provide (all-defined-out))
(provide (rename-out [texbook-command $tex]))

(require scribble/core)
(require scribble/manual)
(require scribble/latex-properties)

(require racket/format)
(require racket/string)

(require "../../tongue.rkt")
(require "backend.rkt")
(require "misc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define phantomsection-style (make-style "phantomsection" null))
(define parbox-style (make-style "parbox" null)) ; for tabular to wrap line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define texbook-prefab-name
  (lambda [TeX]
    (case (string->symbol (string-downcase TeX))
      [(tex) (texbook-command "TeX")]
      [(latex) (texbook-command "LaTeX")]
      [(latexe) (texbook-command "LaTeXe")]
      [else (texbook-command TeX)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define texbook-command ;; \cmd[opt-args]{args}{body}{extra-args}
  (let ([cmd0base (make-hash)])
    (lambda [cmd #:args [maybe-cargs #false] #:extra-args [maybe-eargs #false] #:opt-args [maybe-oargs #false] #:tex-only? [tex-only? #true] . body]
      (define cmdname (~a cmd))
      (define cmd-args (and maybe-cargs (not (null? maybe-cargs)) (if (list? maybe-cargs) maybe-cargs (list maybe-cargs))))
      (define ext-args (and maybe-eargs (not (null? maybe-eargs)) (if (list? maybe-eargs) maybe-eargs (list maybe-eargs))))
      (define opt-args (and maybe-oargs (not (null? maybe-oargs)) (if (list? maybe-oargs) maybe-oargs (list maybe-oargs))))

      (define other-args
        (append (if ext-args (list (command-extras (map ~a ext-args))) null)
                (if opt-args (list (command-optional (map texbook-datum->option-argument opt-args))) null)))

      (define tex-style
        (cond [(null? other-args) cmdname]
              [else (make-style cmdname other-args)]))

      (define tex-elem
        (if (or cmd-args (style? tex-style) (pair? body))
            (cond [(not cmd-args) (make-element tex-style (map handbook-content-filter body))]
                  [else (make-multiarg-element tex-style (map handbook-content-filter (append cmd-args body)))])
            (hash-ref! cmd0base tex-style (λ [] (make-element tex-style null)))))

      (cond [(not tex-only?) tex-elem]
            [else (make-traverse-element
                   (λ [get set!]
                     (if (handbook-latex-renderer? get)
                         tex-elem null)))]))))

(define texbook-command-block ;; \cmd{args}{body}{extra-args}
  (let ([cmd0base (make-hash)])
    (lambda [cmd #:args [maybe-cargs #false] #:extra-args [maybe-eargs #false] #:fallback-block [fallback-block #false] . body]
      (define cmdname (~a cmd))
      (define cmd-args (and maybe-cargs (not (null? maybe-cargs)) (if (list? maybe-cargs) maybe-cargs (list maybe-cargs))))
      (define ext-args (and maybe-eargs (not (null? maybe-eargs)) (if (list? maybe-eargs) maybe-eargs (list maybe-eargs))))
      
      (define other-args? (or (pair? cmd-args) (pair? ext-args)))

      (define tex-style
        (make-style cmdname
                    (list (if (or (pair? cmd-args) (pair? ext-args))
                              'multicommand
                              'command))))

      (define tex-block
        (if (or other-args? (pair? body))
            (cond [(not other-args?) (make-nested-flow tex-style (map handbook-block-filter body))]
                  [else (make-nested-flow tex-style (append (if (pair? cmd-args) (map handbook-block-filter cmd-args) null)
                                                            (list (make-nested-flow plain (map handbook-block-filter body)))
                                                            (if (pair? ext-args) (map handbook-block-filter ext-args) null)))])
            (hash-ref! cmd0base cmdname (λ [] (make-paragraph (make-style cmdname null) null)))))

      (cond [(not fallback-block) tex-block]
            [else (make-traverse-block
                   (λ [get set!]
                     (if (handbook-latex-renderer? get)
                         tex-block fallback-block)))]))))

(define texbook-environment ;; \begin{cmd}body\end{cmd}
  (let ([cmd0base (make-hash)])
    (lambda [cmd . body]
      (define cmdname (~a cmd))

      (if (pair? body)
          (make-nested-flow (make-style cmdname null) (map handbook-block-filter body))
          (hash-ref! cmd0base cmdname (λ [] (make-nested-flow (make-style cmdname null) null)))))))

;; Scribble ignores blocks and elements inbetween included subparts
(define texbook-command-part
  (lambda [#:part? [part? #true] #:tag [tag #false] cmd contents]
    (make-part #false
                `((part ,(or tag cmd)))
                (cond [(pair? contents) contents]
                      [(not part?) null]
                      [else (list (speak (string->symbol cmd) #:dialect 'tamer))])
               (make-style #false (if (not part?) '(unnumbered hidden toc-hidden) '(unnumbered grouper)))
               null
               (list (texbook-command-block cmd))
               null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define texbook-frontmatter
  (lambda []
    (texbook-command "frontmatter")))

(define texbook-mainmatter
  (lambda []
    (texbook-command "mainmatter")))

(define texbook-appendix
  (lambda [#:part? [part? #true] #:tag [tag #false] . contents]
    (texbook-command-part #:part? part? #:tag (or tag "tamer-appendix")
                          "appendix" contents)))

; make following sections unnumbered,
; note that unnumbered sections might be hard to be located in resulting PDF
(define texbook-backmatter
  (lambda [#:part? [part? #false] #:tag [tag #false] . contents]
    (texbook-command-part #:part? part? #:tag (or tag "tamer-backmatter")
                          "backmatter" contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define $tex:phantomsection
  (lambda []
    (texbook-command "phantomsection")))

(define $tex:newcounter
  (lambda [name]
    (texbook-command "newcounter" name)))

(define $tex:refstepcounter
  (lambda [counter]
    (texbook-command "refstepcounter" counter)))

(define $tex:setcounter
  (lambda [counter value]
    (texbook-command "setcounter" #:args (list counter value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define texbook-datum->option-argument
  (lambda [arg]
    (cond [(list? arg) (string-join (map texbook-datum->option-argument arg) ",")]
          [(pair? arg) (format "~a=~a" (car arg) (cdr arg))]
          [else (~a arg)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require racket/class)
  (require racket/port)
  
  (require scribble/base-render)
  (require scribble/latex-render)

  (define scribble->tex-string
    (lambda raw
      (define documentclass
        (make-latex-defaults+replacements
         #"\\documentclass{article}" #"" null
         (hash "scribble-load-replace.tex" #"")))

      (define blocks
        (for/list ([c (in-list raw)])
          (if (block? c) c (para c))))
      
      (define docs (list (make-part #false null #false (make-style #false (list documentclass)) null blocks null)))
      (define names (list #false))
      
      (define tex (new (render-mixin render%) [dest-dir #false]))
      
      (define t:metrics (send tex traverse docs names))
      (define c:metrics (send tex collect  docs names t:metrics))
      (define r:metrics (send tex resolve  docs names c:metrics))

      (call-with-input-string (car (send tex render docs names r:metrics))
        (λ [/dev/texin]
          (let display-tex ([okay? #false])
            (define line (read-line /dev/texin))

            (when (string? line)
              (if (not okay?)
                  (if (regexp-match? #px"^\\\\preDoc" line)
                      (display-tex #true)
                      (display-tex #false))
                  (unless (regexp-match? #px"^\\\\postDoc" line)
                    (displayln line)
                    (display-tex #true)))))))))
  
  (scribble->tex-string
   @texbook-command['multicols #:opt-args '|| #:args "2" #:extra-args 'extra]{@emph{emph} @texbook-prefab-name{TeX}}
   @texbook-command['lstlistings #:opt-args (list (list 'mathescape (cons 'language 'C++)))]{int main();}
   @para{@texbook-command['multicols #:opt-args '(opt1 opt2) #:args "2" #:extra-args 'extra]{@emph{emph} @tt{body}}}
   @texbook-command-block['multicols #:args "%" #:extra-args "extra"]{@emph{emph} @tt{body}}
   ($tex:phantomsection)
   ($tex:refstepcounter 'page)
   ($tex:setcounter 'page 1)
   (texbook-environment "multicols" "2" "haw-haw")))
