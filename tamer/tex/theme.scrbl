#lang scribble/manual

@(require digimon/tamer)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define token-slice
   (lambda [src size]
     (let slice ([ts src]
                 [tokens null])
       (cond [(> (length ts) size)
              (let-values ([(head tail) (split-at ts size)])
                (slice tail (cons (add-between head " ") tokens)))]
             [(pair? ts) (slice null (cons (add-between ts " ") tokens))]
             [else (map para (reverse tokens))]))))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title{Syntax Colors}

@$tex:newcounter:algorithm[]

@(define rkts
   (let* ([ns (module->namespace 'scribble/racket)]
          [ids (sort (namespace-mapped-symbols ns) symbol<?)])
     (filter element?
             (for/list ([id (in-list ids)])
               (define datum (namespace-variable-value id #false (λ [] #false) ns))
               (and (promise? datum)
                    (let ([s (force datum)])
                      (and (style? s)
                           (elem #:style s (format "~a" (style-name s))))))))))

@(define tamers
   (let* ([ns (module->namespace 'digimon/digitama/tamer/theme)]
          [ids (sort (namespace-mapped-symbols ns) symbol<?)])
     (filter element?
             (for/list ([id (in-list ids)])
               (and (string-prefix? (symbol->string id) ":")
                    (let ([::s (namespace-variable-value id #false (λ [] #false) ns)])
                      (and (procedure? ::s)
                           (arity-includes? (procedure-arity ::s) 0)
                           (let ([body (string-append "tamer"
                                                      (string-replace (string-titlecase
                                                                       (string-replace (symbol->string (object-name ::s))
                                                                                       ":" " "))
                                                                      " " ""))])
                             (::s body)))))))))

@handbook-scenario{Gallery}

@(token-slice tamers 5)
@linebreak[]
@(token-slice rkts 8)

@handbook-scenario{C++ Code}

@algo-pseudocode[
 #:tag 'bullet_color @list{实现“子弹变色”功能}
 @list['定义红色分量开关]{@(elem @:type{uint32_t}@~ @:var{r_value}@~ @:operator{=}@~ @:number{0xFF0000}@:meta{;}@~
                         @:comment{// 把@${(F\text{\hspace{-3pt}}F)_{256}}放在红色位})}
 @list['定义绿色分量开关]{@(elem @:type{uint32_t}@~ @:var{g_value}@~ @:operator{=}@~ @:number{0x00FF00}@:meta{;}@~
                         @:comment{// 把@${(F\text{\hspace{-3pt}}F)_{256}}放在绿色位})}
 @list['定义蓝色分量开关]{@(elem @:type{uint32_t}@~ @:var{b_value}@~ @:operator{=}@~ @:number{0x0000FF}@:meta{;}@~
                         @:comment{// 把@${(F\text{\hspace{-3pt}}F)_{256}}放在蓝色位})}
 @list[]
 @list[#false]{@(elem @:type{void}@~ @:namespace{JrLab}@:pn{::}@:class{BullseyeWorld}@:pn{::}@:method{bullet_colorize}@:pn["() { "])}
 @list['红色分量值]{@(elem @hspace[4]@:type{RGBA}@~ @:var{r}@~ @:operator{=}@~ @:val{this}@:operator{->}@:var{r_value}@:meta{;}@~
                         @:comment{// @${0xFF0000} or @${0}})}
 @list['绿色分量值]{@(elem @hspace[4]@:type{RGBA}@~ @:var{g}@~ @:operator{=}@~ @:val{this}@:operator{->}@:var{g_value}@:meta{;}@~
                         @:comment{// @${0x00FF00} or @${0}})}
 @list['蓝色分量值]{@(elem @hspace[4]@:type{RGBA}@~ @:var{b}@~ @:operator{=}@~ @:val{this}@:operator{->}@:var{b_value}@:meta{;}@~
                         @:comment{// @${0x0000FF} or @${0}})}
 @list['加色混合]{@(elem @hspace[4]@:type{RGBA}@~ @:var{color}@~ @:operator{=}@~ @:var{r}@~ @:operator{+}@~ @:var{g}@~ @:operator{+}@~ @:var{b}@:meta{;})}
 @list[]
 @list['设置子弹颜色]{@(elem @hspace[4]@:val{this}@:operator{->}@:var{bullet}@:operator{->}@:method{set_brush_color}@:pn{(}@:var{color}@:pn{)}@:meta{;})}
 @list[#false]{@(elem @:pn["}"])}
 ]

@algo-pseudocode[
 #:tag 'scanline @list{用@tt{C++}描述“扫描线算法”}
 @list[#false]{@(elem @:macro{#include}@~ @:string{<iostream>})}
 @list[]
 @list[@${pos(n, L) = n - L + 1}]{@(elem @:type{int}@~ @:function{pos}@:pn{(}@:type{int}@~ @:var{n}@:meta[","]@~ @:type{int}@~ @:var{L}@:pn[") { "]
                                         @:keyword{return}@~ @:var{n}@~ @:operator{-}@~ @:var{L}@~ @:operator{+}@~ @:number{1}@:meta{;}@~ @:pn["}"])}
 @list[@${span(L) = 2L - 1}]{@(elem @:type{int}@~ @:function{span}@:pn{(}@:type{int}@~ @:var{L}@:pn[") { "]
                                    @:keyword{return}@~ @:number{2}@~ @:operator{*}@~ @:var{L}@~ @:operator{-}@~ @:number{1}@:meta{;}@~ @:pn["}"])}
 @list['定义桩函数]{@(elem @:type{void}@~ @:function{display_line}@:pn{(}@:type{int}@~ @:var{n}@:meta[","]@~ @:type{int}@~ @:var{L}@:pn[") { }"])}
 @list[]
 @list[#false]{@(elem @:type{int}@~ @:function{main}@:pn{(}@:type{int}@:meta[","]@~ @:type{char}@:operator{**}@:pn[") {"])}
 @list['金字塔整数层高]{@(elem @hspace[4]@:type{int}@~ @:var{n}@:meta{;})}
 @list['读取层高]{@(elem @hspace[4]@:namespace{std}@:pn{::}@:sym{cin}@~ @:operator{>>}@~ @:var{n}@:meta{;})}
 @list[]
 @list['对于1至n的每一行L]{@(elem @hspace[4]@:keyword{for}@~ @:pn{(}@:type{int}@~ @:var{L}@~ @:operator{=}@~ @:number{1}@:meta{;}@~
                       @:var{L}@~ @:operator{<=}@~ @:var{n}@:meta{;}@~
                       @:var{L}@~ @:operator{++)}@~ @:pn["{"])}
 @list['输出该行字符]{@(elem @hspace[8]@:function{display_line}@:pn{(}@:var{n}@:meta{,}@~ @:var{L}@:pn{)}@:meta{;})}
 @list[#false]{@(elem @hspace[4]@:pn["}"])}
 @list[]
 @list[#false]{@(elem @hspace[4]@:keyword{return}@~ @:number{0}@:meta{;})}
 @list[#false]{@(elem @:pn["}"])}
 ]
