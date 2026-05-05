#lang scribble/manual

@(require digimon/tamer)

@handbook-title{Tabular}

@tabular[#:sep @hspace[1]
         (list (list "soup" "gazpacho")
               (list "soup" "tonjiru"))]
 
@tabular[#:pad '(1 0)
         #:column-properties '(border)
         (list (list "gazpacho" "cold")
               (list "tonjiru"  "hot"))]
 
@tabular[#:style 'boxed
         #:column-properties '(left right)
         #:row-properties '(bottom-border ())
         (list (list @bold{recipe}   @bold{vegetable})
               (list "caldo verde"   "kale")
               (list "kinpira gobō"  "burdock")
               (list "makizushi"     'cont))]

@tamer-table!['asset "Education Systems"]{
 @(list @bold{Package}  @bold{Lang}         @bold{UI})
 @(list @elem{JrLab}    @elem{C++}          @elem{C++})
 @(list @elem{wizarmon} @elem{Typed Racket} @elem{CLI, VSCode Button})
 @(list @elem{spec}     @elem{Typed Racket} @elem{CLI, VSCode Button})
 @(list @elem{geofun}   @elem{Typed Racket} @elem{Typed Racket, DSL})
 @(list @elem{tamer}    @elem{Racket}       @elem{Scribble, Racket})}
