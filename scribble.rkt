#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(define-type Tag-Body (U String Generated-Tag (Pairof Any (Listof Any))))
(define-type Link-Tag (List Symbol Tag-Body))
(define-type Scribble-Message (-> Symbol String Any * Void))

(define-type Style-Name (U String Symbol False))
(define-type Style-Properties (Listof Any))

(define-type Element-Style (U Style-Name Style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 scribble/core
 [#:opaque Block block?]
 [#:opaque Content content?]

 [#:struct document-date ([text : String]) #:extra-constructor-name make-document-date #:type-name Document-Date]
 [#:struct document-version ([text : String]) #:extra-constructor-name make-document-version #:type-name Document-Version]
 [#:struct generated-tag () #:extra-constructor-name make-generated-tag #:type-name Generated-Tag]

 [#:struct style
  ([name : Style-Name]
   [properties : Style-Properties])
  #:extra-constructor-name make-style
  #:type-name Style]
 
 [#:struct part
  ([tag-prefix : (U False String HashTableTop)]
   [tags : (Listof Link-Tag)]
   [title-content : (Option (Listof Content))]
   [style : Style]
   [to-collect : (Listof Any)]
   [blocks : (Listof Block)]
   [parts : (Listof Part)])
  #:extra-constructor-name make-part
  #:type-name Part]

 [#:struct paragraph
  ([style : Style]
   [content : (Listof Content)])
  #:extra-constructor-name make-paragraph
  #:type-name Paragraph]
 
 [#:struct compound-paragraph
  ([style : Style]
   [blocks : (Listof Block)])
  #:extra-constructor-name make-compound-paragraph
  #:type-name Compound-Paragraph]

 [#:struct nested-flow
  ([style : Style]
   [blocks : (Listof Block)])
  #:extra-constructor-name make-nested-flow
  #:type-name Nested-Flow]

 [#:struct delayed-block
  ([resolve : Procedure])
  #:extra-constructor-name make-delayed-block
  #:type-name Delayed-Block])

(require/typed/provide
 scribble/html-properties
 [#:struct body-id ([value : String]) #:extra-constructor-name make-body-id #:type-name Body-Id]
 [#:struct hover-property ([text : String]) #:extra-constructor-name make-hover-property #:type-name Hover-Property])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 scribble/core
 [content->string (-> (Listof Content) String)])
