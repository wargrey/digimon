#lang typed/racket

(provide (all-defined-out))
(provide (all-from-out "digitama/sugar.rkt"))

(require "digitama/sugar.rkt")
  
(require (for-syntax racket/syntax))
(require (for-syntax racket/string))

(define-syntax (require/typed/provide/enums stx)
  (syntax-case stx []
    [(_ enums ...)
     (with-syntax ([([_etypes define/c->rackets define/racket->cs] ...)
                    (for/list ([elem (in-list (syntax->list #'(enums ...)))])
                      (syntax-case elem []
                        [enum (with-syntax ([_etype (format-id #'enum "_~a" (syntax-e #'enum))]
                                            [c->racket (format-id #'enum "~a-c->racket" (syntax-e #'enum))]
                                            [racket->c (format-id #'enum "~a-racket->c" (syntax-e #'enum))])
                                #'[_etype
                                   (define (c->racket [c : Integer]) : Symbol (cast ((ctype-c->scheme _etype) c) Symbol))
                                   (define (racket->c [r : Symbol]) : Integer (cast ((ctype-scheme->c _etype) r) Integer))])]))])
       #'(begin (require/typed/provide (submod "..")
                                       [_etypes CType]
                                       ...)
                define/c->rackets ...
                define/racket->cs ...))]))

(define-syntax (require/typed/provide/bitmasks stx)
  (syntax-case stx []
    [(_ modpath bitmasks ...)
     (with-syntax ([([_btypes define/c->rackets define/racket->cs] ...)
                    (for/list ([elem (in-list (syntax->list #'(bitmasks ...)))])
                      (syntax-case elem []
                        [bitmask (with-syntax ([_btype (format-id #'bitmask "_~a" (syntax-e #'bitmask))]
                                               [c->racket (format-id #'bitmask "~a-c->racket" (syntax-e #'bitmask))]
                                               [racket->c (format-id #'bitmask "~a-racket->c" (syntax-e #'bitmask))])
                                   #'[_btype
                                      (define (c->racket [c : Natural]) : (Listof Symbol)
                                        (cast ((ctype-c->scheme _btype) c) (Listof Symbol)))
                                      (define (racket->c [r : (Listof Symbol)]) : Natural
                                        (cast ((ctype-scheme->c _btype) r) Natural))])]))])
       #'(begin (require/typed/provide modpath
                                       [_btypes CType]
                                       ...)
                define/c->rackets ...
                define/racket->cs ...))]))
  
(define-syntax (require/typed/provide/pointers stx)
  (syntax-case stx []
    [(_ modpath Pointers/Opaques ...)
     (with-syntax ([([opaques ctypes ctype/nulls definetypes] ...)
                    (for/list ([Pointer/Opqaques (in-list (syntax->list #'(Pointers/Opaques ...)))])
                      (syntax-case Pointer/Opqaques []
                        [(Pointer pointer?)
                         (with-syntax ([Pointer/Null (format-id #'Pointer "~a/Null" (syntax-e #'Pointer))]
                                       [_ctype (format-id #'pointer? "_~a" (string-trim (symbol->string (syntax-e #'pointer?)) #px"\\?$"))]
                                       [_ctype/null (format-id #'pointer? "_~a/null" (string-trim (symbol->string (syntax-e #'pointer?)) #px"\\?$"))])
                           #'[[#:opaque Pointer pointer?]
                              [_ctype CType]
                              [_ctype/null CType]
                              (define-type Pointer/Null (Option Pointer))])]
                        [Pointer
                         (with-syntax ([pointer? (format-id #'Pointer "~a?" (string-downcase (symbol->string (syntax-e #'Pointer))))]
                                       [Pointer/Null (format-id #'Pointer "~a/Null" (syntax-e #'Pointer))]
                                       [_ctype (format-id #'pointer? "_~a" (string-downcase (symbol->string (syntax-e #'Pointer))))]
                                       [_ctype/null (format-id #'pointer? "_~a/null" (string-downcase (symbol->string (syntax-e #'Pointer))))])
                           #'[[#:opaque Pointer pointer?]
                              [_ctype CType]
                              [_ctype/null CType]
                              (define-type Pointer/Null (Option Pointer))])]))])
       #'(begin (require/typed/provide modpath
                                       opaques ...
                                       ctypes ...
                                       ctype/nulls ...)
                definetypes ...))]))
  
(require/typed/provide
 "digitama/ffi.rkt"
 [#:opaque CPointer/Null cpointer?]
 [#:opaque CPointer/GCable cpointer-gcable?]
 [#:opaque CType ctype?]
 [#:opaque Array array?]
 [ctype-basetype (-> CType (U False Symbol CType (Listof CType)))]
 [ctype-c->scheme (-> CType (-> Any Any))]
 [ctype-scheme->c (-> CType (-> Any Any))]
 [array-type (-> Array CType)]
 [array-length (-> Array Index)]
 [in-array (->* [Array] [Positive-Index (Option Positive-Index) Positive-Integer] (Sequenceof Any))]
 [array-ref (-> Array Index * Any)]
 [array-set! (case-> [Array Index Any -> Void]
                     [Array Index Index Any -> Void]
                     [Array Index Index Index Any -> Void])])

(require/typed/provide/batch
 "digitama/ffi.rkt"
 (id: _uintmax _byte _sint32 _string*/utf-8 _void
      _int8 _uint8 _int16 _uint16 _int32 _uint32 _int64 _uint64
      _fixint _ufixint _fixnum _ufixnum _float _double _longdouble
      _double* _bool _stdbool _string/ucs-4 _string/utf-16 _path
      _symbol _pointer _gcpointer _scheme _fpointer _racket _ssize
      _size _uword _word _sbyte _string*/latin-1 _bytes/eof _file
      _intmax _ptrdiff _sintptr _intptr _sllong _ullong _llong
      _slong _ulong _long _sint _uint _int _sshort _ushort _short
      _ubyte _sint64 _sint16 _sint8 _string*/locale _string/latin-1
      _string/locale _string/utf-8 _uintptr _sword)
 CType)

(require/typed/provide
 "digitama/ffi.rkt"
 [#:opaque CPointer cvoid*?]
 [c-extern (-> (U String Bytes Symbol) CType Any)])
