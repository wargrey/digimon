#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require "image.rkt")
(require "quirk.rkt")

(require "../../../enumeration.rkt")
(require "../../../bitmask.rkt")
(require "../../../stdio.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type COFF-Symbol (U COFF-Standard-Symbol COFF-Auxiliary-Symbol))

(define #%pecoff-symbol-record-size : Positive-Byte 18)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* pecoff-machine-type #:+> PECoff-Machine-Type
  pecoff-machine->number number->pecoff-machine
  [Unknown #x0000]
  [AM33 #x01D3] [AMD64 #x8664] [ARM #x01C0] [ARM64 #xAA64] [ARMNT #x01C4]
  [EBC #x0EBC] [I386 #x014C] [IA64 #x0200] [M32R #x9041] [MIPS16 #x0266]
  [MIPSFPU #x0366] [MIPSFPU16 #x0466] [POWERPC #x01F0] [POWERPCFP #x01F1]
  [R4000 #x0166] [RISCV32 #x5032] [RISCV64 #x5064] [RISCV128 #x5128]
  [SH3 #x01A2] [SH3DSP #x01A3] [SH4 #x01A6] [SH5 #x01A8] [THUMB #x01C2]
  [WCEMIPSV2 #x0169])

(define-enumeration* pecoff-complex-datatype #:+> PECoff-Complex-DataType
  pecoff-complex-datatype->number number->pecoff-complex-datatype
  [null #x0] [pointer #x1] [function #x2] [array #x3])

(define-enumeration* pecoff-base-datatype #:+> PECoff-Base-DataType
  pecoff-base-datatype->number number->pecoff-base-datatype
  [null #x0] [void #x1] [char #x2] [short #x3] [int #x4] [long #x5]
  [float #x6] [double #x7] [struct #x8] [union #x9] [enum #xA] [moe #xB]
  [byte #xC] [word #xD] [uint #xE] [dword #xF])

(define-enumeration* pecoff-storage-class #:+> PECoff-Storage-Class
  pecoff-storage-class->number number->pecoff-storage-class
  [null #x0] [automatic #x1] [external #x2] [static #x3] [register #x4]
  [external-def #x5] [label #x6] [undefined-label #x7] [member-of-struct #x8]
  [argument #x9] [struct-tag #xA] [member-of-union #xB] [union-tag #xC]
  [type-definition #xD] [undefined-static #xE] [enum-tag #xF]
  [member-of-enum #x10] [register-param #x11] [bit-field #x12]
  [block 100] [function 101] [end-of-struct 102] [file 103]
  [section 104] [weak-external 105] [clr-token 107]
  [end-of-function #xFF])

(define-bitmask* pecoff-characteristics #:+> PECoff-Characteristics
  attributes->pecoff-flag pecoff-flag->attributes
  [relocs-stripped #x0001] [executable-image #x0002] [line-nums-stripped #x0004]
  [local-syms-stripped #x0008] [aggressive-ws-trim #x0010] [large-address-aware #x0020]
  #;[_ #x0040] ; unused
  [bytes-reversed-lo #x0080] [32bit-machine #x0100] [debug-stripped #x0200]
  [removable-run-from-swap #x0400] [net-run-from-swap #x0800] [system #x1000]
  [DLL #x2000] [up-system-only #x4000] [bytes-reversed-hi #x8000])

(define-bitmask* pecoff-section-characteristics #:+> PECoff-Section-Characteristics
  attributes->pecoff-section-flag pecoff-section-flag->attributes
  [no-pad #x00000008]
  [code #x00000020] [initialized-data #x00000040] [uninitialized-data #x00000080]
  [link-other #x00000100] [link-info #x00000200] [link-remove #x00000800] [link-comdat #x00001000]
  [global-point-reference #x00008000] #;[mem-purgeable #x00020000] #;[mem-16bit #x00020000]
  [mem-locked #x00040000] [mem-preload #x00080000] [align-1bytes #x00100000]
  [align-2bytes #x00200000] [align-4bytes #x00300000] [align-8bytes #x00400000]
  [align-16bytes #x00500000] [align-32bytes #x00600000] [align-64bytes #x00700000]
  [align-128bytes #x00800000] [align-256bytes #x00900000] [align-512bytes #x00A00000]
  [align-1024bytes #x00B00000] [align-2048bytes #x00C00000] [align-4096bytes #x00D00000]
  [align-8192bytes #x00E00000] [link-relocation-overflow #x01000000]
  [mem-discardable #x02000000] [mem-not-cached #x04000000] [mem-not-paged #x08000000]
  [mem-shared #x10000000] [mem-execute #x20000000] [mem-read #x40000000] [mem-write #x80000000])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct coff-header : COFF-Header
  ([machine : (#:enum LUInt16 pecoff-machine->number number->pecoff-machine #:fallback 'Unknown)]
   [section-count : LUInt16]
   [seconds : LUInt32]
   [symbol-table-entry : LUInt32 #:radix 16]
   [symbol-count : LUInt32]
   [additional-size : LUInt16]
   [characteristics : (#:bitmask LUInt16 attributes->pecoff-flag pecoff-flag->attributes) #:radix 16]))

(define-binary-struct coff-section-header : COFF-Section-Header
  ([name : (#:-> Symbol 8 pecoff-section-name->bytes bytes->pecoff-section-name)]
   [virtual-size : LUInt32]
   [virtual-address : LUInt32 #:radix 16]
   [data-size : LUInt32]
   [data-ptr : LUInt32 #:radix 16]
   [relocations-ptr : LUInt32 #:radix 16]
   [line-number-ptr : LUInt32 #:radix 16] ; deprecated
   [relocations-count : LUInt16]
   [line-number-count : LUInt16] ; deprecated
   [characteristics : (#:bitmask LUInt32 attributes->pecoff-section-flag pecoff-section-flag->attributes) #:radix 16]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct coff-auxiliary-symbol () #:type-name COFF-Auxiliary-Symbol #:transparent)

(define-binary-struct coff-standard-symbol : COFF-Standard-Symbol
  ([name : (#:-> (U Symbol Index) 8 pecoff-symbol-name->bytes bytes->pecoff-symbol-name)]
   [value : LUInt32 #:radix 16]
   [section-number : LInt16]
   [complex-type : (#:enum Byte pecoff-complex-datatype->number number->pecoff-complex-datatype #:fallback 'null)]
   [base-type : (#:enum Byte pecoff-base-datatype->number number->pecoff-base-datatype #:fallback 'null)]
   [storage-class : (#:enum Byte pecoff-storage-class->number number->pecoff-storage-class #:fallback 'null)]
   [auxiliary-count : Byte]))

(define-binary-struct [coff-function-definition-symbol coff-auxiliary-symbol] : COFF-Function-Definition-Symbol
  ([tag-index : LUInt32]
   [total-size : LUInt32]
   [line-number-ptr : LUInt32 #:radix 16]
   [next-function-ptr : LUInt32 #:radix 16]
   [_ : LUInt16 #:default 0]))

(define-binary-struct [coff-section-definition-symbol coff-auxiliary-symbol] : COFF-Section-Definition-Symbol
  ([length : LUInt32]
   [relocation-count : LUInt16]
   [line-number-count : LUInt16]
   [checksum : LUInt32 #:radix 16]
   [number : LUInt16]
   [selection : Byte]
   [_ : Byte #:default 0]
   [__ : LUInt16 #:default 0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pecoff-image? : (-> Input-Port Boolean)
  (lambda [/dev/stdin]
    #true))

(define read-pecoff-image : (-> Input-Port C:Image)
  (lambda [/dev/stdin]
    (define coff-header : COFF-Header (read-coff-header /dev/stdin))

    (define sections : (Listof COFF-Section-Header)
      (for/list ([n (in-range (coff-header-section-count coff-header))])
        (read-coff-section-header /dev/stdin)))

    (define symbols : (Listof COFF-Symbol) (read-pecoff-symbol-table /dev/stdin coff-header))
    (define strings : (Listof String) (read-pecoff-string-table /dev/stdin coff-header))
    
    (make-c:image #:symbol-names strings
                  #:raw (list coff-header sections symbols
                              (coff-header-symbol-count coff-header)
                              (length symbols)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-pecoff-symbol-table : (-> Input-Port COFF-Header (Listof COFF-Symbol))
  (lambda [/dev/stdin header]
    (define symbol-table-position : Index (coff-header-symbol-table-entry header))
    
    (if (> symbol-table-position 0)
        (let ([total : Index (coff-header-symbol-count header)])
          (file-position /dev/stdin symbol-table-position)
          (let read-symbol ([c : Nonnegative-Fixnum 0])
            (if (< c total)
                (let* ([self (read-coff-standard-symbol /dev/stdin)]
                       [aux (coff-standard-symbol-auxiliary-count self)])
                  (if (> aux 0)
                      (let ([sc (coff-standard-symbol-storage-class self)]
                            [ct (coff-standard-symbol-complex-type self)]
                            [sn (coff-standard-symbol-section-number self)])
                        (cond [(and (eq? sc 'external) (eq? ct 'function) (> sn 0))
                               (let ([fds (read-coff-function-definition-symbol /dev/stdin)])
                                 (cons self (cons fds (read-symbol (+ c 2)))))]
                              [(and (eq? sc 'static) (symbol? (coff-standard-symbol-name self)))
                               (let ([sds (read-coff-section-definition-symbol /dev/stdin)])
                                 (cons self (cons sds (read-symbol (+ c 2)))))]
                              [else ; auxiliary can have any format, so just ignore unrecognizable ones
                               (read-nbytes /dev/stdin (* aux #%pecoff-symbol-record-size))
                               (cons self (read-symbol (+ c (+ 1 aux))))]))
                      (cons self (read-symbol (+ c 1)))))
                null)))
        null)))

(define read-pecoff-string-table : (-> Input-Port COFF-Header (Listof String))
  (let ([sep (string #\null)])
    (lambda [/dev/stdin header]
      (define symbol-table-position : Index (coff-header-symbol-table-entry header))
      
      (if (> symbol-table-position 0)
          (let ([string-table-position (+ symbol-table-position
                                          (* (coff-header-symbol-count header)
                                             #%pecoff-symbol-record-size))])
            (file-position /dev/stdin string-table-position)
            (let ([table-size (- (read-luint32 /dev/stdin) 4)])
              (if (> table-size 0)
                  (let ([string-tables (read-nbytes /dev/stdin table-size)])
                    (string-split (bytes->string/utf-8 string-tables) sep))
                  null)))
          null))))
