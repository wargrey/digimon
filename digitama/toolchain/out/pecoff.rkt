#lang typed/racket/base

(provide (all-defined-out))

(require "image.rkt")

(require "../../../enumeration.rkt")
(require "../../../stdio.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* pecoff-machine-type #:+> PECoff-Machine-Type
  machine->number number->machine
  [Unknown #x0000]
  [AM33 #x01D3] [AMD64 #x8664] [ARM #x01C0] [ARM64 #xAA64] [ARMNT #x01C4]
  [EBC #x0EBC] [I386 #x014C] [IA64 #x0200] [M32R #x9041] [MIPS16 #x0266]
  [MIPSFPU #x0366] [MIPSFPU16 #x0466] [POWERPC #x01F0] [POWERPCFP #x01F1]
  [R4000 #x0166] [RISCV32 #x5032] [RISCV64 #x5064] [RISCV128 #x5128]
  [SH3 #x01A2] [SH3DSP #x01A3] [SH4 #x01A6] [SH5 #x01A8] [THUMB #x01C2]
  [WCEMIPSV2 #x0169])

(define-enumeration* pecoff-characteristics #:+> PECoff-Characteristics
  attributes->flag flag->attributes
  [relocs-stripped #x0001] [executable-image #x0002] [line-nums-stripped #x0004]
  [local-syms-stripped #x0008] [aggressive-ws-trim #x0010] [large-address-aware #x0020]
  #;[_ #x0040] ; unused
  [bytes-reversed-lo #x0080] [32bit-machine #x0100] [debug-stripped #x0200]
  [removable-run-from-swap #x0400] [net-run-from-swap #x0800] [system #x1000]
  [DLL #x2000] [up-system-only #x4000] [bytes-reversed-hi #x8000])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct coff-header : COFF-Header
  ([machine : (#:enum UInt16 machine->number number->machine #:fallback 'Unknown)]
   [section-count : UInt16]
   [seconds : UInt32]
   [stable-entry : UInt32]
   [symbol-count : UInt32]
   [additional-size : UInt16]
   [characteristics : (#:enum UInt16 attributes->flag flag->attributes)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pecoff-image? : (-> Input-Port Boolean)
  (lambda [/dev/stdin]
    #false))

(define read-pecoff-image : (-> Input-Port C:Image)
  (lambda [/dev/stdin]
    (c:image #false)))
