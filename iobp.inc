
; I/O breakpoint stuff

; I/O access flags
BYTE_INPUT      equ 0x000
BYTE_OUTPUT     equ 0x004
WORD_INPUT      equ 0x008
WORD_OUTPUT     equ 0x00C
DWORD_INPUT     equ 0x010
DWORD_OUTPUT    equ 0x014

OUTPUT_IO_BIT   equ 2
OUTPUT_IO       equ (1 << OUTPUT_IO_BIT)
WORD_IO_BIT     equ 3
WORD_IO         equ (1 << WORD_IO_BIT)
DWORD_IO_BIT    equ 4
DWORD_IO        equ (1 << DWORD_IO_BIT)
STRING_IO_BIT   equ 5
STRING_IO       equ (1 << STRING_IO_BIT)
REP_IO_BIT      equ 6
REP_IO	        equ (1 << REP_IO_BIT)

STRING_IO_BIT   equ 5
STRING_IO       equ (1 << STRING_IO_BIT)
REP_IO_BIT      equ 6
REP_IO	        equ (1 << REP_IO_BIT)
ADDR_32_IO_BIT  equ 7
ADDR_32_IO      equ (1 << ADDR_32_IO_BIT)
REVERSE_IO_BIT  equ 8
REVERSE_IO      equ (1 << REVERSE_IO_BIT)

SEG_OVERRIDE    equ (1 << 8)
SEG_ES          equ (0 << 9) | SEG_OVERRIDE
SEG_CS          equ (1 << 9) | SEG_OVERRIDE
SEG_SS          equ (2 << 9) | SEG_OVERRIDE
SEG_DS          equ (3 << 9) | SEG_OVERRIDE
SEG_FS          equ (4 << 9) | SEG_OVERRIDE
SEG_GS          equ (5 << 9) | SEG_OVERRIDE

; int01 entry stack frame
struc int01_preFrame
    .EDI    resd 1
    .ESI    resd 1
    .EBP    resd 1
    .n_ESP  resd 1
    .EBX    resd 1
    .EDX    resd 1
    .ECX    resd 1
    .EAX    resd 1
    .ES     resw 1
    .DS     resw 1
    .IP     resw 1
    .CS     resw 1
    .FL     resw 1
endstruc
