        ; TNDYBP - Debug Extensions-powered Tandy Sound redirector - wbcbz7 28.o1.2o24

        %include     "iobp.inc"

; defines
signature       equ     'tO'
version         equ     0x0001
tracer_int      equ     0x01

; install check bitfields
INSTALL_BIT_INT01           equ (1 <<  0)
INSTALL_BIT_INT01LAST       equ (1 <<  1)
INSTALL_CHECK_MASK          equ INSTALL_BIT_INT01 | INSTALL_BIT_INT01LAST
INSTALL_TNDLPT              equ (1 <<  4)
INSTALL_SETPORT             equ (1 << 12)
INSTALL_RELEASE             equ (1 << 13)

        use16

; ------------------------
; entrypoint
start:
        org         0x100
        jmp         init
        nop

; INT01 handler
int01:
.signature          dw  signature
.version            dw  version
        ; ---
        ; INT 0x01 handler entrypoint
.entry:
        nop                             ; used to disable INT01 handler

        ; I/O breakpoint hit
.iobp:
        push        ds
        push        es
        pushad

        mov         bp, sp      ; SS:BP = .int01_preFrame
        mov         ax, cs
        mov         ds, ax      ; DS - TSR segment

        ; alright, it's I/O breakpoint
        ; check the cause of exception
        mov         bx, [bp + int01_preFrame.CS]
        mov         es, bx
        mov         si, [bp + int01_preFrame.IP]
        xor         bx, bx
        xor         cx, cx

        ; BX=CX=0, ES:SI = trapped CS:IP (after target instruction)
        ; possible instruction encodings:
        ; dx/str:   [pfx] OP
        ; imm8:     [pfx] OP IMM8
.opcheck:
        mov         ax, [es:si-2]   ; grab 2 bytes

        ; in/out al/ax, imm8?
        cmp         al, 0xE7
        ja          .op_not_imm8
        cmp         al, 0xE4
        jae         .op_imm8
.op_not_imm8:

        ; alright, check one-byte opcodes then

        ; in/out al/ax, dx
        cmp         ah, 0xEF
        ja          .op_not_dx
        cmp         ah, 0xEC
        jae         .op_dx
.op_not_dx:

        ; string i/o?
        cmp         ah, 0x6F
        ja          .op_not_string
        cmp         ah, 0x6C
        jae         .op_string
.op_not_string:
        ; unknown opcode
        jmp         .unk_op

        ; resolve pointer to access byte table in BX
.op_imm8:
        movzx       dx, ah          ; load port num
        dec         si
        mov         ah, al
        jmp         .op_table
.op_string:
        or          cl, STRING_IO
.op_dx:
        mov         dx, word [bp + int01_preFrame.EDX]
.op_table:
        and         ah, 3
        mov         bl, ah
        sub         si, 2

        ; scan for prefixes
.pfxloop:
        mov         al, [es:si]
        dec         si
        ; prefixes?
        cmp         al, 0xF2        ; REPNE
        jz          .pfx_rep
        cmp         al, 0xF3        ; REPE
        jz          .pfx_rep
        cmp         al, 0x66
        jz          .pfx_opsize
        cmp         al, 0x67
        jz          .pfx_easize

        ; segment overrides
        cmp         al, 0x26
        jz          .seg_es
        cmp         al, 0x2E
        jz          .seg_cs
        cmp         al, 0x36
        jz          .seg_ss
        cmp         al, 0x3E
        jz          .seg_ds
        cmp         al, 0x64
        jz          .seg_fs
        cmp         al, 0x65
        jz          .seg_gs
        jmp         .op_done         ; alright, get access type

.seg_gs:
        mov         ch, (SEG_GS >> 8)
        jmp         .pfxloop
.seg_es:
        mov         ch, (SEG_ES >> 8)
        jmp         .pfxloop
.seg_cs:
        mov         ch, (SEG_CS >> 8)
        jmp         .pfxloop
.seg_ss:
        mov         ch, (SEG_SS >> 8)
        jmp         .pfxloop
.seg_ds:
        mov         ch, (SEG_DS >> 8)
        jmp         .pfxloop
.seg_fs:
        mov         ch, (SEG_FS >> 8)
        jmp         .pfxloop

.pfx_opsize:
        xor         bx, 4               ; offset for .io_dx_imm8_flags
        jmp         .pfxloop
.pfx_rep:
        or          cl, REP_IO
        jmp         .pfxloop
.pfx_easize:
        or          cl, ADDR_32_IO
        jmp         .pfxloop

.op_done:
        mov         bl, [.io_dx_imm8_flags + bx]
        or          bx, cx
        add         si, 2                  ; compensate address

        ; now DX - port, BX - access type, ES:SI - CS:IP of trapped opcode
.unk_op:
        test        bx, OUTPUT_IO
        jz          .done_iobp
        cmp         dx, 0xC0
        jnz         .done_iobp

.redirect:
        mov         eax, [bp + int01_preFrame.EAX]
        mov         dx, 0x2C0
.redirect_port      equ     $-2

        ; write to port
        out         dx, al

        ; check if redirect to TNDLPT
        test        byte [cs:vars.flags], INSTALL_TNDLPT
        jnz         .tndlpt

.done_iobp:
        ; reset DR6
        xor         eax, eax
        mov         dr6, eax

        ; restore stack frame
        popad
        pop         es
        pop         ds
        iret

        ; access flags for E4..E7 and EC..EF opcodes
.io_dx_imm8_flags:
        db          BYTE_INPUT, WORD_INPUT,  BYTE_OUTPUT, WORD_OUTPUT
        db          BYTE_INPUT, DWORD_INPUT, BYTE_OUTPUT, DWORD_OUTPUT

        ; output to TNDLPT
        ; DX - port
.tndlpt:
        ; assert strobe
        add         dx, 2
        mov         al, 0x0C
        out         dx, al

        ; wait
        dec         dx
        mov         cx, 32
.tndlpt_loop1:
        in          al, dx
        dec         cx
        jz          .tndlpt_timeout
        test        al, 0x40
        jnz         .tndlpt_loop1
.tndlpt_loop2:
        in          al, dx
        dec         cx
        jz          .tndlpt_timeout
        test        al, 0x40
        jz         .tndlpt_loop2

        ; deassert strobe
.tndlpt_timeout:
        inc         dx
        mov         al, 0x09
        out         dx, al
        jmp         .done_iobp

        ; resident variables (more on this later)
vars:
        align       4
.old_int1           dd    0         ; old/replaced INT01 vector
.flags              dw    0

resident_end:

; ----------------------
; NON-RESIDENT PORTION HERE

; ----------------------
; strings!
        %include "strings.inc"

; ----------------------
; initialization routine
init:
        push        cs
        pop         ds
        xor         bp, bp          ; install flags

        mov         dx, strings.start
        mov         ah, 0x9
        int         0x21

        ; install check
        call        install_check

        ; determine which segment to load to ES
        test        bp, INSTALL_BIT_INT01
        jnz         .not_installed
        mov         ax, cs
        mov         es, ax
        jmp         parse_cmdline
.not_installed:
        mov         es, cx
        ; parse command line
parse_cmdline:
        mov         si, 80h
        lodsb
        test        al, al
        jz          no_params           ; empty commandline

        xor         cx, cx
        mov         cl, al
.loop:
        lodsb
        cmp         al, "?"             ; help
        jz          help
        cmp         al, 40h             ; spaces, slashes and other stuff
        jb          .gottaloop
        cmp         al, 5Fh             ; a-z
        jb          .skip_upcase
        sub         al, 20h             ; upcase these symbols
.skip_upcase:
        ; TODO: parse other stuff
        cmp         al, 'H'
        jz          help
        cmp         al, 'U'
        jz          release
        cmp         al, 'R'
        jz          release
        cmp         al, 'L'
        jz          parm_lpt
        cmp         al, 'P'
        jz          parm_direct
.gottaloop:
        dec         cx
        jz          no_params
        jmp         .loop               ; else gotta loop

        ; ----------------
        ;
parm_lpt:
        or          bp, INSTALL_TNDLPT | INSTALL_SETPORT
        call        hex2val
        cmp         ax, 3
        ja          .skip_bda
        test        ax, ax
        jz          .zero
        dec         ax
.zero:
        ; ax = 0,1,2
        push        ds
        mov         bx, 0x40
        mov         ds, bx
        mov         bx, ax
        add         bx, bx
        mov         ax, [bx + 8]
        pop         ds
        test        ax, ax
        mov         dx, strings.no_port
        jz          fatal_error
.skip_bda:
parm_save_new_port:
        mov         [bss.port], ax
        jmp         eat_cmd_words

        ; ----------------
parm_direct:
        or          bp, INSTALL_SETPORT
        call        hex2val
        test        ax, ax
        jnz         parm_save_new_port
        mov         ax, 0x2C0           ; default port
        jmp         parm_save_new_port

        ; ----------------
        ; eat command line until separator character is found
        ; allows to use "long" switches, i.e. /RELEASE instead of /R
        ; ds:si - cmdline, cx - characters in cmdline left
eat_cmd_words:
        test        cx, cx
        jz          no_params
.loop:
        lodsb
        dec         cx
        cmp         al, 30h             ; spaces, slashes and other stuff
        jb          .separator
        test        cx, cx
        jnz        .loop
.separator:
        test        cx, cx
        jz          no_params
        jmp         parse_cmdline.loop

        ; ----------------
        ; show help and exit
help:
        mov         dx, strings.help
        mov         ah, 9
        int         0x21
        mov         ax, 0x4C00
        int         0x21

        ; ----------------
        ; release
release:
        or          bp, INSTALL_RELEASE
        jmp         eat_cmd_words

        ; ----------------
        ; no more parameters!
no_params:
        ; but at first perform all CPU checks
        ; check for 386+
        call        check386
        jc          .no_de
        ; check for V86 mode
        smsw        ax
        test        ax, 1
        jz          .realmode
        mov         dx, strings.v86
        jmp         fatal_error
.realmode:
        ; check for Debug Extensions
        call        cpuid
        jc          .no_de
        test        dl, (1 << 2)
        jnz         .de_present
.no_de:
        mov         dx, strings.no_de
        jmp         fatal_error
.de_present:

        ; determine if to install, patch or release
        test        bp, INSTALL_RELEASE
        jnz         try_release
        test        bp, INSTALL_BIT_INT01
        jnz         try_patch
        jmp         try_install

; ------------------
; try releasing the TSR
; ES - resident segment
try_release:
        ; if any of interrupt vectors were intercepted, we should check
        ; if the TSR is last in corresponding vector chain
        mov         ax, bp
        and         ax, INSTALL_BIT_INT01 | INSTALL_BIT_INT01LAST
        jz          .not_inst
        cmp         ax, INSTALL_BIT_INT01 | INSTALL_BIT_INT01LAST
        jnz         .not_last

.fine:
        ; disable breakpoints
        xor         eax, eax
        mov         dr0, eax
        mov         dr1, eax
        mov         dr2, eax
        mov         dr3, eax
        mov         dr6, eax
        mov         dr7, eax

        ; remove INT01
        mov         ds, [es:vars.old_int1 + 2]
        mov         dx, [es:vars.old_int1 + 0]
        mov         ax, 0x2500 | tracer_int
        int         0x21
        mov         ax, cs
        mov         ds, ax

        ; free TSR memory
        mov         ah, 0x49
        int         0x21

        ; and terminate us
        mov         ah, 9
        mov         dx, strings.released
        int         0x21
        mov         ax, 0x4C00
        int         0x21

.not_inst:
        mov         dx, strings.not_inst
        jmp         fatal_error

.not_last:
        ; whoops, not last in chain!
        mov         dx, strings.not_last
        jmp         fatal_error

; ------------------
; try patching already loaded TSR
; ES - resident segment
try_patch:
        test        bp, ~(INSTALL_CHECK_MASK)
        jz          .already

        call        patch

        mov         dx, strings.reset
        jmp         .done

.already:
        mov         dx, strings.already
.done:
        mov         ah, 0x9
        int         0x21

        ; print status
        call        status

        ; exit
        mov         ax, 0x4C00
        int         0x21

; ------------------
; display current status
; ES - resident segment
status:
        mov         ah, 0x9
        mov         dx, strings.redirect
        int         0x21

        test        byte [es:vars.flags], INSTALL_TNDLPT
        jnz         .tndlpt
        mov         dx, strings.tandysnd
        jmp         .print_device

.tndlpt:
        mov         dx, strings.tndlpt
.print_device:
        int         0x21

        ; print port
        mov         ax, [es:int01.redirect_port]
        call        printhex4

        ; cr/lf
        mov         ah, 0x9
        mov         dx, strings.crlf
        int         0x21

        ret

; ------------------
; try install a new TSR instance
try_install:
        ; temporary disable the handler
        mov         byte [int01.entry], 0xCF   ; iret

        ; install INT01 handler (ignore INT21 for now)
        mov         ax, 0x3500 | tracer_int
        int         0x21
        mov         [vars.old_int1 + 0], bx
        mov         [vars.old_int1 + 2], es
        mov         ax, 0x2500 | tracer_int
        mov         dx, int01.entry
        int         0x21

        ; free environment block (as it is no longer needed)
        mov         bx, [cs:2Ch]
        mov         es, bx
        mov         ah, 49h
        int         21h

        ; set new resident segment
        push        cs
        pop         es

        ; save old CR4 and DRx registers
        ;call        save_regs

        ; reenable INT01
        mov         byte [int01.entry], 0x90    ; nop

        ; patch the TSR (does INT01 enable/disable by itself)
        call        patch

        ; clear all stale breakpoints
        xor         eax, eax
        mov         dr0, eax
        mov         dr1, eax
        mov         dr2, eax
        mov         dr3, eax
        mov         dr6, eax
        mov         dr7, eax

        ; enable DE in CR4
        mov         eax, cr4
        or          eax, (1 << 3)
        mov         cr4, eax

        ; enable byte breakpoint at 0xC0
        mov         eax, 0xC0
        mov         dr0, eax
        mov         eax, 0x00020703
        mov         dr7, eax

        ; print "installed" message
        mov         ah, 0x9
        mov         dx, strings.installed
        int         0x21

        ; print status
        call        status

        ; and make in TSR
        mov         dx, resident_end
        int         0x27

; ------------------
; apply patches
; ES - resident segment
patch:
        ; temporary disable INT01
        mov         byte [int01.entry], 0xCF   ; iret

        test        bp, INSTALL_SETPORT
        jz          .skip_port
        ; set port number
        mov         ax, [bss.port]
        mov         [es:int01.redirect_port], ax

.skip_port:
        ; reset flags
        mov         ax, bp
        and         ax, INSTALL_TNDLPT
        mov         [es:vars.flags], ax

        ; reenable INT01
        mov         byte [int01.entry], 0x90    ; nop
        ret

; ------------------
; convert ascii hex number to 16bit value
; input:  ds:si - string, cx - number of avail chars in string
; output: ax - number
hex2val:
        xor     ax, ax
        push    dx

        xor     dx, dx
.loop:
        lodsb
        cmp     al, 0xA             ; CR
        jz      .end
        cmp     al, 0xD             ; LF
        jz      .end
        cmp     al, '0'
        jb      .end                ; invalid param
        cmp     al, '9'
        ja      .hex
        sub     al, '0'
        jmp     .advance
.hex:
        cmp     al, 'A'            ; blabla :)
        jb      .gottaloop
        cmp     al, 0x60
        jb      .uppercase
        sub     al, 0x20
.uppercase:
        sub     al, 'A' - 10
.advance:
        shl     dx, 4
        or      dx, ax
.gottaloop:
        loop    .loop
.end:
        mov     ax, dx              ; save given number in ax
        pop     dx
        ret

; -----------------
; print hexadecimal value to stdout
; printhex4: AX = word, destroys DX
; printhex2: AH = byte, destroys DX
printhex4:
        mov         dl, ah
        shr         dl, 4
        and         dl, 0xF
        call        charhex
        mov         dl, ah
        and         dl, 0xF
        call        charhex
printhex2:
        mov         dl, al
        shr         dl, 4
        and         dl, 0xF
        call        charhex
        mov         dl, al
        and         dl, 0xF
        call        charhex
        ret

; EAX = dword, destroys DX
printhex8:
        mov         cx, 8
.1:
        rol         eax, 4
        mov         dl, al
        and         dl, 0xF
        call        charhex
        loop        .1
        ret

; DL - character. saves AX
charhex:
        push        ax
        push        bx
        xor         bx, bx
        mov         bl, dl
        mov         al, [.hex + bx]
        int         0x29
        pop         bx
        pop         ax
        ret

.hex        db          "0123456789ABCDEF"

; ------------------
; check for 386
check386:
        ; first check for 286+ (or 186/286, idk)
        push        sp
        pop         ax
        cmp         ax, sp
        jnz         .fail       ; 8086/88
        ; test FLAGS
        pushf
        push        0x7000
        popf
        pushf
        pop         ax
        popf
        test        ax, 0x7000
        jz          .fail       ; 286
        clc
        ret                 ; 386+, yay!
.fail:
        stc
        ret

; ------------------
; get cpuid
; out: CF=1 if not supported, CF=0 and leaf 1 EAX/EBX/ECX/EDX if supported
cpuid:
        ; test if CPUID is supported
        pushfd
        push        dword (1 << 21)
        popfd
        pushfd
        pop         eax
        popfd
        test        eax, (1 << 21)
        jz          .fail

        ; execute leaf 1
        mov         eax, 1
        cpuid

        clc
        ret

.fail:
        stc
        ret

;----
; installation check
; input:  AL = interrupt
; output: BP = install flags, CX - current ISR segment
install_check:
        ; check for already installed
        mov     ax, 0x3500 | tracer_int
        mov     bx, signature       ; bypass our stealth
        int     0x21                ; get interrupt vector
        mov     cx, es

        cmp     word [es:int01.signature], signature    ; check for signature
        jnz     .ret                ; not installed
        cmp     word [es:int01.version], version        ; check for version
        jnz     .ret                ; not installed

        ; installed
        or      bp, INSTALL_BIT_INT01 | INSTALL_BIT_INT01LAST

.ret:
        ret

; ------------------
; display error message and exit
; in:  DX - offset of error message
fatal_error:
        push        dx
        mov         dx, strings.error
        mov         ah, 9
        int         0x21

        pop         dx
        int         0x21

        ; exit with errorlevel=1
        mov         ax, 0x4C01
        int         0x21

end:

bss:
.port               resw    1
