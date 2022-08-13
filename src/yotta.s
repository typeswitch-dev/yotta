bits 64
global _main
extern _write
extern _read
extern _exit
extern _mmap

section .text

; bindings for amd64-darwin
SYS_EXIT    equ 0x2000001
SYS_WRITE   equ 0x2000004
SYS_MMAP    equ 0x20000C5
PROT_READ   equ 0x1
PROT_WRITE  equ 0x2
PROT_EXEC   equ 0x4
MAP_ANON    equ 0x1000
MAP_JIT     equ 0x800
MAP_PRIVATE equ 0x2
MAP_FAILED  equ -1

NAME_SIZE equ 32
SORD_SIZE equ 128

struc DICT
.LINK resq 1
.NAME resq NAME_SIZE/8
.CODE resq 1
.DATA resq 2
endstruc

struc USER
.HERE resq 1 ; pointer into DATA region
.PROG resq 1 ; base of program memory
.KERS resq 1 ; kernel source
.EVAL resq 1 ; literal evaluator, i.e. code to fall back on if FIND failed
.NAME resq NAME_SIZE/8 ; name buffer, holds last name parsed
.KERW resq 1 ; kernel word list
.MINW resq 1 ; minimal word list
.ASMW resq 1 ; assembler word list
.FORW resq 1 ; forth word list
.MIRW resq 1 ; mirth word list
.DICT resq 1 ; dictionary for new definitions
.WORD resq 1 ; word primitive
.FIND resq 1 ; find primitive
.SORD resq SORD_SIZE/8 ; search order
endstruc

_main:  ; align stack
        and rsp, -16

        ; initialize user data & dictionary
        lea rbp, [rel DATA]
        mov rdi, rbp
        xor eax, eax
        mov rcx, USER_size / 8
        rep stosq

        lea rax, [rel CARET]
        mov [rbp + USER.EVAL], rax
        lea rax, [rel FIND]
        mov [rbp + USER.FIND], rax
        lea rax, [rel _WORD]
        mov [rbp + USER.WORD], rax
        lea rax, [rel KERNEL]
        mov [rbp + USER.KERS], rax
        lea rcx, [rbp + USER.KERW]
        mov [rcx], rdi
        mov [rbp + USER.DICT], rcx
        mov [rbp + USER.SORD], rcx
        lea rcx, [rbp + USER.MIRW]
        mov [rbp + USER.SORD + 8], rcx
        lea rcx, [rbp + USER.FORW]
        mov [rbp + USER.SORD + 16], rcx
        lea rcx, [rbp + USER.ASMW]
        mov [rbp + USER.SORD + 24], rcx
        lea rcx, [rbp + USER.MINW]
        mov [rbp + USER.SORD + 32], rcx

        xor eax, eax
        stosq ; LINK
        mov rax, 0x2020202020203A01  ; ":"
        stosq ; NAME[0:8]
        mov rax, 0x2020202020202020
        mov rcx, NAME_SIZE/8 - 1
        rep stosq
        lea rax, [rel COLON]
        stosq ; CODE
        xor eax, eax
        stosq ; DATA
        stosq

        mov [rbp + USER.HERE], rdi

        ; call _mmap to get a nice chunk of RWX memory
        mov rax, SYS_MMAP
        lea rdi, [rel _main - 0x40000000]           ; get somewhere "close"
        ; xor edi, edi
        mov rsi, 0x10000000                         ; size of JIT region
        mov rdx, PROT_READ | PROT_WRITE | PROT_EXEC ; RWX permission
        mov r10, MAP_ANON | MAP_PRIVATE | MAP_JIT   ; JIT mapping
        mov r8, -1
        xor r9d, r9d
        syscall
        jc .syserr
        cmp rax, MAP_FAILED
        je .bad

        mov [rbp + USER.PROG], rax

        lea rdx, [rel _main]
        lea r8, [rdx + 0x7FFF0000]
        cmp rax, r8
        jae .bad

        lea r8, [rdx - 0x7FFF0000]
        cmp rax, r8
        jae .ok

.syserr mov rdi, rax ; failed to allocate map
        mov rax, SYS_EXIT
        syscall
        jmp .bad

.bad    mov rax, SYS_EXIT
        mov rdi, -1 ; failed to allocate map
        syscall
        jmp .bad

        ; set up registers
.ok     mov rdi, [rbp + USER.PROG]
        lea rsi, [rel KERNEL]

EVAL:   call _WORD
        call FIND
        jz .fail
        call [rax + DICT.CODE]
        jmp EVAL
.fail:  call qword [rbp + USER.EVAL]
        jmp EVAL

BYE:    and rsp, -16
        mov rax, SYS_EXIT
        mov rdi, 43
        syscall

        ; read word from RSI, store it in NAME buffer
        ; doesn't affect registers other than RSI
_WORD:  push rax
        push rcx
        push rdi
        lea rdi, [rbp + USER.NAME]
        mov rax, 0x2020202020202020
        mov rcx, NAME_SIZE/8
        rep stosq
        xor eax, eax
        xor ecx, ecx
        lea rdi, [rbp + USER.NAME + 1] ; leading byte is for length
.skip   lodsb
        cmp al, ' '      ; skip whitespace / control chars
        jbe .skip
.upper  cmp al, 'a'
        jb .store
        cmp al, 'z'
        ja .store
;        sub al, 32       ; convert to uppercase
.store  stosb
        inc cl
        cmp cl, NAME_SIZE-1
        jae .done
        lodsb
        cmp al, ' '     ; stop storing at whitespace / control chars
        ja .upper
        dec rsi         ; restore whitespace character
.done   mov [rbp + USER.NAME], cl  ; store length in name buffer
        pop rdi
        pop rcx
        pop rax
        ret

        ; search for NAME in wordlists
        ; return resulting DICT in RAX, sets ZF if not found
FIND:   push rsi
        push rdi
        push rcx
        push rbx
        xor ebx, ebx
.list   mov rax, [rbp + USER.SORD + rbx*8]  ; continue with list # rcx
        test rax, rax
        jz .fail
        jmp .link

.item   lea rsi, [rbp + USER.NAME]
        lea rdi, [rax + DICT.NAME]
        mov rcx, NAME_SIZE/8
        repe cmpsq
        jz .found

.link   mov rax, [rax]
        test rax, rax
        jnz .item

        inc rbx
        cmp rbx, SORD_SIZE/8
        jb .list

.found  test rax, rax
.fail   pop rbx
        pop rcx
        pop rdi
        pop rsi
        ret

        ; define a new word in current wordlist, pointing at RDI
        ; changes register RSI, preserves all other registers
        ; changes variables NAME, HERE, DICT, and maybe FIND[0]
COLON:  call _WORD
        push rsi
        push rcx
        push rax
        push rdi
        mov rdi, [rbp + USER.HERE]
        add rdi, 7
        and rdi, -8 ; align HERE up
        mov rcx, [rbp + USER.DICT]
        mov rax, [rcx]
        mov [rcx], rdi
        stosq ; LINK
        lea rsi, [rbp + USER.NAME]
        mov rcx, 4
        rep movsq ; NAME
        mov rax, [rsp]
        stosq ; CODE
        xor eax, eax
        stosq ; DATA
        stosq
        mov [rbp + USER.HERE], rdi
        pop rdi
        pop rax
        pop rcx
        pop rsi
        ret

CARET:  push rax
        push rdx
        push rsi
        lea rsi, [rbp + USER.NAME + 1]

        lodsb
        mov dl, al
        cmp dl, '^'
        je .ok
        cmp dl, '$'
        je .ok
        jne .error

.ok:    push rcx
        push rbx

        mov al, [rsi]
        mov dh, al
        cmp dh, '-'
        jne .nosign
        inc rsi
.nosign mov ecx, 1
        xor ebx, ebx
.loop   lodsb
        cmp al, '0'
        jb .done
        cmp al, '9'
        jbe .dec
        cmp al, 'A'
        jb .done
        cmp al, 'F'
        jbe .hex
        jmp .done

.dec    sub al, '0'
        jmp .next
.hex    sub al, 'A' - 10
.next   shl rbx, 4
        or bl, al

        inc rcx
        jmp .loop

.done   shr rcx, 1      ; rcx is number of bytes to emit
        test rcx, rcx
        jz .error       ; need at least one byte

        cmp dh, '-'
        jne .nosgn2
        neg rbx

.nosgn2 cmp dl, '^'
        jne .imm

.caret  mov al, 0xB0    ; MOV AL, imm8
        stosb
        mov al, bl
        stosb
        mov al, 0xAA    ; STOSB
        stosb
        shr rbx, 8
        loop .caret
        jmp .done2

.imm    mov al, bl
        stosb
        shr rbx, 8
        loop .imm

.done2  pop rbx
        pop rcx
        pop rsi
        pop rdx
        pop rax
        ret

.error  and rsp, -16

        mov rax, SYS_WRITE
        mov rdi, 2
        lea rsi, [rel .emsg1]
        mov rdx, .emsg1n
        syscall

        mov rax, SYS_WRITE
        mov rdi, 2
        lea rsi, [rbp + USER.NAME + 1]
        movzx rdx, byte [rbp + USER.NAME]
        syscall

        mov rax, SYS_WRITE
        mov rdi, 2
        lea rsi, [rel .emsg2]
        mov rdx, .emsg2n
        syscall

        mov rax, SYS_EXIT
        mov rdi, -13
        syscall
        jmp .error

.emsg1  db "unknown word: "
.emsg1n equ $ - .emsg1
.emsg2  db 10
.emsg2n equ $ - .emsg2

KERNEL: incbin "src/yotta.fs"
        db 0, -1, 0, -1, 0, -1, 0, -1

section .bss

DATA: resb 0x10000000

