bits 64
global _main
extern _write
extern _read
extern _exit
extern _mmap

section .text

; bindings for amd64-macos
PROT_READ   equ 0x1
PROT_WRITE  equ 0x2
PROT_EXEC   equ 0x4
MAP_ANON    equ 0x1000
MAP_JIT     equ 0x800
MAP_PRIVATE equ 0x2
MAP_FAILED  equ -1

_main:  ; align stack
        and rsp, -16
        mov rdi, 42

        ; initialize dictionary
        lea r15, [rel dict]
        mov rdi, r15
        lea rax, [rel noop]
        mov rcx, 33
        rep stosq
        lea rax, [rel fail]
        mov [r15], rax
        lea rax, [rel colon]
        mov [r15 + 8*':'], rax
        lea rax, [rel caret]
        mov [r15 + 8*'^'], rax
        stosq

        ; call _mmap to get a nice chunk of RWX memory
        and rsp, -16                                ; align RSP to 16 bytes
        lea rdi, [rel _main]                        ; get somewhere close
        mov rsi, 0x10000000                         ; size of JIT region
        mov rdx, PROT_READ | PROT_WRITE | PROT_EXEC ; RWX permission
        mov rcx, MAP_ANON | MAP_PRIVATE | MAP_JIT   ; JIT mapping
        xor r8d, r8d
        xor r9d, r9d
        call _mmap

        cmp rax, MAP_FAILED
        je .bad

        lea rdx, [rel _main]
        lea r8, [rdx + 0x7FFF0000]
        cmp rax, r8
        jae .bad

        lea r8, [rdx - 0x7FFF0000]
        cmp rax, r8
        jae .ok

.bad    mov rdi, -1 ; failed to allocate map
        call _exit

        ; set up registers
.ok     mov rdi, rax
        lea rsi, [rel kernel]

        ; eval loop
.eval   xor eax, eax
.skip   lodsb
        cmp al, 0
        je .unk
        cmp al, 32
        jbe .skip
        mov rax, [r15 + rax*8]    ; load from dispatch table
        test rax, rax
        jz .unk
        call rax
        jmp .eval

.unk    dec rsi
        mov rax, [r15]            ; fallback word is at r15
        call rax
        jmp .eval

fail:   lodsb
        movzx rdi, al
        and rsp, -16
        call _exit
        jmp fail                  ; should not happen

noop:   ret

caret:  push rcx
        push rbx
.bloop  mov rcx, 2
        xor bl, bl
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
.next   shl bl, 4
        or bl, al
        loop .loop

        mov al, bl
        stosb
        jmp .bloop

.done   dec rsi
        pop rbx
        pop rcx
        ret

colon:  xor eax, eax
        lodsb
        mov [r15 + rax*8], rdi
        ret

pound:  and rsp, -16
        xor edi, edi
        call _exit

kernel: incbin "kernel.nf"
        db 0,0 ; the second is the default exit code 0

section .bss

dict:   resb 0x10000000    ; RW dictionary memory

