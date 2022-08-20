[KERNEL-DEFINITIONS]

: ASM-PANIC CALL>
    \ TODO do something nicer ... this is bad
    $0F $38 $FF         \ #UD
    ;

: NO-REX/VEX ( -- R8:0 ) CALL>
    $4D $33 $C0 ;       \ XOR R8, R8
: REX/VEX ( -- R8:RDI ) CALL>
    $49 $89 $F8 ;       \ MOV R8, RDI
: OPCODE ( -- R9:RDI ) CALL>
    $49 $89 $F9 ;       \ MOV R9, RDI

: SHIFT-OP ( R9 RDI -- R9+1 RDI+1 ) CALL>
    \ Shift the entire OP up by one byte to make room for prefix.
    $50                 \ PUSH RAX
    $49 $8B $41 $08     \ MOVQ< RAX, [R9]. $08
    $49 $89 $41 $09     \ MOVQ> RAX, [R9]. $09
    $49 $8B $41 $00     \ MOVQ< RAX, [R9]. $00
    $49 $89 $41 $01     \ MOVQ> RAX, [R9]. $01
    $4D $8D $49 $01     \ LEAQ< R9, [R9]. $01
    $48 $8D $7F $01     \ LEAQ< RDI, [RDI]. $01
    $58                 \ POP RAX
    ;

: IS-REX ( R8 -- R8 ZF:is-rex ) CALL>
    \ Check that R8 points to a REX prefix byte.
    \ Result is returned in ZF:
    \   ZF=1 means it is a REX prefix.
    \   ZF=0 means it's not a REX prefix.
    $50                 \ PUSHQ# RAX
    $41 $8A $00         \ MOVB< AL, [R8]
    $24 $F0             \ AND-AL. $F0
    $3C $40             \ CMP-AL. $40
    $58                 \ POPQ# RAX
    ;

: IS-VEX2 ( R8 -- R8 ZF:is-vex2 ) CALL>
    \ Check that R8 points to the start of a 2-byte VEX prefix.
    \ Result is returned in ZF:
    \   ZF=1 means it is 2-byte VEX prefix.
    \   ZF=0 means it's not a 2-byte VEX prefix.
    $41 $80 $38 $C5     \ CMPB_. [R8] $C5
    ;

: IS-VEX3 ( R8 -- R8 ZF:is-vex3 ) CALL>
    \ Check that R8 points to the start of a 3-byte VEX prefix.
    \ Result is returned in ZF:
    \   ZF=1 means it is 3-byte VEX prefix.
    \   ZF=0 means it's not a 3-byte VEX prefix.
    $41 $80 $38 $C4     \ CMPB_. [R8] $C4
    ;

 : IS-EVEX ( R8 -- R8 ZF:is-evex ) CALL>
    \ Check that R8 points to the start of a EVEX prefix.
    \ Result is returned in ZF:
    \   ZF=1 means it is EVEX prefix.
    \   ZF=0 means it's not a EVEX prefix.
    $41 $80 $38 $62     \ CMPB_. [R8] $62
    ;

: PREPARE-REX ( R8:x|0 R9:op -- R8:x|REX R9:op ZF:is-rex ) CALL>
    \ Make sure REX prefix exists, insert it if necessary.
    \ Note that R9 points to the current opcode (after
    \ legacy prefixes and REX prefix if it exists) and
    \ R8 points to the REX prefix. R8 is NULL if the
    \ current instruction doesn't have a REX prefix yet.
    \ So what PREPARE-REX tries to do is create a blank
    \ REX prefix if it's missing.
    \ If a VEX or EVEX prefix exists instead, this word
    \ leaves them alone, does not insert a REX prefix.
    $4D $85 $C0         \ TEST R8, R8
    $75 $0C             \ JNZ +12
    $4D $8B $C1         \ MOVQ< R8, R9
    SHIFT-OP
    $41 $C6 $00 $40     \ MOVB_. [R8] 0x40
    IS-REX
    ;

: PREPARE-VEX3 ( R8:x|VEX2 R9:op -- R8:x|VEX3 R9:op ZF:is-vex3 ) CALL>
    \ Convert a 2-byte VEX prefix, if present, into
    \ a 3-byte VEX prefix. The 3-byte version is needed
    \ to encode WXB bits and a different map_select.
    IS-VEX2
    $74 $06             \ JZ +06
    IS-VEX3
    $C3                 \ RET
    SHIFT-OP        \ converting VEX2 to VEX3
    $50                 \ PUSH RAX
    $41 $C6 $00 $C4     \ MOVB_. [R8] 0xC4
    $41 $8A $40 $01     \ MOVB< RAX, [R8]. 0x01
    $24 $7F             \ AND-AL. 0x7F
    $41 $88 $40 $02     \ MOVB> RAX, [R8]. 0x02
    $41 $8A $40 $01     \ MOVB< RAX, [R8]. 0x01
    $24 $80             \ AND-AL. 0x80
    $0C $61             \ OR-AL. 0x61
    $41 $88 $40 $01     \ MOVB> RAX, [R8]. 0x01
    $58                 \ POP RAX
    IS-VEX3
    ;

: PREPARE-EVEX ( R8:VEX2|VEX3|EVEX -- R8:EVEX ZF:is-evex ) CALL>
    \ Convert a VEX prefix into an EVEX prefix.
    PREPARE-VEX3
    $74 $06             \ JZ +06
    IS-EVEX
    $C3                 \ RET
    SHIFT-OP        \ converting VEX3 to EVEX:
    $50                 \ PUSH RAX
    $41 $C6 $00 $62     \ MOVB_. [R8] 0x62
    $41 $80 $48 $01 $10 \ ORB_. [R8]. 0x01 0x10   (set R' bit)
    $41 $8A $40 $02     \ MOVB< RAX, [R8]. 0x02
    $24 $04             \ AND-AL. 0x04            (get old L bit)
    $C0 $E0 $03         \ SHLB_. RAX 0x03         (move L to new position)
    $0C $04             \ OR-AL. 0x04             (set V' bit)
    $41 $88 $40 $03     \ MOVB> RAX, [R8]. 0x03   (store 4th byte of EVEX)
    $41 $80 $48 $02 $04 \ ORB_. [R8]. 0x02 0x04   (set mandatory 1 bit)
    $58                 \ POP RAX
    IS-EVEX
    ;

: +W CALL>
    PREPARE-REX
    $75 $05             \ JNZ +05
    $41 $80 $08 $08     \ ORB_. [R8] 0x08
    $C3                 \ RET
    PREPARE-VEX3
    $74 $07             \ JZ +07     \ VEX3 and EVEX have W bit in same place
    IS-EVEX
    $75 $06             \ JNZ +06
    $41 $80 $48 $02 $08 \ ORB_. [R8]. 0x02 0x08
    $C3                 \ RET
    ASM-PANIC ;

: R=1 CALL>
    PREPARE-REX
    $75 $05             \ JNZ +5
    $41 $80 $08 $04     \ ORB_. [R8] 0x04
    $C3                 \ RET
    IS-VEX2
    $74 $12             \ JZ +14
    IS-VEX3
    $74 $07             \ JZ +7
    IS-EVEX
    $75 $06             \ JNZ +6
    $41 $80 $60 $01 $7F \ ANDB_. [R8]. 0x01 0x7F
    $C3                 \ RET
    ASM-PANIC ;
: R=2 CALL>
    PREPARE-EVEX
    $75 $06             \ JNZ +6
    $41 $80 $60 $01 $EF \ ANDB_. [R8]. 0x01 0xEF
    $C3                 \ RET
    ASM-PANIC ;
: R=3 CALL>
    PREPARE-EVEX
    $75 $06             \ JNZ +6
    $41 $80 $60 $01 $6F \ ANDB_. [R8]. 0x01 0x6F
    $C3                 \ RET
    ASM-PANIC ;

: +V' CALL>
    PREPARE-EVEX
    $75 $06             \ JNZ +06
    $41 $80 $60 $03 $F7 \ ANDB_. [R8]. 0x01 0xF7  (clear V' bit)
    $C3                 \ RET
    ASM-PANIC ;

: +X CALL>
    PREPARE-REX
    $75 $05             \ JNZ +05
    $41 $80 $08 $02     \ ORB_. [R8] 0x02
    $C3                 \ RET
    PREPARE-VEX3
    $74 $07             \ JZ +7
    IS-EVEX
    $75 $06             \ JNZ +06
    $41 $80 $60 $01 $BF \ ANDB_. [R8]. 0x01 0xBF
    $C3                 \ RET
    ASM-PANIC ;
\ High 16 of X register is stored in EVEX.V' (when using VSIB)
: X2 CALL> +V' ;
: X3 CALL> +V' +X ;

: +B CALL>
    PREPARE-REX
    $75 $05             \ JNZ +05
    $41 $80 $08 $01     \ ORB_. [R8] 0x01
    $C3                 \ RET
    PREPARE-VEX3
    $74 $07             \ JZ +7
    IS-EVEX
    $75 $06             \ JNZ +06
    $41 $80 $60 $01 $DF \ ANDB_. [R8]. 0x01 0xDF
    $C3                 \ RET
    ASM-PANIC ;

\ High 16 of B register is stored in EVEX.X
: B=2 CALL>
    PREPARE-EVEX
    $75 $06             \ JNZ +06
    $41 $80 $60 $01 $BF \ ANDB_. [R8]. 0x01 0xBF
    $C3                 \ RET
    ASM-PANIC ;
: B=3 CALL>
    PREPARE-EVEX
    $75 $06             \ JNZ +06
    $41 $80 $60 $01 $9F \ ANDB_. [R8]. 0x01 0x9F
    $C3                 \ RET
    ASM-PANIC ;

: >VVVVV ( AL:0..31 R8:EVEX|VEX2|VEX3 -- R8:EVEX|VEX2|VEX3 ) CALL>
    \ Update VEX/EVEX prefix with vvvv given in AL.
    $3C $10             \ CMP-AL. 0x10
    $72 $05             \ JB +5
    +V'                 \ Set V'
    $24 $0F             \ AND-AL. 0x0F
    $34 $0F             \ XOR-AL. 0x0F
    $C0 $E0 $03         \ SHLB_. RAX 0x03
    IS-VEX2
    $75 $0A             \ JNZ +10
    $41 $80 $60 $01 $87 \ ANDB_. [R8]. 0x01 0x87
    $41 $08 $40 $01     \ ORB> RAX, [R8]. 0x01
    $C3                 \ RET
    IS-VEX3
    $74 $07             \ JZ +7
    IS-EVEX
    $75 $0A             \ JNZ +10
    $41 $80 $60 $02 $87 \ ANDB_. [R8]. 0x02 0x87
    $41 $08 $40 $02     \ ORB> RAX, [R8]. 0x02
    $C3                 \ RET
    ASM-PANIC ;

: V=0  CALL> $50 $B0 $00 >VVVVV $58 ; : V=16 CALL> $50 $B0 $10 >VVVVV $58 ;
: V=1  CALL> $50 $B0 $01 >VVVVV $58 ; : V=17 CALL> $50 $B0 $11 >VVVVV $58 ;
: V=2  CALL> $50 $B0 $02 >VVVVV $58 ; : V=18 CALL> $50 $B0 $12 >VVVVV $58 ;
: V=3  CALL> $50 $B0 $03 >VVVVV $58 ; : V=19 CALL> $50 $B0 $13 >VVVVV $58 ;
: V=4  CALL> $50 $B0 $04 >VVVVV $58 ; : V=20 CALL> $50 $B0 $14 >VVVVV $58 ;
: V=5  CALL> $50 $B0 $05 >VVVVV $58 ; : V=21 CALL> $50 $B0 $15 >VVVVV $58 ;
: V=6  CALL> $50 $B0 $06 >VVVVV $58 ; : V=22 CALL> $50 $B0 $16 >VVVVV $58 ;
: V=7  CALL> $50 $B0 $07 >VVVVV $58 ; : V=23 CALL> $50 $B0 $17 >VVVVV $58 ;
: V=8  CALL> $50 $B0 $08 >VVVVV $58 ; : V=24 CALL> $50 $B0 $18 >VVVVV $58 ;
: V=9  CALL> $50 $B0 $09 >VVVVV $58 ; : V=25 CALL> $50 $B0 $19 >VVVVV $58 ;
: V=10 CALL> $50 $B0 $0A >VVVVV $58 ; : V=26 CALL> $50 $B0 $1A >VVVVV $58 ;
: V=11 CALL> $50 $B0 $0B >VVVVV $58 ; : V=27 CALL> $50 $B0 $1B >VVVVV $58 ;
: V=12 CALL> $50 $B0 $0C >VVVVV $58 ; : V=28 CALL> $50 $B0 $1C >VVVVV $58 ;
: V=13 CALL> $50 $B0 $0D >VVVVV $58 ; : V=29 CALL> $50 $B0 $1D >VVVVV $58 ;
: V=14 CALL> $50 $B0 $0E >VVVVV $58 ; : V=30 CALL> $50 $B0 $1E >VVVVV $58 ;
: V=15 CALL> $50 $B0 $0F >VVVVV $58 ; : V=31 CALL> $50 $B0 $1F >VVVVV $58 ;

: OPB CALL>     NO-REX/VEX     OPCODE ;
: OPW CALL> ^66 NO-REX/VEX     OPCODE ;
: OPL CALL>     NO-REX/VEX     OPCODE ;
: OPQ CALL>        REX/VEX ^48 OPCODE ;

: VEX.SX.NP.0F   CALL> REX/VEX ^C5     ^F8 OPCODE ;
: VEX.SX.66.0F   CALL> REX/VEX ^C5     ^F9 OPCODE ;
: VEX.SX.F3.0F   CALL> REX/VEX ^C5     ^FA OPCODE ;
: VEX.SX.F2.0F   CALL> REX/VEX ^C5     ^FB OPCODE ;
: VEX.SX.NP.0F38 CALL> REX/VEX ^C4 ^E2 ^78 OPCODE ;
: VEX.SX.66.0F38 CALL> REX/VEX ^C4 ^E2 ^79 OPCODE ;
: VEX.SX.F3.0F38 CALL> REX/VEX ^C4 ^E2 ^7A OPCODE ;
: VEX.SX.F2.0F38 CALL> REX/VEX ^C4 ^E2 ^7B OPCODE ;
: VEX.SX.NP.0F3A CALL> REX/VEX ^C4 ^E3 ^78 OPCODE ;
: VEX.SX.66.0F3A CALL> REX/VEX ^C4 ^E3 ^79 OPCODE ;
: VEX.SX.F3.0F3A CALL> REX/VEX ^C4 ^E3 ^7A OPCODE ;
: VEX.SX.F2.0F3A CALL> REX/VEX ^C4 ^E3 ^7B OPCODE ;
: VEX.DX.NP.0F   CALL> REX/VEX ^C4 ^E1 ^F8 OPCODE ;
: VEX.DX.66.0F   CALL> REX/VEX ^C4 ^E1 ^F9 OPCODE ;
: VEX.DX.F3.0F   CALL> REX/VEX ^C4 ^E1 ^FA OPCODE ;
: VEX.DX.F2.0F   CALL> REX/VEX ^C4 ^E1 ^FB OPCODE ;
: VEX.DX.NP.0F38 CALL> REX/VEX ^C4 ^E2 ^F8 OPCODE ;
: VEX.DX.66.0F38 CALL> REX/VEX ^C4 ^E2 ^F9 OPCODE ;
: VEX.DX.F3.0F38 CALL> REX/VEX ^C4 ^E2 ^FA OPCODE ;
: VEX.DX.F2.0F38 CALL> REX/VEX ^C4 ^E2 ^FB OPCODE ;
: VEX.DX.NP.0F3A CALL> REX/VEX ^C4 ^E3 ^F8 OPCODE ;
: VEX.DX.66.0F3A CALL> REX/VEX ^C4 ^E3 ^F9 OPCODE ;
: VEX.DX.F3.0F3A CALL> REX/VEX ^C4 ^E3 ^FA OPCODE ;
: VEX.DX.F2.0F3A CALL> REX/VEX ^C4 ^E3 ^FB OPCODE ;

: VEX.SY.NP.0F   CALL> REX/VEX ^C5     ^FC OPCODE ;
: VEX.SY.66.0F   CALL> REX/VEX ^C5     ^FD OPCODE ;
: VEX.SY.F3.0F   CALL> REX/VEX ^C5     ^FE OPCODE ;
: VEX.SY.F2.0F   CALL> REX/VEX ^C5     ^FF OPCODE ;
: VEX.SY.NP.0F38 CALL> REX/VEX ^C4 ^E2 ^7C OPCODE ;
: VEX.SY.66.0F38 CALL> REX/VEX ^C4 ^E2 ^7D OPCODE ;
: VEX.SY.F3.0F38 CALL> REX/VEX ^C4 ^E2 ^7E OPCODE ;
: VEX.SY.F2.0F38 CALL> REX/VEX ^C4 ^E2 ^7F OPCODE ;
: VEX.SY.NP.0F3A CALL> REX/VEX ^C4 ^E3 ^7C OPCODE ;
: VEX.SY.66.0F3A CALL> REX/VEX ^C4 ^E3 ^7D OPCODE ;
: VEX.SY.F3.0F3A CALL> REX/VEX ^C4 ^E3 ^7E OPCODE ;
: VEX.SY.F2.0F3A CALL> REX/VEX ^C4 ^E3 ^7F OPCODE ;
: VEX.DY.NP.0F   CALL> REX/VEX ^C4 ^E1 ^FC OPCODE ;
: VEX.DY.66.0F   CALL> REX/VEX ^C4 ^E1 ^FD OPCODE ;
: VEX.DY.F3.0F   CALL> REX/VEX ^C4 ^E1 ^FE OPCODE ;
: VEX.DY.F2.0F   CALL> REX/VEX ^C4 ^E1 ^FF OPCODE ;
: VEX.DY.NP.0F38 CALL> REX/VEX ^C4 ^E2 ^FC OPCODE ;
: VEX.DY.66.0F38 CALL> REX/VEX ^C4 ^E2 ^FD OPCODE ;
: VEX.DY.F3.0F38 CALL> REX/VEX ^C4 ^E2 ^FE OPCODE ;
: VEX.DY.F2.0F38 CALL> REX/VEX ^C4 ^E2 ^FF OPCODE ;
: VEX.DY.NP.0F3A CALL> REX/VEX ^C4 ^E3 ^FC OPCODE ;
: VEX.DY.66.0F3A CALL> REX/VEX ^C4 ^E3 ^FD OPCODE ;
: VEX.DY.F3.0F3A CALL> REX/VEX ^C4 ^E3 ^FE OPCODE ;
: VEX.DY.F2.0F3A CALL> REX/VEX ^C4 ^E3 ^FF OPCODE ;

: VEX.SZ.NP.0F   CALL> REX/VEX ^62 ^F1 ^7C ^48 OPCODE ;
: VEX.SZ.66.0F   CALL> REX/VEX ^62 ^F1 ^7D ^48 OPCODE ;
: VEX.SZ.F3.0F   CALL> REX/VEX ^62 ^F1 ^7E ^48 OPCODE ;
: VEX.SZ.F2.0F   CALL> REX/VEX ^62 ^F1 ^7F ^48 OPCODE ;
: VEX.SZ.NP.0F38 CALL> REX/VEX ^62 ^F2 ^7C ^48 OPCODE ;
: VEX.SZ.66.0F38 CALL> REX/VEX ^62 ^F2 ^7D ^48 OPCODE ;
: VEX.SZ.F3.0F38 CALL> REX/VEX ^62 ^F2 ^7E ^48 OPCODE ;
: VEX.SZ.F2.0F38 CALL> REX/VEX ^62 ^F2 ^7F ^48 OPCODE ;
: VEX.SZ.NP.0F3A CALL> REX/VEX ^62 ^F3 ^7C ^48 OPCODE ;
: VEX.SZ.66.0F3A CALL> REX/VEX ^62 ^F3 ^7D ^48 OPCODE ;
: VEX.SZ.F3.0F3A CALL> REX/VEX ^62 ^F3 ^7E ^48 OPCODE ;
: VEX.SZ.F2.0F3A CALL> REX/VEX ^62 ^F3 ^7F ^48 OPCODE ;
: VEX.DZ.NP.0F   CALL> REX/VEX ^62 ^F1 ^FC ^48 OPCODE ;
: VEX.DZ.66.0F   CALL> REX/VEX ^62 ^F1 ^FD ^48 OPCODE ;
: VEX.DZ.F3.0F   CALL> REX/VEX ^62 ^F1 ^FE ^48 OPCODE ;
: VEX.DZ.F2.0F   CALL> REX/VEX ^62 ^F1 ^FF ^48 OPCODE ;
: VEX.DZ.NP.0F38 CALL> REX/VEX ^62 ^F2 ^FC ^48 OPCODE ;
: VEX.DZ.66.0F38 CALL> REX/VEX ^62 ^F2 ^FD ^48 OPCODE ;
: VEX.DZ.F3.0F38 CALL> REX/VEX ^62 ^F2 ^FE ^48 OPCODE ;
: VEX.DZ.F2.0F38 CALL> REX/VEX ^62 ^F2 ^FF ^48 OPCODE ;
: VEX.DZ.NP.0F3A CALL> REX/VEX ^62 ^F3 ^FC ^48 OPCODE ;
: VEX.DZ.66.0F3A CALL> REX/VEX ^62 ^F3 ^FD ^48 OPCODE ;
: VEX.DZ.F3.0F3A CALL> REX/VEX ^62 ^F3 ^FE ^48 OPCODE ;
: VEX.DZ.F2.0F3A CALL> REX/VEX ^62 ^F3 ^FF ^48 OPCODE ;

\ Opcode extensions. These are modrm bytes with the REG field
\ prefilled, and the MOD field set to 3 (which is the default
\ we use).
: /0 ^B0 ^C0 ^AA ; \ MOV AL, 0xC0; STOSB
: /1 ^B0 ^C8 ^AA ; \ MOV AL, 0xC8; STOSB
: /2 ^B0 ^D0 ^AA ; \ MOV AL, 0xD0; STOSB
: /3 ^B0 ^D8 ^AA ; \ MOV AL, 0xD8; STOSB
: /4 ^B0 ^E0 ^AA ; \ MOV AL, 0xE0; STOSB
: /5 ^B0 ^E8 ^AA ; \ MOV AL, 0xE8; STOSB
: /6 ^B0 ^F0 ^AA ; \ MOV AL, 0xF0; STOSB
: /7 ^B0 ^F8 ^AA ; \ MOV AL, 0xF8; STOSB

[ASSEMBLER-DEFINITIONS]

\ Legacy prefixes
: LOCK ^F0 ; : REP ^F3 ; : REPE ^F3 ; : REPNE ^F2 ;
: FS: ^64 ; : GS: ^65 ;

\ Ops that take no arguments.

: NOP OPL ^90 ; : PAUSE ^F3 OPL ^90 ;
: CBW OPW ^98 ; : CWDE OPL ^98 ; : CDQE OPQ ^98 ;
: CWD OPW ^99 ; : CDQ OPL ^99 ; : CQO OPQ ^99 ;
: FWAIT OPB ^9B ;
: PUSHFW OPW ^9C ; : PUSHFQ OPL ^9C ;
: POPFW OPW ^9D ; : POPFQ OPL ^9D ;

:  INSB OPB ^6C ; :  INSW OPW ^A5 ; :  INSD OPL ^A5 ;
: OUTSB OPB ^6E ; : OUTSW OPW ^A5 ; : OUTSD OPL ^A5 ; : MOVSQ OPQ ^A5 ;
: MOVSB OPB ^A4 ; : MOVSW OPW ^A5 ; : MOVSL OPL ^A5 ; : MOVSQ OPQ ^A5 ;
: CMPSB OPB ^A6 ; : CMPSW OPW ^A7 ; : CMPSL OPL ^A7 ; : CMPSQ OPQ ^A7 ;
: STOSB OPB ^AA ; : STOSW OPW ^AB ; : STOSL OPL ^AB ; : STOSQ OPQ ^AB ;
: LODSB OPB ^AC ; : LODSW OPW ^AD ; : LODSL OPL ^AD ; : LODSQ OPQ ^AD ;
: SCASB OPB ^AE ; : SCASW OPW ^AF ; : SCASL OPL ^AF ; : LODSQ OPQ ^AF ;

: RET   OPL ^C3 ;
: LEAVE OPL ^C9 ;
: FRET  OPL ^CB ;
: INT3  OPB ^CC ;
: IRETW OPW ^CF ; : IRETL OPL ^CF ; : IRETQ OPQ ^CF ;

: XLATB   OPB ^D7 ;

:  INB-DX OPB ^EC ; :  INW-DX OPW ^ED ; :  INL-DX OPL ^ED ;
: OUTB-DX OPB ^EE ; : OUTW-DX OPW ^EF ; : OUTL-DX OPL ^EF ;

: INT1    OPB ^F1 ;
: HLT     OPB ^F4 ;
: CMC     OPB ^F5 ; \ Complement carry flag.
: CLC     OPB ^F8 ; \ Clear carry flag
: STC     OPB ^F9 ; \ Set carry flag
: CLI     OPB ^FA ; \ Clear interrupt flag
: STI     OPB ^FB ; \ Set interrput flag
: CLD     OPB ^FC ; \ Clear direction flag
: STD     OPB ^FD ; \ Set direction flag

: SYSCALL OPL ^0F ^05 ;
: RDPMC   OPL ^0F ^33 ;

: PUSHFS  OPL ^0F ^A0 ;
: POPFS   OPL ^0F ^A1 ;
: CPUID   OPL ^0F ^A2 ;
: PUSHGS  OPL ^0F ^A8 ;
: POPGS   OPL ^0F ^A9 ;

\ Ops that expect both a register and modrm argument.
\ E.g.  MOVQ< RBX, [RAX]
: ADDB> OPB ^00 ; : ADCB> OPB ^10 ; : ANDB> OPB ^20 ; : XORB> OPB ^30 ;
: ADDW> OPW ^01 ; : ADCW> OPW ^11 ; : ANDW> OPW ^21 ; : XORW> OPW ^31 ;
: ADDL> OPL ^01 ; : ADCL> OPL ^11 ; : ANDL> OPL ^21 ; : XORL> OPL ^31 ;
: ADDQ> OPQ ^01 ; : ADCQ> OPQ ^11 ; : ANDQ> OPQ ^21 ; : XORQ> OPQ ^31 ;

: ADDB< OPB ^02 ; : ADCB< OPB ^12 ; : ANDB< OPB ^22 ; : XORB< OPB ^32 ;
: ADDW< OPW ^03 ; : ADCW< OPW ^13 ; : ANDW< OPW ^23 ; : XORW< OPW ^33 ;
: ADDL< OPL ^03 ; : ADCL< OPL ^13 ; : ANDL< OPL ^23 ; : XORL< OPL ^33 ;
: ADDQ< OPQ ^03 ; : ADCQ< OPQ ^13 ; : ANDQ< OPQ ^23 ; : XORQ< OPQ ^33 ;

: ORB> OPB ^08 ; : SBBB> OPB ^18 ; : SUBB> OPB ^28 ; : CMPB> OPB ^38 ;
: ORW> OPW ^09 ; : SBBW> OPW ^19 ; : SUBW> OPW ^29 ; : CMPW> OPW ^39 ;
: ORL> OPL ^09 ; : SBBL> OPL ^19 ; : SUBL> OPL ^29 ; : CMPL> OPL ^39 ;
: ORQ> OPQ ^09 ; : SBBQ> OPQ ^19 ; : SUBQ> OPQ ^29 ; : CMPQ> OPQ ^39 ;

: ORB< OPB ^0A ; : SBBB< OPB ^1A ; : SUBB< OPB ^2A ; : CMPB< OPB ^3A ;
: ORW< OPW ^0B ; : SBBW< OPW ^1B ; : SUBW< OPW ^2B ; : CMPW< OPW ^3B ;
: ORL< OPL ^0B ; : SBBL< OPL ^1B ; : SUBL< OPL ^2B ; : CMPL< OPL ^3B ;
: ORQ< OPQ ^0B ; : SBBQ< OPQ ^1B ; : SUBQ< OPQ ^2B ; : CMPQ< OPQ ^3B ;

: TESTB> OPB ^84 ; : XCHGB> OPB ^84 ;
: TESTW> OPW ^85 ; : XCHGW> OPW ^85 ;
: TESTL> OPL ^85 ; : XCHGL> OPL ^85 ;
: TESTQ> OPQ ^85 ; : XCHGQ> OPQ ^85 ;

: MOVB> OPB ^88 ; : MOVB< OPB ^8A ;
: MOVW> OPW ^89 ; : MOVW< OPW ^8B ;
: MOVL> OPL ^89 ; : MOVL< OPL ^8B ;
: MOVQ> OPQ ^89 ; : MOVQ< OPQ ^8B ;

: LEAW< OPW ^8D ;
: LEAL< OPL ^8D ;
: LEAQ< OPQ ^8D ;

:   BTW> OPW ^0F ^A3 ; :   BTL> OPL ^0F ^A3 ; :   BTQ> OPQ ^0F ^A3 ;
: SHLDW> OPW ^0F ^A5 ; : SHLDL> OPL ^0F ^A5 ; : SHLDQ> OPQ ^0F ^A5 ;
:  BTSW> OPW ^0F ^AB ; :  BTSL> OPL ^0F ^AB ; :  BTSQ> OPQ ^0F ^AB ;
: SHRDW> OPW ^0F ^AD ; : SHRDL> OPL ^0F ^AD ; : SHRDQ> OPQ ^0F ^AD ;
: IMULW< OPW ^0F ^AF ; : IMULL< OPL ^0F ^AF ; : IMULQ< OPQ ^0F ^AF ;

: CMPXCHGB> OPB ^0F ^B0 ;
: CMPXCHGW> OPW ^0F ^B1 ;
: CMPXCHGL> OPL ^0F ^B1 ;
: CMPXCHGQ> OPQ ^0F ^B1 ;

:  BTRW> OPW ^0F ^B3 ; :  BTRL> OPL ^0F ^B3 ; :  BTRQ> OPQ ^0F ^B3 ;
:  BTCW> OPW ^0F ^BB ; :  BTCL> OPL ^0F ^BB ; :  BTCQ> OPQ ^0F ^BB ;

: POPCNTW< ^F3 OPW ^0F ^B8 ;
: POPCNTL< ^F3 OPL ^0F ^B8 ;
: POPCNTQ< ^F3 OPQ ^0F ^B8 ;

: BSFW< OPW ^0F ^BC ; : BSFL< OPL ^0F ^BC ; : BSFQ< OPQ ^0F ^BC ;
: BSRW< OPW ^0F ^BD ; : BSRL< OPL ^0F ^BD ; : BSRQ< OPQ ^0F ^BD ;

: TZCNTW< ^F3 OPW ^0F ^BC ; : LZCNTW< ^F3 OPW ^0F ^BD ;
: TZCNTL< ^F3 OPL ^0F ^BC ; : LZCNTL< ^F3 OPL ^0F ^BD ;
: TZCNTQ< ^F3 OPQ ^0F ^BC ; : LZCNTQ< ^F3 OPQ ^0F ^BD ;

: MOVZXWB< OPW ^0F ^B6 ; : MOVSXWB< OPW ^0F ^BE ;
: MOVZXLB< OPL ^0F ^B6 ; : MOVSXLB< OPL ^0F ^BE ;
: MOVZXQB< OPQ ^0F ^B6 ; : MOVSXQB< OPQ ^0F ^BE ;
: MOVZXLW< OPL ^0F ^B7 ; : MOVSXLW< OPL ^0F ^BF ;
: MOVZXQW< OPQ ^0F ^B7 ; : MOVSXQW< OPQ ^0F ^BF ;
( -------------------- ) : MOVSXQL< OPQ ^63     ;

: MOVNTIL> OPL ^0F ^C3 ;
: MOVNTIQ> OPQ ^0F ^C3 ;

\ these need a memory operand
: MOVBEW< OPW ^0F ^38 ^F0 ; : MOVBEW> OPW ^0F ^38 ^F1 ;
: MOVBEL< OPL ^0F ^38 ^F0 ; : MOVBEL> OPL ^0F ^38 ^F1 ;
: MOVBEQ< OPQ ^0F ^39 ^F0 ; : MOVBEQ> OPQ ^0F ^39 ^F1 ;

: CRC32B< ^F2 OPB ^0F ^38 ^F0 ;
: CRC32W< ^F2 OPW ^0F ^38 ^F1 ;
: CRC32L< ^F2 OPL ^0F ^38 ^F1 ;
: CRC32Q< ^F2 OPQ ^0F ^38 ^F1 ;

: ADCXL< ^66 OPL ^0F ^38 ^F6 ;
: ADCXQ< ^66 OPQ ^0F ^38 ^F6 ;
: ADOXL< ^F3 OPL ^0F ^38 ^F6 ;
: ADOXQ< ^F3 OPQ ^0F ^38 ^F6 ;

\ Ops that expect register, modrm, and immediate arguments.
\ (Do not write a comma after the modrm argument!)
\ E.g.  IMULW<. RAX, RCX $0A
: IMULW<. OPW ^6B ; : IMULW<.. OPW ^69 ;
: IMULL<. OPL ^6B ; : IMULL<:  OPL ^69 ;
: IMULQ<. OPQ ^6B ; : IMULQ<:  OPQ ^69 ;

: SHLDW>. OPW ^0F ^A4 ; : SHLDL>. OPL ^0F ^A4 ; : SHLDQ>. OPQ ^0F ^A4 ;
: SHRDW>. OPW ^0F ^AC ; : SHRDL>. OPL ^0F ^AC ; : SHRDQ>. OPQ ^0F ^AC ;

\ Ops that expect an immediate.
: ADD-AL.  OPB ^04 ; : ADC-AL.  OPB ^14 ;
: ADD-AX.. OPW ^05 ; : ADC-AX.. OPW ^15 ;
: ADD-EAX: OPL ^05 ; : ADC-EAX: OPL ^15 ;
: ADD-RAX: OPQ ^05 ; : ADC-RAX: OPQ ^15 ;

: AND-AL.  OPB ^24 ; : XOR-AL.  OPB ^34 ;
: AND-AX.. OPW ^25 ; : XOR-AX.. OPW ^35 ;
: AND-EAX: OPL ^25 ; : XOR-EAX: OPL ^35 ;
: AND-RAX: OPQ ^25 ; : XOR-RAX: OPQ ^35 ;

: OR-AL.  OPB ^0C ; : SBB-AL.  OPB ^1C ;
: OR-AX.. OPW ^0D ; : SBB-AX.. OPW ^1D ;
: OR-EAX: OPL ^0D ; : SBB-EAX: OPL ^1D ;
: OR-RAX: OPQ ^0D ; : SBB-RAX: OPQ ^1D ;

: SUB-AL.  OPB ^2C ; : CMP-AL.  OPB ^3C ;
: SUB-AX.. OPW ^2D ; : CMP-AX.. OPW ^3C ;
: SUB-EAX: OPL ^2D ; : CMP-EAX: OPL ^3D ;
: SUB-RAX: OPQ ^2D ; : CMP-RAX: OPQ ^3D ;

: PUSHW.. OPW ^68 ;
: PUSHQ.  OPL ^6A ; \ Pushes 64-bits even without REX.W prefix
: PUSHQ:  OPL ^68 ; \ Pushes 64-bits even without REX.W prefix

: MOV-AL-@::  OPB ^A0 ; : MOV-AL-!::  OPB ^A2 ;
: MOV-AX-@::  OPW ^A1 ; : MOV-AX-!::  OPW ^A3 ;
: MOV-EAX-@:: OPL ^A1 ; : MOV-EAX-!:: OPL ^A3 ;
: MOV-RAX-@:: OPQ ^A1 ; : MOV-RAX-!:: OPQ ^A3 ;

: TEST-AL.  OPB ^A8 ; : TEST-AX.. OPW ^A9 ;
: TEST-EAX: OPL ^A9 ; : TEST-RAX: OPQ ^A9 ;

: RET..    OPL ^C2 ; \ RET while popping extra bytes
: ENTER... OPL ^C8 ;
: FRET..   OPL ^CA ;
: INT.     OPB ^CD ;

:  INB. OPB ^E4 ; :  INW. OPW ^E5 ; :  INL. OPL ^E5 ;
: OUTB. OPB ^E6 ; : OUTW. OPW ^E7 ; : OUTL. OPL ^E7 ;

\ Short jumps. These expect an immediate signed byte offset.
: JO.  ^70 ; : JNO. ^71 ; : JB.  ^72 ; : JAE. ^73 ;
: JZ.  ^74 ; : JNZ. ^75 ; : JBE. ^76 ; : JA.  ^77 ;
: JS.  ^78 ; : JNS. ^79 ; : JPE. ^7A ; : JPO. ^7B ;
: JL.  ^7C ; : JGE. ^7D ; : JLE. ^7E ; : JG.  ^7F ;
: JMP. ^EB ; : JE.  ^74 ; : JNE. ^75 ;
: LOOPNE. ^E0 ; : LOOPE. ^E1 ; : LOOP. ^E2 ; : JRCXZ. ^E3 ;

\ Near jumps. These expect an immediate signed dword offset.
:  JO: ^0F ^80 ; : JNO: ^0F ^81 ; :  JB: ^0F ^82 ; : JAE: ^0F ^83 ;
:  JZ: ^0F ^84 ; : JNZ: ^0F ^85 ; : JBE: ^0F ^86 ; :  JA: ^0F ^87 ;
:  JS: ^0F ^88 ; : JNS: ^0F ^89 ; : JPE: ^0F ^8A ; : JPO: ^0F ^8B ;
:  JL: ^0F ^8C ; : JGE: ^0F ^8D ; : JLE: ^0F ^8E ; :  JG: ^0F ^8F ;
: JMP: ^E9     ; :  JE: ^0F ^84 ; : JNE: ^0F ^85 ;
: CALL: ^E8 ;

\ Ops that expect a direct register argument.
\ (Do not write a comma after the direct register argument!)
\ E.g. PUSHQ# RAX
\      POPQ# R15
: PUSHW# OPW ^50 ;
: POPW#  OPW ^58 ;
: PUSHQ# OPL ^50 ; \ forced to 64 bits
: POPQ#  OPL ^58 ; \ forced to 64 bits

: XCHGW-AX# OPW ^90 ;
: XCHGL-EAX# OPL ^90 ;
: XCHGQ-RAX# OPQ ^90 ;

: BSWAPL# OPL ^0F ^C8 ;
: BSWAPQ# OPQ ^0F ^C8 ;

: RDFSBASEL# ^F3 OPL ^0F ^AE /0 ;
: RDFSBASEQ# ^F3 OPQ ^0F ^AE /0 ;
: RDGSBASEL# ^F3 OPL ^0F ^AE /1 ;
: RDGSBASEQ# ^F3 OPQ ^0F ^AE /1 ;
: WRFSBASEL# ^F3 OPL ^0F ^AE /2 ;
: WRFSBASEQ# ^F3 OPQ ^0F ^AE /2 ;
: WRGSBASEL# ^F3 OPL ^0F ^AE /3 ;
: WRGSBASEQ# ^F3 OPQ ^0F ^AE /3 ;

\ Ops that expect a direct register argument and an immediate.
\ (Do not write a comma after the direct register argument!)
\ E.g.  MOVL#: RAX $10203040  ( MOV EAX, 10203040h )
\       MOVW#: R13 $1020      ( MOV R13, 1020h )
: MOVB#.  OPB ^B0 ;
: MOVW#.. OPW ^B8 ;
: MOVL#:  OPL ^B8 ;
: MOVQ#:: OPQ ^B8 ;

\ Ops that expect a modrm argument only.

: SHLB_1 OPB ^D0 /4 ; : SHRB_1 OPB ^D0 /5 ; : SARB_1 OPB ^D0 /7 ;
: SHLW_1 OPW ^D1 /4 ; : SHRW_1 OPW ^D1 /5 ; : SARW_1 OPW ^D1 /7 ;
: SHLL_1 OPL ^D1 /4 ; : SHRL_1 OPL ^D1 /5 ; : SARL_1 OPL ^D1 /7 ;
: SHLQ_1 OPQ ^D1 /4 ; : SHRQ_1 OPQ ^D1 /5 ; : SARQ_1 OPQ ^D1 /7 ;

: SHLB_CL OPB ^D2 /4 ; : SHRB_CL OPB ^D2 /5 ; : SARB_CL OPB ^D2 /7 ;
: SHLW_CL OPW ^D3 /4 ; : SHRW_CL OPW ^D3 /5 ; : SARW_CL OPW ^D3 /7 ;
: SHLL_CL OPL ^D3 /4 ; : SHRL_CL OPL ^D3 /5 ; : SARL_CL OPL ^D3 /7 ;
: SHLQ_CL OPQ ^D3 /4 ; : SHRQ_CL OPQ ^D3 /5 ; : SARQ_CL OPQ ^D3 /7 ;

:  POPW_ OPW ^8F /0 ;
: PUSHW_ OPW ^FF /6 ;
:  POPQ_ OPL ^8F /0 ; \ forced to 64 bits
: PUSHQ_ OPL ^FF /6 ; \ forced to 64 bits

: NOTB_ OPB ^F6 /2 ; : NEGB_  OPB ^F6 /3 ;
: NOTW_ OPW ^F7 /2 ; : NEGW_  OPW ^F7 /3 ;
: NOTL_ OPL ^F7 /2 ; : NEGL_  OPL ^F7 /3 ;
: NOTQ_ OPQ ^F7 /2 ; : NEGQ_  OPQ ^F7 /3 ;
: MULB_ OPB ^F6 /4 ; : IMULB_ OPB ^F6 /5 ;
: MULW_ OPW ^F7 /4 ; : IMULW_ OPW ^F7 /5 ;
: MULL_ OPL ^F7 /4 ; : IMULL_ OPL ^F7 /5 ;
: MULQ_ OPQ ^F7 /4 ; : IMULQ_ OPQ ^F7 /5 ;
: DIVB_ OPB ^F6 /6 ; : IDIVB_ OPB ^F6 /7 ;
: DIVW_ OPW ^F7 /6 ; : IDIVW_ OPW ^F7 /7 ;
: DIVL_ OPL ^F7 /6 ; : IDIVL_ OPL ^F7 /7 ;
: DIVQ_ OPQ ^F7 /6 ; : IDIVQ_ OPQ ^F7 /7 ;

: INCB_ OPB ^FE /0 ; : DECB_ OPB ^FE /1 ;
: INCW_ OPW ^FF /0 ; : DECW_ OPW ^FF /1 ;
: INCL_ OPL ^FF /0 ; : DECL_ OPL ^FF /1 ;
: INCQ_ OPQ ^FF /0 ; : DECQ_ OPQ ^FF /1 ;

: CALLQ_ OPL ^FF /2 ; \ forced to 64 bits
: JUMPQ_ OPL ^FF /4 ; \ forced to 64 bits

: SETO_  OPB ^0F ^90 /0 ; : SETNO_ OPB ^0F ^91 /0 ;
: SETB_  OPB ^0F ^92 /0 ; : SETAE_ OPB ^0F ^93 /0 ;
: SETZ_  OPB ^0F ^94 /0 ; : SETNZ_ OPB ^0F ^95 /0 ;
: SETBE_ OPB ^0F ^96 /0 ; : SETA_  OPB ^0F ^97 /0 ;
: SETS_  OPB ^0F ^98 /0 ; : SETNS_ OPB ^0F ^99 /0 ;
: SETPE_ OPB ^0F ^9A /0 ; : SETPO_ OPB ^0F ^9B /0 ;
: SETL_  OPB ^0F ^9C /0 ; : SETGE_ OPB ^0F ^9D /0 ;
: SETLE_ OPB ^0F ^9E /0 ; : SETG_  OPB ^0F ^9F /0 ;

: CMPXCHG8B_  OPL ^0F ^C7 /1 ; \ must take memory operand
: CMPXCHG16B_ OPQ ^0F ^C7 /1 ; \ must take memory operand

\ Ops that expect a modrm argument and an immediate.

: ADDB_.  OPB ^80 /0 ; :  ORB_.  OPB ^80 /1 ;
: ADDW_.  OPW ^83 /0 ; :  ORW_.  OPW ^83 /1 ;
: ADDL_.  OPL ^83 /0 ; :  ORL_.  OPL ^83 /1 ;
: ADDQ_.  OPQ ^83 /0 ; :  ORQ_.  OPQ ^83 /1 ;
: ADDW_.. OPW ^81 /0 ; :  ORW_.. OPW ^81 /1 ;
: ADDL_:  OPL ^81 /0 ; :  ORL_:  OPL ^81 /1 ;
: ADDQ_:  OPQ ^81 /0 ; :  ORQ_:  OPQ ^81 /1 ;

: ADCB_.  OPB ^80 /2 ; : SBBB_.  OPB ^80 /3 ;
: ADCW_.  OPW ^83 /2 ; : SBBW_.  OPW ^83 /3 ;
: ADCL_.  OPL ^83 /2 ; : SBBL_.  OPL ^83 /3 ;
: ADCQ_.  OPQ ^83 /2 ; : SBBQ_.  OPQ ^83 /3 ;
: ADCW_.. OPW ^81 /2 ; : SBBW_.. OPW ^81 /3 ;
: ADCL_:  OPL ^81 /2 ; : SBBL_:  OPL ^81 /3 ;
: ADCQ_:  OPQ ^81 /2 ; : SBBQ_:  OPQ ^81 /3 ;

: ANDB_.  OPB ^80 /4 ; : SUBB_.  OPB ^80 /5 ;
: ANDW_.  OPW ^83 /4 ; : SUBW_.  OPW ^83 /5 ;
: ANDL_.  OPL ^83 /4 ; : SUBL_.  OPL ^83 /5 ;
: ANDQ_.  OPQ ^83 /4 ; : SUBQ_.  OPQ ^83 /5 ;
: ANDW_.. OPW ^81 /4 ; : SUBW_.. OPW ^81 /5 ;
: ANDL_:  OPL ^81 /4 ; : SUBL_:  OPL ^81 /5 ;
: ANDQ_:  OPQ ^81 /4 ; : SUBQ_:  OPQ ^81 /5 ;

: XORB_.  OPB ^80 /6 ; : CMPB_.  OPB ^80 /7 ;
: XORW_.  OPW ^83 /6 ; : CMPW_.  OPW ^83 /7 ;
: XORL_.  OPL ^83 /6 ; : CMPL_.  OPL ^83 /7 ;
: XORQ_.  OPQ ^83 /6 ; : CMPQ_.  OPQ ^83 /7 ;
: XORW_.. OPW ^81 /6 ; : CMPW_.. OPW ^81 /7 ;
: XORL_:  OPL ^81 /6 ; : CMPL_:  OPL ^81 /7 ;
: XORQ_:  OPQ ^81 /6 ; : CMPQ_:  OPQ ^81 /7 ;

: SHLB_. OPB ^C0 /4 ; : SHRB_. OPB ^C0 /5 ; : SARB_. OPB ^C0 /7 ;
: SHLW_. OPW ^C1 /4 ; : SHRW_. OPW ^C1 /5 ; : SARW_. OPW ^C1 /7 ;
: SHLL_. OPL ^C1 /4 ; : SHRL_. OPL ^C1 /5 ; : SARL_. OPL ^C1 /7 ;
: SHLQ_. OPQ ^C1 /4 ; : SHRQ_. OPQ ^C1 /5 ; : SARQ_. OPQ ^C1 /7 ;

: MOVB_.  OPB ^C6 /0 ;
: MOVW_.. OPW ^C7 /0 ;
: MOVL_:  OPL ^C7 /0 ;
: MOVQ_:  OPQ ^C7 /0 ; \ 32-bit immediate sign-extended to 64-bits

: TESTB_.  OPB ^F6 /0 ;
: TESTW_.. OPW ^F7 /0 ;
: TESTL_:  OPL ^F7 /0 ;
: TESTQ_:  OPQ ^F7 /0 ; \ 32-bit immediate sign-extended to 64-bits

:  BTW_. OPW ^0F ^BA /4 ; :  BTL_. OPL ^0F ^BA /4 ; :  BTQ_. OPQ ^0F ^BA /4 ;
: BTSW_. OPW ^0F ^BA /5 ; : BTSL_. OPL ^0F ^BA /5 ; : BTSQ_. OPQ ^0F ^BA /5 ;
: BTRW_. OPW ^0F ^BA /6 ; : BTRL_. OPL ^0F ^BA /6 ; : BTRQ_. OPQ ^0F ^BA /6 ;
: BTCW_. OPW ^0F ^BA /7 ; : BTCL_. OPL ^0F ^BA /7 ; : BTCQ_. OPQ ^0F ^BA /7 ;

\ SSE, AVX

:    MOVUPSX<      OPL ^0F ^10 ; :    MOVUPSX>       OPL ^0F ^11 ;
:    MOVUPDX<  ^66 OPL ^0F ^10 ; :    MOVUPDX>   ^66 OPL ^0F ^11 ;
:   VMOVUPSX< VEX.SX.NP.0F ^10 ; :   VMOVUPSX>  VEX.SX.NP.0F ^11 ;
:   VMOVUPSY< VEX.SY.NP.0F ^10 ; :   VMOVUPSY>  VEX.SY.NP.0F ^11 ;
:   VMOVUPDX< VEX.SX.66.0F ^10 ; :   VMOVUPDX>  VEX.SX.66.0F ^11 ;
:   VMOVUPDY< VEX.SY.66.0F ^10 ; :   VMOVUPDY>  VEX.SY.66.0F ^11 ;
:     MOVSSX<  ^F3 OPL ^0F ^10 ; :     MOVSSX>   ^F3 OPL ^0F ^11 ;
:     MOVSDX<  ^F2 OPL ^0F ^10 ; :     MOVSDX>   ^F2 OPL ^0F ^11 ;
:   VMOVSSXM< VEX.SX.F3.0F ^10 ; :   VMOVSSXM>  VEX.SX.F3.0F ^11 ;
:   VMOVSSXX+ VEX.SX.F3.0F ^10 ; :   VMOVSSXX+> VEX.SX.F3.0F ^11 ;
:   VMOVSDXM< VEX.SX.F2.0F ^10 ; :   VMOVSDXM>  VEX.SX.F2.0F ^11 ;
:   VMOVSDXX+ VEX.SX.F2.0F ^10 ; :   VMOVSDXX+> VEX.SX.F2.0F ^11 ;
:   MOVLPSXM<      OPL ^0F ^12 ; :   MOVLPSXM>       OPL ^0F ^13 ;
:   MOVLPDXM<  ^66 OPL ^0F ^12 ; :   MOVLPDXM>   ^66 OPL ^0F ^13 ;
:  VMOVLPSXM+ VEX.SX.NP.0F ^12 ; :  VMOVLPSXM>  VEX.SX.NP.0F ^13 ;
:  VMOVLPDXM+ VEX.SX.66.0F ^12 ; :  VMOVLPDXM>  VEX.SX.66.0F ^13 ;
:  MOVHLPSXX<      OPL ^0F ^12 ;
: VMOVHLPSXX+ VEX.SX.NP.0F ^12 ;
:  MOVSLDUPX<  ^F3 OPL ^0F ^12 ;
: VMOVSLDUPX< VEX.SX.F3.0F ^12 ;
: VMOVSLDUPY< VEX.SY.F3.0F ^12 ;
:   MOVDDUPX<  ^F2 OPL ^0F ^12 ;
:  VMOVDDUPX< VEX.SX.F2.0F ^12 ;
:  VMOVDDUPY< VEX.SY.F2.0F ^12 ;
:   UNPCKLPS<      OPL ^0F ^14 ; :   UNPCKHPS<       OPL ^0F ^15 ;
:   UNPCKLPD<  ^66 OPL ^0F ^14 ; :   UNPCKHPD<   ^66 OPL ^0F ^15 ;
: VUNPCKLPSX+ VEX.SX.NP.0F ^14 ; : VUNPCKHPSX+  VEX.SX.NP.0F ^15 ;
: VUNPCKLPSY+ VEX.SY.NP.0F ^14 ; : VUNPCKHPSY+  VEX.SY.NP.0F ^15 ;
: VUNPCKLPDX+ VEX.SX.66.0F ^14 ; : VUNPCKHPDX+  VEX.SX.66.0F ^15 ;
: VUNPCKLPDY+ VEX.SY.66.0F ^14 ; : VUNPCKHPDY+  VEX.SY.66.0F ^15 ;
:   MOVHPSXM<      OPL ^0F ^16 ; :   MOVHPSXM>       OPL ^0F ^17 ;
:   MOVHPDXM<  ^66 OPL ^0F ^16 ; :   MOVHPDXM>   ^66 OPL ^0F ^17 ;
:  VMOVHPSXM+ VEX.SX.NP.0F ^16 ; :  VMOVHPSXM>  VEX.SX.NP.0F ^17 ;
:  VMOVHPDXM+ VEX.SX.66.0F ^16 ; :  VMOVHPDXM>  VEX.SX.66.0F ^17 ;
:  MOVLHPSXX<      OPL ^0F ^16 ;
: VMOVLHPSXX+ VEX.SX.NP.0F ^16 ;
:  MOVSHDUPX<  ^F3 OPL ^0F ^16 ;
: VMOVSHDUPX< VEX.SX.F3.0F ^16 ;
: VMOVSHDUPY< VEX.SY.F3.0F ^16 ;

:      MOVAPSX<      OPL ^0F ^28 ; :    MOVAPSX>      OPL ^0F ^29 ;
:      MOVAPDX<  ^66 OPL ^0F ^28 ; :    MOVAPDX>  ^66 OPL ^0F ^29 ;
:     VMOVAPSX< VEX.SX.NP.0F ^28 ; :   VMOVAPSX> VEX.SX.NP.0F ^29 ;
:     VMOVAPDX< VEX.SX.66.0F ^28 ; :   VMOVAPDX> VEX.SX.66.0F ^29 ;
:     VMOVAPSY< VEX.SY.NP.0F ^28 ; :   VMOVAPSY> VEX.SY.NP.0F ^29 ;
:     VMOVAPDY< VEX.SY.66.0F ^28 ; :   VMOVAPDY> VEX.SY.66.0F ^29 ;
:   CVTPI2PSXM<      OPL ^0F ^2A ; :  MOVNTPSXM>      OPL ^0F ^2B ;
:   CVTPI2PDXM<  ^66 OPL ^0F ^2A ; : VMOVNTPSXM> VEX.SX.NP.0F ^2B ;
:   CVTSI2SSXL<  ^F3 OPL ^0F ^2A ; : VMOVNTPSYM> VEX.SY.NP.0F ^2B ;
:   CVTSI2SSXQ<  ^F3 OPQ ^0F ^2A ;
:   CVTSI2SDXL<  ^F2 OPL ^0F ^2A ;
:   CVTSI2SDXQ<  ^F2 OPQ ^0F ^2A ;
:  VCVTSI2SSXL+ VEX.SX.F3.0F ^2A ;
:  VCVTSI2SSXQ+ VEX.DX.F3.0F ^2A ;
:  VCVTSI2SDXL+ VEX.SX.F2.0F ^2A ;
:  VCVTSI2SDXQ+ VEX.DX.F2.0F ^2A ;
:  CVTTSS2SILX<  ^F3 OPL ^0F ^2C ; :  CVTSS2SILX<  ^F3 OPL ^0F ^2D ;
:  CVTTSS2SIQX<  ^F3 OPQ ^0F ^2C ; :  CVTSS2SIQX<  ^F3 OPQ ^0F ^2D ;
:  CVTTSD2SILX<  ^F2 OPL ^0F ^2C ; :  CVTSD2SILX<  ^F2 OPL ^0F ^2D ;
:  CVTTSD2SIQX<  ^F2 OPQ ^0F ^2C ; :  CVTSD2SIQX<  ^F2 OPQ ^0F ^2D ;
: VCVTTSS2SILX< VEX.SX.F3.0F ^2C ; : VCVTSS2SILX< VEX.SX.F3.0F ^2D ;
: VCVTTSS2SIQX< VEX.DX.F3.0F ^2C ; : VCVTSS2SIQX< VEX.DX.F3.0F ^2D ;
: VCVTTSD2SILX< VEX.SX.F2.0F ^2C ; : VCVTSD2SILX< VEX.SX.F2.0F ^2D ;
: VCVTTSD2SIQX< VEX.DX.F2.0F ^2C ; : VCVTSD2SIQX< VEX.DX.F2.0F ^2D ;
:     UCOMISSX<      OPL ^0F ^2E ; :     COMISSX<      OPL ^0F ^2F ;
:     UCOMISDX<  ^66 OPL ^0F ^2E ; :     COMISDX<  ^66 OPL ^0F ^2F ;
:    VUCOMISSX< VEX.SX.NP.0F ^2E ; :    VCOMISSX< VEX.SX.NP.0F ^2F ;
:    VUCOMISDX< VEX.SX.66.0F ^2E ; :    VCOMISDX< VEX.SX.66.0F ^2F ;

:  MOVMSKPSX<       OPL ^0F ^50 ; :  SQRTPSX<       OPL ^0F ^51 ;
:  MOVMSKPDX<   ^66 OPL ^0F ^50 ; :  SQRTPDX<   ^66 OPL ^0F ^51 ;
: VMOVMSKPSX<    VEX.SX.NP.0F ^50 ; : VSQRTPSX<    VEX.SX.NP.0F ^51 ;
: VMOVMSKPSY<    VEX.SY.NP.0F ^50 ; : VSQRTPSY<    VEX.SY.NP.0F ^51 ;
: VMOVMSKPDX< VEX.SX.66.0F ^50 ; : VSQRTPDX< VEX.SX.66.0F ^51 ;
: VMOVMSKPDY< VEX.SY.66.0F ^50 ; : VSQRTPDY< VEX.SY.66.0F ^51 ;
                                  :  SQRTSSX<   ^F3 OPL ^0F ^51 ;
                                  :  SQRTSDX<   ^F2 OPL ^0F ^51 ;
                                  : VSQRTSSX+ VEX.SX.F3.0F ^51 ;
                                  : VSQRTSDX+ VEX.SX.F2.0F ^51 ;

:  RSQRTPSX<       OPL ^0F ^52 ; :  RCPPSX<       OPL ^0F ^53 ;
: VRSQRTPSX<    VEX.SX.NP.0F ^52 ; : VRCPPSX<    VEX.SX.NP.0F ^53 ;
: VRSQRTPSY<    VEX.SY.NP.0F ^52 ; : VRCPPSY<    VEX.SY.NP.0F ^53 ;
:  RSQRTSSX<   ^F3 OPL ^0F ^52 ; :  RCPSSX<   ^F3 OPL ^0F ^53 ;
: VRSQRTSSX+ VEX.SX.F3.0F ^52 ; : VRCPSSX+ VEX.SX.F3.0F ^53 ;

:  ANDPSX<       OPL ^0F ^54 ; :  ANDNPSX<       OPL ^0F ^55 ;
:  ANDPDX<   ^66 OPL ^0F ^54 ; :  ANDNPDX<   ^66 OPL ^0F ^55 ;
: VANDPSX+    VEX.SX.NP.0F ^54 ; : VANDNPSX+    VEX.SX.NP.0F ^55 ;
: VANDPDX+ VEX.SX.66.0F ^54 ; : VANDNPDX+ VEX.SX.66.0F ^55 ;
: VANDPSY+    VEX.SY.NP.0F ^54 ; : VANDNPSY+    VEX.SY.NP.0F ^55 ;
: VANDPDY+ VEX.SY.66.0F ^54 ; : VANDNPDY+ VEX.SY.66.0F ^55 ;

:  ORPSX<       OPL ^0F ^56 ; :  XORPSX<       OPL ^0F ^57 ;
:  ORPDX<   ^66 OPL ^0F ^56 ; :  XORPDX<   ^66 OPL ^0F ^57 ;
: VORPSX+    VEX.SX.NP.0F ^56 ; : VXORPSX+    VEX.SX.NP.0F ^57 ;
: VORPDX+ VEX.SX.66.0F ^56 ; : VXORPDX+ VEX.SX.66.0F ^57 ;
: VORPSY+    VEX.SY.NP.0F ^56 ; : VXORPSY+    VEX.SY.NP.0F ^57 ;
: VORPDY+ VEX.SY.66.0F ^56 ; : VXORPDY+ VEX.SY.66.0F ^57 ;

:  ADDPSX<       OPL ^0F    ^58 ; :  MULPSX<       OPL ^0F    ^59 ;
: VADDPSX+    VEX.SX.NP.0F    ^58 ; : VMULPSX+    VEX.SX.NP.0F    ^59 ;
: VADDPSY+    VEX.SY.NP.0F    ^58 ; : VMULPSY+    VEX.SY.NP.0F    ^59 ;
: VADDPSZ+    VEX.SZ.NP.0F    ^58 ; : VMULPSZ+    VEX.SZ.NP.0F    ^59 ;
:  ADDPDX<   ^66 OPL ^0F    ^58 ; :  MULPDX<   ^66 OPL ^0F    ^59 ;
: VADDPDX+ VEX.DX.66.0F ^58 ; : VMULPDX+ VEX.DX.66.0F ^59 ;
: VADDPDY+ VEX.DY.66.0F ^58 ; : VMULPDY+ VEX.DY.66.0F ^59 ;
: VADDPDZ+ VEX.DZ.66.0F ^58 ; : VMULPDZ+ VEX.DZ.66.0F ^59 ;
:  ADDSSX<   ^F3 OPL ^0F    ^58 ; :  MULSSX<   ^F3 OPL ^0F    ^59 ;
:  ADDSDX<   ^F2 OPL ^0F    ^58 ; :  MULSDX<   ^F2 OPL ^0F    ^59 ;
: VADDSSX+ VEX.SX.F3.0F    ^58 ; : VMULSSX+ VEX.SX.F3.0F    ^59 ;
: VADDSDX+ VEX.DX.F2.0F ^58 ; : VMULSDX+ VEX.DX.F2.0F ^59 ;

:  CVTPS2PDX<       OPL ^0F ^5A ; :   CVTDQ2PSX<       OPL ^0F ^5B ;
:  CVTPD2PSX<   ^66 OPL ^0F ^5A ; :   CVTPS2DQX<   ^66 OPL ^0F ^5B ;
: VCVTPS2PDX<    VEX.SX.NP.0F ^5A ; :  CVTTPS2DQX<   ^F3 OPL ^0F ^5B ;
: VCVTPS2PDY<    VEX.SY.NP.0F ^5A ; :  VCVTDQ2PSX<    VEX.SX.NP.0F ^5B ;
: VCVTPD2PSX< VEX.SX.66.0F ^5A ; :  VCVTDQ2PSY<    VEX.SY.NP.0F ^5B ;
: VCVTPD2PSY< VEX.SY.66.0F ^5A ; :  VCVTPS2DQX< VEX.SX.66.0F ^5B ;
:  CVTSS2SDX<   ^F3 OPL ^0F ^5A ; :  VCVTPS2DQY< VEX.SY.66.0F ^5B ;
:  CVTSD2SSX<   ^F2 OPL ^0F ^5A ; : VCVTTPS2DQX< VEX.SX.F3.0F ^5B ;
: VCVTSS2SDX+ VEX.SX.F3.0F ^5A ; : VCVTTPS2DQY< VEX.SX.F3.0F ^5B ;
: VCVTSD2SSX+ VEX.SX.F2.0F ^5A ;

:  SUBPSX<       OPL ^0F ^5C ; :  MINPSX<       OPL ^0F ^5D ;
:  SUBPDX<   ^66 OPL ^0F ^5C ; :  MINPDX<   ^66 OPL ^0F ^5D ;
: VSUBPSX+    VEX.SX.NP.0F ^5C ; : VMINPSX+    VEX.SX.NP.0F ^5D ;
: VSUBPSY+    VEX.SY.NP.0F ^5C ; : VMINPSY+    VEX.SY.NP.0F ^5D ;
: VSUBPDX+ VEX.SX.66.0F ^5C ; : VMINPDX+ VEX.SX.66.0F ^5D ;
: VSUBPDY+ VEX.SY.66.0F ^5C ; : VMINPDY+ VEX.SY.66.0F ^5D ;
:  SUBSSX<   ^F3 OPL ^0F ^5C ; :  MINSSX<   ^F3 OPL ^0F ^5D ;
:  SUBSDX<   ^F2 OPL ^0F ^5C ; :  MINSDX<   ^F2 OPL ^0F ^5D ;
: VSUBSSX+ VEX.SX.F3.0F ^5C ; : VMINSSX+ VEX.SX.F3.0F ^5D ;
: VSUBSDX+ VEX.SX.F2.0F ^5C ; : VMINSDX+ VEX.SX.F2.0F ^5D ;

:  DIVPSX<       OPL ^0F ^5E ; :  MAXPSX<       OPL ^0F ^5F ;
:  DIVPDX<   ^66 OPL ^0F ^5E ; :  MAXPDX<   ^66 OPL ^0F ^5F ;
: VDIVPSX+    VEX.SX.NP.0F ^5E ; : VMAXPSX+    VEX.SX.NP.0F ^5F ;
: VDIVPSY+    VEX.SY.NP.0F ^5E ; : VMAXPSY+    VEX.SY.NP.0F ^5F ;
: VDIVPDX+ VEX.SX.66.0F ^5E ; : VMAXPDX+ VEX.SX.66.0F ^5F ;
: VDIVPDY+ VEX.SY.66.0F ^5E ; : VMAXPDY+ VEX.SY.66.0F ^5F ;
:  DIVSSX<   ^F3 OPL ^0F ^5E ; :  MAXSSX<   ^F3 OPL ^0F ^5F ;
:  DIVSDX<   ^F2 OPL ^0F ^5E ; :  MAXSDX<   ^F2 OPL ^0F ^5F ;
: VDIVSSX+ VEX.SX.F3.0F ^5E ; : VMAXSSX+ VEX.SX.F3.0F ^5F ;
: VDIVSDX+ VEX.SX.F2.0F ^5E ; : VMAXSDX+ VEX.SX.F2.0F ^5F ;

:  PUNPCKLBWX<   ^66 OPL ^0F ^60 ; :  PUNPCKLWDX<   ^66 OPL ^0F ^61 ;
: VPUNPCKLBWX+ VEX.SX.66.0F ^60 ; : VPUNPCKLWDX+ VEX.SX.66.0F ^61 ;
: VPUNPCKLBWY+ VEX.SY.66.0F ^60 ; : VPUNPCKLWDY+ VEX.SY.66.0F ^61 ;
:  PUNPCKLDQX<   ^66 OPL ^0F ^62 ; :   PACKSSWBX<   ^66 OPL ^0F ^63 ;
: VPUNPCKLDQX+ VEX.SX.66.0F ^62 ; :  VPACKSSWBX+ VEX.SX.66.0F ^63 ;
: VPUNPCKLDQY+ VEX.SY.66.0F ^62 ; :  VPACKSSWBY+ VEX.SY.66.0F ^63 ;
:    PCMPGTBX<   ^66 OPL ^0F ^64 ; :    PCMPGTWX<   ^66 OPL ^0F ^65 ;
:   VPCMPGTBX+ VEX.SX.66.0F ^64 ; :   VPCMPGTWX+ VEX.SX.66.0F ^65 ;
:   VPCMPGTBY+ VEX.SY.66.0F ^64 ; :   VPCMPGTWY+ VEX.SY.66.0F ^65 ;
:    PCMPGTDX<   ^66 OPL ^0F ^66 ; :   PACKUSWBX<   ^66 OPL ^0F ^67 ;
:   VPCMPGTDX+ VEX.SX.66.0F ^66 ; :  VPACKUSWBX+ VEX.SX.66.0F ^67 ;
:   VPCMPGTDY+ VEX.SY.66.0F ^66 ; :  VPACKUSWBY+ VEX.SY.66.0F ^67 ;

:   PUNPCKHBWX<   ^66 OPL ^0F ^68 ; :   PUNPCKHWDX<   ^66 OPL ^0F ^69 ;
:  VPUNPCKHBWX+ VEX.SX.66.0F ^68 ; :  VPUNPCKHWDX+ VEX.SX.66.0F ^69 ;
:  VPUNPCKHBWY+ VEX.SY.66.0F ^68 ; :  VPUNPCKHWDY+ VEX.SY.66.0F ^69 ;
:   PUNPCKHDQX<   ^66 OPL ^0F ^6A ; :    PACKSSDWX<   ^66 OPL ^0F ^6B ;
:  VPUNPCKHDQX+ VEX.SX.66.0F ^6A ; :   VPACKSSDWX+ VEX.SX.66.0F ^6B ;
:  VPUNPCKHDQY+ VEX.SY.66.0F ^6A ; :   VPACKSSDWY+ VEX.SY.66.0F ^6B ;
:  PUNPCKLQDQX<   ^66 OPL ^0F ^6C ; :  PUNPCKHQDQX<   ^66 OPL ^0F ^6D ;
: VPUNPCKLQDQX+ VEX.SX.66.0F ^6C ; : VPUNPCKHQDQX< VEX.SX.66.0F ^6D ;
: VPUNPCKLQDQY+ VEX.SY.66.0F ^6C ; : VPUNPCKHQDQY< VEX.SY.66.0F ^6D ;

:  MOVXL<   ^66 OPL ^0F    ^6E ; :  MOVDQAX<   ^66 OPL ^0F ^6F ;
:  MOVXQ<   ^66 OPQ ^0F    ^6E ; : VMOVDQAX< VEX.SX.66.0F ^6F ;
: VMOVXL< VEX.SX.66.0F    ^6E ; : VMOVDQAY< VEX.SY.66.0F ^6F ;
: VMOVYL< VEX.SY.66.0F    ^6E ; :  MOVDQUX<   ^F3 OPL ^0F ^6F ;
: VMOVXQ< VEX.DX.66.0F ^6E ; : VMOVDQUX< VEX.SX.F3.0F ^6F ;
: VMOVYQ< VEX.DX.66.0F ^6E ; : VMOVDQUY< VEX.SY.F3.0F ^6F ;

:   PSHUFDX<.   ^66 OPL ^0F ^70 ;
:  VPSHUFDX<. VEX.SX.66.0F ^70 ;
:  VPSHUFDY<. VEX.SY.66.0F ^70 ;
:  PSHUFHWX<.   ^F3 OPL ^0F ^70 ;
: VPSHUFHWX<. VEX.SX.F3.0F ^70 ;
: VPSHUFHWY<. VEX.SY.F3.0F ^70 ;
:  PSHUFLWX<.   ^F2 OPL ^0F ^70 ;
: VPSHUFLWX<. VEX.SX.F2.0F ^70 ;
: VPSHUFLWY<. VEX.SY.F2.0F ^70 ;

:  PSRLWX#.    ^66 OPL ^0F ^71 /2 ; :  PSRLDX#.    ^66 OPL ^0F ^72 /2 ;
: VPSRLWX+#. VEX.SX.66.0F ^71 /2 ; : VPSRLDX+#. VEX.SX.66.0F ^72 /2 ;
: VPSRLWY+#. VEX.SY.66.0F ^71 /2 ; : VPSRLDY+#. VEX.SY.66.0F ^72 /2 ;
:  PSRAWX#.    ^66 OPL ^0F ^71 /4 ; :  PSRADX#.    ^66 OPL ^0F ^72 /4 ;
: VPSRAWX+#. VEX.SX.66.0F ^71 /4 ; : VPSRADX+#. VEX.SX.66.0F ^72 /4 ;
: VPSRAWY+#. VEX.SY.66.0F ^71 /4 ; : VPSRADY+#. VEX.SY.66.0F ^72 /4 ;
:  PSLLWX#.    ^66 OPL ^0F ^71 /6 ; :  PSLLDX#.    ^66 OPL ^0F ^72 /6 ;
: VPSLLWX+#. VEX.SX.66.0F ^71 /6 ; : VPSLLDX+#. VEX.SX.66.0F ^72 /6 ;
: VPSLLWY+#. VEX.SY.66.0F ^71 /6 ; : VPSLLDY+#. VEX.SY.66.0F ^72 /6 ;

:   PSRLQX#.    ^66 OPL ^0F ^73 /2 ;
:  VPSRLQX+#. VEX.SX.66.0F ^73 /2 ;
:  VPSRLQY+#. VEX.SY.66.0F ^73 /2 ;
:  PSRLDQX#.    ^66 OPL ^0F ^73 /3 ;
: VPSRLDQX+#. VEX.SX.66.0F ^73 /3 ;
: VPSRLDQY+#. VEX.SY.66.0F ^73 /3 ;
:   PSLLQX#.    ^66 OPL ^0F ^73 /6 ;
:  VPSLLQX+#. VEX.SX.66.0F ^73 /6 ;
:  VPSLLQY+#. VEX.SY.66.0F ^73 /6 ;
:  PSLLDQX#.    ^66 OPL ^0F ^73 /7 ;
: VPSLLDQX+#. VEX.SX.66.0F ^73 /7 ;
: VPSLLDQY+#. VEX.SY.66.0F ^73 /7 ;

:  PCMPEQBX<   ^66 OPL ^0F ^74 ; :  PCMPEQWX<   ^66 OPL ^0F ^75 ;
: VPCMPEQBX+ VEX.SX.66.0F ^74 ; : VPCMPEQWX+ VEX.SX.66.0F ^75 ;
: VPCMPEQBY+ VEX.SY.66.0F ^74 ; : VPCMPEQWY+ VEX.SY.66.0F ^75 ;
:  PCMPEQDX<   ^66 OPL ^0F ^76 ;
: VPCMPEQDX+ VEX.SX.66.0F ^76 ; : VZEROUPPER VEX.SX.NP.0F ^77 ;
: VPCMPEQDY+ VEX.SY.66.0F ^76 ; : VZEROALL   VEX.SY.NP.0F ^77 ;

:  HADDPDX<   ^66 OPL ^0F ^7C ; :  HSUBPDX<   ^66 OPL ^0F ^7D ;
: VHADDPDX+ VEX.SX.66.0F ^7C ; : VHSUBPDX+ VEX.SX.66.0F ^7D ;
: VHADDPDY+ VEX.SY.66.0F ^7C ; : VHSUBPDY+ VEX.SY.66.0F ^7D ;
:  HADDPSX<   ^F2 OPL ^0F ^7C ; :  HSUBPSX<   ^F2 OPL ^0F ^7D ;
: VHADDPSX+ VEX.SX.F2.0F ^7C ; : VHSUBPSX+ VEX.SX.F2.0F ^7D ;
: VHADDPSY+ VEX.SY.F2.0F ^7C ; : VHSUBPSY+ VEX.SY.F2.0F ^7D ;

:   MOVXL>   ^66 OPL ^0F    ^7E ; :  MOVDQAX>   ^66 OPL ^0F ^7F ;
:   MOVXQ>   ^66 OPQ ^0F    ^7E ; : VMOVDQAX> VEX.SX.66.0F ^7F ;
:  VMOVXL> VEX.SX.66.0F    ^7E ; : VMOVDQAY> VEX.SY.66.0F ^7F ;
:  VMOVYL> VEX.SY.66.0F    ^7E ; :  MOVDQUX>   ^F3 OPL ^0F ^7F ;
:  VMOVXQ> VEX.DX.66.0F ^7E ; : VMOVDQUX> VEX.SX.F3.0F ^7F ;
:  VMOVYQ> VEX.DX.66.0F ^7E ; : VMOVDQUY> VEX.SY.F3.0F ^7F ;
:  MOVQXX<   ^F3 OPL ^0F    ^7E ;
: VMOVQXX< VEX.SX.F3.0F    ^7E ;

:  CMPPSX<.       OPL ^0F ^C2 ;
: VCMPPSX+.    VEX.SX.NP.0F ^C2 ;
: VCMPPSY+.    VEX.SY.NP.0F ^C2 ;
:  CMPPDX<.   ^66 OPL ^0F ^C2 ;
: VCMPPDX+. VEX.SX.66.0F ^C2 ;
: VCMPPDY+. VEX.SY.66.0F ^C2 ;
:  CMPSSX<.   ^F3 OPL ^0F ^C2 ;
: VCMPSSX+. VEX.SX.F3.0F ^C2 ;
:  CMPSDX<.   ^F2 OPL ^0F ^C2 ;
: VCMPSDX+. VEX.SX.F2.0F ^C2 ;

:  PINSRXW<.   ^66 OPL ^0F ^C4 ; :  PEXTRWX<.   ^66 OPL ^0F ^C5 ;
: VPINSRXW+. VEX.SX.66.0F ^C4 ; : VPEXTRWX<. VEX.SX.66.0F ^C5 ;

:  SHUFPSX<.       OPL ^0F ^C6 ;
: VSHUFPSX+.    VEX.SX.NP.0F ^C6 ;
: VSHUFPSY+.    VEX.SY.NP.0F ^C6 ;
:  SHUFPDX<.   ^66 OPL ^0F ^C6 ;
: VSHUFPDX+. VEX.SX.66.0F ^C6 ;
: VSHUFPDY+. VEX.SY.66.0F ^C6 ;

:  ADDSUBPDX<   ^66 OPL ^0F ^D0 ; :  PSRLWX<   ^66 OPL ^0F ^D1 ;
: VADDSUBPDX+ VEX.SX.66.0F ^D0 ; : VPSRLWX+ VEX.SX.66.0F ^D1 ;
: VADDSUBPDY+ VEX.SY.66.0F ^D0 ; : VPSRLWY+ VEX.SY.66.0F ^D1 ;
:  ADDSUBPSX<   ^F2 OPL ^0F ^D0 ;
: VADDSUBPSX+ VEX.SX.F2.0F ^D0 ;
: VADDSUBPSY+ VEX.SY.F2.0F ^D0 ;

:  PSRLDX<   ^66 OPL ^0F ^D2 ; :  PSRLQX<   ^66 OPL ^0F ^D3 ;
: VPSRLDX+ VEX.SX.66.0F ^D2 ; : VPSRLQX+ VEX.SX.66.0F ^D3 ;
: VPSRLDY+ VEX.SY.66.0F ^D2 ; : VPSRLQY+ VEX.SY.66.0F ^D3 ;

:  PADDQX<   ^66 OPL ^0F ^D4 ; :  PMULLWX<   ^66 OPL ^0F ^D5 ;
: VPADDQX+ VEX.SX.66.0F ^D4 ; : VPMULLWX+ VEX.SX.66.0F ^D5 ;
: VPADDQY+ VEX.SY.66.0F ^D4 ; : VPMULLWY+ VEX.SY.66.0F ^D5 ;

:  MOVQXX>   ^66 OPL ^0F ^D6 ; :  PMOVMSKBX<   ^66 OPL ^0F ^D7 ;
: VMOVQXX> VEX.SX.66.0F ^D6 ; : VPMOVMSKBX< VEX.SX.66.0F ^D7 ;
                               : VPMOVMSKBY< VEX.SY.66.0F ^D7 ;

:  PSUBUSBX<   ^66 OPL ^0F ^D8 ; :  PSUBUSWX<   ^66 OPL ^0F ^D9 ;
: VPSUBUSBX+ VEX.SX.66.0F ^D8 ; : VPSUBUSWX+ VEX.SX.66.0F ^D9 ;
: VPSUBUSBY+ VEX.SY.66.0F ^D8 ; : VPSUBUSWY+ VEX.SY.66.0F ^D9 ;

:  PMINUBX<   ^66 OPL ^0F ^DA ; :  PANDX<   ^66 OPL ^0F ^DB ;
: VPMINUBX+ VEX.SX.66.0F ^DA ; : VPANDX+ VEX.SX.66.0F ^DB ;
: VPMINUBY+ VEX.SY.66.0F ^DA ; : VPANDY+ VEX.SY.66.0F ^DB ;

:  PADDUSBX<   ^66 OPL ^0F ^DC ; :  PADDUSWX<   ^66 OPL ^0F ^DD ;
: VPADDUSBX+ VEX.SX.66.0F ^DC ; : VPADDUSWX+ VEX.SX.66.0F ^DD ;
: VPADDUSBY+ VEX.SY.66.0F ^DC ; : VPADDUSWY+ VEX.SY.66.0F ^DD ;

:  PMAXUBX<   ^66 OPL ^0F ^DE ; :  PANDNX<   ^66 OPL ^0F ^DF ;
: VPMAXUBX+ VEX.SX.66.0F ^DE ; : VPANDNX+ VEX.SX.66.0F ^DF ;
: VPMAXUBY+ VEX.SY.66.0F ^DE ; : VPANDNY+ VEX.SY.66.0F ^DF ;

:  PAVGBX<   ^66 OPL ^0F ^E0 ; :  PSRAWX<   ^66 OPL ^0F ^E1 ;
: VPAVGBX+ VEX.SX.66.0F ^E0 ; : VPSRAWX+ VEX.SX.66.0F ^E1 ;
: VPAVGBY+ VEX.SY.66.0F ^E0 ; : VPSRAWY+ VEX.SY.66.0F ^E1 ;

:  PSRADX<   ^66 OPL ^0F ^E2 ; :  PAVGWX<   ^66 OPL ^0F ^E3 ;
: VPSRADX+ VEX.SX.66.0F ^E2 ; : VPAVGWX+ VEX.SX.66.0F ^E3 ;
: VPSRADY+ VEX.SY.66.0F ^E2 ; : VPAVGWY+ VEX.SY.66.0F ^E3 ;

:  PMULHUWX<   ^66 OPL ^0F ^E4 ; :  PMULHWX<   ^66 OPL ^0F ^E5 ;
: VPMULHUWX+ VEX.SX.66.0F ^E4 ; : VPMULHWX+ VEX.SX.66.0F ^E5 ;
: VPMULHUWY+ VEX.SY.66.0F ^E4 ; : VPMULHWY+ VEX.SY.66.0F ^E5 ;

:  PCVTTPD2DQX<   ^66 OPL ^0F ^E6 ; :  MOVNTDQXM>   ^66 OPL ^0F ^E7 ;
: VPCVTTPD2DQX< VEX.SX.66.0F ^E6 ; : VMOVNTDQXM> VEX.SX.66.0F ^E7 ;
: VPCVTTPD2DQY< VEX.SY.66.0F ^E6 ; : VMOVNTDQYM> VEX.SY.66.0F ^E7 ;
:   PCVTDQ2PDX<   ^F3 OPL ^0F ^E6 ;
:  VPCVTDQ2PDX< VEX.SX.F3.0F ^E6 ;
:  VPCVTDQ2PDY< VEX.SY.F3.0F ^E6 ;
:   PCVTPD2DQX<   ^F2 OPL ^0F ^E6 ;
:  VPCVTPD2DQX< VEX.SX.F2.0F ^E6 ;
:  VPCVTPD2DQY< VEX.SY.F2.0F ^E6 ;

:  PSUBSBX<   ^66 OPL ^0F ^E8 ; :  PSUBSWX<   ^66 OPL ^0F ^E9 ;
: VPSUBSBX+ VEX.SX.66.0F ^E8 ; : VPSUBSWX+ VEX.SX.66.0F ^E9 ;
: VPSUBSBY+ VEX.SY.66.0F ^E8 ; : VPSUBSWY+ VEX.SY.66.0F ^E9 ;

:  PMINSWX<   ^66 OPL ^0F ^EA ; :  PORX<   ^66 OPL ^0F ^EB ;
: VPMINSWX+ VEX.SX.66.0F ^EA ; : VPORX+ VEX.SX.66.0F ^EB ;
: VPMINSWY+ VEX.SY.66.0F ^EA ; : VPORY+ VEX.SY.66.0F ^EB ;

:  PADDSBX<   ^66 OPL ^0F ^EC ; :  PADDSWX<   ^66 OPL ^0F ^ED ;
: VPADDSBX+ VEX.SX.66.0F ^EC ; : VPADDSWX+ VEX.SX.66.0F ^ED ;
: VPADDSBY+ VEX.SY.66.0F ^EC ; : VPADDSWY+ VEX.SY.66.0F ^ED ;

:  PMAXSWX<   ^66 OPL ^0F ^EE ; :  PXORX<   ^66 OPL ^0F ^EF ;
: VPMAXSWX+ VEX.SX.66.0F ^EE ; : VPXORX+ VEX.SX.66.0F ^EF ;
: VPMAXSWY+ VEX.SY.66.0F ^EE ; : VPXORY+ VEX.SY.66.0F ^EF ;

:  LDDQUXM<   ^F2 OPL ^0F ^F0 ; :  PSLLWX<   ^66 OPL ^0F ^F1 ;
: VLDDQUXM< VEX.SX.F2.0F ^F0 ; : VPSLLWX+ VEX.SX.66.0F ^F1 ;
: VLDDQUYM< VEX.SY.F2.0F ^F0 ; : VPSLLWY+ VEX.SY.66.0F ^F1 ;

:  PSLLDX<   ^66 OPL ^0F ^F2 ; :  PSLLQX<   ^66 OPL ^0F ^F3 ;
: VPSLLDX+ VEX.SX.66.0F ^F2 ; : VPSLLQX+ VEX.SX.66.0F ^F3 ;
: VPSLLDY+ VEX.SY.66.0F ^F2 ; : VPSLLQY+ VEX.SY.66.0F ^F3 ;

:  PMULUDQX<   ^66 OPL ^0F ^F4 ; :  PMADDWDX<   ^66 OPL ^0F ^F5 ;
: VPMULUDQX+ VEX.SX.66.0F ^F4 ; : VPMADDWDX+ VEX.SX.66.0F ^F5 ;
: VPMULUDQY+ VEX.SY.66.0F ^F4 ; : VPMADDWDY+ VEX.SY.66.0F ^F5 ;

:  PSADBWX<   ^66 OPL ^0F ^F6 ; :  MASKMOVDQU<   ^66 OPL ^0F ^F7 ;
: VPSADBWX+ VEX.SX.66.0F ^F6 ; : VMASKMOVDQU< VEX.SX.66.0F ^F7 ;
: VPSADBWY+ VEX.SY.66.0F ^F6 ;

:  PSUBBX<   ^66 OPL ^0F ^F8 ; :  PSUBWX<   ^66 OPL ^0F ^F9 ;
: VPSUBBX+ VEX.SX.66.0F ^F8 ; : VPSUBWX+ VEX.SX.66.0F ^F9 ;
: VPSUBBY+ VEX.SY.66.0F ^F8 ; : VPSUBWY+ VEX.SY.66.0F ^F9 ;

:  PSUBDX<   ^66 OPL ^0F ^FA ; :  PSUBQX<   ^66 OPL ^0F ^FB ;
: VPSUBDX+ VEX.SX.66.0F ^FA ; : VPSUBQX+ VEX.SX.66.0F ^FB ;
: VPSUBDY+ VEX.SY.66.0F ^FA ; : VPSUBQY+ VEX.SY.66.0F ^FB ;

:  PADDBX<   ^66 OPL ^0F ^FC ; :  PADDWX<   ^66 OPL ^0F ^FD ;
: VPADDBX+ VEX.SX.66.0F ^FC ; : VPADDWX+ VEX.SX.66.0F ^FD ;
: VPADDBY+ VEX.SY.66.0F ^FC ; : VPADDWY+ VEX.SY.66.0F ^FD ;

:  PADDDX<   ^66 OPL ^0F ^FE ;
: VPADDDX+ VEX.SX.66.0F ^FE ;
: VPADDDY+ VEX.SY.66.0F ^FE ;

:  PSHUFBX< ^66 OPL ^0F ^38 ^00 ; :  PHADDWX< ^66 OPL ^0F ^38 ^01 ;
: VPSHUFBX+ VEX.SX.66.0F38 ^00 ; : VPHADDWX+ VEX.SX.66.0F38 ^01 ;
: VPSHUFBY+ VEX.SY.66.0F38 ^00 ; : VPHADDWY+ VEX.SY.66.0F38 ^01 ;

:  PHADDDX< ^66 OPL ^0F ^38 ^02 ; :  PHADDSWX< ^66 OPL ^0F ^38 ^03 ;
: VPHADDDX+ VEX.SX.66.0F38 ^02 ; : VPHADDSWX+ VEX.SX.66.0F38 ^03 ;
: VPHADDDY+ VEX.SY.66.0F38 ^02 ; : VPHADDSWY+ VEX.SY.66.0F38 ^03 ;

:  PMADDUBSX< ^66 OPL ^0F ^38 ^04 ; :  PHSUBWX< ^66 OPL ^0F ^38 ^05 ;
: VPMADDUBSX+ VEX.SX.66.0F38 ^04 ; : VPHSUBWX+ VEX.SX.66.0F38 ^05 ;
: VPMADDUBSY+ VEX.SY.66.0F38 ^04 ; : VPHSUBWY+ VEX.SY.66.0F38 ^05 ;

:  PHSUBDX< ^66 OPL ^0F ^38 ^06 ; :  PHSUBSWX< ^66 OPL ^0F ^38 ^07 ;
: VPHSUBDX+ VEX.SX.66.0F38 ^06 ; : VPHSUBSWX+ VEX.SX.66.0F38 ^07 ;
: VPHSUBDY+ VEX.SY.66.0F38 ^06 ; : VPHSUBSWY+ VEX.SY.66.0F38 ^07 ;

:  PSIGNBX< ^66 OPL ^0F ^38 ^08 ; :  PSIGNWX< ^66 OPL ^0F ^38 ^09 ;
: VPSIGNBX+ VEX.SX.66.0F38 ^08 ; : VPSIGNWX+ VEX.SX.66.0F38 ^09 ;
: VPSIGNBY+ VEX.SY.66.0F38 ^08 ; : VPSIGNWY+ VEX.SY.66.0F38 ^09 ;

:  PSIGNDX< ^66 OPL ^0F ^38 ^0A ; :  PMULHRSWX< ^66 OPL ^0F ^38 ^0B ;
: VPSIGNDX+ VEX.SX.66.0F38 ^0A ; : VPMULHRSWX+ VEX.SX.66.0F38 ^0B ;
: VPSIGNDY+ VEX.SY.66.0F38 ^0A ; : VPMULHRSWY+ VEX.SY.66.0F38 ^0B ;

: VPERMILPSX+ VEX.SX.66.0F38 ^0C ; : VPERMILPDX+ VEX.SX.66.0F38 ^0D ;
: VPERMILPSY+ VEX.SY.66.0F38 ^0C ; : VPERMILPDY+ VEX.SY.66.0F38 ^0D ;

: VTESTPSX< VEX.SX.66.0F38 ^0E ; : VTESTPDX< VEX.SX.66.0F38 ^0F ;
: VTESTPSY< VEX.SY.66.0F38 ^0E ; : VTESTPDY< VEX.SY.66.0F38 ^0F ;

: PBLENDVBX< ^66 OPL ^0F ^38 ^10 ; : VCVTPH2PSX< VEX.SX.66.0F38 ^13 ;
                                   : VCVTPH2PSY< VEX.SY.66.0F38 ^13 ;

: BLENDVPSX< ^66 OPL ^0F ^38 ^14 ; : BLENDVPDX< ^66 OPL ^0F ^38 ^15 ;

                                  :  PTESTX< ^66 OPL ^0F ^38 ^17 ;
                                  : VPTESTX< VEX.SX.66.0F38 ^17 ;
: VPERMPSY+ VEX.SY.66.0F38 ^16 ; : VPTESTY< VEX.SY.66.0F38 ^17 ;

: VBROADCASTSSX< VEX.SX.66.0F38 ^18 ;
: VBROADCASTSSY< VEX.SY.66.0F38 ^18 ; : VBROADCASTSDY< VEX.SY.66.0F38 ^19 ;

: VBROADCASTF128YM< VEX.SY.66.0F38 ^1A ;

:  PABSBX< ^66 OPL ^0F ^38 ^1C ; :  PABSWX< ^66 OPL ^0F ^38 ^1D ;
: VPABSBX< VEX.SX.66.0F38 ^1C ; : VPABSWX< VEX.SX.66.0F38 ^1D ;
: VPABSBY< VEX.SY.66.0F38 ^1C ; : VPABSWY< VEX.SY.66.0F38 ^1D ;

:  PABSDX< ^66 OPL ^0F ^38 ^1E ;
: VPABSDX< VEX.SX.66.0F38 ^1E ;
: VPABSDY< VEX.SY.66.0F38 ^1E ;

:   PMOVSXBWX<  ^66 OPL ^0F ^38 ^20 ; :    PMOVSXBDX<  ^66 OPL ^0F ^38 ^21 ;
:  VPMOVSXBWX<  VEX.SX.66.0F38 ^20 ; :   VPMOVSXBDX<  VEX.SX.66.0F38 ^21 ;
:  VPMOVSXBWY<  VEX.SY.66.0F38 ^20 ; :   VPMOVSXBDY<  VEX.SY.66.0F38 ^21 ;
:   PMOVSXBQX<  ^66 OPL ^0F ^38 ^22 ; :    PMOVSXWDX<  ^66 OPL ^0F ^38 ^23 ;
:  VPMOVSXBQX<  VEX.SX.66.0F38 ^22 ; :   VPMOVSXWDX<  VEX.SX.66.0F38 ^23 ;
:  VPMOVSXBQY<  VEX.SY.66.0F38 ^22 ; :   VPMOVSXWDY<  VEX.SY.66.0F38 ^23 ;
:   PMOVSXWQX<  ^66 OPL ^0F ^38 ^24 ; :    PMOVSXDQX<  ^66 OPL ^0F ^38 ^25 ;
:  VPMOVSXWQX<  VEX.SX.66.0F38 ^24 ; :   VPMOVSXDQX<  VEX.SX.66.0F38 ^25 ;
:  VPMOVSXWQY<  VEX.SY.66.0F38 ^24 ; :   VPMOVSXDQY<  VEX.SY.66.0F38 ^25 ;
:     PMULDQX<  ^66 OPL ^0F ^38 ^28 ; :     PCMPEQQX<  ^66 OPL ^0F ^38 ^29 ;
:    VPMULDQX+  VEX.SX.66.0F38 ^28 ; :    VPCMPEQQX+  VEX.SX.66.0F38 ^29 ;
:    VPMULDQY+  VEX.SY.66.0F38 ^28 ; :    VPCMPEQQY+  VEX.SY.66.0F38 ^29 ;
:   MOVNTDQAX<  ^66 OPL ^0F ^38 ^2A ; :    PACKUSDWX<  ^66 OPL ^0F ^39 ^2B ;
:  VMOVNTDQAX<  VEX.SX.66.0F38 ^2A ; :   VPACKUSDWX+  VEX.SX.66.0F38 ^2B ;
:  VMOVNTDQAY<  VEX.SY.66.0F38 ^2A ; :   VPACKUSDWY+  VEX.SY.66.0F38 ^2B ;
: VMASKMOVPSX+  VEX.SX.66.0F38 ^2C ; :  VMASKMOVPDX+  VEX.SX.66.0F38 ^2D ;
: VMASKMOVPSY+  VEX.SY.66.0F38 ^2C ; :  VMASKMOVPDY+  VEX.SY.66.0F38 ^2D ;
: VMASKMOVPSX+> VEX.SX.66.0F38 ^2E ; :  VMASKMOVPDX+> VEX.SX.66.0F38 ^2F ;
: VMASKMOVPSY+> VEX.SY.66.0F38 ^2E ; :  VMASKMOVPDY+> VEX.SY.66.0F38 ^2F ;
:   PMOVZXBWX<  ^66 OPL ^0F ^38 ^30 ; :    PMOVZXBDX<  ^66 OPL ^0F ^38 ^31 ;
:  VPMOVZXBWX<  VEX.SX.66.0F38 ^30 ; :   VPMOVZXBDX<  VEX.SX.66.0F38 ^31 ;
:  VPMOVZXBWY<  VEX.SY.66.0F38 ^30 ; :   VPMOVZXBDY<  VEX.SY.66.0F38 ^31 ;
:   PMOVZXBQX<  ^66 OPL ^0F ^38 ^32 ; :    PMOVZXWDX<  ^66 OPL ^0F ^38 ^33 ;
:  VPMOVZXBQX<  VEX.SX.66.0F38 ^32 ; :   VPMOVZXWDX<  VEX.SX.66.0F38 ^33 ;
:  VPMOVZXBQY<  VEX.SY.66.0F38 ^32 ; :   VPMOVZXWDY<  VEX.SY.66.0F38 ^33 ;
:   PMOVZXWQX<  ^66 OPL ^0F ^38 ^34 ; :    PMOVZXDQX<  ^66 OPL ^0F ^38 ^35 ;
:  VPMOVZXWQX<  VEX.SX.66.0F38 ^34 ; :   VPMOVZXDQX<  VEX.SX.66.0F38 ^35 ;
:  VPMOVZXWQY<  VEX.SY.66.0F38 ^34 ; :   VPMOVZXDQY<  VEX.SY.66.0F38 ^35 ;
                                      :     PCMPGTQX<  ^66 OPL ^0F ^38 ^37 ;
                                      :    VPCMPGTQX+  VEX.SX.66.0F38 ^37 ;
:     VPERMDY+  VEX.SY.66.0F38 ^36 ; :    VPCMPGTQY+  VEX.SY.66.0F38 ^37 ;
:     PMINSBX<  ^66 OPL ^0F ^38 ^38 ; :      PMINSDX<  ^66 OPL ^0F ^38 ^39 ;
:    VPMINSBX+  VEX.SX.66.0F38 ^38 ; :     VPMINSDX+  VEX.SX.66.0F38 ^39 ;
:    VPMINSBY+  VEX.SY.66.0F38 ^38 ; :     VPMINSBY+  VEX.SY.66.0F38 ^39 ;
:     PMINUWX<  ^66 OPL ^0F ^38 ^3A ; :      PMINUDX<  ^66 OPL ^0F ^38 ^3B ;
:    VPMINUWX+  VEX.SX.66.0F38 ^3A ; :     VPMINUDX+  VEX.SX.66.0F38 ^3B ;
:    VPMINUWY+  VEX.SY.66.0F38 ^3A ; :     VPMINUDY+  VEX.SY.66.0F38 ^3B ;
:     PMAXSBX<  ^66 OPL ^0F ^38 ^3C ; :      PMAXSDX<  ^66 OPL ^0F ^38 ^3D ;
:    VPMAXSBX+  VEX.SX.66.0F38 ^3C ; :     VPMAXSDX+  VEX.SX.66.0F38 ^3D ;
:    VPMAXSBY+  VEX.SY.66.0F38 ^3C ; :     VPMAXSBY+  VEX.SY.66.0F38 ^3D ;
:     PMAXUWX<  ^66 OPL ^0F ^38 ^3E ; :      PMAXUDX<  ^66 OPL ^0F ^38 ^3F ;
:    VPMAXUWX+  VEX.SX.66.0F38 ^3E ; :     VPMAXUDX+  VEX.SX.66.0F38 ^3F ;
:    VPMAXUWY+  VEX.SY.66.0F38 ^3E ; :     VPMAXUDY+  VEX.SY.66.0F38 ^3F ;
:     PMULLDX<  ^66 OPL ^0F ^38 ^40 ; :  PHMINPOSUWX<  ^66 OPL ^0F ^38 ^41 ;
:    VPMULLDX+  VEX.SX.66.0F38 ^40 ; : VPHMINPOSUWX<  VEX.SX.66.0F38 ^41 ;
:    VPMULLDY+  VEX.SY.66.0F38 ^40 ;
                                      :   VPSRLVDX+ VEX.SX.66.0F38    ^45 ;
                                      :   VPSRLVDY+ VEX.SY.66.0F38    ^45 ;
                                      :   VPSRLVQX+ VEX.DX.66.0F38 ^45 ;
                                      :   VPSRLVQY+ VEX.DY.66.0F38 ^45 ;
:    VPSRAVDX+  VEX.SX.66.0F38 ^46 ; :   VPSLLVDX+ VEX.SX.66.0F38    ^47 ;
:    VPSRAVDY+  VEX.SY.66.0F38 ^46 ; :   VPSLLVDY+ VEX.SY.66.0F38    ^47 ;
                                      :   VPSLLVQX+ VEX.DX.66.0F38 ^47 ;
                                      :   VPSLLVQY+ VEX.DY.66.0F38 ^47 ;

: VPMASKMOVDX+  VEX.SX.66.0F38    ^8C ;
: VPMASKMOVDY+  VEX.SY.66.0F38    ^8C ;
: VPMASKMOVQX+  VEX.DX.66.0F38 ^8C ;
: VPMASKMOVQY+  VEX.DY.66.0F38 ^8C ;
: VPMASKMOVDX+> VEX.SX.66.0F38    ^8E ;
: VPMASKMOVDY+> VEX.SY.66.0F38    ^8E ;
: VPMASKMOVQX+> VEX.DX.66.0F38 ^8E ;
: VPMASKMOVQY+> VEX.DY.66.0F38 ^8E ;

: VPGATHERDDX+ VEX.SX.66.0F38    ^90 ; : VPGATHERQDX+ VEX.SX.66.0F38    ^91 ;
: VPGATHERDDY+ VEX.SY.66.0F38    ^90 ; : VPGATHERQDY+ VEX.SY.66.0F38    ^91 ;
: VPGATHERDQX+ VEX.DX.66.0F38 ^90 ; : VPGATHERQQX+ VEX.DX.66.0F38 ^91 ;
: VPGATHERDQY+ VEX.DY.66.0F38 ^90 ; : VPGATHERQQY+ VEX.DY.66.0F38 ^91 ;
: VGATHERDPSX+ VEX.SX.66.0F38    ^92 ; : VGATHERQPSX+ VEX.SX.66.0F38    ^93 ;
: VGATHERDPSY+ VEX.SY.66.0F38    ^92 ; : VGATHERQPSY+ VEX.SY.66.0F38    ^93 ;
: VGATHERDPDX+ VEX.DX.66.0F38 ^92 ; : VGATHERQPDX+ VEX.DX.66.0F38 ^93 ;
: VGATHERDPDY+ VEX.DY.66.0F38 ^92 ; : VGATHERQPDY+ VEX.DY.66.0F38 ^93 ;

: VFMADDSUB132PSX+ VEX.SX.66.0F38    ^96 ;
: VFMADDSUB132PSY+ VEX.SY.66.0F38    ^96 ;
: VFMADDSUB132PDX+ VEX.DX.66.0F38 ^96 ;
: VFMADDSUB132PDY+ VEX.DY.66.0F38 ^96 ;
: VFMSUBADD132PSX+ VEX.SX.66.0F38    ^97 ;
: VFMSUBADD132PSY+ VEX.SY.66.0F38    ^97 ;
: VFMSUBADD132PDX+ VEX.DX.66.0F38 ^97 ;
: VFMSUBADD132PDY+ VEX.DY.66.0F38 ^97 ;
:    VFMADD132PSX+ VEX.SX.66.0F38    ^98 ;
:    VFMADD132PSY+ VEX.SY.66.0F38    ^98 ;
:    VFMADD132PDX+ VEX.DX.66.0F38 ^98 ;
:    VFMADD132PDY+ VEX.DY.66.0F38 ^98 ;
:    VFMADD132SSX+ VEX.SX.66.0F38    ^99 ;
:    VFMADD132SSY+ VEX.SY.66.0F38    ^99 ;
:    VFMADD132SDX+ VEX.DX.66.0F38 ^99 ;
:    VFMADD132SDY+ VEX.DY.66.0F38 ^99 ;
:    VFMSUB132PSX+ VEX.SX.66.0F38    ^9A ;
:    VFMSUB132PSY+ VEX.SY.66.0F38    ^9A ;
:    VFMSUB132PDX+ VEX.DX.66.0F38 ^9A ;
:    VFMSUB132PDY+ VEX.DY.66.0F38 ^9A ;
:    VFMSUB132SSX+ VEX.SX.66.0F38    ^9B ;
:    VFMSUB132SSY+ VEX.SY.66.0F38    ^9B ;
:    VFMSUB132SDX+ VEX.DX.66.0F38 ^9B ;
:    VFMSUB132SDY+ VEX.DY.66.0F38 ^9B ;
:   VFNMADD132PSX+ VEX.SX.66.0F38    ^9C ;
:   VFNMADD132PSY+ VEX.SY.66.0F38    ^9C ;
:   VFNMADD132PDX+ VEX.DX.66.0F38 ^9C ;
:   VFNMADD132PDY+ VEX.DY.66.0F38 ^9C ;
:   VFNMADD132SSX+ VEX.SX.66.0F38    ^9D ;
:   VFNMADD132SSY+ VEX.SY.66.0F38    ^9D ;
:   VFNMADD132SDX+ VEX.DX.66.0F38 ^9D ;
:   VFNMADD132SDY+ VEX.DY.66.0F38 ^9D ;
:   VFNMSUB132PSX+ VEX.SX.66.0F38    ^9E ;
:   VFNMSUB132PSY+ VEX.SY.66.0F38    ^9E ;
:   VFNMSUB132PDX+ VEX.DX.66.0F38 ^9E ;
:   VFNMSUB132PDY+ VEX.DY.66.0F38 ^9E ;
:   VFNMSUB132SSX+ VEX.SX.66.0F38    ^9F ;
:   VFNMSUB132SSY+ VEX.SY.66.0F38    ^9F ;
:   VFNMSUB132SDX+ VEX.DX.66.0F38 ^9F ;
:   VFNMSUB132SDY+ VEX.DY.66.0F38 ^9F ;
: VFMADDSUB213PSX+ VEX.SX.66.0F38    ^A6 ;
: VFMADDSUB213PSY+ VEX.SY.66.0F38    ^A6 ;
: VFMADDSUB213PDX+ VEX.DX.66.0F38 ^A6 ;
: VFMADDSUB213PDY+ VEX.DY.66.0F38 ^A6 ;
: VFMSUBADD213PSX+ VEX.SX.66.0F38    ^A7 ;
: VFMSUBADD213PSY+ VEX.SY.66.0F38    ^A7 ;
: VFMSUBADD213PDX+ VEX.DX.66.0F38 ^A7 ;
: VFMSUBADD213PDY+ VEX.DY.66.0F38 ^A7 ;
:    VFMADD213PSX+ VEX.SX.66.0F38    ^A8 ;
:    VFMADD213PSY+ VEX.SY.66.0F38    ^A8 ;
:    VFMADD213PDX+ VEX.DX.66.0F38 ^A8 ;
:    VFMADD213PDY+ VEX.DY.66.0F38 ^A8 ;
:    VFMADD213SSX+ VEX.SX.66.0F38    ^A9 ;
:    VFMADD213SSY+ VEX.SY.66.0F38    ^A9 ;
:    VFMADD213SDX+ VEX.DX.66.0F38 ^A9 ;
:    VFMADD213SDY+ VEX.DY.66.0F38 ^A9 ;
:    VFMSUB213PSX+ VEX.SX.66.0F38    ^AA ;
:    VFMSUB213PSY+ VEX.SY.66.0F38    ^AA ;
:    VFMSUB213PDX+ VEX.DX.66.0F38 ^AA ;
:    VFMSUB213PDY+ VEX.DY.66.0F38 ^AA ;
:    VFMSUB213SSX+ VEX.SX.66.0F38    ^AB ;
:    VFMSUB213SSY+ VEX.SY.66.0F38    ^AB ;
:    VFMSUB213SDX+ VEX.DX.66.0F38 ^AB ;
:    VFMSUB213SDY+ VEX.DY.66.0F38 ^AB ;
:   VFNMADD213PSX+ VEX.SX.66.0F38    ^AC ;
:   VFNMADD213PSY+ VEX.SY.66.0F38    ^AC ;
:   VFNMADD213PDX+ VEX.DX.66.0F38 ^AC ;
:   VFNMADD213PDY+ VEX.DY.66.0F38 ^AC ;
:   VFNMADD213SSX+ VEX.SX.66.0F38    ^AD ;
:   VFNMADD213SSY+ VEX.SY.66.0F38    ^AD ;
:   VFNMADD213SDX+ VEX.DX.66.0F38 ^AD ;
:   VFNMADD213SDY+ VEX.DY.66.0F38 ^AD ;
:   VFNMSUB213PSX+ VEX.SX.66.0F38    ^AE ;
:   VFNMSUB213PSY+ VEX.SY.66.0F38    ^AE ;
:   VFNMSUB213PDX+ VEX.DX.66.0F38 ^AE ;
:   VFNMSUB213PDY+ VEX.DY.66.0F38 ^AE ;
:   VFNMSUB213SSX+ VEX.SX.66.0F38    ^AF ;
:   VFNMSUB213SSY+ VEX.SY.66.0F38    ^AF ;
:   VFNMSUB213SDX+ VEX.DX.66.0F38 ^AF ;
:   VFNMSUB213SDY+ VEX.DY.66.0F38 ^AF ;
: VFMADDSUB231PSX+ VEX.SX.66.0F38    ^B6 ;
: VFMADDSUB231PSY+ VEX.SY.66.0F38    ^B6 ;
: VFMADDSUB231PDX+ VEX.DX.66.0F38 ^B6 ;
: VFMADDSUB231PDY+ VEX.DY.66.0F38 ^B6 ;
: VFMSUBADD231PSX+ VEX.SX.66.0F38    ^B7 ;
: VFMSUBADD231PSY+ VEX.SY.66.0F38    ^B7 ;
: VFMSUBADD231PDX+ VEX.DX.66.0F38 ^B7 ;
: VFMSUBADD231PDY+ VEX.DY.66.0F38 ^B7 ;
:    VFMADD231PSX+ VEX.SX.66.0F38    ^B8 ;
:    VFMADD231PSY+ VEX.SY.66.0F38    ^B8 ;
:    VFMADD231PDX+ VEX.DX.66.0F38 ^B8 ;
:    VFMADD231PDY+ VEX.DY.66.0F38 ^B8 ;
:    VFMADD231SSX+ VEX.SX.66.0F38    ^B9 ;
:    VFMADD231SSY+ VEX.SY.66.0F38    ^B9 ;
:    VFMADD231SDX+ VEX.DX.66.0F38 ^B9 ;
:    VFMADD231SDY+ VEX.DY.66.0F38 ^B9 ;
:    VFMSUB231PSX+ VEX.SX.66.0F38    ^BA ;
:    VFMSUB231PSY+ VEX.SY.66.0F38    ^BA ;
:    VFMSUB231PDX+ VEX.DX.66.0F38 ^BA ;
:    VFMSUB231PDY+ VEX.DY.66.0F38 ^BA ;
:    VFMSUB231SSX+ VEX.SX.66.0F38    ^BB ;
:    VFMSUB231SSY+ VEX.SY.66.0F38    ^BB ;
:    VFMSUB231SDX+ VEX.DX.66.0F38 ^BB ;
:    VFMSUB231SDY+ VEX.DY.66.0F38 ^BB ;
:   VFNMADD231PSX+ VEX.SX.66.0F38    ^BC ;
:   VFNMADD231PSY+ VEX.SY.66.0F38    ^BC ;
:   VFNMADD231PDX+ VEX.DX.66.0F38 ^BC ;
:   VFNMADD231PDY+ VEX.DY.66.0F38 ^BC ;
:   VFNMADD231SSX+ VEX.SX.66.0F38    ^BD ;
:   VFNMADD231SSY+ VEX.SY.66.0F38    ^BD ;
:   VFNMADD231SDX+ VEX.DX.66.0F38 ^BD ;
:   VFNMADD231SDY+ VEX.DY.66.0F38 ^BD ;
:   VFNMSUB231PSX+ VEX.SX.66.0F38    ^BE ;
:   VFNMSUB231PSY+ VEX.SY.66.0F38    ^BE ;
:   VFNMSUB231PDX+ VEX.DX.66.0F38 ^BE ;
:   VFNMSUB231PDY+ VEX.DY.66.0F38 ^BE ;
:   VFNMSUB231SSX+ VEX.SX.66.0F38    ^BF ;
:   VFNMSUB231SSY+ VEX.SY.66.0F38    ^BF ;
:   VFNMSUB231SDX+ VEX.DX.66.0F38 ^BF ;
:   VFNMSUB231SDY+ VEX.DY.66.0F38 ^BF ;

:  SHA1NEXTEX< OPL ^0F ^38 ^C8 ; :    SHA1MSG1X< OPL ^0F ^38 ^C9 ;
:   SHA1MSG2X< OPL ^0F ^38 ^CA ; : SHA256RNDS2X< OPL ^0F ^38 ^CB ;
: SHA256MSG1X< OPL ^0F ^38 ^CC ; :  SHA256MSG2X< OPL ^0F ^38 ^CD ;

                                  :      AESIMCX< ^66 OPL ^0F ^38 ^DB ;
                                  :     VAESIMCX< VEX.SX.66.0F38 ^DB ;
:  AESENCX< ^66 OPL ^0F ^38 ^DC ; :  AESENCLASTX< ^66 OPL ^0F ^38 ^DD ;
: VAESENCX+ VEX.SX.66.0F38 ^DC ; : VAESENCLASTX+ VEX.SX.66.0F38 ^DD ;
:  AESDECX< ^66 OPL ^0F ^38 ^DE ; :  AESDECLASTX< ^66 OPL ^0F ^38 ^DF ;
: VAESDECX+ VEX.SX.66.0F38 ^DE ; : VAESDECLASTX+ VEX.SX.66.0F38 ^DF ;

: ANDNL+ VEX.SX.NP.0F38    ^F2 ;
: ANDNQ+ VEX.DX.NP.0F38 ^F2 ;

:   BLSRL+_ VEX.SX.NP.0F38    ^F3 /1 ;
:   BLSRQ+_ VEX.DX.NP.0F38 ^F3 /1 ;
: BLSMSKL+_ VEX.SX.NP.0F38    ^F3 /2 ;
: BLSMSKQ+_ VEX.DX.NP.0F38 ^F3 /2 ;
:   BLSIL+_ VEX.SX.NP.0F38    ^F3 /3 ;
:   BLSIQ+_ VEX.DX.NP.0F38 ^F3 /3 ;

:      BZHIL+ VEX.SX.NP.0F38    ^F5 ; :     BEXTRL+     VEX.SX.NP.0F38    ^F7 ;
:      BZHIQ+ VEX.DX.NP.0F38 ^F5 ; :     BEXTRQ+     VEX.DX.NP.0F38 ^F7 ;
:      PEXTL+ VEX.SX.F3.0F38    ^F5 ; :      SHLXL+  VEX.SX.66.0F38    ^F7 ;
:      PEXTQ+ VEX.DX.F3.0F38 ^F5 ; :      SHLXQ+  VEX.DX.66.0F38 ^F7 ;
:      PDEPL+ VEX.SX.F2.0F38    ^F5 ; :      SARXL+  VEX.SX.F3.0F38    ^F7 ;
:      PDEPQ+ VEX.DX.F2.0F38 ^F5 ; :      SARXQ+  VEX.DX.F3.0F38 ^F7 ;
:      MULXL+ VEX.SX.F2.0F38    ^F6 ; :      SHRXL+  VEX.SX.F2.0F38    ^F7 ;
:      MULXQ+ VEX.DX.F2.0F38 ^F6 ; :      SHRXQ+  VEX.DX.F2.0F38 ^F7 ;

:   VPERMQY<. VEX.DY.66.0F3A ^00 ; :   VPERMPDY<. VEX.DY.66.0F3A ^01 ;
: VPBLENDDX+. VEX.SX.66.0F3A    ^02 ;
: VPBLENDDY+. VEX.SY.66.0F3A    ^02 ;
:  VPERMILPSX<. VEX.SX.66.0F3A  ^04 ; :    VPERMILPDX<. VEX.SX.66.0F3A ^05 ;
:  VPERMILPSY<. VEX.SY.66.0F3A  ^04 ; :    VPERMILPDY<. VEX.SY.66.0F3A ^05 ;
: VPERM2F128Y+. VEX.SY.66.0F3A  ^06 ;
:  ROUNDPSX<. ^66 OPL ^0F ^3A    ^08 ; :      ROUNDPDX<. ^66 OPL ^0F ^3A ^09 ;
: VROUNDPSX<. VEX.SX.66.0F3A    ^08 ; :     VROUNDPDX<. VEX.SX.66.0F3A ^09 ;
: VROUNDPSY<. VEX.SY.66.0F3A    ^08 ; :     VROUNDPDY<. VEX.SY.66.0F3A ^09 ;
:  ROUNDSSX<. ^66 OPL ^0F ^3A    ^0A ; :      ROUNDSDX<. ^66 OPL ^0F ^3A ^0B ;
: VROUNDSSX+. VEX.SX.66.0F3A    ^0A ; :     VROUNDSDX+. VEX.SX.66.0F3A ^0B ;
: VROUNDSSY+. VEX.SY.66.0F3A    ^0A ; :     VROUNDSDY+. VEX.SY.66.0F3A ^0B ;
:  BLENDPSX<. ^66 OPL ^0F ^3A    ^0C ; :      BLENDPDX<. ^66 OPL ^0F ^3A ^0C ;
: VBLENDPSX+. VEX.SX.66.0F3A    ^0C ; :     VBLENDPDX+. VEX.SX.66.0F3A ^0C ;
: VBLENDPSY+. VEX.SY.66.0F3A    ^0C ; :     VBLENDPDY+. VEX.SY.66.0F3A ^0C ;
:  PBLENDWX<. ^66 OPL ^0F ^3A    ^0E ; :      PALIGNRX<. ^66 OPL ^0F ^3A ^0F ;
: VPBLENDWX+. VEX.SX.66.0F3A    ^0E ; :     VPALIGNRX+. VEX.SX.66.0F3A ^0F ;
: VPBLENDWY+. VEX.SY.66.0F3A    ^0E ; :     VPALIGNRY+. VEX.SY.66.0F3A ^0F ;
:   PEXTRBX>. ^66 OPL ^0F ^3A    ^14 ; :       PEXTRWX>. ^66 OPL ^0F ^3A ^15 ;
:  VPEXTRBX>. VEX.SX.66.0F3A    ^14 ; :      VPEXTRWX>. VEX.SX.66.0F3A ^15 ;
:   PEXTRDX>. ^66 OPL ^0F ^3A    ^16 ; :    EXTRACTPSX>. ^66 OPL ^0F ^3A ^17 ;
:  VPEXTRDX>. VEX.SX.66.0F3A    ^16 ; :   VEXTRACTPSX>. VEX.SX.66.0F3A ^17 ;
:   PEXTRQX>. ^66 OPQ ^0F ^3A    ^16 ;
:  VPEXTRQX>. VEX.DX.66.0F3A ^16 ;
: VINSERTF128Y+. VEX.SY.66.0F3A ^18 ; : VEXTRACTF128Y>. VEX.SY.66.0F3A ^19 ;
                                       :    VCVTPS2PHX>. VEX.SX.66.0F3A ^1D ;
                                       :    VCVTPS2PHY>. VEX.SY.66.0F3A ^1D ;
:      PINSRBX<. ^66 OPL ^0F ^3A ^20 ; :     INSERTPSX<. ^66 OPL ^0F ^3A ^21 ;
:     VPINSRBX+. VEX.SX.66.0F3A ^20 ; :    VINSERTPSX+. VEX.SX.66.0F3A ^21 ;
:      PINSRDX<. ^66 OPL ^0F ^3A ^22 ;
:     VPINSRDX+. VEX.SX.66.0F3A ^22 ;
:   PINSRQX<. ^66 OPQ ^0F ^3A    ^22 ;
:  VPINSRQX+. VEX.DX.66.0F3A ^22 ;
: VINSERTI128Y+. VEX.SY.66.0F3A ^38 ; : VEXTRACTI128Y>. VEX.SY.66.0F3A ^39 ;
:        DPPSX<. ^66 OPL ^0F ^3A ^40 ; :         DPPDX<. ^66 OPL ^0F ^3A ^41 ;
:       VDPPSX+. VEX.SX.66.0F3A ^40 ; :        VDPPDX+. VEX.SX.66.0F3A ^41 ;
:       VDPPSY+. VEX.SY.66.0F3A ^40 ;
:     MPSADBWX<. ^66 OPL ^0F ^3A ^42 ;
:    VMPSADBWX+. VEX.SX.66.0F3A ^42 ;
:    VMPSADBWY+. VEX.SY.66.0F3A ^42 ;
:   PCLMULQDQX<. ^66 OPL ^0F ^3A ^44 ;
:  VPCLMULQDQX<. VEX.SX.66.0F3A ^44 ;
:  VPERM2I128Y+. VEX.SY.66.0F3A ^46 ;
:   VBLENDVPSX+. VEX.SX.66.0F3A ^4A ; :    VBLENDVPDX+. VEX.SX.66.0F3A ^4D ;
:   VBLENDVPSY+. VEX.SY.66.0F3A ^4A ; :    VBLENDVPDY+. VEX.SY.66.0F3A ^4D ;
:   VPBLENDVBX+. VEX.SX.66.0F3A ^4C ;
:   VPBLENDVBY+. VEX.SY.66.0F3A ^4C ;
:   PCMPESTRMX<. ^66 OPL ^0F ^3A ^60 ; :    PCMPESTRIX<. ^66 OPL ^0F ^3A ^61 ;
:  VPCMPESTRMX<. VEX.SX.66.0F3A ^60 ; :   VPCMPESTRIX<. VEX.SX.66.0F3A ^61 ;
:   PCMPISTRMX<. ^66 OPL ^0F ^3A ^62 ; :    PCMPISTRIX<. ^66 OPL ^0F ^3A ^63 ;
:  VPCMPISTRMX<. VEX.SX.66.0F3A ^62 ; :   VPCMPISTRIX<. VEX.SX.66.0F3A ^63 ;
:   SHA1RNDS4X<.     OPL ^0F ^3A ^CC ;
                                       :  AESKEYGENASX<. ^66 OPL ^0F ^3A ^DF ;
                                       : VAESKEYGENASX<. VEX.SX.66.0F3A ^DF ;
:     RORXL<. VEX.SX.F2.0F3A    ^F0 ;
:     RORXQ<. VEX.DX.F2.0F3A ^F0 ;

[ASSEMBLER-DEFINITIONS]

\ Encoding the ModR/M byte starting with the REG field.
\ We set MOD=3 by default, so we can reuse plain register names
\ for some operands (see above).
: RAX, ^C0     ; : RCX, ^C8     ; : RDX, ^D0     ; : RBX, ^D8     ;
: RSP, ^E0     ; : RBP, ^E8     ; : RSI, ^F0     ; : RDI, ^F8     ;
: R8,  ^C0 R=1 ; : R9,  ^C8 R=1 ; : R10, ^D0 R=1 ; : R11, ^D8 R=1 ;
: R12, ^E0 R=1 ; : R13, ^E8 R=1 ; : R14, ^F0 R=1 ; : R15, ^F8 R=1 ;

\ Vector registers. Whether it is XMM, YMM, ZMM depends on the op.
: V0,  ^C0     ; : V1,  ^C8     ; : V2,  ^D0     ; : V3,  ^D8     ;
: V4,  ^E0     ; : V5,  ^E8     ; : V6,  ^F0     ; : V7,  ^F8     ;
: V8,  ^C0 R=1 ; : V9,  ^C8 R=1 ; : V10, ^D0 R=1 ; : V11, ^D8 R=1 ;
: V12, ^E0 R=1 ; : V13, ^E8 R=1 ; : V14, ^F0 R=1 ; : V15, ^F8 R=1 ;
: V16, ^C0 R=2 ; : V17, ^C8 R=2 ; : V18, ^D0 R=2 ; : V19, ^D8 R=2 ;
: V20, ^E0 R=2 ; : V21, ^E8 R=2 ; : V22, ^F0 R=2 ; : V23, ^F8 R=2 ;
: V24, ^C0 R=3 ; : V25, ^C8 R=3 ; : V26, ^D0 R=3 ; : V27, ^D8 R=3 ;
: V28, ^E0 R=3 ; : V29, ^E8 R=3 ; : V30, ^F0 R=3 ; : V31, ^F8 R=3 ;

: K0, ^C0 ; : K1, ^C8 ; : K2, ^D0 ; : K3, ^D8 ;
: K4, ^E0 ; : K5, ^E8 ; : K6, ^F0 ; : K7, ^F8 ;

\ "2nd source register" in AVX instructions

: V0+  V=0  ; : V1+  V=1  ; : V2+  V=2  ; : V3+  V=3  ;
: V4+  V=4  ; : V5+  V=5  ; : V6+  V=6  ; : V7+  V=7  ;
: V8+  V=8  ; : V9+  V=9  ; : V10+ V=10 ; : V11+ V=11 ;
: V12+ V=12 ; : V13+ V=13 ; : V14+ V=14 ; : V15+ V=15 ;
: V16+ V=16 ; : V17+ V=17 ; : V18+ V=18 ; : V19+ V=19 ;
: V20+ V=20 ; : V21+ V=21 ; : V22+ V=22 ; : V23+ V=23 ;
: V24+ V=24 ; : V25+ V=25 ; : V26+ V=26 ; : V27+ V=27 ;
: V28+ V=28 ; : V29+ V=29 ; : V30+ V=30 ; : V31+ V=31 ;

: K0+ V=0 ; : K1+ V=1 ; : K2+ V=2 ; : K3+ V=3 ;
: K4+ V=4 ; : K5+ V=5 ; : K6+ V=6 ; : K7+ V=7 ;

[KERNEL-DEFINITIONS]

\ Modify ModR/M byte to set MOD field.
: MOD=0 ^80 ^77 ^FF ^C0 ; \ XOR [RDI-1], 0xC0
: MOD=1 ^80 ^77 ^FF ^80 ; \ XOR [RDI-1], 0x80
: MOD=2 ^80 ^77 ^FF ^40 ; \ XOR [RDI-1], 0x40
: MOD=3                 ; \ default MOD is 3

\ Modify ModR/M byte to set RM field.
\ (This is also used by SIB encoding to modify the BASE field.)
: RM=0                 ; \ default RM is 0
: RM=1 ^80 ^77 ^FF ^01 ; \ XOR [RDI-1], 0x01
: RM=2 ^80 ^77 ^FF ^02 ; \ XOR [RDI-1], 0x02
: RM=3 ^80 ^77 ^FF ^03 ; \ XOR [RDI-1], 0x03
: RM=4 ^80 ^77 ^FF ^04 ; \ XOR [RDI-1], 0x04
: RM=5 ^80 ^77 ^FF ^05 ; \ XOR [RDI-1], 0x05
: RM=6 ^80 ^77 ^FF ^06 ; \ XOR [RDI-1], 0x06
: RM=7 ^80 ^77 ^FF ^07 ; \ XOR [RDI-1], 0x07

\ Modify the previous byte (ModR/M) to encode the appropriate SIB mode.
: SIB=0 ^80 ^77 ^FE ^C4 ; \ set prev MOD=0 RM=4
: SIB=1 ^80 ^77 ^FE ^84 ; \ set prev MOD=1 RM=4
: SIB=2 ^80 ^77 ^FE ^44 ; \ set prev MOD=2 RM=4

[ASSEMBLER-DEFINITIONS]

\ Direct register addressing.
: RAX MOD=3 RM=0 ; : R8  MOD=3 RM=0 +B ;
: RCX MOD=3 RM=1 ; : R9  MOD=3 RM=1 +B ;
: RDX MOD=3 RM=2 ; : R10 MOD=3 RM=2 +B ;
: RBX MOD=3 RM=3 ; : R11 MOD=3 RM=3 +B ;
: RSP MOD=3 RM=4 ; : R12 MOD=3 RM=4 +B ;
: RBP MOD=3 RM=5 ; : R13 MOD=3 RM=5 +B ;
: RSI MOD=3 RM=6 ; : R14 MOD=3 RM=6 +B ;
: RDI MOD=3 RM=7 ; : R15 MOD=3 RM=7 +B ;

: V0  MOD=3 RM=0    ; : V16 MOD=3 RM=0 B=2 ;
: V1  MOD=3 RM=1    ; : V17 MOD=3 RM=1 B=2 ;
: V2  MOD=3 RM=2    ; : V18 MOD=3 RM=2 B=2 ;
: V3  MOD=3 RM=3    ; : V19 MOD=3 RM=3 B=2 ;
: V4  MOD=3 RM=4    ; : V20 MOD=3 RM=4 B=2 ;
: V5  MOD=3 RM=5    ; : V21 MOD=3 RM=5 B=2 ;
: V6  MOD=3 RM=6    ; : V22 MOD=3 RM=6 B=2 ;
: V7  MOD=3 RM=7    ; : V23 MOD=3 RM=7 B=2 ;
: V8  MOD=3 RM=0 +B ; : V24 MOD=3 RM=0 B=3 ;
: V9  MOD=3 RM=1 +B ; : V25 MOD=3 RM=1 B=3 ;
: V10 MOD=3 RM=2 +B ; : V26 MOD=3 RM=2 B=3 ;
: V11 MOD=3 RM=3 +B ; : V27 MOD=3 RM=3 B=3 ;
: V12 MOD=3 RM=4 +B ; : V28 MOD=3 RM=4 B=3 ;
: V13 MOD=3 RM=5 +B ; : V29 MOD=3 RM=5 B=3 ;
: V14 MOD=3 RM=6 +B ; : V30 MOD=3 RM=6 B=3 ;
: V15 MOD=3 RM=7 +B ; : V31 MOD=3 RM=7 B=3 ;

: K0 MOD=3 RM=0 ; : K1 MOD=3 RM=1 ;
: K2 MOD=3 RM=2 ; : K3 MOD=3 RM=3 ;
: K4 MOD=3 RM=4 ; : K5 MOD=3 RM=5 ;
: K6 MOD=3 RM=6 ; : K7 MOD=3 RM=7 ;

\ Indirect addressing, with or without offsets.
: [RAX]  MOD=0 RM=0    ; : [RAX]. MOD=1 RM=0    ; : [RAX]: MOD=2 RM=0    ;
: [RCX]  MOD=0 RM=1    ; : [RCX]. MOD=1 RM=1    ; : [RCX]: MOD=2 RM=1    ;
: [RDX]  MOD=0 RM=2    ; : [RDX]. MOD=1 RM=2    ; : [RDX]: MOD=2 RM=2    ;
: [RBX]  MOD=0 RM=3    ; : [RBX]. MOD=1 RM=3    ; : [RBX]: MOD=2 RM=3    ;
( --------------------------------- SIB -------------------------------- )
: [RIP]: MOD=0 RM=5    ; : [RBP]. MOD=1 RM=5    ; : [RBP]: MOD=2 RM=5    ;
: [RSI]  MOD=0 RM=6    ; : [RSI]. MOD=1 RM=6    ; : [RSI]: MOD=2 RM=6    ;
: [RDI]  MOD=0 RM=7    ; : [RDI]. MOD=1 RM=7    ; : [RDI]: MOD=2 RM=7    ;
: [R8]   MOD=0 RM=0 +B ; : [R8].  MOD=1 RM=0 +B ; : [R8]:  MOD=2 RM=0 +B ;
: [R9]   MOD=0 RM=1 +B ; : [R9].  MOD=1 RM=1 +B ; : [R9]:  MOD=2 RM=1 +B ;
: [R10]  MOD=0 RM=2 +B ; : [R10]. MOD=1 RM=2 +B ; : [R10]: MOD=2 RM=2 +B ;
: [R11]  MOD=0 RM=3 +B ; : [R11]. MOD=1 RM=3 +B ; : [R11]: MOD=2 RM=3 +B ;
( --------------------------------- SIB -------------------------------- )
( -------- RIP ------- ) : [R13]. MOD=1 RM=5 +B ; : [R13]: MOD=2 RM=5 +B ;
: [R14]  MOD=0 RM=6 +B ; : [R14]. MOD=1 RM=6 +B ; : [R14]: MOD=2 RM=6 +B ;
: [R15]  MOD=0 RM=7 +B ; : [R15]. MOD=1 RM=7 +B ; : [R15]: MOD=2 RM=7 +B ;

\ Start encoding a SIB byte, starting with the SCALE and INDEX fields.
: [1*RAX+ ^00    ; : [2*RAX+ ^40    ; : [4*RAX+ ^80    ; : [8*RAX+ ^C0    ;
: [1*RCX+ ^08    ; : [2*RCX+ ^48    ; : [4*RCX+ ^88    ; : [8*RCX+ ^C8    ;
: [1*RDX+ ^10    ; : [2*RDX+ ^50    ; : [4*RDX+ ^90    ; : [8*RDX+ ^D0    ;
: [1*RBX+ ^18    ; : [2*RBX+ ^58    ; : [4*RBX+ ^98    ; : [8*RBX+ ^D8    ;
: [0+     ^20    ; : [2*0+   ^60    ; : [4*0+   ^A0    ; : [8*0+   ^E0    ;
: [1*RBP+ ^28    ; : [2*RBP+ ^68    ; : [4*RBP+ ^A8    ; : [8*RBP+ ^E8    ;
: [1*RSI+ ^30    ; : [2*RSI+ ^70    ; : [4*RSI+ ^B0    ; : [8*RSI+ ^F0    ;
: [1*RDI+ ^38    ; : [2*RDI+ ^78    ; : [4*RDI+ ^B8    ; : [8*RDI+ ^F8    ;
: [1*R8+  ^00 +X ; : [2*R8+  ^40 +X ; : [4*R8+  ^80 +X ; : [8*R8+  ^C0 +X ;
: [1*R9+  ^08 +X ; : [2*R9+  ^48 +X ; : [4*R9+  ^88 +X ; : [8*R9+  ^C8 +X ;
: [1*R10+ ^10 +X ; : [2*R10+ ^50 +X ; : [4*R10+ ^90 +X ; : [8*R10+ ^D0 +X ;
: [1*R11+ ^18 +X ; : [2*R10+ ^58 +X ; : [4*R11+ ^98 +X ; : [8*R11+ ^D8 +X ;
: [1*R12+ ^20 +X ; : [2*R12+ ^60 +X ; : [4*R12+ ^A0 +X ; : [8*R12+ ^E0 +X ;
: [1*R13+ ^28 +X ; : [2*R13+ ^68 +X ; : [4*R13+ ^A8 +X ; : [8*R13+ ^E8 +X ;
: [1*R14+ ^30 +X ; : [2*R14+ ^70 +X ; : [4*R14+ ^B0 +X ; : [8*R14+ ^F0 +X ;
: [1*R15+ ^38 +X ; : [2*R15+ ^78 +X ; : [4*R15+ ^B8 +X ; : [8*R15+ ^F8 +X ;

\ Start encoding a VSIB byte, used in a few AVX instructions.
: [1*V0+  ^00    ; : [2*V0+  ^40    ; : [4*V0+  ^80    ; : [8*V0+  ^C0    ;
: [1*V1+  ^08    ; : [2*V1+  ^48    ; : [4*V1+  ^88    ; : [8*V1+  ^C8    ;
: [1*V2+  ^10    ; : [2*V2+  ^50    ; : [4*V2+  ^90    ; : [8*V2+  ^D0    ;
: [1*V3+  ^18    ; : [2*V3+  ^58    ; : [4*V3+  ^98    ; : [8*V3+  ^D8    ;
: [1*V4+  ^20    ; : [2*V4+  ^60    ; : [4*V4+  ^A0    ; : [8*V4+  ^E0    ;
: [1*V5+  ^28    ; : [2*V5+  ^68    ; : [4*V5+  ^A8    ; : [8*V5+  ^E8    ;
: [1*V6+  ^30    ; : [2*V6+  ^70    ; : [4*V6+  ^B0    ; : [8*V6+  ^F0    ;
: [1*V7+  ^38    ; : [2*V7+  ^78    ; : [4*V7+  ^B8    ; : [8*V7+  ^F8    ;
: [1*V8+  ^00 +X ; : [2*V8+  ^40 +X ; : [4*V8+  ^80 +X ; : [8*V8+  ^C0 +X ;
: [1*V9+  ^08 +X ; : [2*V9+  ^48 +X ; : [4*V9+  ^88 +X ; : [8*V9+  ^C8 +X ;
: [1*V10+ ^10 +X ; : [2*V10+ ^50 +X ; : [4*V10+ ^90 +X ; : [8*V10+ ^D0 +X ;
: [1*V11+ ^18 +X ; : [2*V10+ ^58 +X ; : [4*V11+ ^98 +X ; : [8*V11+ ^D8 +X ;
: [1*V12+ ^20 +X ; : [2*V12+ ^60 +X ; : [4*V12+ ^A0 +X ; : [8*V12+ ^E0 +X ;
: [1*V13+ ^28 +X ; : [2*V13+ ^68 +X ; : [4*V13+ ^A8 +X ; : [8*V13+ ^E8 +X ;
: [1*V14+ ^30 +X ; : [2*V14+ ^70 +X ; : [4*V14+ ^B0 +X ; : [8*V14+ ^F0 +X ;
: [1*V15+ ^38 +X ; : [2*V15+ ^78 +X ; : [4*V15+ ^B8 +X ; : [8*V15+ ^F8 +X ;
: [1*V16+ ^00 X2 ; : [2*V16+ ^40 X2 ; : [4*V16+ ^80 X2 ; : [8*V16+ ^C0 X2 ;
: [1*V17+ ^08 X2 ; : [2*V17+ ^48 X2 ; : [4*V17+ ^88 X2 ; : [8*V17+ ^C8 X2 ;
: [1*V18+ ^10 X2 ; : [2*V18+ ^50 X2 ; : [4*V18+ ^90 X2 ; : [8*V18+ ^D0 X2 ;
: [1*V19+ ^18 X2 ; : [2*V19+ ^58 X2 ; : [4*V19+ ^98 X2 ; : [8*V19+ ^D8 X2 ;
: [1*V20+ ^20 X2 ; : [2*V20+ ^60 X2 ; : [4*V20+ ^A0 X2 ; : [8*V20+ ^E0 X2 ;
: [1*V21+ ^28 X2 ; : [2*V21+ ^68 X2 ; : [4*V21+ ^A8 X2 ; : [8*V21+ ^E8 X2 ;
: [1*V22+ ^30 X2 ; : [2*V22+ ^70 X2 ; : [4*V22+ ^B0 X2 ; : [8*V22+ ^F0 X2 ;
: [1*V23+ ^38 X2 ; : [2*V23+ ^78 X2 ; : [4*V23+ ^B8 X2 ; : [8*V23+ ^F8 X2 ;
: [1*V24+ ^00 X3 ; : [2*V24+ ^40 X3 ; : [4*V24+ ^80 X3 ; : [8*V24+ ^C0 X3 ;
: [1*V25+ ^08 X3 ; : [2*V25+ ^48 X3 ; : [4*V25+ ^88 X3 ; : [8*V25+ ^C8 X3 ;
: [1*V26+ ^10 X3 ; : [2*V26+ ^50 X3 ; : [4*V26+ ^90 X3 ; : [8*V26+ ^D0 X3 ;
: [1*V27+ ^18 X3 ; : [2*V27+ ^58 X3 ; : [4*V27+ ^98 X3 ; : [8*V27+ ^D8 X3 ;
: [1*V28+ ^20 X3 ; : [2*V28+ ^60 X3 ; : [4*V28+ ^A0 X3 ; : [8*V28+ ^E0 X3 ;
: [1*V29+ ^28 X3 ; : [2*V29+ ^68 X3 ; : [4*V29+ ^A8 X3 ; : [8*V29+ ^E8 X3 ;
: [1*V30+ ^30 X3 ; : [2*V30+ ^70 X3 ; : [4*V30+ ^B0 X3 ; : [8*V30+ ^F0 X3 ;
: [1*V31+ ^38 X3 ; : [2*V31+ ^78 X3 ; : [4*V31+ ^B8 X3 ; : [8*V31+ ^F8 X3 ;

\ Finish encoding SIB byte. This sets the BASE field and MOD from ModR/M.
: RAX] SIB=0 RM=0    ; : RAX]. SIB=1 RM=0    ; : RAX]: SIB=2 RM=0    ;
: RCX] SIB=0 RM=1    ; : RCX]. SIB=1 RM=1    ; : RCX]: SIB=2 RM=1    ;
: RDX] SIB=0 RM=2    ; : RDX]. SIB=1 RM=2    ; : RDX]: SIB=2 RM=2    ;
: RBX] SIB=0 RM=3    ; : RBX]. SIB=1 RM=3    ; : RBX]: SIB=2 RM=3    ;
: RSP] SIB=0 RM=4    ; : RSP]. SIB=1 RM=4    ; : RSP]: SIB=2 RM=4    ;
: 0]:  SIB=0 RM=5    ; : RBP]. SIB=1 RM=5    ; : RBP]: SIB=2 RM=5    ;
: RSI] SIB=0 RM=6    ; : RSI]. SIB=1 RM=6    ; : RSI]: SIB=2 RM=6    ;
: RDI] SIB=0 RM=7    ; : RDI]. SIB=1 RM=7    ; : RDI]: SIB=2 RM=7    ;
: R8]  SIB=0 RM=0 +B ; : R8].  SIB=1 RM=0 +B ; : R8]:  SIB=2 RM=0 +B ;
: R9]  SIB=0 RM=1 +B ; : R9].  SIB=1 RM=1 +B ; : R9]:  SIB=2 RM=1 +B ;
: R10] SIB=0 RM=2 +B ; : R10]. SIB=1 RM=2 +B ; : R10]: SIB=2 RM=2 +B ;
: R11] SIB=0 RM=3 +B ; : R11]. SIB=1 RM=3 +B ; : R11]: SIB=2 RM=3 +B ;
: R12] SIB=0 RM=4 +B ; : R12]. SIB=1 RM=4 +B ; : R12]: SIB=2 RM=4 +B ;
( ------------------ ) : R13]. SIB=1 RM=5 +B ; : R13]: SIB=2 RM=5 +B ;
: R14] SIB=0 RM=6 +B ; : R14]. SIB=1 RM=6 +B ; : R14]: SIB=2 RM=6 +B ;
: R15] SIB=0 RM=7 +B ; : R15]. SIB=1 RM=7 +B ; : R15]: SIB=2 RM=7 +B ;

\ Fourth operand for VBLENDV/VPBLENDV ops (occupies the immediate slot).
: $V0  ^00 ; : $V1  ^10 ; : $V2  ^20 ; : $V3  ^30 ;
: $V4  ^40 ; : $V5  ^50 ; : $V6  ^60 ; : $V7  ^70 ;
: $V8  ^80 ; : $V9  ^90 ; : $V10 ^A0 ; : $V11 ^B0 ;
: $V12 ^C0 ; : $V13 ^D0 ; : $V14 ^E0 ; : $V15 ^F0 ;

