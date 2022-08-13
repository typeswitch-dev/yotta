[KERNEL-DEFINITIONS]

: MAIN CALL>
    \ 'H' EMIT 'e' EMIT 'l' EMIT 'l' EMIT 'o' EMIT '!' EMIT CR
    U. $0A . CR
    I8_MAX .
    I8_MIN . CR
    I16_MAX .
    I16_MIN . CR
    I32_MAX .
    I32_MIN . CR
    I64_MAX .
    I64_MIN . CR
    I8_MAX I8_MAX = . CR
    DUP
    MOVQ< RBX, RDI
    SUBQ< RBX, [RBP]: $USER.PROG
    . CR
    ;

[ MAIN BYE ]

