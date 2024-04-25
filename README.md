# Yotta

Welcome to Yotta! Yotta is a minimalistic forth-like language. Its most distinctive feature is that it comes with very few primitives:

- `$XX` emits the machine code `XX` given in hexadecimal.
- `^XX` emits machine code that emits machine code `XX`.
- `: A` defines a new word `A`

The rest is built-up from there. Yotta is split into several parts, in the `src` directory:

- [`src/0-preamble.fs`](https://github.com/typeswitch-dev/yotta/blob/main/src/0-preamble.fs) is the zeroth chapter. It defines some bare necessities, like `;` and comments.
- [`src/1-assembler.fs`](https://github.com/typeswitch-dev/yotta/blob/main/src/1-assembler.fs) defines an x86-64 assembler inside of yotta.
- [`src/2-forth.fs`](https://github.com/typeswitch-dev/yotta/blob/main/src/2-forth.fs) defines a set of forth-like words, built on top of the assembler.
- [`src/3-main.fs`](https://github.com/typeswitch-dev/yotta/blob/main/src/3-main.fs) is the program.
- [`src/yotta.s`](https://github.com/typeswitch-dev/yotta/blob/main/src/yotta.s) contains the engine, written in NASM x86-64 assembly.

If this piques your interest, I recommend reading the [preamble](https://github.com/typeswitch-dev/yotta/blob/main/src/0-preamble.fs) and the [forth chapter](https://github.com/typeswitch-dev/yotta/blob/main/src/2-forth.fs), and the rest as interests you.

This code supports x86-64 linux and macos:

- To build and run on linux, use `make run-linux`.
- To build and run on intel macs, use `make run-macos`.
