ENGINE := src/yotta.s
CHAPTERS := src/0-preamble.fs \
			src/1-assembler.fs \
			src/2-forth.fs \
			src/3-main.fs

.PHONY: run-linux run-macos time-linux time-macos
run-linux: bin/yotta-linux-x64
	bin/yotta-linux-x64
time-linux: bin/yotta-linux-x64
	time bin/yotta-linux-x64
run-macos: bin/yotta-macos-x64
	bin/yotta-macos-x64
time-macos: bin/yotta-macos-x64
	time bin/yotta-macos-x64

bin/constants: src/constants.c
	gcc -o bin/constants src/constants.c

bin/yotta-macos-x64: bin/yotta-macos-x64.o
bin/yotta-macos-x64.o: $(ENGINE) $(CHAPTERS) | bin
	nasm -DOS_MACOS -f macho64 -o bin/yotta-macos-x64.o $(ENGINE)

bin/yotta-linux-x64: bin/yotta-linux-x64.o
bin/yotta-linux-x64.o: $(ENGINE) $(CHAPTERS) | bin
	nasm -DOS_LINUX -f elf64 -o bin/yotta-linux-x64.o $(ENGINE)

bin:
	mkdir bin
