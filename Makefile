ENGINE := src/yotta.s
CHAPTERS := src/0-preamble.fs \
			src/1-assembler.fs \
			src/2-forth.fs \
			src/3-main.fs

.PHONY: run time
run: bin/yotta
	bin/yotta
time: bin/yotta
	time bin/yotta

bin/yotta: bin/yotta.o
bin/yotta.o: $(ENGINE) $(CHAPTERS) | bin
	nasm -f macho64 -o bin/yotta.o $(ENGINE)

bin:
	mkdir bin

