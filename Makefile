.PHONY: run time
run: bin/yotta
	bin/yotta
time: bin/yotta
	time bin/yotta

bin/yotta: bin/yotta.o
bin/yotta.o: src/yotta.s src/yotta.fs | bin
	nasm -f macho64 -o bin/yotta.o src/yotta.s

bin:
	mkdir bin

