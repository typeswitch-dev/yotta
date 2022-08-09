.PHONY: run
run: bin/yotta
	bin/yotta

bin/yotta: bin/yotta.o
bin/yotta.o: src/yotta.s src/yotta.fs
	nasm -f macho64 -o bin/yotta.o src/yotta.s

