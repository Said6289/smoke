CC=clang

.PHONY: odin
odin: stb_truetype.a
	odin build . -out:smoke -opt:2

stb_truetype.o: stb_truetype.c stb_truetype.h
	$(CC) -c -O2 -fPIC -o stb_truetype.o stb_truetype.c

stb_truetype.a: stb_truetype.o
	ar rcs stb_truetype.a stb_truetype.o
