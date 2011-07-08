CFLAGS=-Wall -D_POSIX_C_SOURCE=200112 -std=c99 -O3
LIB=c-common/lib-c-common.a -ljack -lpthread -lm
BIN=jack-dl jack-osc jack-play jack-plumbing jack-record jack-scope jack-transport jack-udp


all:
	gcc $(CFLAGS) -o jack-dl jack-dl.c $(LIB) -ldl -llo
	gcc $(CFLAGS) -o jack-osc jack-osc.c $(LIB)
	gcc $(CFLAGS) -o jack-play jack-play.c $(LIB) -lsndfile -lsamplerate
	gcc $(CFLAGS) -o jack-plumbing jack-plumbing.c $(LIB)
	gcc $(CFLAGS) -o jack-record jack-record.c $(LIB) -lsndfile
	gcc $(CFLAGS) -o jack-scope jack-scope.c $(LIB) -lX11 -lXext
	gcc $(CFLAGS) -o jack-transport jack-transport.c $(LIB) -lcurses
	gcc $(CFLAGS) -o jack-udp jack-udp.c $(LIB)

install:
	cp $(BIN) $(HOME)/bin

clean:
	rm -f $(BIN) *.o
