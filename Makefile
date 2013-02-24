prefix=$(HOME)/opt
CFLAGS=-Wall -D_POSIX_C_SOURCE=200112 -std=c99 -O3 -g
LIB=c-common/lib-c-common.a -ljack -lpthread -lm
BIN=jack-dl jack-osc jack-play jack-plumbing jack-record jack-scope jack-transport jack-udp

jack-transport: jack-transport.c
	gcc $(CFLAGS) -o jack-transport jack-transport.c $(LIB) -lcurses

all: jack-transport
	gcc $(CFLAGS) -o jack-dl jack-dl.c $(LIB) -ldl -llo
	gcc $(CFLAGS) -o jack-osc jack-osc.c $(LIB)
	gcc $(CFLAGS) -o jack-play jack-play.c $(LIB) -lsndfile -lsamplerate
	gcc $(CFLAGS) -o jack-plumbing jack-plumbing.c $(LIB)
	gcc $(CFLAGS) -o jack-record jack-record.c $(LIB) -lsndfile
	gcc $(CFLAGS) -o jack-scope jack-scope.c $(LIB) -lX11 -lXext
	gcc $(CFLAGS) -o jack-udp jack-udp.c $(LIB)

clean:
	rm -f $(BIN) *.o

install:
	cp $(BIN) $(prefix)/bin
	cp jack-dl.h $(prefix)/include

uninstall:
	(cd $(prefix)/bin ; rm -f $(BIN))

ln-local-c-common:
	rm -f c-common
	ln -s $(HOME)/sw/c-common c-common

mk-local-c-common:
	(cd c-common ; make)
