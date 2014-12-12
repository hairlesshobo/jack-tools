prefix=$(HOME)/opt
bin=jack-dl jack-osc jack-play jack-plumbing jack-record jack-scope jack-transport jack-udp

CFLAGS=-Wall -D_POSIX_C_SOURCE=200112 -std=c99 -O3 -g
LDLIBS=c-common/lib-c-common.a -ljack -lpthread -lm

all: $(bin)

jack-transport: jack-transport.c
	gcc $(CFLAGS) -o jack-transport jack-transport.c $(LDLIBS) -lcurses

jack-dl: jack-dl.c
	gcc $(CFLAGS) -o jack-dl jack-dl.c $(LDLIBS) -ldl -llo

jack-play: jack-play.c
	gcc $(CFLAGS) -o jack-play jack-play.c $(LDLIBS) -lsndfile -lsamplerate

jack-record: jack-record.c
	gcc $(CFLAGS) -o jack-record jack-record.c $(LDLIBS) -lsndfile

jack-scope: jack-scope.c
	gcc $(CFLAGS) -o jack-scope jack-scope.c $(LDLIBS) -lX11 -lXext

clean:
	rm -f $(bin) *.o

install:
	cp $(bin) $(prefix)/bin
	cp jack-dl.h $(prefix)/include

uninstall:
	(cd $(prefix)/bin ; rm -f $(bin))

ln-local-c-common:
	rm -f c-common
	ln -s $(HOME)/sw/c-common c-common

mk-local-c-common:
	(cd c-common ; make)

push-sp:
	darcs push -a rd@slavepianos.org:sw/rju

pull-sp:
	darcs pull -a http://rd.slavepianos.org/sw/rju

indent:
	indent -kr -nut -l96 -i2 -brf *.c
