prefix = $(HOME)/opt

bin = jack-data jack-dl jack-level jack-lxvst jack-osc jack-play jack-play-sc3 jack-plumbing jack-record jack-scope jack-transport jack-udp lxvst-query

VST_SDK = $(HOME)/opt/src/vst3_sdk

CC = gcc # gcc clang
AR = ar # ar llvm-ar
CXX = g++ # g++ clang++

CPPFLAGS += -Wall -pedantic -O3 -g -ffast-math
CFLAGS += -D_POSIX_C_SOURCE=200809 -std=c99
CXXFLAGS +=
LDLIBS = c-common/lib-c-common.a -ljack -lpthread -lm

all: $(bin)

jack-data: jack-data.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o jack-data jack-data.c $(LDLIBS) -lsamplerate

jack-dl: jack-dl.c
	$(CC) $(CPPFLAGS) -Wno-pedantic $(CFLAGS) -o jack-dl jack-dl.c $(LDLIBS) -ldl -llo

jack-dx7: jack-dx7.cpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -o jack-dx7 jack-dx7.cpp $(LDLIBS) dx7/lib-dx7.a

jack-level: jack-level.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o jack-level jack-level.c $(LDLIBS) -lncurses -ltinfo

jack-lxvst: jack-lxvst.cpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -I$(VST_SDK) -o jack-lxvst jack-lxvst.cpp $(LDLIBS) -ldl -lX11 -llo

jack-play: jack-play.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o jack-play jack-play.c $(LDLIBS) -lsndfile -lsamplerate

jack-record: jack-record.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o jack-record jack-record.c $(LDLIBS) -lsndfile

jack-scope: jack-scope.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o jack-scope jack-scope.c $(LDLIBS) -lsamplerate -lX11 -lXext -lpng

jack-transport: jack-transport.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o jack-transport jack-transport.c $(LDLIBS) -lncurses -ltinfo

lxvst-query: lxvst-query.cpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -I$(VST_SDK) -o lxvst-query lxvst-query.cpp $(LDLIBS) -ldl

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

indent:
	indent -kr -nut -l96 -i2 -brf jack-scope.c
	indent -kr -nut -l96 -i4 -blf -bls jack-lxvst.cpp

# `pkg-config --libs jack liblo ncurses sndfile samplerate x11 xext`
