prefix = $(HOME)/opt

bin = rju-data rju-dl rju-level rju-lxvst rju-osc rju-play rju-plumbing rju-record rju-scope rju-transport rju-udp lxvst-query

VST_SDK = $(HOME)/opt/src/vst3_sdk

CC = gcc # gcc clang
CXX = g++ # g++ clang++
AR = ar # ar llvm-ar

CPPFLAGS += -Wall -pedantic -O3 -g -ffast-math
CFLAGS += -D_POSIX_C_SOURCE=200809 -std=c99
CXXFLAGS +=
LDLIBS = r-common/c/lib-r-common.a -ljack -lpthread -lm

all: $(bin)

rju-data: rju-data.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o rju-data rju-data.c $(LDLIBS) -lsamplerate

rju-dl: rju-dl.c
	$(CC) $(CPPFLAGS) -Wno-pedantic $(CFLAGS) -o rju-dl rju-dl.c $(LDLIBS) -ldl -llo

rju-dx7: rju-dx7.cpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -o rju-dx7 rju-dx7.cpp $(LDLIBS) dx7/lib-dx7.a

rju-level: rju-level.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o rju-level rju-level.c $(LDLIBS) -lncurses -ltinfo

rju-lxvst: rju-lxvst.cpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -I$(VST_SDK) -o rju-lxvst rju-lxvst.cpp $(LDLIBS) -ldl -lX11 -llo

rju-play: rju-play.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o rju-play rju-play.c $(LDLIBS) -lsndfile -lsamplerate

rju-record: rju-record.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o rju-record rju-record.c $(LDLIBS) -lsndfile

rju-scope: rju-scope.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o rju-scope rju-scope.c $(LDLIBS) -lsamplerate -lX11 -lXext -lpng

rju-transport: rju-transport.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o rju-transport rju-transport.c $(LDLIBS) -lncurses -ltinfo

lxvst-query: lxvst-query.cpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -I$(VST_SDK) -o lxvst-query lxvst-query.cpp $(LDLIBS) -ldl

clean:
	rm -f $(bin) *.o

install:
	cp $(bin) $(prefix)/bin
	cp rju-dl.h $(prefix)/include

uninstall:
	(cd $(prefix)/bin ; rm -f $(bin))

ln-local-r-common:
	rm -f r-common
	ln -s $(HOME)/sw/r-common r-common

mk-local-r-common:
	(cd r-common ; make)

indent:
	indent -k r-nut -l96 -i2 -brf rju-scope.c
	indent -kr -nut -l96 -i4 -blf -bls rju-lxvst.cpp

# `pkg-config --libs jack liblo ncurses sndfile samplerate x11 xext`
