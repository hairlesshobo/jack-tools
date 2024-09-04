prefix = $(HOME)/opt

#bin = rju-data rju-dl rju-level rju-lxvst rju-osc rju-play rju-plumbing rju-record rju-scope rju-transport rju-udp lxvst-query
bin = rju-record

VST_SDK = $(HOME)/opt/build/vst3_sdk

CC = gcc # gcc clang
CXX = g++ # g++ clang++
AR = ar # ar llvm-ar

OUTDIR = ./bin/
INCLUDES += -I/opt/homebrew/Cellar/liblo/0.32/include -I/opt/homebrew/Cellar/libsamplerate/0.2.2/include -I/opt/homebrew/Cellar/libsndfile/1.2.2/include
LIBS += -L/opt/homebrew/lib 
CPPFLAGS += -Wall -pedantic -O3 -g -ffast-math $(INCLUDES) $(LIBS)
CFLAGS += -D_POSIX_C_SOURCE=200809 -std=c99
CXXFLAGS +=
LDLIBS = r-common/c/lib-r-common.a -ljack -lpthread -lm

mk-r-common:
	(cd r-common/c ; make)

all: 
	mkdir -p $(OUTDIR)
	make mk-r-common $(bin)

#rju-data: rju-data.c
#	$(CC) $(CPPFLAGS) $(CFLAGS) -o rju-data rju-data.c $(LDLIBS) -lsamplerate

#rju-dl: rju-dl.c
#	$(CC) $(CPPFLAGS) -Wno-pedantic $(CFLAGS) -o rju-dl rju-dl.c $(LDLIBS) -ldl -llo

#rju-dx7: rju-dx7.cpp
#	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -o rju-dx7 rju-dx7.cpp $(LDLIBS) dx7/lib-dx7.a

# rju-level: rju-level.c
# 	$(CC) $(CPPFLAGS) $(CFLAGS) -o $(OUTDIR)rju-level rju-level.c $(LDLIBS) -lncurses
#-ltinfo

#rju-lxvst: rju-lxvst.cpp
#	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -I$(VST_SDK) -o rju-lxvst rju-lxvst.cpp $(LDLIBS) -ldl -lX11 -llo

#rju-play: rju-play.c
#	$(CC) $(CPPFLAGS) $(CFLAGS) -o rju-play rju-play.c $(LDLIBS) -lsndfile -lsamplerate

rju-record: rju-record.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $(OUTDIR)rju-record rju-record.c $(LDLIBS) -lsndfile -lncurses 

#rju-scope: rju-scope.c
#	$(CC) $(CPPFLAGS) $(CFLAGS) -o rju-scope rju-scope.c $(LDLIBS) -lsamplerate -lX11 -lXext -lpng

#rju-transport: rju-transport.c
#	$(CC) $(CPPFLAGS) $(CFLAGS) -o rju-transport rju-transport.c $(LDLIBS) -lncurses -ltinfo

#lxvst-query: lxvst-query.cpp
#	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -I$(VST_SDK) -o lxvst-query lxvst-query.cpp $(LDLIBS) -ldl

clean:
	rm -f $(bin) *.o
	(cd r-common/c ; make clean)

install:
	cp $(bin) $(prefix)/bin
	cp rju-dl.h $(prefix)/include

uninstall:
	(cd $(prefix)/bin ; rm -f $(bin))

indent:
	clang-format -i *.c *.h

diff-ignoring-whitespace:
	git diff --ignore-space-at-eol --ignore-space-change --ignore-all-space

# `pkg-config --libs jack liblo ncurses sndfile samplerate x11 xext`
