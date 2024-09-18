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
LDLIBS = r-common/c/lib-r-common.a $(OUTDIR)lib-common.a -ljack -lpthread -lm
obj = status.o \
      status_utils.o \
      status_curses.o \
	  status_json.o 

%.o : %.c %.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $*.c

mk-r-common:
	(cd r-common/c ; make)

all: $(obj) 
	mkdir -p $(OUTDIR)
	$(AR) -rcs ${OUTDIR}lib-common.a $(obj)
	make mk-r-common $(bin)

#rju-play: rju-play.c
#	$(CC) $(CPPFLAGS) $(CFLAGS) -o rju-play rju-play.c $(LDLIBS) -lsndfile -lsamplerate
	
rju-record: rju-record.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $(OUTDIR)rju-record rju-record.c $(LDLIBS) -lsndfile -lncurses 

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
