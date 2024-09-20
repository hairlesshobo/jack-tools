prefix = $(HOME)/opt

bin = recorder

CC = gcc # gcc clang
CXX = g++ # g++ clang++
AR = ar # ar llvm-ar

OUTDIR = ./bin/
INCLUDES += -I/opt/homebrew/Cellar/liblo/0.32/include -I/opt/homebrew/Cellar/libsamplerate/0.2.2/include -I/opt/homebrew/Cellar/libsndfile/1.2.2/include
LIBS += -L/opt/homebrew/lib 
CPPFLAGS += -Wall -pedantic -O3 -g -ffast-math $(INCLUDES) $(LIBS)
CFLAGS += -D_POSIX_C_SOURCE=200809 -std=c99 -Wno-unused-command-line-argument
CXXFLAGS +=
LDLIBS = lib/lib.a common.a -ljack -lpthread -lm

obj = args.o \
	  status.o \
      status_curses.o \
	  status_json.o \
      status_utils.o

%.o : %.c %.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $*.c

libs:
	(cd lib ; make)

support: $(obj)
	mkdir -p $(OUTDIR)
	$(AR) -rcs common.a $(obj)

all:
	make libs support $(bin)

recorder: recorder.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $(OUTDIR)recorder recorder.c $(LDLIBS) -lsndfile -lncurses 

clean:
	rm -fr $(OUTDIR) $(bin) *.o *.wav *.a *.log
	(cd lib ; make clean)

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
