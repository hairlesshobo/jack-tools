prefix = $(HOME)/opt

jack =	jack-client.o \
	jack-port.o #\
	# jack-ringbuffer.o \
	# jack-transport.o

sf =	sf-sndfile.o

obj =	file.o \
	float.o \
	logging.o \
	memory.o \
	observe-signal.o \
	ringbuffer.o \
	ringbuffer-fd.o \
	signal-interleave.o

CC = gcc # gcc clang
AR = ar # ar llvm-ar-13

CPPFLAGS += -g -Wall -pedantic -O3 -ffast-math
CFLAGS += -D_POSIX_C_SOURCE=200809 -std=c11

%.o : %.c %.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $*.c

all: $(obj)
	$(AR) -rcs lib.a $(obj)

clean:
	rm -f *.o *.a

indent:
	clang-format -i *.c *.h

diff-ignoring-whitespace:
	git diff --ignore-space-at-eol --ignore-space-change --ignore-all-space
