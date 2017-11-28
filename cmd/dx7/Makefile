MSFA = $(HOME)/opt/src/music-synthesizer-for-android/app/src/main/jni/

D = $(HOME)/sw/rju/cmd/dx7/

H = aligned_buf.h controllers.h module.h synth.h

F = dx7note env exp2 fir fm_core fm_op_kernel freqlut lfo log2 patch pitchenv resofilter ringbuffer sawtooth sin synth_unit

O = $(F:=.o)

copy:
	(cd $(MSFA); cp -f $(H) $(F:=.cc) $(F:=.h) $(D))

patch:
	patch < aligned_buf.h.patch

obj: $(O)

arc: $(O)
	ar -rcs lib-dx7.a $(O)

all:
	make copy
	make patch
	make obj
	make arc

clean:
	rm -f $(H) $(F:=.cc) $(F:=.h)
	rm -f $(O)
	rm -f lib-dx7.a

DEXED = $(HOME)/opt/src/dexed/Source/msfa/

diff-dexed:
	(for i in $(F) ; do diff -u $(MSFA)/$$i $(DEXED)/$$i ; done)
