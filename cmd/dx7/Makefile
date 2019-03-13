DEXED = $(HOME)/opt/src/dexed/Source/msfa/

D = $(HOME)/sw/rju/cmd/dx7/

DEXED_H = aligned_buf controllers module synth

DEXED_CC = dx7note env exp2 fm_core fm_op_kernel freqlut lfo pitchenv sin

DEXED_O = $(DEXED_CC:=.o)

dexed-copy:
	(cd $(DEXED)/; cp -f $(DEXED_H:=.h) $(DEXED_CC:=.cc) $(DEXED_CC:=.h) $(D))

dexed-obj: $(DEXED_O)

dexed-ar: $(DEXED_O)
	ar -rcs lib-dx7.a $(DEXED_O)

all:
	make copy
	make patch
	make obj
	make arc

clean:
	rm -f $(DEXED_H:=.h) $(DEXED_CC:=.cc) $(DEXED_CC:=.h)
	rm -f $(DEXED_O)
	rm -f lib-dx7.a

diff-dexed:
	(for i in $(F) ; do diff -u $(MSFA)/$$i $(DEXED)/$$i ; done)

# MFSA

MSFA = $(HOME)/opt/src/music-synthesizer-for-android/app/src/main/jni/

MFSA_H = aligned_buf controllers module synth

MFSA_CC = dx7note env exp2 fir fm_core fm_op_kernel freqlut lfo log2 patch pitchenv resofilter ringbuffer sawtooth sin synth_unit

copy-mfsa:
	(cd $(MSFA); cp -f $(MFSA_H:=h) $(MFSA_CC:=.cc) $(MFSA_CC:=.h) $(D))

patch-mfsa:
	patch < aligned_buf.h.patch
