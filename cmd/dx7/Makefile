MSFA=$(HOME)/opt/src/music-synthesizer-for-android/app/src/main/jni/
D=$(HOME)/sw/rju/cmd/dx7/

F = aligned_buf.h controllers.h dx7note.cc dx7note.h env.cc env.h exp2.cc exp2.h fir.cc fir.h fm_core.cc fm_core.h fm_op_kernel.cc fm_op_kernel.h freqlut.cc freqlut.h lfo.cc lfo.h log2.cc log2.h module.h patch.cc patch.h pitchenv.cc pitchenv.h resofilter.cc resofilter.h ringbuffer.cc ringbuffer.h sawtooth.cc sawtooth.h sin.cc sin.h synth.h synth_unit.cc synth_unit.h

O = dx7note.o env.o exp2.o fir.o fm_core.o fm_op_kernel.o freqlut.o lfo.o log2.o patch.o pitchenv.o resofilter.o ringbuffer.o sawtooth.o sin.o synth_unit.o

copy:
	(cd $(MSFA); cp -f $(F) $(D))

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
	rm -f $(F)
	rm -f $(O)
	rm -f lib-dx7.a
