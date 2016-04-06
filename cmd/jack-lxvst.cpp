/* https://github.com/ekenberg/vstminihost */

#define __cdecl

#include "pluginterfaces/vst2.x/aeffectx.h" /* VST */

#include <stdio.h> /* C99 */
#include <stdlib.h> /* C99 */

#include <dlfcn.h> /* POSIX */
#include <pthread.h> /* POSIX */

#include <jack/jack.h> /* JACK */
#include <jack/midiport.h>

#include <X11/Xlib.h> /* X11 */
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include "c-common/jack-client.h" /* sw/c-common */
#include "c-common/jack-client.c"
#include "c-common/jack-port.h"
#include "c-common/jack-port.c"
#include "c-common/jack-ringbuffer.h"
#include "c-common/jack-ringbuffer.c"
#include "c-common/memory.h"
#include "c-common/memory.c"
#include "c-common/time-pause.h"
#include "c-common/time-pause.c"
#include "c-common/time-timespec.h"
#include "c-common/time-timespec.c"

typedef AEffect* (*PluginEntryProc) (audioMasterCallback audioMaster);
static VstIntPtr VSTCALLBACK vst_callback (AEffect* effect, VstInt32 opcode, VstInt32 index, VstIntPtr value, void* ptr, float opt);

void *x11_thread_proc (void *ptr);

void verify_platform (void)
{
    printf("HOST> SIZEOF VSTINTPTR = %lu\n", sizeof(VstIntPtr));
    printf("HOST> SIZEOF VSTINT32 = %lu\n", sizeof(VstInt32));
    printf("HOST> SIZEOF VOID* = %lu\n", sizeof(void*));
    printf("HOST> SIZEOF AEFFECT = %lu\n", sizeof(AEffect));
    if (sizeof(VstIntPtr) != sizeof(void*)) {
	printf("HOST> PLATFORM VERIFICATION FAILED\n");
	exit(EXIT_FAILURE);
    }
}

struct lxvst {
    AEffect *effect;
    Display *x11_dpy;
    bool x11_closed;
    float sample_rate;
    int channels;
    jack_port_t **audio_out;
    jack_port_t *midi_in;
    float **out;
};

typedef unsigned char u8;

void pack_midi_event(jack_midi_data_t *b,size_t n,VstMidiEvent *e) {
#if 0
    u8 *m = (u8 *)b;
    u8 st = m[0] & 0xF0;
    printf("HOST> STATUS = %2X\n",st);
#endif
    e->type = kVstMidiType;
    e->byteSize = sizeof(VstMidiEvent);
    e->deltaFrames = 0;
    e->flags = kVstMidiEventIsRealtime;
    e->noteLength = 0;
    e->noteOffset = 0;
    memset(e->midiData,0,4);
    memcpy(e->midiData,b,n);
    e->detune = 0; /* char: -64 - +63 */
#if 0
    if(st == 0x90 && m[1] == 60) e->detune = 50;
    printf("HOST> DETUNE = %d\n",e->detune);
#endif
    e->noteOffVelocity = 0;
    e->reserved1 = 0;
    e->reserved2 = 0;
}

#define MAX_MIDI_MESSAGES 64

struct VstEventSet {
    VstInt32 numEvents;
    VstIntPtr reserved;
    VstMidiEvent *events[MAX_MIDI_MESSAGES];
};

void midi_proc(lxvst *d,jack_nframes_t nframes) {
    void *b = jack_port_get_buffer(d->midi_in, nframes);
    jack_nframes_t jack_e_n = jack_midi_get_event_count(b);
    if(jack_e_n > MAX_MIDI_MESSAGES) {
	printf("HOST> TOO MANY INCOMING MIDI EVENTS\n");
	return;
    }
    if(jack_e_n > 0) {
	VstEventSet vst_e;
	vst_e.numEvents = 0;
	vst_e.reserved = 0;
	for(jack_nframes_t i = 0; i < jack_e_n; i++) {
	    jack_midi_event_t e;
	    jack_midi_event_get(&e,b,i);
	    if(e.size <= 4) {
		vst_e.events[vst_e.numEvents] = (VstMidiEvent*)xmalloc(sizeof(VstMidiEvent));
		pack_midi_event(e.buffer,e.size,vst_e.events[vst_e.numEvents]);
		vst_e.numEvents += 1;
	    }
	}
	d->effect->dispatcher (d->effect, effProcessEvents, 0, 0, &vst_e, 0);
	for(int i = 0; i < vst_e.numEvents; i++) {
	    free(vst_e.events[i]);
	}
    }
}

int audio_proc(jack_nframes_t nframes, void *ptr)
{
    struct lxvst *d = (struct lxvst *)ptr;
    midi_proc(d,nframes);
    d->effect->dispatcher (d->effect, effSetBlockSize, 0, nframes, 0, 0);
    for(int i = 0; i < d->channels; i++) {
	d->out[i] = (float *)jack_port_get_buffer(d->audio_out[i], nframes);
    }
    d->effect->processReplacing (d->effect, NULL, d->out, (VstInt32)nframes);
    return 0;
}

int main (int argc, char* argv[])
{
    void* module;
    struct lxvst d = {NULL,NULL,false,-1,2,NULL,NULL,NULL};
    printf("HOST> ALLOCATE LXVST MEMORY\n");
    d.out = (float **)xmalloc(d.channels * sizeof(float *));
    d.audio_out = (jack_port_t **)xmalloc(d.channels * sizeof(jack_port_t *));
    printf("HOST> VERIFY PLATFORM\n");
    verify_platform();
    printf("HOST> PROCESS ARGUMENTS\n");
    if (argc < 2) {
	printf("HOST> USAGE = JACK-LXVST VST-FILE\n");
	return -1;
    }
    const char* vst_file = argv[1];
    printf("HOST> VST FILE=%s\n",vst_file);
    printf("HOST> LOAD VST LIBRARY\n");
    module = dlopen(vst_file, RTLD_LAZY);
    if (!module) {
	printf("HOST> DLOPEN ERROR: %s\n", dlerror());
	return -1;
    }
    printf("HOST> XINITTHREADS\n");
    XInitThreads();
    printf("HOST> DLYSM VSTPLUGINMAIN\n");
    PluginEntryProc vst_main = (PluginEntryProc) dlsym(module, "VSTPluginMain");
    if (!vst_main) {
	printf("HOST> NO VSTPLUGINMAIN\n");
	return -1;
    }
    printf("HOST> RUN VSTPLUGINMAIN\n");
    d.effect = vst_main (vst_callback);
    if (!d.effect) {
	printf("HOST> VSTPLUGINMAIN FAILED\n");
	return -1;
    }
    printf("HOST> CALL EFFOPEN\n");
    d.effect->dispatcher (d.effect, effOpen, 0, 0, 0, 0);
    printf("HOST> CHECK AUDIO I/O\n");
    if (d.effect->numInputs != 0 || d.effect->numOutputs != 2) {
	printf("HOST> NOT 0-IN/2-OUT\n");
	return -1;
    }
    printf("HOST> CHECK MIDI I/O\n");
    char can_do_midi[64] = "receiveVstMidiEvent";
    if (d.effect->dispatcher (d.effect, effCanDo, 0, 0, can_do_midi, 0) != 1) {
	printf("HOST> NO MIDI INPUT\n");
	return -1;
    }
    printf("HOST> SET SAMPLE RATE\n");
    d.effect->dispatcher (d.effect, effSetSampleRate, 0, 0, 0, d.sample_rate);
    printf("HOST> CREATE X11 EDITOR THREAD\n");
    pthread_t x11_thread;
    pthread_create (&x11_thread,NULL,x11_thread_proc,&d);
    printf("HOST> START EFFECT\n");
    d.effect->dispatcher (d.effect, effMainsChanged, 0, 1, 0, 0);
    printf("HOST> CONNECT TO JACK\n");
    char client_name[64] = "jack-lxvst";
    jack_client_t *client = jack_client_unique_store(client_name);
    jack_set_error_function(jack_client_minimal_error_handler);
    jack_on_shutdown(client, jack_client_minimal_shutdown_handler, 0);
    jack_set_process_callback(client, audio_proc, &d);
    d.sample_rate = jack_get_sample_rate(client);
    printf("HOST> MAKE JACK MIDI INPUT PORT\n");
    jack_port_make_standard(client, &d.midi_in, 1, false, true);
    printf("HOST> MAKE JACK AUDIO OUTPUT PORTS\n");
    jack_port_make_standard(client, d.audio_out, d.channels, true, false);
    printf("HOST> ACTIVATE JACK CLIENT\n");
    jack_client_activate(client);
    char *midi_src_name = getenv("JACK_LXVST_MIDI_CONNECT_FROM");
    printf("HOST> CONNECT MIDI\n");
    if (midi_src_name) {
	printf("HOST> MIDI INPUT = %s\n",midi_src_name);
	char midi_dst_name[128];
	snprintf(midi_dst_name,128,"%s:midi_in_1",client_name);
	jack_port_connect_named(client,midi_src_name,midi_dst_name);
    }
    printf("HOST> CONNECT AUDIO\n");
    char *audio_dst_pattern = getenv("JACK_LXVST_CONNECT_TO");
    if (audio_dst_pattern) {
	printf("HOST> AUDIO OUTPUT = %s\n",audio_dst_pattern);
	char audio_src_pattern[128];
	snprintf(audio_src_pattern,128,"%s:out_%%d",client_name);
	jack_port_connect_pattern(client,d.channels,0,audio_src_pattern,audio_dst_pattern);
    }
    printf("HOST> WAIT FOR EDITOR TO CLOSE\n");
    while(d.x11_closed == false) {
	pause_for(0.5);
    }
    printf("HOST> JACK CLIENT CLOSE\n");
    jack_client_close(client);
    printf("HOST> CLOSE EDITOR\n");
    d.effect->dispatcher (d.effect, effEditClose, 0, 0, 0, 0);
    printf("HOST> CLOSE EFFECT\n");
    d.effect->dispatcher (d.effect, effClose, 0, 0, 0, 0);
    printf("HOST> JOIN X11 THREAD\n");
    pthread_join(x11_thread, NULL);
    printf("HOST> CLOSE X11\n");
    XCloseDisplay(d.x11_dpy);
    printf("HOST> CLOSE MODULE\n");
    dlclose(module);
    printf("HOST> FREE MEMORY\n");
    free(d.out);
    free(d.audio_out);
    return 0;
}

/* e = effect, c = opcode, i = index, v = value, p = ptr, o = opt */
VstIntPtr VSTCALLBACK
vst_callback (AEffect* e, VstInt32 c, VstInt32 i, VstIntPtr v, void* p, float o)
{
    VstIntPtr result = 0;
    switch (c) {
    case audioMasterVersion :
	result = kVstVersion;
	break;
    }
    return result;
}

void *x11_thread_proc (void *ptr)
{
    struct lxvst *d = (struct lxvst *)ptr;
    AEffect* effect = d->effect;

    if ((effect->flags & effFlagsHasEditor) == 0)
    {
	printf("HOST> NO EDITOR\n");
	return NULL;
    }

    Display *dpy;
    Window win;
    char effect_name[256]; // arbitrary, vst GetEffectName is max 32 chars

    // create the window
    dpy = XOpenDisplay(NULL);
    d->x11_dpy = dpy;
    win = XCreateSimpleWindow(dpy, DefaultRootWindow(dpy), 0, 0, 300, 300, 0, 0, 0);

    // we want an event when the window is being closed
    Atom wmDeleteMessage = XInternAtom(dpy, "WM_DELETE_WINDOW", false);
    XSetWMProtocols(dpy, win, &wmDeleteMessage, 1);

    // Make the window a Dialog, maybe the window manager will place it centered
    Atom prop_atom = XInternAtom(dpy, "_NET_WM_WINDOW_TYPE", False);
    Atom val_atom = XInternAtom(dpy, "_NET_WM_WINDOW_TYPE_DIALOG", False);
    XChangeProperty(dpy, win, prop_atom, XA_ATOM, 32, PropModeReplace, (unsigned char *)&val_atom, 1);

    // prepare the plugin name in the title bar
    effect->dispatcher(effect, effGetEffectName, 0, 0, effect_name, 0);
    strcat(effect_name, " [jack-lxvst]");
    XStoreName(dpy, win, effect_name);

    // Get and prepare editor size
    ERect* eRect = 0;
    printf("HOST> EDITOR GET RECT\n");
    effect->dispatcher (effect, effEditGetRect, 0, 0, &eRect, 0);
    if (eRect) {
	int width = eRect->right - eRect->left;
	int height = eRect->bottom - eRect->top;
	printf("HOST> GETRECT = (%d,%d)\n", width, height);
	XResizeWindow(dpy, win, width, height);
    }

    // ? Is it correct to effEditGetRect above, before effEditOpen ?
    // Display the window, let the plugin populate it
    printf("HOST> EDITOR OPEN\n");
    XMapWindow(dpy, win);
    XFlush(dpy);
    effect->dispatcher (effect, effEditOpen, 0, (VstIntPtr) dpy, (void*) win, 0);

    // Needs adjusting according to events we want to handle in the loop below
    XSelectInput(dpy, win, SubstructureNotifyMask | ButtonPressMask | ButtonReleaseMask
		 | ButtonMotionMask | ExposureMask | KeyPressMask);

    while (true) {
	XEvent e;
	XNextEvent(dpy, &e);
	// handle events as needed
	if (e.type == ClientMessage && (Atom)e.xclient.data.l[0] == wmDeleteMessage) {
	    printf("HOST> XEVENT == wmDeleteMessage\n");
	    d->x11_closed = true;
	    return NULL;
	}
    }

    return NULL;
}
