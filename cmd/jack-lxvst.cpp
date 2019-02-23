/* https://github.com/ekenberg/vstminihost */

#define __cdecl

/* VST */
#include "pluginterfaces/vst2.x/aeffectx.h"

/* C99 */
#include <stdio.h>
#include <stdlib.h>

/* POSIX */
#include <dlfcn.h>
#include <pthread.h>

/* JACK */
#include <jack/jack.h>
#include <jack/midiport.h>

/* X11 */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include <lo/lo.h>

/* sw/c-common */
#include "c-common/int.h"
#include "c-common/jack-client.h"
#include "c-common/jack-client.c"
#include "c-common/jack-port.h"
#include "c-common/jack-port.c"
#include "c-common/jack-ringbuffer.h"
#include "c-common/jack-ringbuffer.c"
#include "c-common/memory.h"
#include "c-common/memory.c"
#include "c-common/midi.h"
#include "c-common/midi.c"
#include "c-common/time-pause.h"
#include "c-common/time-pause.c"
#include "c-common/time-timespec.h"
#include "c-common/time-timespec.c"

typedef AEffect *(*PluginEntryProc) (audioMasterCallback audioMaster);
static VstIntPtr VSTCALLBACK vst_callback(AEffect * effect, VstInt32 opcode, VstInt32 index,
                                          VstIntPtr value, void *ptr, float opt);

void *x11_thread_proc(void *ptr);

void verify_platform(void)
{
    printf("HOST> SIZEOF VSTINTPTR = %lu\n", sizeof(VstIntPtr));
    printf("HOST> SIZEOF VSTINT32 = %lu\n", sizeof(VstInt32));
    printf("HOST> SIZEOF VOID* = %lu\n", sizeof(void *));
    printf("HOST> SIZEOF AEFFECT = %lu\n", sizeof(AEffect));
    if (sizeof(VstIntPtr) != sizeof(void *)) {
        printf("HOST> PLATFORM VERIFICATION FAILED\n");
        exit(EXIT_FAILURE);
    }
}

struct lxvst_opt
{
    bool unique_name;
    bool verbose;
    float sample_rate;
    u8 note_data_ch;
    u8 param_data_ch;
    float vel_mul;
};

struct lxvst
{
    AEffect *effect;
    Display *x11_dpy;
    bool x11_closed;
    float sample_rate;
    int channels;
    jack_port_t **audio_out;
    jack_port_t *midi_in;
    float **out;
    struct lxvst_opt opt;
};

struct lxvst_opt lxvst_opt_default()
{
    return {false, false, 0, 0, 0, 1};
}

struct lxvst lxvst_default()
{
    return { NULL, NULL, false, -1, 2, NULL, NULL, NULL, lxvst_opt_default()};
}

#define break_on(x,s)                                                          \
    if(x){								       \
	fprintf(stderr,"HOST> BREAK ON> %s: %s, %d\n", s, __FILE__, __LINE__); \
	return 0;							       \
    }

/* channel-voice-message events */
int pack_midi_event_cvm(u8 *b, size_t n, VstMidiEvent * e)
{
#if 0
    u8 st = b[0] & 0xF0;
    printf("HOST> STATUS = %2X\n", st);
#endif
    break_on(n > 4,"PACK_MIDI_EVENT");
    e->type = kVstMidiType;
    e->byteSize = sizeof(VstMidiEvent);
    e->deltaFrames = 0;
    e->flags = kVstMidiEventIsRealtime;
    e->noteLength = 0;
    e->noteOffset = 0;
    memset(e->midiData, 0, 4);  /* clear all four bytes */
    memcpy(e->midiData, b, n);  /* write as many bytes as jack has provided */
    e->detune = 0;              /* char: -64 - +63 */
#if 0
    if (st == 0x90 && b[1] == 60) {
        e->detune = 50;
    }
    printf("HOST> DETUNE = %d\n", e->detune);
#endif
    e->noteOffVelocity = 0;
    e->reserved1 = 0;
    e->reserved2 = 0;
    return 0;
}

/* sysex events */
int pack_midi_event_sysex(u8 *b, size_t n, VstMidiSysexEvent * e)
{
    e->type = kVstSysExType;
    e->byteSize = sizeof(VstMidiSysexEvent);
    e->deltaFrames = 0;
    e->flags = 0;
    e->dumpBytes = n;
    e->resvd1 = 0;
    e->sysexDump = (char *)b; /* the jack event data will remain valid? */
    e->resvd2 = 0;
    return 0;
}

#define MAX_MIDI_MESSAGES 64

struct VstEventSet
{
    VstInt32 numEvents;
    VstIntPtr reserved;
    VstEvent *events[MAX_MIDI_MESSAGES];
};

void midi_proc_cvm(const lxvst *d,const jack_midi_event_t e,VstEventSet *vst_e)
{
    u8 st = e.buffer[0];
    u8 st_ty = status_ty(st);
    u8 st_ch = status_ch(st);
    bool is_n = is_note_data(st);
    vprintf(d->opt.verbose,"st=0x%2X,ty=0x%02X,ch=0x%02X\n",st,st_ty,st_ch);
    if ((is_n && st_ch == d->opt.note_data_ch) || st_ch == d->opt.param_data_ch) {
        VstMidiEvent *e_ptr = (VstMidiEvent *)xmalloc(sizeof(VstMidiEvent));
        if (is_n) {
            e.buffer[2] = (u8)((float)e.buffer[2] * d->opt.vel_mul);
        }
        pack_midi_event_cvm(e.buffer, e.size, e_ptr);
        vst_e->events[vst_e->numEvents] = (VstEvent *)e_ptr;
        vst_e->numEvents += 1;
    } else {
        vprintf(d->opt.verbose,"HOST> MIDI EVENT> NON-CHANNEL DATA: ST=0x%2X\n",st);
    }
}

void midi_proc_sysex(const lxvst *d,const jack_midi_event_t e,VstEventSet *vst_e)
{
    VstMidiSysexEvent *e_ptr = (VstMidiSysexEvent *)xmalloc(sizeof(VstMidiSysexEvent));
    pack_midi_event_sysex(e.buffer, e.size, e_ptr);
    vst_e->events[vst_e->numEvents] = (VstEvent *)e_ptr;
    vst_e->numEvents += 1;
}

void midi_proc(lxvst *d, jack_nframes_t nframes)
{
    void *b = jack_port_get_buffer(d->midi_in, nframes);
    jack_nframes_t jack_e_n = jack_midi_get_event_count(b);
    if (jack_e_n > MAX_MIDI_MESSAGES) {
        printf("HOST> TOO MANY INCOMING MIDI EVENTS\n");
        return;
    }
    if (jack_e_n > 0) {
        VstEventSet vst_e;
        vst_e.numEvents = 0;
        vst_e.reserved = 0;
        for (jack_nframes_t i = 0; i < jack_e_n; i++) {
            jack_midi_event_t e;
            jack_midi_event_get(&e, b, i);
            if (e.size <= 4 && is_channel_voice_message(e.buffer[0])) {
                midi_proc_cvm(d,e,&vst_e);
            } else if (is_sysex_message(e.buffer[0])) {
                midi_proc_sysex(d,e,&vst_e);
            } else {
                vprintf(d->opt.verbose,"HOST> MIDI EVENT> NON IMMEDIATE EVENT: ST=0x%2X\n",e.buffer[0]);
            }
        }
        d->effect->dispatcher(d->effect, effProcessEvents, 0, 0, &vst_e, 0);
        for (int i = 0; i < vst_e.numEvents; i++) {
            free(vst_e.events[i]);
        }
    }
}

int audio_proc(jack_nframes_t nframes, void *ptr)
{
    struct lxvst *d = (struct lxvst *) ptr;
    midi_proc(d, nframes);
    d->effect->dispatcher(d->effect, effSetBlockSize, 0, nframes, 0, 0);
    for (int i = 0; i < d->channels; i++) {
        d->out[i] = (float *) jack_port_get_buffer(d->audio_out[i], nframes);
    }
    d->effect->processReplacing(d->effect, NULL, d->out, (VstInt32) nframes);
    return 0;
}

void osc_error(int n, const char *m, const char *p)
{
    fprintf(stderr, "HOST> OSC> error %d in path %s: %s\n", n, p, m);
}

int osc_pgm_set(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct lxvst *lxvst = (struct lxvst *)u;
  VstIntPtr pgm = (VstIntPtr)a[0]->i;
  break_on(pgm >= lxvst->effect->numPrograms, "PROGRAM INDEX");
  fprintf(stderr,"PGM_SET: %ld\n", pgm);
  lxvst->effect->dispatcher(lxvst->effect, effSetProgram, 0, pgm, NULL, 0.0);
  return 0;
}

int osc_p_set(const char *p, const char *t, lo_arg ** a, int n, void *d, void *u)
{
    struct lxvst *lxvst = (struct lxvst *) u;
    VstInt32 ix = (VstInt32) a[0]->i;
    float val = a[1]->f;
    break_on(ix >= lxvst->effect->numParams, "PARAMETER INDEX");
    fprintf(stderr, "P_SET: %d = %f\n", ix, val);
    lxvst->effect->setParameter(lxvst->effect, ix, val);
    return 0;
}

void usage(void)
{
    eprintf("Usage: jack-lxvst [ options ] lxvst-file\n");
    eprintf("    -l   : Log (verbose) midi data etc.\n");
    eprintf("    -n N : Note data channel (default=0)\n");
    eprintf("    -p N : Parameter data channel (default=0)\n");
    eprintf("    -r N : Sample rate (default=JACK SR)\n");
    eprintf("    -u   : Generate unique jack client name (ie. append PID)\n");
    eprintf("    -v N : Key velocity multiplier (default=1)\n");
    FAILURE;
}

int main(int argc, char *argv[])
{
    struct lxvst d = lxvst_default();

    int c;
    while ((c = getopt(argc, argv, "hln:p:r:uv:")) != -1) {
        switch (c) {
        case 'h':
            usage();
            break;
        case 'l':
            d.opt.verbose = true;
            break;
        case 'n':
            d.opt.note_data_ch = (u8)strtol(optarg, NULL, 10);
            break;
        case 'p':
            d.opt.param_data_ch = (u8)strtol(optarg, NULL, 10);
            break;
        case 'r':
            d.opt.sample_rate = strtof(optarg, NULL);
            break;
        case 'u':
            d.opt.unique_name = true;
            break;
        case 'v':
            d.opt.vel_mul = strtof(optarg, NULL);
            break;
        }
    }

    printf("HOST> ALLOCATE LXVST MEMORY\n");
    d.out = (float **) xmalloc(d.channels * sizeof(float *));
    d.audio_out = (jack_port_t **) xmalloc(d.channels * sizeof(jack_port_t *));
    printf("HOST> VERIFY PLATFORM\n");
    verify_platform();
    printf("HOST> PROCESS ARGUMENTS\n");
    if (argc < 2) {
        printf("HOST> USAGE = JACK-LXVST VST-FILE\n");
        usage();
        return -1;
    }
    if (optind > argc - 1) {
        usage();
    }
    const char *vst_file = argv[optind];
    printf("HOST> VST FILE=%s\n", vst_file);
    printf("HOST> LOAD VST LIBRARY\n");
    void *module = dlopen(vst_file, RTLD_LAZY);
    if (!module) {
        printf("HOST> DLOPEN ERROR: %s\n", dlerror());
        return -1;
    }
    printf("HOST> XINITTHREADS\n");
    XInitThreads();
    printf("HOST> START OSC THREAD\n");
    lo_server_thread osc = lo_server_thread_new("57210", osc_error);
    lo_server_thread_add_method(osc, "/pgm_set", "i", osc_pgm_set, &d);
    lo_server_thread_add_method(osc, "/p_set", "if", osc_p_set, &d);
    lo_server_thread_start(osc);
    printf("HOST> DLYSM VSTPLUGINMAIN\n");
    PluginEntryProc vst_main = (PluginEntryProc) dlsym(module, "VSTPluginMain");
    if (!vst_main) {
        printf("HOST> NO VSTPLUGINMAIN\n");
        return -1;
    }
    printf("HOST> RUN VSTPLUGINMAIN\n");
    d.effect = vst_main(vst_callback);
    if (!d.effect) {
        printf("HOST> VSTPLUGINMAIN FAILED\n");
        return -1;
    }
    printf("HOST> CALL EFFOPEN\n");
    d.effect->dispatcher(d.effect, effOpen, 0, 0, 0, 0);
    printf("HOST> CHECK AUDIO I/O\n");
    if (d.effect->numInputs != 0 || d.effect->numOutputs != 2) {
        printf("HOST> NOT 0-IN/2-OUT\n");
        return -1;
    }
    printf("HOST> CHECK MIDI I/O\n");
    char can_do_midi[64] = "receiveVstMidiEvent";
    if (d.effect->dispatcher(d.effect, effCanDo, 0, 0, can_do_midi, 0) != 1) {
        printf("HOST> NO MIDI INPUT\n");
        return -1;
    }
    printf("HOST> #PROGRAMS = %d, #PARAMS = %d\n", d.effect->numPrograms, d.effect->numParams);
    printf("HOST> CREATE X11 EDITOR THREAD\n");
    pthread_t x11_thread;
    pthread_create(&x11_thread, NULL, x11_thread_proc, &d);
    printf("HOST> CONNECT TO JACK\n");
    char client_name[64] = "jack-lxvst";
    jack_client_t *client;
    if (d.opt.unique_name) {
        client = jack_client_unique_store(client_name);
    } else {
        client = jack_client_open(client_name, JackNullOption, NULL);
    }
    jack_set_error_function(jack_client_minimal_error_handler);
    jack_on_shutdown(client, jack_client_minimal_shutdown_handler, 0);
    jack_set_process_callback(client, audio_proc, &d);
    d.sample_rate = d.opt.sample_rate > 0 ? d.opt.sample_rate : jack_get_sample_rate(client);
    printf("HOST> SET SAMPLE RATE = %f\n", d.sample_rate);
    d.effect->dispatcher(d.effect, effSetSampleRate, 0, 0, 0, d.sample_rate);
    printf("HOST> START EFFECT\n");
    d.effect->dispatcher(d.effect, effMainsChanged, 0, 1, 0, 0);
    printf("HOST> MAKE JACK MIDI INPUT PORT\n");
    jack_port_make_standard(client, &d.midi_in, 1, false, true);
    printf("HOST> MAKE JACK AUDIO OUTPUT PORTS\n");
    jack_port_make_standard(client, d.audio_out, d.channels, true, false);
    printf("HOST> ACTIVATE JACK CLIENT\n");
    jack_client_activate(client);
    char *midi_src_name = getenv("JACK_LXVST_MIDI_CONNECT_FROM");
    printf("HOST> CONNECT MIDI\n");
    if (midi_src_name) {
        printf("HOST> MIDI INPUT = %s\n", midi_src_name);
        char midi_dst_name[128];
        snprintf(midi_dst_name, 128, "%s:midi_in_1", client_name);
        jack_port_connect_named(client, midi_src_name, midi_dst_name);
    }
    printf("HOST> CONNECT AUDIO\n");
    char *audio_dst_pattern = getenv("JACK_LXVST_CONNECT_TO");
    if (audio_dst_pattern) {
        printf("HOST> AUDIO OUTPUT = %s\n", audio_dst_pattern);
        char audio_src_pattern[128];
        snprintf(audio_src_pattern, 128, "%s:out_%%d", client_name);
        jack_port_connect_pattern(client, d.channels, 0, audio_src_pattern, audio_dst_pattern);
    }
    printf("HOST> WAIT FOR EDITOR TO CLOSE\n");
    while (d.x11_closed == false) {
        pause_for(0.5);
    }
    printf("HOST> JACK CLIENT CLOSE\n");
    jack_client_close(client);
    printf("HOST> CLOSE EDITOR\n");
    d.effect->dispatcher(d.effect, effEditClose, 0, 0, 0, 0);
    printf("HOST> CLOSE EFFECT\n");
    d.effect->dispatcher(d.effect, effClose, 0, 0, 0, 0);
    printf("HOST> JOIN X11 THREAD\n");
    pthread_join(x11_thread, NULL);
    printf("HOST> CLOSE X11\n");
    XCloseDisplay(d.x11_dpy);
    printf("HOST> OSC THREAD FREE\n");
    lo_server_thread_free(osc);
    printf("HOST> CLOSE MODULE\n");
    dlclose(module);
    printf("HOST> FREE MEMORY\n");
    free(d.out);
    free(d.audio_out);
    return 0;
}

/* e = effect, c = opcode, i = index, v = value, p = ptr, o = opt */
VstIntPtr VSTCALLBACK
vst_callback(AEffect * e, VstInt32 c, VstInt32 i, VstIntPtr v, void *p, float o)
{
    VstIntPtr result = 0;
    switch (c) {
    case audioMasterVersion:
        result = kVstVersion;
        break;
    }
    return result;
}

void *x11_thread_proc(void *ptr)
{
    struct lxvst *d = (struct lxvst *) ptr;
    AEffect *effect = d->effect;

    if ((effect->flags & effFlagsHasEditor) == 0) {
        printf("HOST> NO EDITOR\n");
        return NULL;
    }

    Display *dpy;
    Window win;
    char effect_name[256];      // arbitrary, vst GetEffectName is max 32 chars

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
    XChangeProperty(dpy, win, prop_atom, XA_ATOM, 32, PropModeReplace,
                    (unsigned char *) &val_atom, 1);

    // prepare the plugin name in the title bar
    effect->dispatcher(effect, effGetEffectName, 0, 0, effect_name, 0);
    strcat(effect_name, " [jack-lxvst]");
    XStoreName(dpy, win, effect_name);

    // Get and prepare editor size
    ERect *eRect = 0;
    printf("HOST> EDITOR GET RECT\n");
    effect->dispatcher(effect, effEditGetRect, 0, 0, &eRect, 0);
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
    effect->dispatcher(effect, effEditOpen, 0, (VstIntPtr) dpy, (void *) win, 0);

    // Needs adjusting according to events we want to handle in the loop below
    XSelectInput(dpy, win, SubstructureNotifyMask | ButtonPressMask | ButtonReleaseMask
                 | ButtonMotionMask | ExposureMask | KeyPressMask);

    while (true) {
        XEvent e;
        XNextEvent(dpy, &e);
        // handle events as needed
        if (e.type == ClientMessage && (Atom) e.xclient.data.l[0] == wmDeleteMessage) {
            printf("HOST> XEVENT == wmDeleteMessage\n");
            d->x11_closed = true;
            return NULL;
        }
    }

    return NULL;
}
