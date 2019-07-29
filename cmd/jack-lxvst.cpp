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
#include "c-common/dl.c"
#include "c-common/failure.h"
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
#include "c-common/vst.c"

void *x11_thread_proc(void *ptr);

struct lxvst_opt
{
    bool unique_name;
    bool verbose;
    float sample_rate;
    u8 note_data_ch;
    u8 param_data_ch;
    char udp_port[6]; /* UDP port number as ASCII string */
    int n_in_channels;
    int n_out_channels;
    bool use_midi;
    bool x11;
};

struct lxvst
{
    AEffect *effect;
    Display *x11_dpy;
    bool usr_exit;
    float sample_rate;
    jack_port_t **audio_in;
    jack_port_t **audio_out;
    jack_port_t *midi_in;
    float **in;
    float **out;
    struct lxvst_opt opt;
};

struct lxvst_opt lxvst_opt_default(void)
{
    return {false, false, -1.0, 0, 0, {'5','7','2','1','0',0}, 0, 2, true, true};
}

struct lxvst lxvst_default()
{
    return { NULL, NULL, false, -1, NULL, NULL, NULL, NULL, NULL, lxvst_opt_default()};
}

#define break_on(x,s)                                                          \
    if(x){								       \
	eprintf("HOST> BREAK ON> %s: %s, %d\n", s, __FILE__, __LINE__); \
	return 0;							       \
    }

/* channel-voice-message events */
int pack_midi_event_cvm(const u8 *b, size_t n, VstMidiEvent *e)
{
    dprintf("HOST> MIDI CVM> STATUS=%2X\n", b[0]);
    break_on(n > 4,__func__);
    e->type = kVstMidiType;
    e->byteSize = sizeof(VstMidiEvent);
    e->deltaFrames = 0;
    e->flags = kVstMidiEventIsRealtime;
    e->noteLength = 0;
    e->noteOffset = 0;
    memset(e->midiData, 0, 4);  /* clear all four bytes */
    memcpy(e->midiData, b, n);  /* write as many bytes as jack has provided */
    e->detune = 0;              /* char: -64 - +63 */
    /*dprintf("HOST> DETUNE = %d\n", e->detune);*/
    e->noteOffVelocity = 0;
    e->reserved1 = 0;
    e->reserved2 = 0;
    return 0;
}

/* sysex events */
int pack_midi_event_sysex(const u8 *b, size_t n, VstMidiSysexEvent *e)
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

void midi_proc_cvm(const lxvst *d,const u8 *b, size_t n,VstEventSet *vst_e)
{
    u8 st = b[0];
    u8 st_ch = status_ch(st);
    bool is_n = is_note_data(st);
    vprintf(d->opt.verbose,"HOST> MIDI EVENT> STATUS=0x%2X\n",st);
    if ((is_n && st_ch == d->opt.note_data_ch) || st_ch == d->opt.param_data_ch) {
        VstMidiEvent *e_ptr = (VstMidiEvent *)xmalloc(sizeof(VstMidiEvent));
        pack_midi_event_cvm(b, n, e_ptr);
        vst_e->events[vst_e->numEvents] = (VstEvent *)e_ptr;
        vst_e->numEvents += 1;
    } else {
        vprintf(d->opt.verbose,"HOST> MIDI EVENT> NON-CHANNEL DATA: ST=0x%2X\n",st);
    }
}

void midi_proc_sysex(const lxvst *d,const u8 *b, size_t n,VstEventSet *vst_e)
{
    VstMidiSysexEvent *e_ptr = (VstMidiSysexEvent *)xmalloc(sizeof(VstMidiSysexEvent));
    pack_midi_event_sysex(b, n, e_ptr);
    vst_e->events[vst_e->numEvents] = (VstEvent *)e_ptr;
    vst_e->numEvents += 1;
}

void midi_proc(lxvst *d, jack_nframes_t nframes)
{
    void *b = jack_port_get_buffer(d->midi_in, nframes);
    jack_nframes_t jack_e_n = jack_midi_get_event_count(b);
    if (jack_e_n > VST_MAX_MIDI_MESSAGES) {
        printf("HOST> MIDI> TOO MANY INCOMING MIDI EVENTS\n");
        return;
    }
    if (jack_e_n > 0) {
        VstEventSet vst_e;
        vst_e.numEvents = 0;
        vst_e.reserved = 0;
        for (jack_nframes_t i = 0; i < jack_e_n; i++) {
            jack_midi_event_t e;
            jack_midi_event_get(&e, b, i);
            vprintf(d->opt.verbose,"HOST> MIDI> #=%ld ST=0x%02X\n",e.size,e.buffer[0]);
            if (e.size <= 4 && is_channel_voice_message(e.buffer[0])) {
                midi_proc_cvm(d,e.buffer,e.size,&vst_e);
            } else if (is_sysex_message(e.buffer[0])) {
                midi_proc_sysex(d,e.buffer,e.size,&vst_e);
            } else {
                vprintf(d->opt.verbose,"HOST> MIDI> UNKNOWN EVENT: ST=0x%2X\n",e.buffer[0]);
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
    if(d->opt.use_midi) {
        midi_proc(d, nframes);
    }
    d->effect->dispatcher(d->effect, effSetBlockSize, 0, nframes, 0, 0);
    for (int i = 0; i < d->opt.n_in_channels; i++) {
        d->in[i] = (float *) jack_port_get_buffer(d->audio_in[i], nframes);
    }
    for (int i = 0; i < d->opt.n_out_channels; i++) {
        d->out[i] = (float *) jack_port_get_buffer(d->audio_out[i], nframes);
    }
    d->effect->processReplacing(d->effect, d->in, d->out, (VstInt32) nframes);
    return 0;
}

void osc_error(int n, const char *m, const char *p)
{
    eprintf("HOST> OSC> ERROR=%d PATH=%s: %s\n", n, p, m);
}

int osc_exit(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
    struct lxvst *lxvst = (struct lxvst *) u;
    lxvst->usr_exit = true;
    return 0;
}

/* set_program ix:int */
int osc_set_program(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
    struct lxvst *lxvst = (struct lxvst *) u;
    VstInt32 ix = (VstInt32) a[0]->i;
    break_on(ix >= lxvst->effect->numPrograms, "PROGRAM INDEX");
    vprintf(lxvst->opt.verbose, "HOST> OSC> SET-PROGRAM %d\n", ix);
    vst_set_program(lxvst->effect, ix);
    return 0;
}

/* set_param ix:int value:float ; set-parameter */
int osc_set_param(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
    struct lxvst *lxvst = (struct lxvst *) u;
    VstInt32 ix = (VstInt32) a[0]->i;
    float val = a[1]->f;
    break_on(ix >= lxvst->effect->numParams, "PARAMETER INDEX");
    vprintf(lxvst->opt.verbose, "HOST> OSC> SET-PARAM %d=%f\n", ix, val);
    lxvst->effect->setParameter(lxvst->effect, ix, val);
    return 0;
}

/* set_param_seq ix:int cnt:int value0:float... ; set-parameter-sequence */
int osc_set_param_seq(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
    break_on(n < 3, "ARG-CNT");
    break_on(t[0] != 'i' || t[1] != 'i', "ARG-TYPE");
    struct lxvst *lxvst = (struct lxvst *) u;
    VstInt32 ix = (VstInt32) a[0]->i;
    int cnt = (int) a[1]->i;
    break_on(cnt != (n - 2), "PARAMETER-CNT");
    break_on((ix + cnt) > lxvst->effect->numParams, "PARAMETER INDEX");
    for(int i = 2, j = 0; i < n; i++, j++) {
        float val = a[i]->f;
        vprintf(lxvst->opt.verbose, "HOST> OSC> SET-PARAM-SEQ %d=%f\n", ix + j, val);
        lxvst->effect->setParameter(lxvst->effect, ix + j, val);
    }
    return 0;
}

/* set_print_param ; print-parameters */
int osc_print_param(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
    break_on(n != 0, "ARG-CNT");
    struct lxvst *lxvst = (struct lxvst *) u;
    int cnt = (int) lxvst->effect->numParams;
    for(int i = 0; i < cnt; i++) {
        float val = lxvst->effect->getParameter(lxvst->effect, i);
        printf ("%f%c",val,i == cnt - 1 ? '\n' : ' ');
    }
    return 0;
}

/* midi msg:blob ; send-midi-message */
int osc_midi(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
    struct lxvst *lxvst = (struct lxvst *) u;
    VstEventSet vst_e;
    u8* m_data = (u8*)&(a[0]->blob.data);
    size_t m_size = (size_t)a[0]->blob.size;
    u8 st = m_data[0];
    vst_e.numEvents = 0;
    vst_e.reserved = 0;
    if(is_sysex_message(st)) {
        midi_proc_sysex(lxvst,m_data,m_size,&vst_e);
    } else if(is_channel_voice_message(st)) {
        midi_proc_cvm(lxvst,m_data,m_size,&vst_e);
    } else {
        eprintf("HOST> OSC> MIDI> UNKNOWN MESSAGE> STATUS=%02X\n",st);
    }
    lxvst->effect->dispatcher(lxvst->effect, effProcessEvents, 0, 0, &vst_e, 0);
    for (int i = 0; i < vst_e.numEvents; i++) {
        free(vst_e.events[i]);
    }
    return 0;
}

void usage(void)
{
    eprintf("Usage: jack-lxvst [ options ] lxvst-file\n");
    eprintf("    -c N : Note data channel (default=0)\n");
    eprintf("    -i N : Number of input audio channels (default=0)\n");
    eprintf("    -l   : Log (verbose) midi data etc.\n");
    eprintf("    -m   : No midi\n");
    eprintf("    -n   : Generate unique jack client name (ie. append PID)\n");
    eprintf("    -o N : Number of audio channels (default=2)\n");
    eprintf("    -p N : Parameter data channel (default=0)\n");
    eprintf("    -r N : Sample rate (default=JACK SR)\n");
    eprintf("    -u N : UDP port number (default=57210)\n");
    eprintf("    -x   : Do not run X11 interface\n");
    FAILURE;
}

void parse_arg(struct lxvst *d,int argc, char *argv[])
{
    int c;
    while ((c = getopt(argc, argv, "c:hi:lmno:p:r:u:x")) != -1) {
        switch (c) {
        case 'c':
            d->opt.note_data_ch = (u8)strtol(optarg, NULL, 10);
            break;
        case 'h':
            usage();
            break;
        case 'i':
            d->opt.n_in_channels = (int)strtol(optarg, NULL, 10);
            break;
        case 'l':
            d->opt.verbose = true;
            break;
        case 'm':
            d->opt.use_midi = false;
            break;
        case 'n':
            d->opt.unique_name = true;
            break;
        case 'o':
            d->opt.n_out_channels = (int)strtol(optarg, NULL, 10);
            break;
        case 'p':
            d->opt.param_data_ch = (u8)strtol(optarg, NULL, 10);
            break;
        case 'r':
            d->opt.sample_rate = strtof(optarg, NULL);
            break;
        case 'u':
            strncpy(d->opt.udp_port,optarg,5);
            break;
        case 'x':
            d->opt.x11 = false;
            break;
        default:
            usage();
        }
    }
}

int main(int argc, char *argv[])
{
    struct lxvst d = lxvst_default();
    printf("HOST> PARSE ARGUMENTS\n");
    parse_arg(&d, argc, argv);
    printf("HOST> ALLOCATE LXVST MEMORY\n");
    d.in = (float **) xmalloc(d.opt.n_in_channels * sizeof(float *));
    d.audio_in = (jack_port_t **) xmalloc(d.opt.n_in_channels * sizeof(jack_port_t *));
    d.out = (float **) xmalloc(d.opt.n_out_channels * sizeof(float *));
    d.audio_out = (jack_port_t **) xmalloc(d.opt.n_out_channels * sizeof(jack_port_t *));
    printf("HOST> VERIFY PLATFORM\n");
    vst_verify_platform();
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
    printf("HOST> VST FILE = %s\n", vst_file);
    printf("HOST> LOAD VST LIBRARY\n");
    void *module = xdlopen(vst_file,RTLD_LAZY);
    printf("HOST> XINITTHREADS\n");
    XInitThreads();
    printf("HOST> START OSC THREAD\n");
    lo_server_thread osc = lo_server_thread_new(d.opt.udp_port, osc_error);
    lo_server_thread_add_method(osc, "/exit", "", osc_exit, &d);
    if(d.opt.use_midi) {
        lo_server_thread_add_method(osc, "/midi", "b", osc_midi, &d);
    }
    lo_server_thread_add_method(osc, "/set_param", "if", osc_set_param, &d);
    lo_server_thread_add_method(osc, "/set_param_seq", NULL, osc_set_param_seq, &d);
    lo_server_thread_add_method(osc, "/print_param", "", osc_print_param, &d);
    lo_server_thread_add_method(osc, "/set_program", "i", osc_set_program, &d);
    lo_server_thread_start(osc);
    printf("HOST> VST BEGIN\n");
    d.effect = vst_begin(module);
    printf("HOST> VST CHECK AUDIO AND MIDI I/O\n");
    vst_require_audio_io(d.effect,(VstInt32)d.opt.n_in_channels,(VstInt32)d.opt.n_out_channels);
    if(d.opt.use_midi) {
        vst_require_midi(d.effect);
    }
    printf("HOST> #PROGRAMS = %d, #PARAMS = %d\n", d.effect->numPrograms, d.effect->numParams);
    if(d.opt.verbose) {
        printf("HOST> PARAM NAMES\n");
        vst_param_pp(d.effect);
    }
    pthread_t x11_thread;
    printf("HOST> X11 EDITOR = %s\n",d.opt.x11 ? "TRUE" : "FALSE");
    if(d.opt.x11) {
        printf("HOST> CREATE X11 EDITOR THREAD\n");
        pthread_create(&x11_thread, NULL, x11_thread_proc, &d);
    }
    printf("HOST> CONNECT TO JACK\n");
    char client_name[64] = "jack-lxvst"; /* LIMIT */
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
    if(d.opt.use_midi) {
        printf("HOST> MAKE JACK MIDI INPUT PORT\n");
        jack_port_make_standard(client, &d.midi_in, 1, false, true);
    }
    printf("HOST> MAKE JACK AUDIO INPUT & OUTPUT PORTS\n");
    jack_port_make_standard(client, d.audio_in, d.opt.n_in_channels, false, false);
    jack_port_make_standard(client, d.audio_out, d.opt.n_out_channels, true, false);
    printf("HOST> ACTIVATE JACK CLIENT\n");
    jack_client_activate(client);
    if(d.opt.use_midi) {
        char *midi_src_name = getenv("JACK_LXVST_MIDI_CONNECT_FROM");
        printf("HOST> CONNECT MIDI\n");
        if (midi_src_name) {
            printf("HOST> MIDI INPUT = %s\n", midi_src_name);
            char midi_dst_name[128]; /* LIMIT */
            snprintf(midi_dst_name, 128, "%s:midi_in_1", client_name);
            jack_port_connect_named(client, midi_src_name, midi_dst_name);
        }
    }
    printf("HOST> CONNECT AUDIO\n");
    char *audio_src_pattern = getenv("JACK_LXVST_CONNECT_FROM");
    if (audio_src_pattern) {
        printf("HOST> AUDIO INPUT = %s\n", audio_src_pattern);
        char cl_pattern[128]; /* LIMIT */
        snprintf(cl_pattern, 128, "%s:in_%%d", client_name);
        jack_port_connect_pattern(client, d.opt.n_in_channels, 0, audio_src_pattern, cl_pattern);
    }
    char *audio_dst_pattern = getenv("JACK_LXVST_CONNECT_TO");
    if (audio_dst_pattern) {
        printf("HOST> AUDIO OUTPUT = %s\n", audio_dst_pattern);
        char cl_pattern[128]; /* LIMIT */
        snprintf(cl_pattern, 128, "%s:out_%%d", client_name);
        jack_port_connect_pattern(client, d.opt.n_out_channels, 0, cl_pattern, audio_dst_pattern);
    }
    printf("HOST> WAIT FOR USER EXIT (OSC /EXIT OR EDITOR CLOSE)\n");
    while (d.usr_exit == false) {
        pause_for(0.5);
    }
    printf("HOST> JACK CLIENT CLOSE\n");
    jack_client_close(client);
    if(d.opt.x11) {
        printf("HOST> CLOSE EDITOR\n");
        d.effect->dispatcher(d.effect, effEditClose, 0, 0, 0, 0);
    }
    printf("HOST> CLOSE EFFECT\n");
    d.effect->dispatcher(d.effect, effClose, 0, 0, 0, 0);
    if(d.opt.x11) {
        printf("HOST> JOIN X11 THREAD\n");
        pthread_join(x11_thread, NULL);
        printf("HOST> CLOSE X11\n");
        XCloseDisplay(d.x11_dpy);
    }
    printf("HOST> OSC THREAD FREE\n");
    lo_server_thread_free(osc);
    printf("HOST> CLOSE MODULE\n");
    xdlclose(module);
    printf("HOST> FREE MEMORY\n");
    free(d.in);
    free(d.audio_in);
    free(d.out);
    free(d.audio_out);
    return 0;
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
            d->usr_exit = true;
            return NULL;
        }
    }

    return NULL;
}
