/* https://github.com/google/music-synthesizer-for-android */

#define __cdecl

/* C99 */
#include <stdio.h>
#include <stdlib.h>

/* POSIX */
#include <dlfcn.h>
#include <pthread.h>

/* JACK */
#include <jack/jack.h>
#include <jack/midiport.h>

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
#include "c-common/observe-signal.h"
#include "c-common/observe-signal.c"
#include "c-common/time-pause.h"
#include "c-common/time-pause.c"
#include "c-common/time-timespec.h"
#include "c-common/time-timespec.c"

/* MSFA */
#include "dx7/synth.h"
#include "dx7/module.h"
#include "dx7/freqlut.h"
#include "dx7/sin.h"
//#include "dx7/sawtooth.h"
//#include "dx7/synth_unit.h"


struct dx7_opt
{
    bool unique_name;
    bool verbose;
    float sample_rate;
    u8 note_data_ch;
    u8 param_data_ch;
    float vel_mul;
};

struct dx7_opt dx7_opt_default()
{
    return {false, false, 0, 0, 0, 1};
}

#define MAX_BUF_SZ 16384

struct dx7
{
    float sample_rate;
    jack_port_t *audio_out;
    jack_port_t *midi_in;
    float *out;
    struct dx7_opt opt;
    SynthUnit *synth_unit_;
    RingBuffer ring_buffer_;
    int16_t buf[MAX_BUF_SZ];
};

#define MAX_MIDI_MESSAGES 64

void midi_proc(dx7 * d, jack_nframes_t nframes)
{
    void *b = jack_port_get_buffer(d->midi_in, nframes);
    jack_nframes_t jack_e_n = jack_midi_get_event_count(b);
    if (jack_e_n > MAX_MIDI_MESSAGES) {
        printf("HOST> TOO MANY INCOMING MIDI EVENTS\n");
        return;
    }
    if (jack_e_n > 0) {
        for (jack_nframes_t i = 0; i < jack_e_n; i++) {
            /* printf("HOST> PROC MIDI EVENT\n"); */
            jack_midi_event_t e;
            jack_midi_event_get(&e, b, i);
            d->ring_buffer_.Write(e.buffer, e.size);
        }
    }
}

#define break_on(x,s)                                                          \
    if(x){								       \
	fprintf(stderr,"HOST> BREAK ON> %s: %s, %d\n", s, __FILE__, __LINE__); \
	return 0;							       \
    }

int audio_proc(jack_nframes_t nframes, void *ptr)
{
    break_on(nframes > MAX_BUF_SZ, "MAX_BUF_SZ");
    struct dx7 *d = (struct dx7 *) ptr;
    midi_proc(d, nframes);
    d->out = (float *) jack_port_get_buffer(d->audio_out, nframes);
    d->synth_unit_->GetSamples(nframes, d->buf);
    for (jack_nframes_t i = 0; i < nframes; i++) {
        d->out[i] = ((float)d->buf[i]) / 16384.0;
    }
    return 0;
}

int load_syx(dx7 * d, const char *filename) {
  uint8_t syx_data[4104];
  FILE *fp;
  fp = fopen(filename, "rb");
  break_on(!fp, "LOAD SYX (OPEN)");
  size_t err = fread((char *)syx_data, 1, 4104, fp);
  break_on(err != 4104, "LOAD SYX (READ)");
  d->ring_buffer_.Write(syx_data, 4104);
  return 0;
}

void usage(void)
{
    eprintf("Usage: jack-dx7 [ options ] dx7-syx\n");
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
    observe_signals();
    struct dx7 d;

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

    printf("HOST> PROCESS ARGUMENTS\n");
    if (argc < 2) {
        printf("HOST> USAGE = JACK-DX7 DX7-SYX\n");
        return -1;
    }
    if (optind > argc - 1) {
        usage();
    }
    const char *syx_fn = argv[optind];
    printf("HOST> CONNECT TO JACK\n");
    char client_name[64] = "jack-dx7";
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
    printf("HOST> INIT DX7\n");
    d.synth_unit_ = new SynthUnit(&(d.ring_buffer_));
    SynthUnit::Init(d.sample_rate);
    load_syx(&d,syx_fn);
    printf("HOST> MAKE JACK MIDI INPUT PORT\n");
    jack_port_make_standard(client, &d.midi_in, 1, false, true);
    printf("HOST> MAKE JACK AUDIO OUTPUT PORTS\n");
    jack_port_make_standard(client, &d.audio_out, 1, true, false);
    printf("HOST> ACTIVATE JACK CLIENT\n");
    jack_client_activate(client);
    char *midi_src_name = getenv("JACK_DX7_MIDI_CONNECT_FROM");
    printf("HOST> CONNECT MIDI\n");
    if (midi_src_name) {
        printf("HOST> MIDI INPUT = %s\n", midi_src_name);
        char midi_dst_name[128];
        snprintf(midi_dst_name, 128, "%s:midi_in_1", client_name);
        jack_port_connect_named(client, midi_src_name, midi_dst_name);
    }
    printf("HOST> CONNECT AUDIO\n");
    char *audio_dst_name = getenv("JACK_DX7_CONNECT_TO");
    if (audio_dst_name) {
        printf("HOST> AUDIO OUTPUT = %s\n", audio_dst_name);
        char audio_src_name[128];
        snprintf(audio_src_name, 128, "%s:out_1", client_name);
        jack_port_connect_named(client, audio_src_name, audio_dst_name);
    }
    printf("HOST> WAIT FOR...\n");
    while(!observe_end_of_process()) {
        pause_for(0.25);
    }
    printf("HOST> JACK CLIENT CLOSE\n");
    jack_client_close(client);
    return 0;
}
