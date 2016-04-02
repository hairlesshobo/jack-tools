/* https://github.com/ekenberg/vstminihost */

#define __cdecl

#include "pluginterfaces/vst2.x/aeffectx.h" /* VST */

#include <stdio.h> /* C99 */
#include <stdlib.h> /* C99 */

#include <dlfcn.h> /* POSIX */
#include <pthread.h> /* POSIX */

#include <X11/Xlib.h> /* X11 */
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include "c-common/jack-client.h"
#include "c-common/jack-client.c"
#include "c-common/jack-port.h"
#include "c-common/jack-port.c"
#include "c-common/jack-ringbuffer.h"
#include "c-common/jack-ringbuffer.c"
#include "c-common/memory.h"
#include "c-common/memory.c"

typedef AEffect* (*PluginEntryProc) (audioMasterCallback audioMaster);
static VstIntPtr VSTCALLBACK HostCallback (AEffect* effect, VstInt32 opcode, VstInt32 index, VstIntPtr value, void* ptr, float opt);

void *x11_thread_proc (void *ptr);

static bool checkPlatform ()
{
#if VST_64BIT_PLATFORM
    printf ("*** This is a 64 Bit Build! ***\n");
#else
    printf ("*** This is a 32 Bit Build! ***\n");
#endif

    int sizeOfVstIntPtr = sizeof (VstIntPtr);
    int sizeOfVstInt32 = sizeof (VstInt32);
    int sizeOfPointer = sizeof (void*);
    int sizeOfAEffect = sizeof (AEffect);

    printf ("VstIntPtr = %d Bytes, VstInt32 = %d Bytes, Pointer = %d Bytes, AEffect = %d Bytes\n\n",
	    sizeOfVstIntPtr, sizeOfVstInt32, sizeOfPointer, sizeOfAEffect);

    return sizeOfVstIntPtr == sizeOfPointer;
}

struct lxvst {
    AEffect *effect;
    Display *x11_dpy;
    bool x11_closed;
    float sample_rate;
    int channels;
    jack_port_t **output_port;
    float **out;
};

int audio_proc(jack_nframes_t nframes, void *ptr)
{
    struct lxvst *d = (struct lxvst *)ptr;
    if (d->effect->numInputs != 0 || d->effect->numOutputs != 2) {
	printf("HOST> PLUGIN NOT 0-IN/2-OUT\n");
	return 0;
    }
    d->effect->dispatcher (d->effect, effSetBlockSize, 0, nframes, 0, 0);
    for(int i = 0; i < d->channels; i++) {
	d->out[i] = (float *)jack_port_get_buffer(d->output_port[i], nframes);
    }
    d->effect->processReplacing (d->effect, NULL, d->out, (VstInt32)nframes);
    return 0;
}

int main (int argc, char* argv[])
{
    void* module;
    struct lxvst d;
    d.effect = NULL;
    d.x11_dpy = NULL;
    d.x11_closed = false;
    d.channels = 2;
    d.out = (float **)xmalloc(d.channels * sizeof(float *));
    d.output_port = (jack_port_t **)xmalloc(d.channels * sizeof(jack_port_t *));

    if (!checkPlatform ()) {
	printf ("Platform verification failed! Please check your Compiler Settings!\n");
	return -1;
    }

    if (argc < 2) {
	printf ("Usage: %s plugin-to-load\n", argv[0]);
	return -1;
    }

    const char* fileName = argv[1];

    /* Connect to JACK. */
    char client_name[64] = "jack-lxvst";
    jack_client_t *client = jack_client_unique_store(client_name);
    jack_set_error_function(jack_client_minimal_error_handler);
    jack_on_shutdown(client, jack_client_minimal_shutdown_handler, 0);
    jack_set_process_callback(client, audio_proc, &d);
    d.sample_rate = jack_get_sample_rate(client);

    // prepare Xlib for threads
    XInitThreads();

    printf ("HOST> LOAD LIBRARY\n");
    module = dlopen(fileName, RTLD_LAZY);
    if (! module) {
	printf("dlopen error: %s\n", dlerror());
	printf ("Failed to load VST Plugin library!\n");
	return -1;
    }

    PluginEntryProc mainEntry = 0;
    mainEntry = (PluginEntryProc) dlsym(module, "VSTPluginMain");
    if (!mainEntry) mainEntry = (PluginEntryProc) dlsym(module, "main");
    if (!mainEntry)
    {
	printf ("VST Plugin main entry not found!\n");
	return -1;
    }

    printf ("HOST> CREATE EFFECT\n");
    d.effect = mainEntry (HostCallback);
    if (!d.effect)
    {
	printf ("Failed to create effect instance!\n");
	return -1;
    }

    printf ("HOST> Init sequence...\n");
    d.effect->dispatcher (d.effect, effOpen, 0, 0, 0, 0);
    d.effect->dispatcher (d.effect, effSetSampleRate, 0, 0, 0, d.sample_rate);

    pthread_t x11_thread;
    pthread_create (&x11_thread,NULL,x11_thread_proc,&d);

    printf ("HOST> START EFFECT\n");
    d.effect->dispatcher (d.effect, effMainsChanged, 0, 1, 0, 0);
    printf ("HOST> MAKE JACK OUTPUT PORTS\n");
    jack_port_make_standard(client, d.output_port, d.channels, true);
    printf ("HOST> ACTIVATE JACK CLIENT\n");
    jack_client_activate(client);

    printf ("HOST> WAIT FOR EDITOR TO CLOSE\n");
    while(d.x11_closed == false) {
	sleep(1);
    }

    printf ("HOST> JACK CLIENT CLOSE\n");
    jack_client_close(client);
    printf ("HOST> CLOSE EDITOR\n");
    d.effect->dispatcher (d.effect, effEditClose, 0, 0, 0, 0);
    printf ("HOST> CLOSE EFFECT\n");
    d.effect->dispatcher (d.effect, effClose, 0, 0, 0, 0);
    printf ("HOST> JOIN X11 THREAD\n");
    pthread_join(x11_thread, NULL);
    printf ("HOST> CLOSE X11\n");
    XCloseDisplay(d.x11_dpy);
    printf ("HOST> CLOSE MODULE\n");
    dlclose(module);
    printf ("HOST> FREE MEMORY\n");
    free(d.output_port);
    return 0;
}

VstIntPtr VSTCALLBACK HostCallback (AEffect* effect, VstInt32 opcode, VstInt32 index, VstIntPtr value, void* ptr, float opt)
{
    VstIntPtr result = 0;
    switch (opcode) {
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
	printf ("This plug does not have an editor!\n");
	return NULL;
    }

    Display *dpy;
    Window win;
    char effect_name[256]; // arbitrary, vst GetEffectName is max 32 chars
    Atom wmDeleteMessage, prop_atom, val_atom;

    // create the window
    dpy = XOpenDisplay(NULL);
    d->x11_dpy = dpy;
    win = XCreateSimpleWindow(dpy, DefaultRootWindow(dpy), 0, 0, 300, 300, 0, 0, 0);

    // we want an event when the window is being closed
    wmDeleteMessage = XInternAtom(dpy, "WM_DELETE_WINDOW", false);
    XSetWMProtocols(dpy, win, &wmDeleteMessage, 1);

    // Make the window a Dialog, maybe the window manager will place it centered
    prop_atom = XInternAtom(dpy, "_NET_WM_WINDOW_TYPE", False);
    val_atom = XInternAtom(dpy, "_NET_WM_WINDOW_TYPE_DIALOG", False);
    XChangeProperty(dpy, win, prop_atom, XA_ATOM, 32, PropModeReplace, (unsigned char *)&val_atom, 1);

    // prepare the plugin name in the title bar
    effect->dispatcher(effect, effGetEffectName, 0, 0, effect_name, 0);
    strcat(effect_name, " [jack-lxvst]");
    XStoreName(dpy, win, effect_name);

    // Get and prepare editor size
    ERect* eRect = 0;
    printf ("HOST> Get editor rect..\n");
    effect->dispatcher (effect, effEditGetRect, 0, 0, &eRect, 0);
    if (eRect) {
	int width = eRect->right - eRect->left;
	int height = eRect->bottom - eRect->top;
	printf("GetRect -> %d, %d\n", width, height);
	XResizeWindow(dpy, win, width, height);
    }

    // ? Is it correct to effEditGetRect above, before effEditOpen ?
    // Display the window, let the plugin populate it
    printf ("HOST> Open editor...\n");
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
	if (e.type == ClientMessage && e.xclient.data.l[0] == wmDeleteMessage) {
	    printf("HOST> XEVENT == wmDeleteMessage\n");
	    d->x11_closed = true;
	    return NULL;
	}
    }

    return NULL;
}
