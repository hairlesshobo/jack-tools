#define __cdecl

/* VST */
#include "pluginterfaces/vst2.x/aeffectx.h"

/* C99 */
#include <stdio.h>

#include "r-common/c/dl.c"
#include "r-common/c/vst.c"

int main(int argc, char *argv[])
{
    if (argc != 2) {
        printf("USAGE = LXVST-QUERY LXVST-FILE\n");
        return -1;
    }
    vst_verify_platform();
    const char *vst_file = argv[1];
    printf("VST FILE = %s\n", vst_file);
    void *m = xdlopen(vst_file,RTLD_LAZY);
    AEffect *e = vst_begin(m);
    printf("VST AUDIO I/O: IN = %d OUT = %d\n",e->numInputs, e->numOutputs);
    printf("VST MIDI: %s\n",vst_check_midi(e) ? "TRUE" : "FALSE");
    printf("NUM-PROGRAMS = %d\n", e->numPrograms);
    printf("NUM-PARAMS = %d\n", e->numParams);
    printf("PARAM NAMES\n");
    vst_param_pp(e);
    e->dispatcher(e, effClose, 0, 0, 0, 0);
    xdlclose(m);
    return 0;
}
