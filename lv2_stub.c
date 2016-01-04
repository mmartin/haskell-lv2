#include <stdlib.h>
#include <lv2/lv2plug.in/ns/lv2core/lv2.h>

#include "LV2_stub.h"

LV2_SYMBOL_EXPORT
const LV2_Descriptor*
lv2_descriptor(uint32_t index)
{
    HsStablePtr p = lv2descriptors(index);
    if (!p) {
        return NULL;
    }

    LV2_Descriptor* d = malloc(sizeof(LV2_Descriptor));

    d->URI = descUri(p);
    d->instantiate = (LV2_Handle(*)(const struct _LV2_Descriptor*,
                                    double, const char*,
                                    const LV2_Feature *const*)) descInst(p);
    d->connect_port = (void(*)(LV2_Handle, uint32_t, void*)) connectPortHs;
    d->activate = (void(*)(LV2_Handle)) activateHs;
    d->run = (void(*)(LV2_Handle, uint32_t)) runHs;
    d->deactivate = (void(*)(LV2_Handle)) deactivateHs;
    d->cleanup = (void(*)(LV2_Handle)) cleanupHs;
    d->extension_data = NULL;

    return d;
}

static void init(void) __attribute__((constructor));
void init(void)
{
    static char* argv[] = { "lv2.so", NULL }, **argv_ = argv;
    static int argc = 1;
    hs_init(&argc, &argv_);
}

static void finit(void) __attribute__((destructor));
void finit(void)
{
    hs_exit();
}
