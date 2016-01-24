#include <stdlib.h>
#include <lv2/lv2plug.in/ns/lv2core/lv2.h>

#include "LV2_stub.h"

extern void hs_register_plugins();

typedef struct {
    HsStablePtr plugin;
    LV2_Descriptor* descriptor;
} Plugin;

// TODO: make this dynamic
static Plugin plugins[256];
static uint32_t plugin_count = 0;

void
register_plugin(HsStablePtr p)
{
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

    plugins[plugin_count].plugin = p;
    plugins[plugin_count].descriptor = d;

    ++plugin_count;
}

LV2_SYMBOL_EXPORT
const LV2_Descriptor*
lv2_descriptor(uint32_t index)
{
    if (plugin_count == 0) {
        hs_register_plugins();
    }

    if (index < 0 || index >= plugin_count) {
        return NULL;
    }
    return plugins[index].descriptor;
}

static void init(void) __attribute__((constructor));
void
init(void)
{
    static char* argv[] = { "libHShaskell-lv2.so", NULL }, **argv_ = argv;
    static int argc = 1;
    hs_init(&argc, &argv_);
}

static void finit(void) __attribute__((destructor));
void
finit(void)
{
    for (size_t i = 0; i < plugin_count; ++i) {
        Plugin* p = &plugins[i];
        free((void*)p->descriptor->URI);
        hs_free_fun_ptr((HsFunPtr)p->descriptor->instantiate);
        hs_free_stable_ptr(p->plugin);
    }
    hs_exit();
}
