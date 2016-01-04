#include <stdlib.h>

#include "lv2/lv2plug.in/ns/lv2core/lv2.h"

#include "Amp_stub.h"

static const LV2_Descriptor descriptor = {
    (const char*)"http://github.com/mmartin/amp",
    (LV2_Handle(*)(const struct _LV2_Descriptor*, double, const char*, const LV2_Feature *const*)) instantiate,
    (void(*)(LV2_Handle, uint32_t, void*)) connect_port,
    (void(*)(LV2_Handle)) activate,
    (void(*)(LV2_Handle, uint32_t)) run,
    (void(*)(LV2_Handle)) deactivate,
    (void(*)(LV2_Handle)) cleanup,
    (const void*(*)(const char*)) extension_data
};

LV2_SYMBOL_EXPORT
const LV2_Descriptor*
lv2_descriptor(uint32_t index)
{
    switch (index) {
        case 0:  return &descriptor;
        default: return NULL;
    }
}
