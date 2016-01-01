#include <stdlib.h>
#include <stdio.h>

#include "lv2/lv2plug.in/ns/lv2core/lv2.h"

#include "Amp_stub.h"

#define AMP_URI "http://github.com/mmartin/amp"

static LV2_Handle
instantiate(const LV2_Descriptor*     descriptor,
            double                    rate,
            const char*               bundle_path,
            const LV2_Feature* const* features)
{
    static char* argv[] = { "amp.so", NULL }, **argv_ = argv;
    static int argc = 1;
    hs_init(&argc, &argv_);
    return (LV2_Handle)instantiate_hs((void*)descriptor,
                                      rate,
                                      (void*)bundle_path,
                                      (void*)features);
}

static void
cleanup(LV2_Handle instance)
{
    cleanup_hs(instance);
    hs_exit();
}

static const void*
extension_data(const char* uri)
{
    return NULL;
}

static const LV2_Descriptor descriptor = {
    AMP_URI,
    instantiate,
    connect_port,
    activate,
    run,
    deactivate,
    cleanup,
    extension_data
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
