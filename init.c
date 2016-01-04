#include <stdlib.h>
#include "HsFFI.h"

static void init (void) __attribute__ ((constructor));
void init (void) {
    static char* argv[] = { "amp.so", NULL }, **argv_ = argv;
    static int argc = 1;
    hs_init(&argc, &argv_);
}

static void finit (void) __attribute__ ((destructor));
void finit (void) {
hs_exit();
}
