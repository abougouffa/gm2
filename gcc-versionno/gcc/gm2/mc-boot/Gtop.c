/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/top.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GmcOptions.h"
#   include "GmcComp.h"

static void init (void);
static void init (void);

static void init (void)
{
  mcComp_compile (mcOptions_handleOptions ());
}

void _M2_top_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  init ();
}

void _M2_top_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
