#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GmcOptions.h"
#   include "GmcComp.h"
#   include "GM2RTS.h"


/*
   init - translate the source file after handling all the
          program arguments.
*/

static void init (void);

/*
   init - translate the source file after handling all the
          program arguments.
*/

static void init (void);


/*
   init - translate the source file after handling all the
          program arguments.
*/

static void init (void)
{
  M2RTS_ExitOnHalt (1);
  mcComp_compile (mcOptions_handleOptions ());
}

void _M2_top_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  init ();
}

void _M2_top_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
