/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcPreprocess.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#define _mcPreprocess_H
#define _mcPreprocess_C

#   include "GSYSTEM.h"
#   include "GDynamicStrings.h"
#   include "Glibc.h"
#   include "Galists.h"
#   include "GM2RTS.h"
#   include "GFIO.h"
#   include "GmcPrintf.h"
#   include "GmcOptions.h"

static alists_alist listOfFiles;
DynamicStrings_String mcPreprocess_preprocessModule (DynamicStrings_String filename);
static DynamicStrings_String makeTempFile (DynamicStrings_String ext);
static DynamicStrings_String onExitDelete (DynamicStrings_String filename);
static void removeFile (void * a);
static void removeFiles (void);

static DynamicStrings_String makeTempFile (DynamicStrings_String ext)
{
  return DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "/tmp/mctemp.", 12), ext);
}

static DynamicStrings_String onExitDelete (DynamicStrings_String filename)
{
  alists_includeItemIntoList (listOfFiles, (void *) DynamicStrings_Dup (filename));
  return filename;
}

static void removeFile (void * a)
{
  DynamicStrings_String s;

  s = a;
  if ((libc_unlink (DynamicStrings_string (s))) != 0)
    ;  /* empty.  */
}

static void removeFiles (void)
{
  alists_foreachItemInListDo (listOfFiles, (alists_performOperation) {(alists_performOperation_t) removeFile});
}

DynamicStrings_String mcPreprocess_preprocessModule (DynamicStrings_String filename)
{
  DynamicStrings_String tempfile;
  DynamicStrings_String command;
  DynamicStrings_String commandLine;
  unsigned int pos;

  command = mcOptions_getCppCommandLine ();
  if (DynamicStrings_EqualArray (command, (char *) "", 0))
    return filename;
  else
    {
      tempfile = DynamicStrings_InitStringCharStar ((void *) makeTempFile (DynamicStrings_InitString ((char *) "cpp", 3)));
      commandLine = DynamicStrings_Dup (command);
      commandLine = DynamicStrings_ConCat (DynamicStrings_ConCat (DynamicStrings_ConCat (DynamicStrings_ConCatChar (DynamicStrings_Dup (commandLine), ' '), filename), DynamicStrings_Mark (DynamicStrings_InitString ((char *) " -o ", 4))), tempfile);
      if (mcOptions_getVerbose ())
        mcPrintf_fprintf1 (FIO_StdOut, (char *) "%s\\", 4, (unsigned char *) &commandLine, sizeof (commandLine));
      if ((libc_system (DynamicStrings_string (commandLine))) != 0)
        {
          mcPrintf_fprintf1 (FIO_StdErr, (char *) "C preprocessor failed when preprocessing %s\\", 45, (unsigned char *) &filename, sizeof (filename));
          libc_exit (1);
        }
      commandLine = DynamicStrings_KillString (commandLine);
      return onExitDelete (tempfile);
    }
}

void _M2_mcPreprocess_init (int argc, char *argv[])
{
  listOfFiles = alists_initList ();
  if (M2RTS_InstallTerminationProcedure ((PROC ) {(PROC_t) removeFiles}))
    M2RTS_HALT (0);
}