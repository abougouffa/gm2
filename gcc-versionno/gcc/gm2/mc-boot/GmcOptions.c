/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcOptions.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   if !defined (TRUE)
#      define TRUE (1==1)
#   endif

#   if !defined (FALSE)
#      define FALSE (1==0)
#   endif

#include <stddef.h>
#include <string.h>
#include <limits.h>
#define _mcOptions_H
#define _mcOptions_C

#   include "GSArgs.h"
#   include "GmcSearch.h"
#   include "Glibc.h"
#   include "GmcPrintf.h"
#   include "GDebug.h"
#   include "GStrLib.h"
#   include "Gdecl.h"
#   include "GDynamicStrings.h"

#   define YEAR "2016"
static unsigned int caseRuntime;
static unsigned int arrayRuntime;
static unsigned int returnRuntime;
static unsigned int ignoreFQ;
static unsigned int debugTopological;
static unsigned int extendedOpaque;
static unsigned int internalDebugging;
static unsigned int verbose;
static unsigned int quiet;
static DynamicStrings_String hPrefix;
static DynamicStrings_String outputFile;
static DynamicStrings_String cppArgs;
static DynamicStrings_String cppProgram;
DynamicStrings_String mcOptions_handleOptions (void);
unsigned int mcOptions_getQuiet (void);
unsigned int mcOptions_getVerbose (void);
unsigned int mcOptions_getInternalDebugging (void);
DynamicStrings_String mcOptions_getCppCommandLine (void);
DynamicStrings_String mcOptions_getOutputFile (void);
unsigned int mcOptions_getExtendedOpaque (void);
void mcOptions_setDebugTopological (unsigned int value);
unsigned int mcOptions_getDebugTopological (void);
DynamicStrings_String mcOptions_getHPrefix (void);
unsigned int mcOptions_getIgnoreFQ (void);
static void displayVersion (unsigned int mustExit);
static void displayHelp (void);
static void setOutputFile (DynamicStrings_String output);
static void setQuiet (unsigned int value);
static void setVerbose (unsigned int value);
static void setExtendedOpaque (unsigned int value);
static void setSearchPath (DynamicStrings_String arg);
static void setInternalDebugging (unsigned int value);
static void setHPrefix (DynamicStrings_String s);
static void setIgnoreFQ (unsigned int value);
static unsigned int optionIs (char *left_, unsigned int _left_high, DynamicStrings_String right);
static void setLang (DynamicStrings_String arg);
static void handleOption (DynamicStrings_String arg);

static void displayVersion (unsigned int mustExit)
{
  mcPrintf_printf0 ((char *) "Copyright (C) ''2016'' Free Software Foundation, Inc.\\", 55);
  mcPrintf_printf0 ((char *) "License GPLv2: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>\\", 78);
  mcPrintf_printf0 ((char *) "This is free software: you are free to change and redistribute it.\\", 68);
  mcPrintf_printf0 ((char *) "There is NO WARRANTY, to the extent permitted by law.\\", 55);
  if (mustExit)
    libc_exit (0);
}

static void displayHelp (void)
{
  mcPrintf_printf0 ((char *) "usage: mc [--cpp] [-g] [--quiet] [--extended-opaque] [-q] [-v] [--verbose] [--version] [--help] [-h] [-Ipath] [--olang=c] [--olang=c++] [--olang=m2] [--debug-top] [--h-file-prefix=foo] [-o=foo] filename\\", 204);
  mcPrintf_printf0 ((char *) "  --cpp               preprocess through the C preprocessor\\", 61);
  mcPrintf_printf0 ((char *) "  -g                  emit debugging directives in the output language", 70);
  mcPrintf_printf0 ((char *) "                      so that the debugger will refer to the source\\", 69);
  mcPrintf_printf0 ((char *) "  -q --quiet          no output unless an error occurs\\", 56);
  mcPrintf_printf0 ((char *) "  -v --verbose        display preprocessor if invoked\\", 55);
  mcPrintf_printf0 ((char *) "  --version           display version and exit\\", 48);
  mcPrintf_printf0 ((char *) "  -h --help           display this help message\\", 49);
  mcPrintf_printf0 ((char *) "  -Ipath              set the module search path\\", 50);
  mcPrintf_printf0 ((char *) "  --olang=c           generate ansi C output\\", 46);
  mcPrintf_printf0 ((char *) "  --olang=c++         generate ansi C++ output\\", 48);
  mcPrintf_printf0 ((char *) "  --olang=m2          generate PIM4 output\\", 44);
  mcPrintf_printf0 ((char *) "  --extended-opaque   parse definition and implementation modules to\\", 70);
  mcPrintf_printf0 ((char *) "                      generate full type debugging of opaque types\\", 68);
  mcPrintf_printf0 ((char *) "  --debug-top         debug topological data structure resolving (internal)\\", 77);
  mcPrintf_printf0 ((char *) "  --h-file-prefix=foo set the h file prefix to foo\\", 52);
  mcPrintf_printf0 ((char *) "  -o=foo              set the output file to foo\\", 50);
  mcPrintf_printf0 ((char *) "  --ignore-fq         do not generate fully qualified idents\\", 62);
  mcPrintf_printf0 ((char *) "  filename            the source file must be the last option\\", 63);
  libc_exit (0);
}

static void setOutputFile (DynamicStrings_String output)
{
  outputFile = output;
}

static void setQuiet (unsigned int value)
{
  quiet = value;
}

static void setVerbose (unsigned int value)
{
  verbose = value;
}

static void setExtendedOpaque (unsigned int value)
{
  extendedOpaque = value;
}

static void setSearchPath (DynamicStrings_String arg)
{
  mcSearch_prependSearchPath (arg);
}

static void setInternalDebugging (unsigned int value)
{
  internalDebugging = value;
}

static void setHPrefix (DynamicStrings_String s)
{
  hPrefix = s;
}

static void setIgnoreFQ (unsigned int value)
{
  ignoreFQ = value;
}

static unsigned int optionIs (char *left_, unsigned int _left_high, DynamicStrings_String right)
{
  DynamicStrings_String s;
  char left[_left_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (left, left_, _left_high);

  if ((DynamicStrings_Length (right)) == (StrLib_StrLen ((char *) left, _left_high)))
    return DynamicStrings_EqualArray (right, (char *) left, _left_high);
}

static void setLang (DynamicStrings_String arg)
{
  if (optionIs ((char *) "c", 1, arg))
    decl_setLangC ();
}

static void handleOption (DynamicStrings_String arg)
{
  if ((optionIs ((char *) "--quiet", 7, arg)) || (optionIs ((char *) "-q", 2, arg)))
    setQuiet (TRUE);
}

DynamicStrings_String mcOptions_handleOptions (void)
{
  unsigned int i;
  DynamicStrings_String arg;

  i = 1;
  while (SArgs_GetArg (&arg, i))
    {
      if ((DynamicStrings_Length (arg)) > 0)
        if ((DynamicStrings_char (arg, 0)) == '-')
          handleOption (arg);
        else
          return arg;
      i += 1;
    }
  return NULL;
}

unsigned int mcOptions_getQuiet (void)
{
  return quiet;
}

unsigned int mcOptions_getVerbose (void)
{
  return verbose;
}

unsigned int mcOptions_getInternalDebugging (void)
{
  return internalDebugging;
}

DynamicStrings_String mcOptions_getCppCommandLine (void)
{
  DynamicStrings_String s;

  if (DynamicStrings_EqualArray (cppProgram, (char *) "", 0))
    return NULL;
  else
    {
      s = DynamicStrings_Dup (cppProgram);
      s = DynamicStrings_ConCat (DynamicStrings_ConCatChar (s, ' '), cppArgs);
      if (mcOptions_getQuiet ())
        s = DynamicStrings_ConCat (DynamicStrings_ConCatChar (s, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "-quiet", 6)));
      return s;
    }
}

DynamicStrings_String mcOptions_getOutputFile (void)
{
  return outputFile;
}

unsigned int mcOptions_getExtendedOpaque (void)
{
  return extendedOpaque;
}

void mcOptions_setDebugTopological (unsigned int value)
{
  debugTopological = value;
}

unsigned int mcOptions_getDebugTopological (void)
{
  return debugTopological;
}

DynamicStrings_String mcOptions_getHPrefix (void)
{
  return hPrefix;
}

unsigned int mcOptions_getIgnoreFQ (void)
{
  return ignoreFQ;
}

void _M2_mcOptions_init (int argc, char *argv[])
{
  caseRuntime = FALSE;
  arrayRuntime = FALSE;
  returnRuntime = FALSE;
  internalDebugging = FALSE;
  quiet = FALSE;
  verbose = FALSE;
  extendedOpaque = FALSE;
  debugTopological = FALSE;
  ignoreFQ = FALSE;
  hPrefix = DynamicStrings_InitString ((char *) "", 0);
  cppArgs = DynamicStrings_InitString ((char *) "", 0);
  cppProgram = DynamicStrings_InitString ((char *) "", 0);
  outputFile = DynamicStrings_InitString ((char *) "-", 1);
}
