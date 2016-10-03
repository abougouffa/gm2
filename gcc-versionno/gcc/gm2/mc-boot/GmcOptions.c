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
  mcPrintf_printf0 ((char *) "Copyright (C) ''2016'' Free Software Foundation, Inc.\\n", 55);
  mcPrintf_printf0 ((char *) "License GPLv2: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>\\n", 78);
  mcPrintf_printf0 ((char *) "This is free software: you are free to change and redistribute it.\\n", 68);
  mcPrintf_printf0 ((char *) "There is NO WARRANTY, to the extent permitted by law.\\n", 55);
  if (mustExit)
    libc_exit (0);
}

static void displayHelp (void)
{
  mcPrintf_printf0 ((char *) "usage: mc [--cpp] [-g] [--quiet] [--extended-opaque] [-q] [-v] [--verbose] [--version] [--help] [-h] [-Ipath] [--olang=c] [--olang=c++] [--olang=m2] [--debug-top] [--h-file-prefix=foo] [-o=foo] filename\\n", 204);
  mcPrintf_printf0 ((char *) "  --cpp               preprocess through the C preprocessor\\n", 61);
  mcPrintf_printf0 ((char *) "  -g                  emit debugging directives in the output language", 70);
  mcPrintf_printf0 ((char *) "                      so that the debugger will refer to the source\\n", 69);
  mcPrintf_printf0 ((char *) "  -q --quiet          no output unless an error occurs\\n", 56);
  mcPrintf_printf0 ((char *) "  -v --verbose        display preprocessor if invoked\\n", 55);
  mcPrintf_printf0 ((char *) "  --version           display version and exit\\n", 48);
  mcPrintf_printf0 ((char *) "  -h --help           display this help message\\n", 49);
  mcPrintf_printf0 ((char *) "  -Ipath              set the module search path\\n", 50);
  mcPrintf_printf0 ((char *) "  --olang=c           generate ansi C output\\n", 46);
  mcPrintf_printf0 ((char *) "  --olang=c++         generate ansi C++ output\\n", 48);
  mcPrintf_printf0 ((char *) "  --olang=m2          generate PIM4 output\\n", 44);
  mcPrintf_printf0 ((char *) "  --extended-opaque   parse definition and implementation modules to\\n", 70);
  mcPrintf_printf0 ((char *) "                      generate full type debugging of opaque types\\n", 68);
  mcPrintf_printf0 ((char *) "  --debug-top         debug topological data structure resolving (internal)\\n", 77);
  mcPrintf_printf0 ((char *) "  --h-file-prefix=foo set the h file prefix to foo\\n", 52);
  mcPrintf_printf0 ((char *) "  -o=foo              set the output file to foo\\n", 50);
  mcPrintf_printf0 ((char *) "  --ignore-fq         do not generate fully qualified idents\\n", 62);
  mcPrintf_printf0 ((char *) "  filename            the source file must be the last option\\n", 63);
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
  memcpy (left, left_, _left_high+1);

  if ((DynamicStrings_Length (right)) == (StrLib_StrLen ((char *) left, _left_high)))
    return DynamicStrings_EqualArray (right, (char *) left, _left_high);
  else if ((DynamicStrings_Length (right)) > (StrLib_StrLen ((char *) left, _left_high)))
    {
      s = DynamicStrings_Mark (DynamicStrings_Slice (right, 0, (int ) StrLib_StrLen ((char *) left, _left_high)));
      return DynamicStrings_EqualArray (s, (char *) left, _left_high);
    }
  else
    return FALSE;
}

static void setLang (DynamicStrings_String arg)
{
  if (optionIs ((char *) "c++", 3, arg))
    decl_setLangCP ();
  else if (optionIs ((char *) "c", 1, arg))
    decl_setLangC ();
  else if (optionIs ((char *) "m2", 2, arg))
    decl_setLangM2 ();
  else
    displayHelp ();
}

static void handleOption (DynamicStrings_String arg)
{
  if ((optionIs ((char *) "--quiet", 7, arg)) || (optionIs ((char *) "-q", 2, arg)))
    setQuiet (TRUE);
  else if ((optionIs ((char *) "--verbose", 9, arg)) || (optionIs ((char *) "-v", 2, arg)))
    setVerbose (TRUE);
  else if (optionIs ((char *) "--version", 9, arg))
    displayVersion (TRUE);
  else if (optionIs ((char *) "--olang=", 8, arg))
    setLang (DynamicStrings_Slice (arg, 8, 0));
  else if (optionIs ((char *) "-I", 2, arg))
    setSearchPath (DynamicStrings_Slice (arg, 2, 0));
  else if ((optionIs ((char *) "--help", 6, arg)) || (optionIs ((char *) "-h", 2, arg)))
    displayHelp ();
  else if (optionIs ((char *) "--cpp", 5, arg))
    cppProgram = DynamicStrings_InitString ((char *) "cpp", 3);
  else if (optionIs ((char *) "-o=", 3, arg))
    setOutputFile (DynamicStrings_Slice (arg, 3, 0));
  else if (optionIs ((char *) "--extended-opaque", 17, arg))
    setExtendedOpaque (TRUE);
  else if (optionIs ((char *) "--debug-top", 11, arg))
    mcOptions_setDebugTopological (TRUE);
  else if (optionIs ((char *) "--h-file-prefix=", 16, arg))
    setHPrefix (DynamicStrings_Slice (arg, 16, 0));
  else if (optionIs ((char *) "--ignore-fq", 11, arg))
    setIgnoreFQ (TRUE);
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

void _M2_mcOptions_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
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

void _M2_mcOptions_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
