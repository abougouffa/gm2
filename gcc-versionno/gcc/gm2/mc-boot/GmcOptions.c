/* automatically created by mc from ../../gcc-versionno/gcc/gm2/mc/mcOptions.mod.  */

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
#   include "GFIO.h"
#   include "GSFIO.h"

#   define YEAR "2019"
static unsigned int langC;
static unsigned int langCPP;
static unsigned int langM2;
static unsigned int gplHeader;
static unsigned int glplHeader;
static unsigned int summary;
static unsigned int contributed;
static unsigned int caseRuntime;
static unsigned int arrayRuntime;
static unsigned int returnRuntime;
static unsigned int ignoreFQ;
static unsigned int debugTopological;
static unsigned int extendedOpaque;
static unsigned int internalDebugging;
static unsigned int verbose;
static unsigned int quiet;
static DynamicStrings_String projectContents;
static DynamicStrings_String summaryContents;
static DynamicStrings_String contributedContents;
static DynamicStrings_String hPrefix;
static DynamicStrings_String outputFile;
static DynamicStrings_String cppArgs;
static DynamicStrings_String cppProgram;

/*
   handleOptions - iterates over all options setting appropriate
                   values and returns the single source file
                   if found at the end of the arguments.
*/

DynamicStrings_String mcOptions_handleOptions (void);

/*
   getQuiet - return the value of quiet.
*/

unsigned int mcOptions_getQuiet (void);

/*
   getVerbose - return the value of verbose.
*/

unsigned int mcOptions_getVerbose (void);

/*
   getInternalDebugging - return the value of internalDebugging.
*/

unsigned int mcOptions_getInternalDebugging (void);

/*
   getCppCommandLine - returns the Cpp command line and all arguments.
*/

DynamicStrings_String mcOptions_getCppCommandLine (void);

/*
   getOutputFile - sets the output filename to output.
*/

DynamicStrings_String mcOptions_getOutputFile (void);

/*
   getExtendedOpaque - return the extendedOpaque value.
*/

unsigned int mcOptions_getExtendedOpaque (void);

/*
   setDebugTopological - sets the flag debugTopological to value.
*/

void mcOptions_setDebugTopological (unsigned int value);

/*
   getDebugTopological - returns the flag value of the command
                         line option --debug-top.
*/

unsigned int mcOptions_getDebugTopological (void);

/*
   getHPrefix - saves the H file prefix.
*/

DynamicStrings_String mcOptions_getHPrefix (void);

/*
   getIgnoreFQ - returns the ignorefq flag.
*/

unsigned int mcOptions_getIgnoreFQ (void);

/*
   writeGPLheader - writes out the GPL or the LGPL as a comment.
*/

void mcOptions_writeGPLheader (FIO_File f);

/*
   displayVersion - displays the version of the compiler.
*/

static void displayVersion (unsigned int mustExit);

/*
   displayHelp - display the mc help summary.
*/

static void displayHelp (void);

/*
   commentBegin - issue a start of comment for the appropriate language.
*/

static void commentBegin (FIO_File f);

/*
   commentEnd - issue an end of comment for the appropriate language.
*/

static void commentEnd (FIO_File f);

/*
   comment - write a comment to file, f, and also a newline.
*/

static void comment (FIO_File f, char *a_, unsigned int _a_high);

/*
   commentS - write a comment to file, f, and also a newline.
*/

static void commentS (FIO_File f, DynamicStrings_String s);

/*
   gplBody -
*/

static void gplBody (FIO_File f);

/*
   glplBody -
*/

static void glplBody (FIO_File f);

/*
   issueGPL - writes out the summary, GPL/LGPL and/or contributed as a single comment.
*/

static void issueGPL (FIO_File f);

/*
   setOutputFile - sets the output filename to output.
*/

static void setOutputFile (DynamicStrings_String output);

/*
   setQuiet - sets the quiet flag to, value.
*/

static void setQuiet (unsigned int value);

/*
   setVerbose - sets the verbose flag to, value.
*/

static void setVerbose (unsigned int value);

/*
   setExtendedOpaque - set extendedOpaque to value.
*/

static void setExtendedOpaque (unsigned int value);

/*
   setSearchPath - set the search path for the module sources.
*/

static void setSearchPath (DynamicStrings_String arg);

/*
   setInternalDebugging - turn on/off internal debugging.
*/

static void setInternalDebugging (unsigned int value);

/*
   setHPrefix - saves the H file prefix.
*/

static void setHPrefix (DynamicStrings_String s);

/*
   setIgnoreFQ - sets the ignorefq flag.
*/

static void setIgnoreFQ (unsigned int value);

/*
   optionIs - returns TRUE if the first len (right) characters
              match left.
*/

static unsigned int optionIs (char *left_, unsigned int _left_high, DynamicStrings_String right);

/*
   setLang - set the appropriate output language.
*/

static void setLang (DynamicStrings_String arg);

/*
   handleOption -
*/

static void handleOption (DynamicStrings_String arg);


/*
   displayVersion - displays the version of the compiler.
*/

static void displayVersion (unsigned int mustExit)
{
  mcPrintf_printf0 ((char *) "Copyright (C) ''2019'' Free Software Foundation, Inc.\\n", 55);
  mcPrintf_printf0 ((char *) "License GPLv2: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>\\n", 78);
  mcPrintf_printf0 ((char *) "This is free software: you are free to change and redistribute it.\\n", 68);
  mcPrintf_printf0 ((char *) "There is NO WARRANTY, to the extent permitted by law.\\n", 55);
  if (mustExit)
    libc_exit (0);
}


/*
   displayHelp - display the mc help summary.
*/

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
  mcPrintf_printf0 ((char *) "  --gpl-header        generate a GPL3 header comment at the top of the file\\n", 77);
  mcPrintf_printf0 ((char *) "  --glpl-header       generate a LGPL3 header comment at the top of the file\\n", 78);
  mcPrintf_printf0 ((char *) "  --summary=\"foo\"     generate a one line summary comment at the top of the file\\n", 82);
  mcPrintf_printf0 ((char *) "  --contributed=\"foo\" generate a one line contribution comment near the top of the file\\n", 89);
  mcPrintf_printf0 ((char *) "  filename            the source file must be the last option\\n", 63);
  libc_exit (0);
}


/*
   commentBegin - issue a start of comment for the appropriate language.
*/

static void commentBegin (FIO_File f)
{
  if (langC || langCPP)
    FIO_WriteString (f, (char *) "/* ", 3);
  else if (langM2)
    FIO_WriteString (f, (char *) "(* ", 3);
}


/*
   commentEnd - issue an end of comment for the appropriate language.
*/

static void commentEnd (FIO_File f)
{
  if (langC || langCPP)
    {
      FIO_WriteString (f, (char *) " */", 3);
      FIO_WriteLine (f);
    }
  else if (langM2)
    {
      FIO_WriteString (f, (char *) " *)", 3);
      FIO_WriteLine (f);
    }
}


/*
   comment - write a comment to file, f, and also a newline.
*/

static void comment (FIO_File f, char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  FIO_WriteString (f, (char *) a, _a_high);
  FIO_WriteLine (f);
}


/*
   commentS - write a comment to file, f, and also a newline.
*/

static void commentS (FIO_File f, DynamicStrings_String s)
{
  s = SFIO_WriteS (f, s);
  FIO_WriteLine (f);
}


/*
   gplBody -
*/

static void gplBody (FIO_File f)
{
  comment (f, (char *) "Copyright (C) \"'2019'\" Free Software Foundation, Inc.", 53);
  if (contributed)
    {
      FIO_WriteString (f, (char *) "Contributed by ", 15);
      contributedContents = SFIO_WriteS (f, contributedContents);
      FIO_WriteString (f, (char *) ".", 1);
      FIO_WriteLine (f);
    }
  FIO_WriteLine (f);
  comment (f, (char *) "This file is part of ", 21);
  projectContents = SFIO_WriteS (f, projectContents);
  FIO_WriteString (f, (char *) ".", 1);
  FIO_WriteLine (f);
  FIO_WriteLine (f);
  projectContents = SFIO_WriteS (f, projectContents);
  comment (f, (char *) " is software; you can redistribute it and/or modify", 51);
  comment (f, (char *) "it under the terms of the GNU General Public License as published by", 68);
  comment (f, (char *) "the Free Software Foundation; either version 3, or (at your option)", 67);
  comment (f, (char *) "any later version.", 18);
  FIO_WriteLine (f);
  projectContents = SFIO_WriteS (f, projectContents);
  comment (f, (char *) " is distributed in the hope that it will be useful, but", 55);
  comment (f, (char *) "WITHOUT ANY WARRANTY; without even the implied warranty of", 58);
  comment (f, (char *) "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU", 65);
  comment (f, (char *) "General Public License for more details.", 40);
  FIO_WriteLine (f);
  comment (f, (char *) "You should have received a copy of the GNU General Public License", 65);
  FIO_WriteString (f, (char *) "along with ", 11);
  projectContents = SFIO_WriteS (f, projectContents);
  comment (f, (char *) "; see the file COPYING.  If not,", 32);
  comment (f, (char *) "see <https://www.gnu.org/licenses/>.", 36);
}


/*
   glplBody -
*/

static void glplBody (FIO_File f)
{
  comment (f, (char *) "Copyright (C) \"'2019'\" Free Software Foundation, Inc.", 53);
  if (contributed)
    {
      FIO_WriteString (f, (char *) "Contributed by ", 15);
      contributedContents = SFIO_WriteS (f, contributedContents);
      FIO_WriteString (f, (char *) ".", 1);
      FIO_WriteLine (f);
    }
  FIO_WriteLine (f);
  comment (f, (char *) "This file is part of ", 21);
  projectContents = SFIO_WriteS (f, projectContents);
  FIO_WriteString (f, (char *) ".", 1);
  FIO_WriteLine (f);
  FIO_WriteLine (f);
  projectContents = SFIO_WriteS (f, projectContents);
  comment (f, (char *) " is software; you can redistribute it and/or modify", 51);
  comment (f, (char *) "it under the terms of the GNU Lesser General Public License", 59);
  comment (f, (char *) "as published by the Free Software Foundation; either version 3,", 63);
  comment (f, (char *) "or (at your option) any later version.", 38);
  FIO_WriteLine (f);
  projectContents = SFIO_WriteS (f, projectContents);
  comment (f, (char *) " is distributed in the hope that it will be useful, but", 55);
  comment (f, (char *) "WITHOUT ANY WARRANTY; without even the implied warranty of", 58);
  comment (f, (char *) "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU", 65);
  comment (f, (char *) "Lesser General Public License for more details.", 47);
  FIO_WriteLine (f);
  comment (f, (char *) "You should have received a copy of the GNU Lesser General Public License", 72);
  FIO_WriteString (f, (char *) "along with ", 11);
  projectContents = SFIO_WriteS (f, projectContents);
  comment (f, (char *) "; see the file COPYING.  If not,", 32);
  comment (f, (char *) "see <https://www.gnu.org/licenses/>.", 36);
}


/*
   issueGPL - writes out the summary, GPL/LGPL and/or contributed as a single comment.
*/

static void issueGPL (FIO_File f)
{
  if (((summary || contributed) || gplHeader) || glplHeader)
    {
      commentBegin (f);
      if (summary)
        {
          commentS (f, summaryContents);
          FIO_WriteLine (f);
        }
      if (gplHeader)
        gplBody (f);
      if (glplHeader)
        glplBody (f);
      commentEnd (f);
    }
}


/*
   setOutputFile - sets the output filename to output.
*/

static void setOutputFile (DynamicStrings_String output)
{
  outputFile = output;
}


/*
   setQuiet - sets the quiet flag to, value.
*/

static void setQuiet (unsigned int value)
{
  quiet = value;
}


/*
   setVerbose - sets the verbose flag to, value.
*/

static void setVerbose (unsigned int value)
{
  verbose = value;
}


/*
   setExtendedOpaque - set extendedOpaque to value.
*/

static void setExtendedOpaque (unsigned int value)
{
  extendedOpaque = value;
}


/*
   setSearchPath - set the search path for the module sources.
*/

static void setSearchPath (DynamicStrings_String arg)
{
  mcSearch_prependSearchPath (arg);
}


/*
   setInternalDebugging - turn on/off internal debugging.
*/

static void setInternalDebugging (unsigned int value)
{
  internalDebugging = value;
}


/*
   setHPrefix - saves the H file prefix.
*/

static void setHPrefix (DynamicStrings_String s)
{
  hPrefix = s;
}


/*
   setIgnoreFQ - sets the ignorefq flag.
*/

static void setIgnoreFQ (unsigned int value)
{
  ignoreFQ = value;
}


/*
   optionIs - returns TRUE if the first len (right) characters
              match left.
*/

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
      s = DynamicStrings_Mark (DynamicStrings_Slice (right, 0, (int) StrLib_StrLen ((char *) left, _left_high)));
      return DynamicStrings_EqualArray (s, (char *) left, _left_high);
    }
  else
    return FALSE;
}


/*
   setLang - set the appropriate output language.
*/

static void setLang (DynamicStrings_String arg)
{
  /* must check the longest distinctive string first.  */
  if (optionIs ((char *) "c++", 3, arg))
    {
      decl_setLangCP ();
      langCPP = TRUE;
    }
  else if (optionIs ((char *) "c", 1, arg))
    {
      decl_setLangC ();
      langC = TRUE;
    }
  else if (optionIs ((char *) "m2", 2, arg))
    {
      decl_setLangM2 ();
      langM2 = TRUE;
    }
  else
    displayHelp ();
}


/*
   handleOption -
*/

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
  else if (optionIs ((char *) "--gpl-header", 12, arg))
    gplHeader = TRUE;
  else if (optionIs ((char *) "--glpl-header", 13, arg))
    glplHeader = TRUE;
  else if (optionIs ((char *) "--summary=\"", 11, arg))
    {
      summary = TRUE;
      summaryContents = DynamicStrings_Slice (arg, 11, -1);
    }
  else if (optionIs ((char *) "--contributed=\"", 15, arg))
    {
      contributed = TRUE;
      contributedContents = DynamicStrings_Slice (arg, 13, -1);
    }
  else if (optionIs ((char *) "--project=\"", 11, arg))
    projectContents = DynamicStrings_Slice (arg, 10, -1);
}


/*
   handleOptions - iterates over all options setting appropriate
                   values and returns the single source file
                   if found at the end of the arguments.
*/

DynamicStrings_String mcOptions_handleOptions (void)
{
  unsigned int i;
  DynamicStrings_String arg;

  i = 1;
  while (SArgs_GetArg (&arg, i))
    {
      if ((DynamicStrings_Length (arg)) > 0)
        {
          /* avoid gcc warning by using compound statement even if not strictly necessary.  */
          if ((DynamicStrings_char (arg, 0)) == '-')
            handleOption (arg);
          else
            {
              if (! summary)
                {
                  summaryContents = DynamicStrings_ConCatChar (DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "automatically created by mc from ", 33), arg), '.');
                  summary = FALSE;
                }
              return arg;
            }
        }
      i += 1;
    }
  return NULL;
}


/*
   getQuiet - return the value of quiet.
*/

unsigned int mcOptions_getQuiet (void)
{
  return quiet;
}


/*
   getVerbose - return the value of verbose.
*/

unsigned int mcOptions_getVerbose (void)
{
  return verbose;
}


/*
   getInternalDebugging - return the value of internalDebugging.
*/

unsigned int mcOptions_getInternalDebugging (void)
{
  return internalDebugging;
}


/*
   getCppCommandLine - returns the Cpp command line and all arguments.
*/

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


/*
   getOutputFile - sets the output filename to output.
*/

DynamicStrings_String mcOptions_getOutputFile (void)
{
  return outputFile;
}


/*
   getExtendedOpaque - return the extendedOpaque value.
*/

unsigned int mcOptions_getExtendedOpaque (void)
{
  return extendedOpaque;
}


/*
   setDebugTopological - sets the flag debugTopological to value.
*/

void mcOptions_setDebugTopological (unsigned int value)
{
  debugTopological = value;
}


/*
   getDebugTopological - returns the flag value of the command
                         line option --debug-top.
*/

unsigned int mcOptions_getDebugTopological (void)
{
  return debugTopological;
}


/*
   getHPrefix - saves the H file prefix.
*/

DynamicStrings_String mcOptions_getHPrefix (void)
{
  return hPrefix;
}


/*
   getIgnoreFQ - returns the ignorefq flag.
*/

unsigned int mcOptions_getIgnoreFQ (void)
{
  return ignoreFQ;
}


/*
   writeGPLheader - writes out the GPL or the LGPL as a comment.
*/

void mcOptions_writeGPLheader (FIO_File f)
{
  issueGPL (f);
}

void _M2_mcOptions_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  langC = FALSE;
  langCPP = FALSE;
  langM2 = FALSE;
  gplHeader = FALSE;
  glplHeader = FALSE;
  summary = FALSE;
  contributed = FALSE;
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
  summaryContents = DynamicStrings_InitString ((char *) "", 0);
  contributedContents = DynamicStrings_InitString ((char *) "", 0);
  projectContents = DynamicStrings_InitString ((char *) "GNU Modula-2", 12);
}

void _M2_mcOptions_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
