/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcSearch.mod.  */

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
#include "Gmcrts.h"
#define _mcSearch_H
#define _mcSearch_C

#   include "GSFIO.h"
#   include "GmcFileName.h"
#   include "GDynamicStrings.h"

#   define Directory '/'
static DynamicStrings_String Def;
static DynamicStrings_String Mod;
static DynamicStrings_String UserPath;
static DynamicStrings_String InitialPath;
void mcSearch_initSearchPath (DynamicStrings_String path);
void mcSearch_prependSearchPath (DynamicStrings_String path);
unsigned int mcSearch_findSourceFile (DynamicStrings_String FileName, DynamicStrings_String *fullPath);
unsigned int mcSearch_findSourceDefFile (DynamicStrings_String stem, DynamicStrings_String *fullPath);
unsigned int mcSearch_findSourceModFile (DynamicStrings_String stem, DynamicStrings_String *fullPath);
void mcSearch_setDefExtension (DynamicStrings_String ext);
void mcSearch_setModExtension (DynamicStrings_String ext);
static void doDSdbEnter (void);
static void doDSdbExit (DynamicStrings_String s);
static void DSdbEnter (void);
static void DSdbExit (DynamicStrings_String s);
static void Init (void);

static void doDSdbEnter (void)
{
  DynamicStrings_PushAllocation ();
}

static void doDSdbExit (DynamicStrings_String s)
{
  s = DynamicStrings_PopAllocationExemption (TRUE, s);
}

static void DSdbEnter (void)
{
}

static void DSdbExit (DynamicStrings_String s)
{
}

static void Init (void)
{
  UserPath = DynamicStrings_InitString ((char *) "", 0);
  InitialPath = DynamicStrings_InitStringChar ('.');
  Def = NULL;
  Mod = NULL;
}

void mcSearch_initSearchPath (DynamicStrings_String path)
{
  if (InitialPath != NULL)
    InitialPath = DynamicStrings_KillString (InitialPath);
  InitialPath = path;
}

void mcSearch_prependSearchPath (DynamicStrings_String path)
{
  DSdbEnter ();
  if (DynamicStrings_EqualArray (UserPath, (char *) "", 0))
    {
      UserPath = DynamicStrings_KillString (UserPath);
      UserPath = DynamicStrings_Dup (path);
    }
  else
    UserPath = DynamicStrings_ConCat (DynamicStrings_ConCatChar (UserPath, ':'), path);
  DSdbExit (UserPath);
}

unsigned int mcSearch_findSourceFile (DynamicStrings_String FileName, DynamicStrings_String *fullPath)
{
  DynamicStrings_String completeSearchPath;
  int start;
  int end;
  DynamicStrings_String newpath;

  if (DynamicStrings_EqualArray (UserPath, (char *) "", 0))
    if (DynamicStrings_EqualArray (InitialPath, (char *) "", 0))
      completeSearchPath = DynamicStrings_InitString ((char *) ".", 1);
    else
      completeSearchPath = DynamicStrings_Dup (InitialPath);
  else
    completeSearchPath = DynamicStrings_ConCat (DynamicStrings_ConCatChar (DynamicStrings_Dup (UserPath), ':'), InitialPath);
  start = 0;
  end = DynamicStrings_Index (completeSearchPath, ':', (unsigned int ) (start));
  do {
    if (end == -1)
      end = 0;
    newpath = DynamicStrings_Slice (completeSearchPath, start, end);
    if (DynamicStrings_EqualArray (newpath, (char *) ".", 1))
      {
        newpath = DynamicStrings_KillString (newpath);
        newpath = DynamicStrings_Dup (FileName);
      }
    else
      newpath = DynamicStrings_ConCat (DynamicStrings_ConCatChar (newpath, Directory), FileName);
    if (SFIO_Exists (newpath))
      {
        (*fullPath) = newpath;
        completeSearchPath = DynamicStrings_KillString (completeSearchPath);
        return TRUE;
      }
    newpath = DynamicStrings_KillString (newpath);
    if (end != 0)
      {
        start = end+1;
        end = DynamicStrings_Index (completeSearchPath, ':', (unsigned int ) (start));
      }
  } while (! (end == 0));
  (*fullPath) = NULL;
  newpath = DynamicStrings_KillString (newpath);
  completeSearchPath = DynamicStrings_KillString (completeSearchPath);
  return FALSE;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/mc/mcSearch.def", 19, 0);
}

unsigned int mcSearch_findSourceDefFile (DynamicStrings_String stem, DynamicStrings_String *fullPath)
{
  DynamicStrings_String f;

  if (Def != NULL)
    {
      f = mcFileName_calculateFileName (stem, Def);
      if (mcSearch_findSourceFile (f, fullPath))
        return TRUE;
      f = DynamicStrings_KillString (f);
    }
  f = mcFileName_calculateFileName (stem, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "def", 3)));
  return mcSearch_findSourceFile (f, fullPath);
  ReturnException ("../../gcc-5.2.0/gcc/gm2/mc/mcSearch.def", 19, 0);
}

unsigned int mcSearch_findSourceModFile (DynamicStrings_String stem, DynamicStrings_String *fullPath)
{
  DynamicStrings_String f;

  if (Mod != NULL)
    {
      f = mcFileName_calculateFileName (stem, Mod);
      if (mcSearch_findSourceFile (f, fullPath))
        return TRUE;
      f = DynamicStrings_KillString (f);
    }
  f = mcFileName_calculateFileName (stem, DynamicStrings_Mark (DynamicStrings_InitString ((char *) "mod", 3)));
  return mcSearch_findSourceFile (f, fullPath);
  ReturnException ("../../gcc-5.2.0/gcc/gm2/mc/mcSearch.def", 19, 0);
}

void mcSearch_setDefExtension (DynamicStrings_String ext)
{
  Def = DynamicStrings_KillString (Def);
  Def = DynamicStrings_Dup (ext);
}

void mcSearch_setModExtension (DynamicStrings_String ext)
{
  Mod = DynamicStrings_KillString (Mod);
  Mod = DynamicStrings_Dup (ext);
}

void _M2_mcSearch_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  Init ();
}

void _M2_mcSearch_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
