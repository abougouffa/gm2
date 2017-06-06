/* automatically created by mc from ../../gcc-versionno/gcc/gm2/mc/mcSearch.mod.  */

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

/*
   initSearchPath - assigns the search path to Path.
                    The string Path may take the form:

                    Path           ::= IndividualPath { ":" IndividualPath }
                    IndividualPath ::= "." | DirectoryPath
                    DirectoryPath  ::= [ "/" ] Name { "/" Name }
                    Name           ::= Letter { (Letter | Number) }
                    Letter         ::= A..Z | a..z
                    Number         ::= 0..9
*/

void mcSearch_initSearchPath (DynamicStrings_String path);

/*
   prependSearchPath - prepends a new path to the initial search path.
*/

void mcSearch_prependSearchPath (DynamicStrings_String path);

/*
   findSourceFile - attempts to locate the source file FileName.
                    If a file is found then TRUE is returned otherwise
                    FALSE is returned.
                    The parameter fullPath is set indicating the
                    absolute location of source FileName.
                    fullPath will be totally overwritten and should
                    not be initialized by InitString before this function
                    is called.
                    fullPath is set to NIL if this function returns FALSE.
                    findSourceFile sets fullPath to a new string if successful.
                    The string, FileName, is not altered.
*/

unsigned int mcSearch_findSourceFile (DynamicStrings_String FileName, DynamicStrings_String *fullPath);

/*
   findSourceDefFile - attempts to find the definition module for
                       a module, stem. If successful it returns
                       the full path and returns TRUE. If unsuccessful
                       then FALSE is returned and fullPath is set to NIL.
*/

unsigned int mcSearch_findSourceDefFile (DynamicStrings_String stem, DynamicStrings_String *fullPath);

/*
   findSourceModFile - attempts to find the implementation module for
                       a module, stem. If successful it returns
                       the full path and returns TRUE. If unsuccessful
                       then FALSE is returned and fullPath is set to NIL.
*/

unsigned int mcSearch_findSourceModFile (DynamicStrings_String stem, DynamicStrings_String *fullPath);

/*
   setDefExtension - sets the default extension for definition modules to, ext.
                     The string, ext, should be deallocated by the caller at
                     an appropriate time.
*/

void mcSearch_setDefExtension (DynamicStrings_String ext);

/*
   setModExtension - sets the default extension for implementation and program
                     modules to, ext. The string, ext, should be deallocated
                     by the caller at an appropriate time.
*/

void mcSearch_setModExtension (DynamicStrings_String ext);

/*
   doDSdbEnter -
*/

static void doDSdbEnter (void);

/*
   doDSdbExit -
*/

static void doDSdbExit (DynamicStrings_String s);

/*
   DSdbEnter -
*/

static void DSdbEnter (void);

/*
   DSdbExit -
*/

static void DSdbExit (DynamicStrings_String s);

/*
   Init - initializes the search path.
*/

static void Init (void);


/*
   doDSdbEnter -
*/

static void doDSdbEnter (void)
{
  DynamicStrings_PushAllocation ();
}


/*
   doDSdbExit -
*/

static void doDSdbExit (DynamicStrings_String s)
{
  s = DynamicStrings_PopAllocationExemption (TRUE, s);
}


/*
   DSdbEnter -
*/

static void DSdbEnter (void)
{
}


/*
   DSdbExit -
*/

static void DSdbExit (DynamicStrings_String s)
{
}


/*
   Init - initializes the search path.
*/

static void Init (void)
{
  UserPath = DynamicStrings_InitString ((char *) "", 0);
  InitialPath = DynamicStrings_InitStringChar ('.');
  Def = NULL;
  Mod = NULL;
}


/*
   initSearchPath - assigns the search path to Path.
                    The string Path may take the form:

                    Path           ::= IndividualPath { ":" IndividualPath }
                    IndividualPath ::= "." | DirectoryPath
                    DirectoryPath  ::= [ "/" ] Name { "/" Name }
                    Name           ::= Letter { (Letter | Number) }
                    Letter         ::= A..Z | a..z
                    Number         ::= 0..9
*/

void mcSearch_initSearchPath (DynamicStrings_String path)
{
  if (InitialPath != NULL)
    InitialPath = DynamicStrings_KillString (InitialPath);
  InitialPath = path;
}


/*
   prependSearchPath - prepends a new path to the initial search path.
*/

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


/*
   findSourceFile - attempts to locate the source file FileName.
                    If a file is found then TRUE is returned otherwise
                    FALSE is returned.
                    The parameter fullPath is set indicating the
                    absolute location of source FileName.
                    fullPath will be totally overwritten and should
                    not be initialized by InitString before this function
                    is called.
                    fullPath is set to NIL if this function returns FALSE.
                    findSourceFile sets fullPath to a new string if successful.
                    The string, FileName, is not altered.
*/

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
}


/*
   findSourceDefFile - attempts to find the definition module for
                       a module, stem. If successful it returns
                       the full path and returns TRUE. If unsuccessful
                       then FALSE is returned and fullPath is set to NIL.
*/

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
}


/*
   findSourceModFile - attempts to find the implementation module for
                       a module, stem. If successful it returns
                       the full path and returns TRUE. If unsuccessful
                       then FALSE is returned and fullPath is set to NIL.
*/

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
}


/*
   setDefExtension - sets the default extension for definition modules to, ext.
                     The string, ext, should be deallocated by the caller at
                     an appropriate time.
*/

void mcSearch_setDefExtension (DynamicStrings_String ext)
{
  Def = DynamicStrings_KillString (Def);
  Def = DynamicStrings_Dup (ext);
}


/*
   setModExtension - sets the default extension for implementation and program
                     modules to, ext. The string, ext, should be deallocated
                     by the caller at an appropriate time.
*/

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
