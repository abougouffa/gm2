/* keyc maintains the C name scope and avoids C/C++ name conflicts.
   Copyright (C) 2016-2019 Free Software Foundation, Inc.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

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
#   include "GStorage.h"
#   include "Gmcrts.h"
#define _keyc_H
#define _keyc_C

#   include "GmcPretty.h"
#   include "GStorage.h"
#   include "GDynamicStrings.h"
#   include "GsymbolKey.h"
#   include "GnameKey.h"
#   include "GmcOptions.h"
#   include "GM2RTS.h"

#if !defined (decl_node_D)
#  define decl_node_D
   typedef void *decl_node;
#endif

typedef struct _T1_r _T1;

typedef _T1 *scope;

struct _T1_r {
               decl_node scoped;
               symbolKey_symbolTree symbols;
               scope next;
             };

static scope stack;
static scope freeList;
static symbolKey_symbolTree keywords;
static symbolKey_symbolTree macros;
static unsigned int initializedCP;
static unsigned int seenIntMin;
static unsigned int seenUIntMin;
static unsigned int seenLongMin;
static unsigned int seenULongMin;
static unsigned int seenCharMin;
static unsigned int seenUCharMin;
static unsigned int seenIntMax;
static unsigned int seenUIntMax;
static unsigned int seenLongMax;
static unsigned int seenULongMax;
static unsigned int seenCharMax;
static unsigned int seenUCharMax;
static unsigned int seenLabs;
static unsigned int seenAbs;
static unsigned int seenFabs;
static unsigned int seenFabsl;
static unsigned int seenSize_t;
static unsigned int seenSSize_t;
static unsigned int seenFree;
static unsigned int seenMalloc;
static unsigned int seenStorage;
static unsigned int seenProc;
static unsigned int seenTrue;
static unsigned int seenFalse;
static unsigned int seenNull;
static unsigned int seenMemcpy;
static unsigned int seenException;
static unsigned int seenComplex;
static unsigned int seenM2RTS;
static unsigned int seenStrlen;
static unsigned int seenCtype;

/*
   useStorage - indicate we have used storage.
*/

void keyc_useStorage (void);

/*
   useFree - indicate we have used free.
*/

void keyc_useFree (void);

/*
   useMalloc - indicate we have used malloc.
*/

void keyc_useMalloc (void);

/*
   useProc - indicate we have used proc.
*/

void keyc_useProc (void);

/*
   useTrue - indicate we have used TRUE.
*/

void keyc_useTrue (void);

/*
   useFalse - indicate we have used FALSE.
*/

void keyc_useFalse (void);

/*
   useNull - indicate we have used NULL.
*/

void keyc_useNull (void);

/*
   useMemcpy - indicate we have used memcpy.
*/

void keyc_useMemcpy (void);

/*
   useIntMin - indicate we have used INT_MIN.
*/

void keyc_useIntMin (void);

/*
   useUIntMin - indicate we have used UINT_MIN.
*/

void keyc_useUIntMin (void);

/*
   useLongMin - indicate we have used LONG_MIN.
*/

void keyc_useLongMin (void);

/*
   useULongMin - indicate we have used ULONG_MIN.
*/

void keyc_useULongMin (void);

/*
   useCharMin - indicate we have used CHAR_MIN.
*/

void keyc_useCharMin (void);

/*
   useUCharMin - indicate we have used UCHAR_MIN.
*/

void keyc_useUCharMin (void);

/*
   useIntMax - indicate we have used INT_MAX.
*/

void keyc_useIntMax (void);

/*
   useUIntMax - indicate we have used UINT_MAX.
*/

void keyc_useUIntMax (void);

/*
   useLongMax - indicate we have used LONG_MAX.
*/

void keyc_useLongMax (void);

/*
   useULongMax - indicate we have used ULONG_MAX.
*/

void keyc_useULongMax (void);

/*
   useCharMax - indicate we have used CHAR_MAX.
*/

void keyc_useCharMax (void);

/*
   useUCharMax - indicate we have used UChar_MAX.
*/

void keyc_useUCharMax (void);

/*
   useSize_t - indicate we have used size_t.
*/

void keyc_useSize_t (void);

/*
   useSSize_t - indicate we have used ssize_t.
*/

void keyc_useSSize_t (void);

/*
   useLabs - indicate we have used labs.
*/

void keyc_useLabs (void);

/*
   useAbs - indicate we have used abs.
*/

void keyc_useAbs (void);

/*
   useFabs - indicate we have used fabs.
*/

void keyc_useFabs (void);

/*
   useFabsl - indicate we have used fabsl.
*/

void keyc_useFabsl (void);

/*
   useException - use the exceptions module, mcrts.
*/

void keyc_useException (void);

/*
   useComplex - use the complex data type.
*/

void keyc_useComplex (void);

/*
   useM2RTS - indicate we have used M2RTS in the converted code.
*/

void keyc_useM2RTS (void);

/*
   useStrlen - indicate we have used strlen in the converted code.
*/

void keyc_useStrlen (void);

/*
   useCtype - indicate we have used the toupper function.
*/

void keyc_useCtype (void);

/*
   genDefs - generate definitions or includes for all
             macros and prototypes used.
*/

void keyc_genDefs (mcPretty_pretty p);

/*
   enterScope - enter a scope defined by, n.
*/

void keyc_enterScope (decl_node n);

/*
   leaveScope - leave the scope defined by, n.
*/

void keyc_leaveScope (decl_node n);

/*
   cname - attempts to declare a symbol with name, n, in the
           current scope.  If there is no conflict with the
           target language then NIL is returned, otherwise
           a mangled name is returned as a String.
           If scopes is FALSE then only the keywords and
           macros are detected for a clash (all scoping
           is ignored).
*/

DynamicStrings_String keyc_cname (nameKey_Name n, unsigned int scopes);

/*
   cnamen - attempts to declare a symbol with name, n, in the
            current scope.  If there is no conflict with the
            target language then NIL is returned, otherwise
            a mangled name is returned as a Name
            If scopes is FALSE then only the keywords and
            macros are detected for a clash (all scoping
            is ignored).
*/

nameKey_Name keyc_cnamen (nameKey_Name n, unsigned int scopes);

/*
   cp - include C++ keywords and standard declarations to avoid.
*/

void keyc_cp (void);

/*
   checkCtype -
*/

static void checkCtype (mcPretty_pretty p);

/*
   checkAbs - check to see if the abs family, size_t or ssize_t have been used.
*/

static void checkAbs (mcPretty_pretty p);

/*
   checkLimits -
*/

static void checkLimits (mcPretty_pretty p);

/*
   checkFreeMalloc -
*/

static void checkFreeMalloc (mcPretty_pretty p);

/*
   checkStorage -
*/

static void checkStorage (mcPretty_pretty p);

/*
   checkProc -
*/

static void checkProc (mcPretty_pretty p);

/*
   checkTrue -
*/

static void checkTrue (mcPretty_pretty p);

/*
   checkFalse -
*/

static void checkFalse (mcPretty_pretty p);

/*
   checkNull -
*/

static void checkNull (mcPretty_pretty p);

/*
   checkMemcpy -
*/

static void checkMemcpy (mcPretty_pretty p);

/*
   checkM2RTS -
*/

static void checkM2RTS (mcPretty_pretty p);

/*
   checkException - check to see if exceptions were used.
*/

static void checkException (mcPretty_pretty p);

/*
   checkComplex - check to see if the type complex was used.
*/

static void checkComplex (mcPretty_pretty p);

/*
   new -
*/

static scope new (decl_node n);

/*
   mangle1 - returns TRUE if name is unique if we add _
             to its end.
*/

static unsigned int mangle1 (nameKey_Name n, DynamicStrings_String *m, unsigned int scopes);

/*
   mangle2 - returns TRUE if name is unique if we prepend _
             to, n.
*/

static unsigned int mangle2 (nameKey_Name n, DynamicStrings_String *m, unsigned int scopes);

/*
   mangleN - keep adding '_' to the end of n until it
             no longer clashes.
*/

static unsigned int mangleN (nameKey_Name n, DynamicStrings_String *m, unsigned int scopes);

/*
   clash - returns TRUE if there is a clash with name, n,
           in the current scope or C keywords or C macros.
*/

static unsigned int clash (nameKey_Name n, unsigned int scopes);

/*
   initCP - add the extra keywords and standard definitions used by C++.
*/

static void initCP (void);

/*
   add -
*/

static void add (symbolKey_symbolTree s, char *a_, unsigned int _a_high);

/*
   initMacros - macros and library function names to avoid.
*/

static void initMacros (void);

/*
   initKeywords - keywords to avoid.
*/

static void initKeywords (void);

/*
   init -
*/

static void init (void);


/*
   checkCtype -
*/

static void checkCtype (mcPretty_pretty p)
{
  if (seenCtype)
    {
      mcPretty_print (p, (char *) "#include <ctype.h>\\n", 20);
    }
}


/*
   checkAbs - check to see if the abs family, size_t or ssize_t have been used.
*/

static void checkAbs (mcPretty_pretty p)
{
  if (((((seenLabs || seenAbs) || seenFabs) || seenFabsl) || seenSize_t) || seenSSize_t)
    {
      mcPretty_print (p, (char *) "#include <stdlib.h>\\n", 21);
    }
}


/*
   checkLimits -
*/

static void checkLimits (mcPretty_pretty p)
{
  if ((((((((((((((seenMemcpy || seenIntMin) || seenUIntMin) || seenLongMin) || seenULongMin) || seenCharMin) || seenUCharMin) || seenUIntMin) || seenIntMax) || seenUIntMax) || seenLongMax) || seenULongMax) || seenCharMax) || seenUCharMax) || seenUIntMax)
    {
      mcPretty_print (p, (char *) "#include <limits.h>\\n", 21);
    }
}


/*
   checkFreeMalloc -
*/

static void checkFreeMalloc (mcPretty_pretty p)
{
  if (seenFree || seenMalloc)
    {
      mcPretty_print (p, (char *) "#include <stdlib.h>\\n", 21);
    }
}


/*
   checkStorage -
*/

static void checkStorage (mcPretty_pretty p)
{
  if (seenStorage)
    {
      mcPretty_print (p, (char *) "#   include \"", 13);
      mcPretty_prints (p, mcOptions_getHPrefix ());
      mcPretty_print (p, (char *) "Storage.h\"\\n", 12);
    }
}


/*
   checkProc -
*/

static void checkProc (mcPretty_pretty p)
{
  if (seenProc)
    {
      mcPretty_print (p, (char *) "#   if !defined (PROC_D)\\n", 26);
      mcPretty_print (p, (char *) "#      define PROC_D\\n", 22);
      mcPretty_print (p, (char *) "       typedef void (*PROC_t) (void);\\n", 39);
      mcPretty_print (p, (char *) "       typedef struct { PROC_t proc; } PROC;\\n", 46);
      mcPretty_print (p, (char *) "#   endif\\n\\n", 13);
    }
}


/*
   checkTrue -
*/

static void checkTrue (mcPretty_pretty p)
{
  if (seenTrue)
    {
      mcPretty_print (p, (char *) "#   if !defined (TRUE)\\n", 24);
      mcPretty_print (p, (char *) "#      define TRUE (1==1)\\n", 27);
      mcPretty_print (p, (char *) "#   endif\\n\\n", 13);
    }
}


/*
   checkFalse -
*/

static void checkFalse (mcPretty_pretty p)
{
  if (seenFalse)
    {
      mcPretty_print (p, (char *) "#   if !defined (FALSE)\\n", 25);
      mcPretty_print (p, (char *) "#      define FALSE (1==0)\\n", 28);
      mcPretty_print (p, (char *) "#   endif\\n\\n", 13);
    }
}


/*
   checkNull -
*/

static void checkNull (mcPretty_pretty p)
{
  if (seenNull)
    {
      mcPretty_print (p, (char *) "#include <stddef.h>\\n", 21);
    }
}


/*
   checkMemcpy -
*/

static void checkMemcpy (mcPretty_pretty p)
{
  if (seenMemcpy || seenStrlen)
    {
      mcPretty_print (p, (char *) "#include <string.h>\\n", 21);
    }
}


/*
   checkM2RTS -
*/

static void checkM2RTS (mcPretty_pretty p)
{
  if (seenM2RTS)
    {
      mcPretty_print (p, (char *) "#   include \"", 13);
      mcPretty_prints (p, mcOptions_getHPrefix ());
      mcPretty_print (p, (char *) "M2RTS.h\"\\n", 10);
    }
}


/*
   checkException - check to see if exceptions were used.
*/

static void checkException (mcPretty_pretty p)
{
  if (seenException)
    {
      mcPretty_print (p, (char *) "#   include \"Gmcrts.h\"\\n", 24);
    }
}


/*
   checkComplex - check to see if the type complex was used.
*/

static void checkComplex (mcPretty_pretty p)
{
  if (seenComplex)
    {
      mcPretty_print (p, (char *) "#   include <complex.h>\\n", 25);
    }
}


/*
   new -
*/

static scope new (decl_node n)
{
  scope s;

  if (freeList == NULL)
    {
      Storage_ALLOCATE ((void **) &s, sizeof (_T1));
    }
  else
    {
      s = freeList;
      freeList = freeList->next;
    }
  return s;
}


/*
   mangle1 - returns TRUE if name is unique if we add _
             to its end.
*/

static unsigned int mangle1 (nameKey_Name n, DynamicStrings_String *m, unsigned int scopes)
{
  (*m) = DynamicStrings_KillString ((*m));
  (*m) = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n));
  (*m) = DynamicStrings_ConCatChar ((*m), '_');
  return ! (clash (nameKey_makekey (DynamicStrings_string ((*m))), scopes));
}


/*
   mangle2 - returns TRUE if name is unique if we prepend _
             to, n.
*/

static unsigned int mangle2 (nameKey_Name n, DynamicStrings_String *m, unsigned int scopes)
{
  (*m) = DynamicStrings_KillString ((*m));
  (*m) = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n));
  (*m) = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "_", 1), DynamicStrings_Mark ((*m)));
  return ! (clash (nameKey_makekey (DynamicStrings_string ((*m))), scopes));
}


/*
   mangleN - keep adding '_' to the end of n until it
             no longer clashes.
*/

static unsigned int mangleN (nameKey_Name n, DynamicStrings_String *m, unsigned int scopes)
{
  (*m) = DynamicStrings_KillString ((*m));
  (*m) = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n));
  for (;;)
  {
    (*m) = DynamicStrings_ConCatChar ((*m), '_');
    if (! (clash (nameKey_makekey (DynamicStrings_string ((*m))), scopes)))
      {
        return TRUE;
      }
  }
  ReturnException ("../../gcc-versionno/gcc/m2/mc/keyc.def", 20, 1);
}


/*
   clash - returns TRUE if there is a clash with name, n,
           in the current scope or C keywords or C macros.
*/

static unsigned int clash (nameKey_Name n, unsigned int scopes)
{
  if (((symbolKey_getSymKey (macros, n)) != NULL) || ((symbolKey_getSymKey (keywords, n)) != NULL))
    {
      return TRUE;
    }
  return scopes && ((symbolKey_getSymKey (stack->symbols, n)) != NULL);
}


/*
   initCP - add the extra keywords and standard definitions used by C++.
*/

static void initCP (void)
{
  add (macros, (char *) "true", 4);
  add (macros, (char *) "false", 5);
  add (keywords, (char *) "new", 3);
  add (keywords, (char *) "delete", 6);
  add (keywords, (char *) "try", 3);
  add (keywords, (char *) "catch", 5);
  add (keywords, (char *) "operator", 8);
  add (keywords, (char *) "complex", 7);
}


/*
   add -
*/

static void add (symbolKey_symbolTree s, char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  symbolKey_putSymKey (s, nameKey_makeKey ((char *) a, _a_high), (void *) DynamicStrings_InitString ((char *) a, _a_high));
}


/*
   initMacros - macros and library function names to avoid.
*/

static void initMacros (void)
{
  macros = symbolKey_initTree ();
  add (macros, (char *) "FILE", 4);
  add (macros, (char *) "stdio", 5);
  add (macros, (char *) "stdout", 6);
  add (macros, (char *) "stderr", 6);
  add (macros, (char *) "write", 5);
  add (macros, (char *) "read", 4);
  add (macros, (char *) "exit", 4);
  add (macros, (char *) "abs", 3);
  add (macros, (char *) "optarg", 6);
  add (macros, (char *) "div", 3);
  add (macros, (char *) "sin", 3);
  add (macros, (char *) "cos", 3);
  add (macros, (char *) "tan", 3);
  add (macros, (char *) "log10", 5);
  add (macros, (char *) "I", 1);
  add (macros, (char *) "csqrt", 5);
  add (macros, (char *) "strlen", 6);
  add (macros, (char *) "strcpy", 6);
  add (macros, (char *) "free", 4);
  add (macros, (char *) "malloc", 6);
  add (macros, (char *) "time", 4);
  add (macros, (char *) "main", 4);
}


/*
   initKeywords - keywords to avoid.
*/

static void initKeywords (void)
{
  keywords = symbolKey_initTree ();
  add (keywords, (char *) "auto", 4);
  add (keywords, (char *) "break", 5);
  add (keywords, (char *) "case", 4);
  add (keywords, (char *) "char", 4);
  add (keywords, (char *) "const", 5);
  add (keywords, (char *) "continue", 8);
  add (keywords, (char *) "default", 7);
  add (keywords, (char *) "do", 2);
  add (keywords, (char *) "double", 6);
  add (keywords, (char *) "else", 4);
  add (keywords, (char *) "enum", 4);
  add (keywords, (char *) "extern", 6);
  add (keywords, (char *) "float", 5);
  add (keywords, (char *) "for", 3);
  add (keywords, (char *) "goto", 4);
  add (keywords, (char *) "if", 2);
  add (keywords, (char *) "int", 3);
  add (keywords, (char *) "long", 4);
  add (keywords, (char *) "register", 8);
  add (keywords, (char *) "return", 6);
  add (keywords, (char *) "short", 5);
  add (keywords, (char *) "signed", 6);
  add (keywords, (char *) "sizeof", 6);
  add (keywords, (char *) "static", 6);
  add (keywords, (char *) "struct", 6);
  add (keywords, (char *) "switch", 6);
  add (keywords, (char *) "typedef", 7);
  add (keywords, (char *) "union", 5);
  add (keywords, (char *) "unsigned", 8);
  add (keywords, (char *) "void", 4);
  add (keywords, (char *) "volatile", 8);
  add (keywords, (char *) "while", 5);
}


/*
   init -
*/

static void init (void)
{
  seenFree = FALSE;
  seenMalloc = FALSE;
  seenStorage = FALSE;
  seenProc = FALSE;
  seenTrue = FALSE;
  seenFalse = FALSE;
  seenNull = FALSE;
  seenMemcpy = FALSE;
  seenIntMin = FALSE;
  seenUIntMin = FALSE;
  seenLongMin = FALSE;
  seenULongMin = FALSE;
  seenCharMin = FALSE;
  seenUCharMin = FALSE;
  seenUIntMin = FALSE;
  seenIntMax = FALSE;
  seenUIntMax = FALSE;
  seenLongMax = FALSE;
  seenULongMax = FALSE;
  seenCharMax = FALSE;
  seenUCharMax = FALSE;
  seenUIntMax = FALSE;
  seenLabs = FALSE;
  seenAbs = FALSE;
  seenFabs = FALSE;
  seenFabsl = FALSE;
  seenException = FALSE;
  seenComplex = FALSE;
  seenM2RTS = FALSE;
  seenStrlen = FALSE;
  seenCtype = FALSE;
  seenSize_t = FALSE;
  seenSSize_t = FALSE;
  initializedCP = FALSE;
  stack = NULL;
  freeList = NULL;
  initKeywords ();
  initMacros ();
}


/*
   useStorage - indicate we have used storage.
*/

void keyc_useStorage (void)
{
  seenStorage = TRUE;
}


/*
   useFree - indicate we have used free.
*/

void keyc_useFree (void)
{
  seenFree = TRUE;
}


/*
   useMalloc - indicate we have used malloc.
*/

void keyc_useMalloc (void)
{
  seenMalloc = TRUE;
}


/*
   useProc - indicate we have used proc.
*/

void keyc_useProc (void)
{
  seenProc = TRUE;
}


/*
   useTrue - indicate we have used TRUE.
*/

void keyc_useTrue (void)
{
  seenTrue = TRUE;
}


/*
   useFalse - indicate we have used FALSE.
*/

void keyc_useFalse (void)
{
  seenFalse = TRUE;
}


/*
   useNull - indicate we have used NULL.
*/

void keyc_useNull (void)
{
  seenNull = TRUE;
}


/*
   useMemcpy - indicate we have used memcpy.
*/

void keyc_useMemcpy (void)
{
  seenMemcpy = TRUE;
}


/*
   useIntMin - indicate we have used INT_MIN.
*/

void keyc_useIntMin (void)
{
  seenIntMin = TRUE;
}


/*
   useUIntMin - indicate we have used UINT_MIN.
*/

void keyc_useUIntMin (void)
{
  seenUIntMin = TRUE;
}


/*
   useLongMin - indicate we have used LONG_MIN.
*/

void keyc_useLongMin (void)
{
  seenLongMin = TRUE;
}


/*
   useULongMin - indicate we have used ULONG_MIN.
*/

void keyc_useULongMin (void)
{
  seenULongMin = TRUE;
}


/*
   useCharMin - indicate we have used CHAR_MIN.
*/

void keyc_useCharMin (void)
{
  seenCharMin = TRUE;
}


/*
   useUCharMin - indicate we have used UCHAR_MIN.
*/

void keyc_useUCharMin (void)
{
  seenUCharMin = TRUE;
}


/*
   useIntMax - indicate we have used INT_MAX.
*/

void keyc_useIntMax (void)
{
  seenIntMax = TRUE;
}


/*
   useUIntMax - indicate we have used UINT_MAX.
*/

void keyc_useUIntMax (void)
{
  seenUIntMax = TRUE;
}


/*
   useLongMax - indicate we have used LONG_MAX.
*/

void keyc_useLongMax (void)
{
  seenLongMax = TRUE;
}


/*
   useULongMax - indicate we have used ULONG_MAX.
*/

void keyc_useULongMax (void)
{
  seenULongMax = TRUE;
}


/*
   useCharMax - indicate we have used CHAR_MAX.
*/

void keyc_useCharMax (void)
{
  seenCharMax = TRUE;
}


/*
   useUCharMax - indicate we have used UChar_MAX.
*/

void keyc_useUCharMax (void)
{
  seenUCharMax = TRUE;
}


/*
   useSize_t - indicate we have used size_t.
*/

void keyc_useSize_t (void)
{
  seenSize_t = TRUE;
}


/*
   useSSize_t - indicate we have used ssize_t.
*/

void keyc_useSSize_t (void)
{
  seenSSize_t = TRUE;
}


/*
   useLabs - indicate we have used labs.
*/

void keyc_useLabs (void)
{
  seenLabs = TRUE;
}


/*
   useAbs - indicate we have used abs.
*/

void keyc_useAbs (void)
{
  seenAbs = TRUE;
}


/*
   useFabs - indicate we have used fabs.
*/

void keyc_useFabs (void)
{
  seenFabs = TRUE;
}


/*
   useFabsl - indicate we have used fabsl.
*/

void keyc_useFabsl (void)
{
  seenFabsl = TRUE;
}


/*
   useException - use the exceptions module, mcrts.
*/

void keyc_useException (void)
{
  seenException = TRUE;
}


/*
   useComplex - use the complex data type.
*/

void keyc_useComplex (void)
{
  seenComplex = TRUE;
}


/*
   useM2RTS - indicate we have used M2RTS in the converted code.
*/

void keyc_useM2RTS (void)
{
  seenM2RTS = TRUE;
}


/*
   useStrlen - indicate we have used strlen in the converted code.
*/

void keyc_useStrlen (void)
{
  seenStrlen = TRUE;
}


/*
   useCtype - indicate we have used the toupper function.
*/

void keyc_useCtype (void)
{
  seenCtype = TRUE;
}


/*
   genDefs - generate definitions or includes for all
             macros and prototypes used.
*/

void keyc_genDefs (mcPretty_pretty p)
{
  checkFreeMalloc (p);
  checkProc (p);
  checkTrue (p);
  checkFalse (p);
  checkNull (p);
  checkMemcpy (p);
  checkLimits (p);
  checkAbs (p);
  checkStorage (p);
  checkException (p);
  checkComplex (p);
  checkCtype (p);
  checkM2RTS (p);
}


/*
   enterScope - enter a scope defined by, n.
*/

void keyc_enterScope (decl_node n)
{
  scope s;

  s = new (n);
  s->scoped = n;
  s->symbols = symbolKey_initTree ();
  s->next = stack;
  stack = s;
}


/*
   leaveScope - leave the scope defined by, n.
*/

void keyc_leaveScope (decl_node n)
{
  scope s;

  if (n == stack->scoped)
    {
      s = stack;
      stack = stack->next;
      s->scoped = NULL;
      symbolKey_killTree (&s->symbols);
      s->next = NULL;
    }
  else
    {
      M2RTS_HALT (-1);
    }
}


/*
   cname - attempts to declare a symbol with name, n, in the
           current scope.  If there is no conflict with the
           target language then NIL is returned, otherwise
           a mangled name is returned as a String.
           If scopes is FALSE then only the keywords and
           macros are detected for a clash (all scoping
           is ignored).
*/

DynamicStrings_String keyc_cname (nameKey_Name n, unsigned int scopes)
{
  DynamicStrings_String m;

  m = NULL;
  if (clash (n, scopes))
    {
      if (((mangle1 (n, &m, scopes)) || (mangle2 (n, &m, scopes))) || (mangleN (n, &m, scopes)))
        {
          /* avoid dangling else.  */
          if (scopes)
            {
              /* no longer a clash with, m, so add it to the current scope.  */
              n = nameKey_makekey (DynamicStrings_string (m));
              symbolKey_putSymKey (stack->symbols, n, (void *) m);
            }
        }
      else
        {
          /* mangleN must always succeed.  */
          M2RTS_HALT (-1);
        }
    }
  else if (scopes)
    {
      /* avoid dangling else.  */
      /* no clash, add it to the current scope.  */
      symbolKey_putSymKey (stack->symbols, n, (void *) DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
    }
  return m;
}


/*
   cnamen - attempts to declare a symbol with name, n, in the
            current scope.  If there is no conflict with the
            target language then NIL is returned, otherwise
            a mangled name is returned as a Name
            If scopes is FALSE then only the keywords and
            macros are detected for a clash (all scoping
            is ignored).
*/

nameKey_Name keyc_cnamen (nameKey_Name n, unsigned int scopes)
{
  DynamicStrings_String m;

  m = NULL;
  if (clash (n, scopes))
    {
      if (((mangle1 (n, &m, scopes)) || (mangle2 (n, &m, scopes))) || (mangleN (n, &m, scopes)))
        {
          /* avoid dangling else.  */
          n = nameKey_makekey (DynamicStrings_string (m));
          if (scopes)
            {
              /* no longer a clash with, m, so add it to the current scope.  */
              symbolKey_putSymKey (stack->symbols, n, (void *) m);
            }
        }
      else
        {
          /* mangleN must always succeed.  */
          M2RTS_HALT (-1);
        }
    }
  else if (scopes)
    {
      /* avoid dangling else.  */
      /* no clash, add it to the current scope.  */
      symbolKey_putSymKey (stack->symbols, n, (void *) DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
    }
  m = DynamicStrings_KillString (m);
  return n;
}


/*
   cp - include C++ keywords and standard declarations to avoid.
*/

void keyc_cp (void)
{
  if (! initializedCP)
    {
      initializedCP = TRUE;
      initCP ();
    }
}

void _M2_keyc_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  init ();
}

void _M2_keyc_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
