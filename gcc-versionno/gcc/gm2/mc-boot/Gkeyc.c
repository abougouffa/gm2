/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/keyc.mod.  */

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
static unsigned int seenFree;
static unsigned int seenMalloc;
static unsigned int seenStorage;
static unsigned int seenProc;
static unsigned int seenTrue;
static unsigned int seenFalse;
static unsigned int seenNull;
static unsigned int seenMemcpy;
void keyc_useStorage (void);
void keyc_useFree (void);
void keyc_useMalloc (void);
void keyc_useProc (void);
void keyc_useTrue (void);
void keyc_useFalse (void);
void keyc_useNull (void);
void keyc_useMemcpy (void);
void keyc_useIntMin (void);
void keyc_useUIntMin (void);
void keyc_useLongMin (void);
void keyc_useULongMin (void);
void keyc_useCharMin (void);
void keyc_useUCharMin (void);
void keyc_useIntMax (void);
void keyc_useUIntMax (void);
void keyc_useLongMax (void);
void keyc_useULongMax (void);
void keyc_useCharMax (void);
void keyc_useUCharMax (void);
void keyc_useLabs (void);
void keyc_useAbs (void);
void keyc_useFabs (void);
void keyc_useFabsl (void);
void keyc_genDefs (mcPretty_pretty p);
void keyc_enterScope (decl_node n);
void keyc_leaveScope (decl_node n);
DynamicStrings_String keyc_cname (nameKey_Name n, unsigned int scopes);
static void checkAbs (mcPretty_pretty p);
static void checkLimits (mcPretty_pretty p);
static void checkFreeMalloc (mcPretty_pretty p);
static void checkStorage (mcPretty_pretty p);
static void checkProc (mcPretty_pretty p);
static void checkTrue (mcPretty_pretty p);
static void checkFalse (mcPretty_pretty p);
static void checkNull (mcPretty_pretty p);
static void checkMemcpy (mcPretty_pretty p);
static scope new (decl_node n);
static unsigned int mangle1 (nameKey_Name n, DynamicStrings_String *m, unsigned int scopes);
static unsigned int mangle2 (nameKey_Name n, DynamicStrings_String *m, unsigned int scopes);
static unsigned int mangleN (nameKey_Name n, DynamicStrings_String *m, unsigned int scopes);
static unsigned int clash (nameKey_Name n, unsigned int scopes);
static void add (symbolKey_symbolTree s, char *a_, unsigned int _a_high);
static void initMacros (void);
static void initKeywords (void);
static void init (void);

static void checkAbs (mcPretty_pretty p)
{
  if (((seenLabs || seenAbs) || seenFabs) || seenFabsl)
    mcPretty_print (p, (char *) "#include <stdlib.h>\\n", 21);
}

static void checkLimits (mcPretty_pretty p)
{
  if ((((((((((((((seenMemcpy || seenIntMin) || seenUIntMin) || seenLongMin) || seenULongMin) || seenCharMin) || seenUCharMin) || seenUIntMin) || seenIntMax) || seenUIntMax) || seenLongMax) || seenULongMax) || seenCharMax) || seenUCharMax) || seenUIntMax)
    mcPretty_print (p, (char *) "#include <limits.h>\\n", 21);
}

static void checkFreeMalloc (mcPretty_pretty p)
{
  if (seenFree || seenMalloc)
    mcPretty_print (p, (char *) "#include <stdlib.h>\\n", 21);
}

static void checkStorage (mcPretty_pretty p)
{
  if (seenStorage)
    {
      mcPretty_print (p, (char *) "#   include \"", 13);
      mcPretty_prints (p, mcOptions_getHPrefix ());
      mcPretty_print (p, (char *) "Storage.h\"\\n", 12);
    }
}

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

static void checkTrue (mcPretty_pretty p)
{
  if (seenTrue)
    {
      mcPretty_print (p, (char *) "#   if !defined (TRUE)\\n", 24);
      mcPretty_print (p, (char *) "#      define TRUE (1==1)\\n", 27);
      mcPretty_print (p, (char *) "#   endif\\n\\n", 13);
    }
}

static void checkFalse (mcPretty_pretty p)
{
  if (seenFalse)
    {
      mcPretty_print (p, (char *) "#   if !defined (FALSE)\\n", 25);
      mcPretty_print (p, (char *) "#      define FALSE (1==0)\\n", 28);
      mcPretty_print (p, (char *) "#   endif\\n\\n", 13);
    }
}

static void checkNull (mcPretty_pretty p)
{
  if (seenNull)
    mcPretty_print (p, (char *) "#include <stddef.h>\\n", 21);
}

static void checkMemcpy (mcPretty_pretty p)
{
  if (seenMemcpy)
    mcPretty_print (p, (char *) "#include <string.h>\\n", 21);
}

static scope new (decl_node n)
{
  scope s;

  if (freeList == NULL)
    Storage_ALLOCATE ((void **) &s, sizeof (_T1));
  else
    {
      s = freeList;
      freeList = freeList->next;
    }
  return s;
}

static unsigned int mangle1 (nameKey_Name n, DynamicStrings_String *m, unsigned int scopes)
{
  (*m) = DynamicStrings_KillString ((*m));
  (*m) = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n));
  (*m) = DynamicStrings_ConCatChar ((*m), '_');
  return ! (clash (nameKey_makekey (DynamicStrings_string ((*m))), scopes));
}

static unsigned int mangle2 (nameKey_Name n, DynamicStrings_String *m, unsigned int scopes)
{
  (*m) = DynamicStrings_KillString ((*m));
  (*m) = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n));
  (*m) = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "_", 1), DynamicStrings_Mark ((*m)));
  return ! (clash (nameKey_makekey (DynamicStrings_string ((*m))), scopes));
}

static unsigned int mangleN (nameKey_Name n, DynamicStrings_String *m, unsigned int scopes)
{
  (*m) = DynamicStrings_KillString ((*m));
  (*m) = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n));
  for (;;)
  {
    (*m) = DynamicStrings_ConCatChar ((*m), '_');
    if (! (clash (nameKey_makekey (DynamicStrings_string ((*m))), scopes)))
      return TRUE;
  }
}

static unsigned int clash (nameKey_Name n, unsigned int scopes)
{
  if (((symbolKey_getSymKey (macros, n)) != NULL) || ((symbolKey_getSymKey (keywords, n)) != NULL))
    return TRUE;
  return scopes && ((symbolKey_getSymKey (stack->symbols, n)) != NULL);
}

static void add (symbolKey_symbolTree s, char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  symbolKey_putSymKey (s, nameKey_makeKey ((char *) a, _a_high), (void *) DynamicStrings_InitString ((char *) a, _a_high));
}

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
}

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
  stack = NULL;
  freeList = NULL;
  initKeywords ();
  initMacros ();
}

void keyc_useStorage (void)
{
  seenStorage = TRUE;
}

void keyc_useFree (void)
{
  seenFree = TRUE;
}

void keyc_useMalloc (void)
{
  seenMalloc = TRUE;
}

void keyc_useProc (void)
{
  seenProc = TRUE;
}

void keyc_useTrue (void)
{
  seenTrue = TRUE;
}

void keyc_useFalse (void)
{
  seenFalse = TRUE;
}

void keyc_useNull (void)
{
  seenNull = TRUE;
}

void keyc_useMemcpy (void)
{
  seenMemcpy = TRUE;
}

void keyc_useIntMin (void)
{
  seenIntMin = TRUE;
}

void keyc_useUIntMin (void)
{
  seenUIntMin = TRUE;
}

void keyc_useLongMin (void)
{
  seenLongMin = TRUE;
}

void keyc_useULongMin (void)
{
  seenULongMin = TRUE;
}

void keyc_useCharMin (void)
{
  seenCharMin = TRUE;
}

void keyc_useUCharMin (void)
{
  seenUCharMin = TRUE;
}

void keyc_useIntMax (void)
{
  seenIntMax = TRUE;
}

void keyc_useUIntMax (void)
{
  seenUIntMax = TRUE;
}

void keyc_useLongMax (void)
{
  seenLongMax = TRUE;
}

void keyc_useULongMax (void)
{
  seenULongMax = TRUE;
}

void keyc_useCharMax (void)
{
  seenCharMax = TRUE;
}

void keyc_useUCharMax (void)
{
  seenUCharMax = TRUE;
}

void keyc_useLabs (void)
{
  seenLabs = TRUE;
}

void keyc_useAbs (void)
{
  seenAbs = TRUE;
}

void keyc_useFabs (void)
{
  seenFabs = TRUE;
}

void keyc_useFabsl (void)
{
  seenFabsl = TRUE;
}

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
}

void keyc_enterScope (decl_node n)
{
  scope s;

  s = new (n);
  s->scoped = n;
  s->symbols = symbolKey_initTree ();
  s->next = stack;
  stack = s;
}

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
    M2RTS_HALT (0);
}

DynamicStrings_String keyc_cname (nameKey_Name n, unsigned int scopes)
{
  DynamicStrings_String m;

  m = NULL;
  if (clash (n, scopes))
    if (((mangle1 (n, &m, scopes)) || (mangle2 (n, &m, scopes))) || (mangleN (n, &m, scopes)))
    {
      /* avoid dangling else.  */
      if (scopes)
        {
          n = nameKey_makekey (DynamicStrings_string (m));
          symbolKey_putSymKey (stack->symbols, n, (void *) m);
        }
    }
    else
      M2RTS_HALT (0);
  else if (scopes)
    symbolKey_putSymKey (stack->symbols, n, (void *) DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
  return m;
}

void _M2_keyc_init (int argc, char *argv[])
{
  init ();
}

void _M2_keyc_finish (int argc, char *argv[])
{
}
