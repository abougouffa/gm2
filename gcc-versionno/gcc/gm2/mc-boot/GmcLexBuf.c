/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcLexBuf.mod.  */

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
#   include "GStorage.h"
#define _mcLexBuf_H
#define _mcLexBuf_C

#   include "Gmcflex.h"
#   include "Glibc.h"
#   include "GSYSTEM.h"
#   include "GStorage.h"
#   include "GDynamicStrings.h"
#   include "GFormatStrings.h"
#   include "GnameKey.h"
#   include "GmcReserved.h"
#   include "GmcPrintf.h"
#   include "GmcDebug.h"

int mcLexBuf_currentinteger;
unsigned int mcLexBuf_currentcolumn;
void * mcLexBuf_currentstring;
mcReserved_toktype mcLexBuf_currenttoken;
#   define MaxBucketSize 100
#   define Debugging FALSE
typedef struct tokenDesc_r tokenDesc;

typedef struct listDesc_r listDesc;

typedef struct _T1_r _T1;

typedef _T1 *sourceList;

typedef struct _T2_r _T2;

typedef _T2 *tokenBucket;

typedef struct _T3_a _T3;

struct tokenDesc_r {
                     mcReserved_toktype token;
                     nameKey_Name str;
                     int int_;
                     unsigned int line;
                     unsigned int col;
                     sourceList file;
                   };

struct listDesc_r {
                    tokenBucket head;
                    tokenBucket tail;
                    unsigned int lastBucketOffset;
                  };

struct _T1_r {
               sourceList left;
               sourceList right;
               DynamicStrings_String name;
               unsigned int line;
               unsigned int col;
             };

struct _T3_a { tokenDesc array[MaxBucketSize+1]; };
struct _T2_r {
               _T3 buf;
               unsigned int len;
               tokenBucket next;
             };

static sourceList currentSource;
static unsigned int useBufferedTokens;
static unsigned int currentUsed;
static listDesc listOfTokens;
static unsigned int currentTokNo;
unsigned int mcLexBuf_openSource (DynamicStrings_String s);
void mcLexBuf_closeSource (void);
void mcLexBuf_reInitialize (void);
void mcLexBuf_resetForNewPass (void);
void mcLexBuf_getToken (void);
void mcLexBuf_insertToken (mcReserved_toktype token);
void mcLexBuf_insertTokenAndRewind (mcReserved_toktype token);
unsigned int mcLexBuf_getPreviousTokenLineNo (void);
unsigned int mcLexBuf_getLineNo (void);
unsigned int mcLexBuf_getTokenNo (void);
unsigned int mcLexBuf_tokenToLineNo (unsigned int tokenNo, unsigned int depth);
unsigned int mcLexBuf_getColumnNo (void);
unsigned int mcLexBuf_tokenToColumnNo (unsigned int tokenNo, unsigned int depth);
DynamicStrings_String mcLexBuf_findFileNameFromToken (unsigned int tokenNo, unsigned int depth);
DynamicStrings_String mcLexBuf_getFileName (void);
void mcLexBuf_addTok (mcReserved_toktype t);
void mcLexBuf_addTokCharStar (mcReserved_toktype t, void * s);
void mcLexBuf_addTokInteger (mcReserved_toktype t, int i);
void mcLexBuf_setFile (void * filename);
void mcLexBuf_pushFile (void * filename);
void mcLexBuf_popFile (void * filename);
static void init (void);
static void addTo (sourceList l);
static void subFrom (sourceList l);
static sourceList newElement (void * s);
static sourceList newList (void);
static void checkIfNeedToDuplicate (void);
static void killList (void);
static void displayToken (void);
static void updateFromBucket (tokenBucket b, unsigned int offset);
static void syncOpenWithBuffer (void);
static tokenBucket findtokenBucket (unsigned int *tokenNo);
static void stop (void);
static void addTokToList (mcReserved_toktype t, nameKey_Name n, int i, unsigned int l, unsigned int c, sourceList f);
static unsigned int isLastTokenEof (void);

static void init (void)
{
  mcLexBuf_currenttoken = mcReserved_eoftok;
  currentTokNo = 0;
  currentSource = NULL;
  listOfTokens.head = NULL;
  listOfTokens.tail = NULL;
  useBufferedTokens = FALSE;
}

static void addTo (sourceList l)
{
  l->right = currentSource;
  l->left = currentSource->left;
  currentSource->left->right = l;
  currentSource->left = l;
  l->left->line = mcflex_getLineNo ();
  l->left->col = mcflex_getColumnNo ();
}

static void subFrom (sourceList l)
{
  l->left->right = l->right;
  l->right->left = l->left;
}

static sourceList newElement (void * s)
{
  sourceList l;

  Storage_ALLOCATE ((void **) &l, sizeof (_T1));
  if (l == NULL)
    M2RTS_HALT (0);
  else
    {
      l->name = DynamicStrings_InitStringCharStar (s);
      l->left = NULL;
      l->right = NULL;
    }
  return l;
}

static sourceList newList (void)
{
  sourceList l;

  Storage_ALLOCATE ((void **) &l, sizeof (_T1));
  l->left = l;
  l->right = l;
  l->name = NULL;
  return l;
}

static void checkIfNeedToDuplicate (void)
{
  sourceList l;
  sourceList h;

  if (currentUsed)
    {
      l = currentSource->right;
      h = currentSource;
      currentSource = newList ();
      while (l != h)
        {
          addTo (newElement ((void *) l->name));
          l = l->right;
        }
    }
}

static void killList (void)
{
  sourceList l;
  sourceList k;

  if (! currentUsed && (currentSource != NULL))
    {
      l = currentSource;
      do {
        k = l;
        l = l->right;
        Storage_DEALLOCATE ((void **) &k, sizeof (_T1));
      } while (! (l == currentSource));
    }
}

static void displayToken (void)
{
  if (mcLexBuf_currenttoken == mcReserved_identtok)
    mcPrintf_printf1 ((char *) "currenttoken = %a\\n", 19, (unsigned char *) &mcLexBuf_currentstring, sizeof (mcLexBuf_currentstring));
  else
    switch (mcLexBuf_currenttoken)
      {
        case mcReserved_eoftok:
          mcPrintf_printf0 ((char *) "eoftok\\n", 8);
          break;

        case mcReserved_plustok:
          mcPrintf_printf0 ((char *) "plustok\\n", 9);
          break;

        case mcReserved_minustok:
          mcPrintf_printf0 ((char *) "minustok\\n", 10);
          break;

        case mcReserved_timestok:
          mcPrintf_printf0 ((char *) "timestok\\n", 10);
          break;

        case mcReserved_dividetok:
          mcPrintf_printf0 ((char *) "dividetok\\n", 11);
          break;

        case mcReserved_becomestok:
          mcPrintf_printf0 ((char *) "becomestok\\n", 12);
          break;

        case mcReserved_ambersandtok:
          mcPrintf_printf0 ((char *) "ambersandtok\\n", 14);
          break;

        case mcReserved_periodtok:
          mcPrintf_printf0 ((char *) "periodtok\\n", 11);
          break;

        case mcReserved_commatok:
          mcPrintf_printf0 ((char *) "commatok\\n", 10);
          break;

        case mcReserved_semicolontok:
          mcPrintf_printf0 ((char *) "semicolontok\\n", 14);
          break;

        case mcReserved_lparatok:
          mcPrintf_printf0 ((char *) "lparatok\\n", 10);
          break;

        case mcReserved_rparatok:
          mcPrintf_printf0 ((char *) "rparatok\\n", 10);
          break;

        case mcReserved_lsbratok:
          mcPrintf_printf0 ((char *) "lsbratok\\n", 10);
          break;

        case mcReserved_rsbratok:
          mcPrintf_printf0 ((char *) "rsbratok\\n", 10);
          break;

        case mcReserved_lcbratok:
          mcPrintf_printf0 ((char *) "lcbratok\\n", 10);
          break;

        case mcReserved_rcbratok:
          mcPrintf_printf0 ((char *) "rcbratok\\n", 10);
          break;

        case mcReserved_uparrowtok:
          mcPrintf_printf0 ((char *) "uparrowtok\\n", 12);
          break;

        case mcReserved_singlequotetok:
          mcPrintf_printf0 ((char *) "singlequotetok\\n", 16);
          break;

        case mcReserved_equaltok:
          mcPrintf_printf0 ((char *) "equaltok\\n", 10);
          break;

        case mcReserved_hashtok:
          mcPrintf_printf0 ((char *) "hashtok\\n", 9);
          break;

        case mcReserved_lesstok:
          mcPrintf_printf0 ((char *) "lesstok\\n", 9);
          break;

        case mcReserved_greatertok:
          mcPrintf_printf0 ((char *) "greatertok\\n", 12);
          break;

        case mcReserved_lessgreatertok:
          mcPrintf_printf0 ((char *) "lessgreatertok\\n", 16);
          break;

        case mcReserved_lessequaltok:
          mcPrintf_printf0 ((char *) "lessequaltok\\n", 14);
          break;

        case mcReserved_greaterequaltok:
          mcPrintf_printf0 ((char *) "greaterequaltok\\n", 17);
          break;

        case mcReserved_periodperiodtok:
          mcPrintf_printf0 ((char *) "periodperiodtok\\n", 17);
          break;

        case mcReserved_colontok:
          mcPrintf_printf0 ((char *) "colontok\\n", 10);
          break;

        case mcReserved_doublequotestok:
          mcPrintf_printf0 ((char *) "doublequotestok\\n", 17);
          break;

        case mcReserved_bartok:
          mcPrintf_printf0 ((char *) "bartok\\n", 8);
          break;

        case mcReserved_andtok:
          mcPrintf_printf0 ((char *) "andtok\\n", 8);
          break;

        case mcReserved_arraytok:
          mcPrintf_printf0 ((char *) "arraytok\\n", 10);
          break;

        case mcReserved_begintok:
          mcPrintf_printf0 ((char *) "begintok\\n", 10);
          break;

        case mcReserved_bytok:
          mcPrintf_printf0 ((char *) "bytok\\n", 7);
          break;

        case mcReserved_casetok:
          mcPrintf_printf0 ((char *) "casetok\\n", 9);
          break;

        case mcReserved_consttok:
          mcPrintf_printf0 ((char *) "consttok\\n", 10);
          break;

        case mcReserved_definitiontok:
          mcPrintf_printf0 ((char *) "definitiontok\\n", 15);
          break;

        case mcReserved_divtok:
          mcPrintf_printf0 ((char *) "divtok\\n", 8);
          break;

        case mcReserved_dotok:
          mcPrintf_printf0 ((char *) "dotok\\n", 7);
          break;

        case mcReserved_elsetok:
          mcPrintf_printf0 ((char *) "elsetok\\n", 9);
          break;

        case mcReserved_elsiftok:
          mcPrintf_printf0 ((char *) "elsiftok\\n", 10);
          break;

        case mcReserved_endtok:
          mcPrintf_printf0 ((char *) "endtok\\n", 8);
          break;

        case mcReserved_exittok:
          mcPrintf_printf0 ((char *) "exittok\\n", 9);
          break;

        case mcReserved_exporttok:
          mcPrintf_printf0 ((char *) "exporttok\\n", 11);
          break;

        case mcReserved_fortok:
          mcPrintf_printf0 ((char *) "fortok\\n", 8);
          break;

        case mcReserved_fromtok:
          mcPrintf_printf0 ((char *) "fromtok\\n", 9);
          break;

        case mcReserved_iftok:
          mcPrintf_printf0 ((char *) "iftok\\n", 7);
          break;

        case mcReserved_implementationtok:
          mcPrintf_printf0 ((char *) "implementationtok\\n", 19);
          break;

        case mcReserved_importtok:
          mcPrintf_printf0 ((char *) "importtok\\n", 11);
          break;

        case mcReserved_intok:
          mcPrintf_printf0 ((char *) "intok\\n", 7);
          break;

        case mcReserved_looptok:
          mcPrintf_printf0 ((char *) "looptok\\n", 9);
          break;

        case mcReserved_modtok:
          mcPrintf_printf0 ((char *) "modtok\\n", 8);
          break;

        case mcReserved_moduletok:
          mcPrintf_printf0 ((char *) "moduletok\\n", 11);
          break;

        case mcReserved_nottok:
          mcPrintf_printf0 ((char *) "nottok\\n", 8);
          break;

        case mcReserved_oftok:
          mcPrintf_printf0 ((char *) "oftok\\n", 7);
          break;

        case mcReserved_ortok:
          mcPrintf_printf0 ((char *) "ortok\\n", 7);
          break;

        case mcReserved_pointertok:
          mcPrintf_printf0 ((char *) "pointertok\\n", 12);
          break;

        case mcReserved_proceduretok:
          mcPrintf_printf0 ((char *) "proceduretok\\n", 14);
          break;

        case mcReserved_qualifiedtok:
          mcPrintf_printf0 ((char *) "qualifiedtok\\n", 14);
          break;

        case mcReserved_unqualifiedtok:
          mcPrintf_printf0 ((char *) "unqualifiedtok\\n", 16);
          break;

        case mcReserved_recordtok:
          mcPrintf_printf0 ((char *) "recordtok\\n", 11);
          break;

        case mcReserved_repeattok:
          mcPrintf_printf0 ((char *) "repeattok\\n", 11);
          break;

        case mcReserved_returntok:
          mcPrintf_printf0 ((char *) "returntok\\n", 11);
          break;

        case mcReserved_settok:
          mcPrintf_printf0 ((char *) "settok\\n", 8);
          break;

        case mcReserved_thentok:
          mcPrintf_printf0 ((char *) "thentok\\n", 9);
          break;

        case mcReserved_totok:
          mcPrintf_printf0 ((char *) "totok\\n", 7);
          break;

        case mcReserved_typetok:
          mcPrintf_printf0 ((char *) "typetok\\n", 9);
          break;

        case mcReserved_untiltok:
          mcPrintf_printf0 ((char *) "untiltok\\n", 10);
          break;

        case mcReserved_vartok:
          mcPrintf_printf0 ((char *) "vartok\\n", 8);
          break;

        case mcReserved_whiletok:
          mcPrintf_printf0 ((char *) "whiletok\\n", 10);
          break;

        case mcReserved_withtok:
          mcPrintf_printf0 ((char *) "withtok\\n", 9);
          break;

        case mcReserved_asmtok:
          mcPrintf_printf0 ((char *) "asmtok\\n", 8);
          break;

        case mcReserved_volatiletok:
          mcPrintf_printf0 ((char *) "volatiletok\\n", 13);
          break;

        case mcReserved_periodperiodperiodtok:
          mcPrintf_printf0 ((char *) "periodperiodperiodtok\\n", 23);
          break;

        case mcReserved_datetok:
          mcPrintf_printf0 ((char *) "datetok\\n", 9);
          break;

        case mcReserved_linetok:
          mcPrintf_printf0 ((char *) "linetok\\n", 9);
          break;

        case mcReserved_filetok:
          mcPrintf_printf0 ((char *) "filetok\\n", 9);
          break;

        case mcReserved_integertok:
          mcPrintf_printf0 ((char *) "integertok\\n", 12);
          break;

        case mcReserved_identtok:
          mcPrintf_printf0 ((char *) "identtok\\n", 10);
          break;

        case mcReserved_realtok:
          mcPrintf_printf0 ((char *) "realtok\\n", 9);
          break;

        case mcReserved_stringtok:
          mcPrintf_printf0 ((char *) "stringtok\\n", 11);
          break;


        default:
          break;
      }
}

static void updateFromBucket (tokenBucket b, unsigned int offset)
{
  mcLexBuf_currenttoken = b->buf.array[offset].token;
  mcLexBuf_currentstring = nameKey_keyToCharStar (b->buf.array[offset].str);
  mcLexBuf_currentcolumn = b->buf.array[offset].col;
  mcLexBuf_currentinteger = b->buf.array[offset].int_;
  if (Debugging)
    mcPrintf_printf3 ((char *) "line %d (# %d  %d) ", 19, (unsigned char *) &b->buf.array[offset].line, sizeof (b->buf.array[offset].line), (unsigned char *) &offset, sizeof (offset), (unsigned char *) &currentTokNo, sizeof (currentTokNo));
}

static void syncOpenWithBuffer (void)
{
  if (listOfTokens.tail != NULL)
    currentTokNo = listOfTokens.lastBucketOffset+listOfTokens.tail->len;
}

static tokenBucket findtokenBucket (unsigned int *tokenNo)
{
  tokenBucket b;

  b = listOfTokens.head;
  while (b != NULL)
    {
      if ((*tokenNo) < b->len)
        return b;
      else
        (*tokenNo) -= b->len;
      b = b->next;
    }
  return NULL;
}

static void stop (void)
{
}

static void addTokToList (mcReserved_toktype t, nameKey_Name n, int i, unsigned int l, unsigned int c, sourceList f)
{
  tokenBucket b;

  if (listOfTokens.head == NULL)
    {
      Storage_ALLOCATE ((void **) &listOfTokens.head, sizeof (_T2));
      if (listOfTokens.head == NULL)
        ;  /* empty.  */
      listOfTokens.tail = listOfTokens.head;
      listOfTokens.tail->len = 0;
    }
  else if (listOfTokens.tail->len == MaxBucketSize)
    {
      mcDebug_assert (listOfTokens.tail->next == NULL);
      Storage_ALLOCATE ((void **) &listOfTokens.tail->next, sizeof (_T2));
      if (listOfTokens.tail->next == NULL)
        ;  /* empty.  */
      else
        {
          listOfTokens.tail = listOfTokens.tail->next;
          listOfTokens.tail->len = 0;
        }
      listOfTokens.lastBucketOffset += MaxBucketSize;
    }
  listOfTokens.tail->next = NULL;
  listOfTokens.tail->buf.array[listOfTokens.tail->len].token = t;
  listOfTokens.tail->buf.array[listOfTokens.tail->len].str = n;
  listOfTokens.tail->buf.array[listOfTokens.tail->len].int_ = i;
  listOfTokens.tail->buf.array[listOfTokens.tail->len].line = l;
  listOfTokens.tail->buf.array[listOfTokens.tail->len].col = c;
  listOfTokens.tail->buf.array[listOfTokens.tail->len].file = f;
  listOfTokens.tail->len += 1;
}

static unsigned int isLastTokenEof (void)
{
  unsigned int t;
  tokenBucket b;

  if (listOfTokens.tail != NULL)
    {
      if (listOfTokens.tail->len == 0)
        {
          b = listOfTokens.head;
          if (b == listOfTokens.tail)
            return FALSE;
          while (b->next != listOfTokens.tail)
            b = b->next;
        }
      else
        b = listOfTokens.tail;
      mcDebug_assert (b->len > 0);
      return b->buf.array[b->len-1].token == mcReserved_eoftok;
    }
  return FALSE;
}

unsigned int mcLexBuf_openSource (DynamicStrings_String s)
{
  if (useBufferedTokens)
    {
      mcLexBuf_getToken ();
      return TRUE;
    }
  else
    if (mcflex_openSource (DynamicStrings_string (s)))
      {
        mcLexBuf_setFile (DynamicStrings_string (s));
        syncOpenWithBuffer ();
        mcLexBuf_getToken ();
        return TRUE;
      }
    else
      return FALSE;
}

void mcLexBuf_closeSource (void)
{
  if (useBufferedTokens)
    while (mcLexBuf_currenttoken != mcReserved_eoftok)
      mcLexBuf_getToken ();
}

void mcLexBuf_reInitialize (void)
{
  tokenBucket s;
  tokenBucket t;

  if (listOfTokens.head != NULL)
    {
      t = listOfTokens.head;
      do {
        s = t;
        t = t->next;
        Storage_DEALLOCATE ((void **) &s, sizeof (_T2));
      } while (! (t == NULL));
      currentUsed = FALSE;
      killList ();
    }
  init ();
}

void mcLexBuf_resetForNewPass (void)
{
  currentTokNo = 0;
  useBufferedTokens = TRUE;
}

void mcLexBuf_getToken (void)
{
  void * a;
  unsigned int t;
  tokenBucket b;

  if (useBufferedTokens)
    {
      t = currentTokNo;
      b = findtokenBucket (&t);
      updateFromBucket (b, t);
    }
  else
    {
      if (listOfTokens.tail == NULL)
        {
          a = mcflex_getToken ();
          if (listOfTokens.tail == NULL)
            M2RTS_HALT (0);
        }
      if (currentTokNo >= listOfTokens.lastBucketOffset)
        if ((currentTokNo-listOfTokens.lastBucketOffset) < listOfTokens.tail->len)
          updateFromBucket (listOfTokens.tail, currentTokNo-listOfTokens.lastBucketOffset);
        else
          {
            a = mcflex_getToken ();
            mcLexBuf_getToken ();
            return;
          }
      else
        {
          t = currentTokNo;
          b = findtokenBucket (&t);
          updateFromBucket (b, t);
        }
    }
  if (Debugging)
    displayToken ();
  currentTokNo += 1;
}

void mcLexBuf_insertToken (mcReserved_toktype token)
{
  if (listOfTokens.tail != NULL)
    {
      if (listOfTokens.tail->len > 0)
        listOfTokens.tail->buf.array[listOfTokens.tail->len-1].token = token;
      addTokToList (mcLexBuf_currenttoken, (nameKey_Name) nameKey_NulName, 0, mcLexBuf_getLineNo (), mcLexBuf_getColumnNo (), currentSource);
      mcLexBuf_getToken ();
    }
}

void mcLexBuf_insertTokenAndRewind (mcReserved_toktype token)
{
  if (listOfTokens.tail != NULL)
    {
      if (listOfTokens.tail->len > 0)
        listOfTokens.tail->buf.array[listOfTokens.tail->len-1].token = token;
      addTokToList (mcLexBuf_currenttoken, (nameKey_Name) nameKey_NulName, 0, mcLexBuf_getLineNo (), mcLexBuf_getColumnNo (), currentSource);
      mcLexBuf_currenttoken = token;
    }
}

unsigned int mcLexBuf_getPreviousTokenLineNo (void)
{
  return mcLexBuf_getLineNo ();
}

unsigned int mcLexBuf_getLineNo (void)
{
  if (currentTokNo == 0)
    return 0;
  else
    return mcLexBuf_tokenToLineNo (mcLexBuf_getTokenNo (), 0);
}

unsigned int mcLexBuf_getTokenNo (void)
{
  if (currentTokNo == 0)
    return 0;
  else
    return currentTokNo-1;
}

unsigned int mcLexBuf_tokenToLineNo (unsigned int tokenNo, unsigned int depth)
{
  tokenBucket b;
  sourceList l;

  b = findtokenBucket (&tokenNo);
  if (b == NULL)
    return 0;
  else
    if (depth == 0)
      return b->buf.array[tokenNo].line;
    else
      {
        l = b->buf.array[tokenNo].file->left;
        while (depth > 0)
          {
            l = l->left;
            if (l == b->buf.array[tokenNo].file->left)
              return 0;
            depth -= 1;
          }
        return l->line;
      }
}

unsigned int mcLexBuf_getColumnNo (void)
{
  if (currentTokNo == 0)
    return 0;
  else
    return mcLexBuf_tokenToColumnNo (mcLexBuf_getTokenNo (), 0);
}

unsigned int mcLexBuf_tokenToColumnNo (unsigned int tokenNo, unsigned int depth)
{
  tokenBucket b;
  sourceList l;

  b = findtokenBucket (&tokenNo);
  if (b == NULL)
    return 0;
  else
    if (depth == 0)
      return b->buf.array[tokenNo].col;
    else
      {
        l = b->buf.array[tokenNo].file->left;
        while (depth > 0)
          {
            l = l->left;
            if (l == b->buf.array[tokenNo].file->left)
              return 0;
            depth -= 1;
          }
        return l->col;
      }
}

DynamicStrings_String mcLexBuf_findFileNameFromToken (unsigned int tokenNo, unsigned int depth)
{
  tokenBucket b;
  sourceList l;

  b = findtokenBucket (&tokenNo);
  if (b == NULL)
    return NULL;
  else
    {
      l = b->buf.array[tokenNo].file->left;
      while (depth > 0)
        {
          l = l->left;
          if (l == b->buf.array[tokenNo].file->left)
            return NULL;
          depth -= 1;
        }
      return l->name;
    }
}

DynamicStrings_String mcLexBuf_getFileName (void)
{
  return mcLexBuf_findFileNameFromToken (mcLexBuf_getTokenNo (), 0);
}

void mcLexBuf_addTok (mcReserved_toktype t)
{
  if (! ((t == mcReserved_eoftok) && (isLastTokenEof ())))
    {
      addTokToList (t, (nameKey_Name) nameKey_NulName, 0, mcflex_getLineNo (), mcflex_getColumnNo (), currentSource);
      currentUsed = TRUE;
    }
}

void mcLexBuf_addTokCharStar (mcReserved_toktype t, void * s)
{
  if ((libc_strlen (s)) > 80)
    stop ();
  addTokToList (t, nameKey_makekey (s), 0, mcflex_getLineNo (), mcflex_getColumnNo (), currentSource);
  currentUsed = TRUE;
}

void mcLexBuf_addTokInteger (mcReserved_toktype t, int i)
{
  DynamicStrings_String s;
  unsigned int c;
  unsigned int l;

  l = mcflex_getLineNo ();
  c = mcflex_getColumnNo ();
  s = FormatStrings_Sprintf1 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) "%d", 2)), (unsigned char *) &i, sizeof (i));
  addTokToList (t, nameKey_makekey (DynamicStrings_string (s)), i, l, c, currentSource);
  s = DynamicStrings_KillString (s);
  currentUsed = TRUE;
}

void mcLexBuf_setFile (void * filename)
{
  killList ();
  currentUsed = FALSE;
  currentSource = newList ();
  addTo (newElement (filename));
}

void mcLexBuf_pushFile (void * filename)
{
  sourceList l;

  checkIfNeedToDuplicate ();
  addTo (newElement (filename));
  if (Debugging)
    if (currentSource->right != currentSource)
      {
        l = currentSource;
        do {
          mcPrintf_printf3 ((char *) "name = %s, line = %d, col = %d\\n", 32, (unsigned char *) &l->name, sizeof (l->name), (unsigned char *) &l->line, sizeof (l->line), (unsigned char *) &l->col, sizeof (l->col));
          l = l->right;
        } while (! (l == currentSource));
      }
}

void mcLexBuf_popFile (void * filename)
{
  sourceList l;

  checkIfNeedToDuplicate ();
  if ((currentSource != NULL) && (currentSource->left != currentSource))
    {
      l = currentSource->left;
      subFrom (l);
      Storage_DEALLOCATE ((void **) &l, sizeof (_T1));
      if ((currentSource->left != currentSource) && (! (DynamicStrings_Equal (currentSource->name, DynamicStrings_Mark (DynamicStrings_InitStringCharStar (filename))))))
        ;  /* empty.  */
    }
}

void _M2_mcLexBuf_init (int argc, char *argv[])
{
  init ();
}

void _M2_mcLexBuf_finish (int argc, char *argv[])
{
}
