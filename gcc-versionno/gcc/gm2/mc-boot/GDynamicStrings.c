/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod.  */

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
#define _DynamicStrings_H
#define _DynamicStrings_C

#   include "Glibc.h"
#   include "GStrLib.h"
#   include "GStorage.h"
#   include "GAssertion.h"
#   include "GSYSTEM.h"
#   include "GASCII.h"
#   include "GM2RTS.h"

#   define MaxBuf 127
#   define PoisonOn FALSE
#   define DebugOn FALSE
#   define CheckOn FALSE
#   define TraceOn FALSE
typedef struct Contents_r Contents;

typedef struct DebugInfo_r DebugInfo;

typedef struct stringRecord_r stringRecord;

typedef struct descriptor_r descriptor;

typedef descriptor *Descriptor;

typedef struct frameRec_r frameRec;

typedef frameRec *frame;

typedef struct _T1_a _T1;

typedef enum {inuse, marked, onlist, poisoned} desState;

typedef stringRecord *DynamicStrings_String;

struct DebugInfo_r {
                     DynamicStrings_String next;
                     void *file;
                     unsigned int line;
                     void *proc;
                   };

struct descriptor_r {
                      unsigned int charStarUsed;
                      void *charStar;
                      unsigned int charStarSize;
                      unsigned int charStarValid;
                      desState state;
                      DynamicStrings_String garbage;
                    };

struct frameRec_r {
                    DynamicStrings_String alloc;
                    DynamicStrings_String dealloc;
                    frame next;
                  };

struct _T1_a { char array[(MaxBuf-1)+1]; };
struct Contents_r {
                    _T1 buf;
                    unsigned int len;
                    DynamicStrings_String next;
                  };

struct stringRecord_r {
                        Contents contents;
                        Descriptor head;
                        DebugInfo debug;
                      };

static unsigned int Initialized;
static frame frameHead;
static DynamicStrings_String captured;
DynamicStrings_String DynamicStrings_InitString (char *a_, unsigned int _a_high);
DynamicStrings_String DynamicStrings_KillString (DynamicStrings_String s);
void DynamicStrings_Fin (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_InitStringCharStar (void * a);
DynamicStrings_String DynamicStrings_InitStringChar (char ch);
DynamicStrings_String DynamicStrings_Mark (DynamicStrings_String s);
unsigned int DynamicStrings_Length (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_ConCat (DynamicStrings_String a, DynamicStrings_String b);
DynamicStrings_String DynamicStrings_ConCatChar (DynamicStrings_String a, char ch);
DynamicStrings_String DynamicStrings_Assign (DynamicStrings_String a, DynamicStrings_String b);
DynamicStrings_String DynamicStrings_Dup (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_Add (DynamicStrings_String a, DynamicStrings_String b);
unsigned int DynamicStrings_Equal (DynamicStrings_String a, DynamicStrings_String b);
unsigned int DynamicStrings_EqualCharStar (DynamicStrings_String s, void * a);
unsigned int DynamicStrings_EqualArray (DynamicStrings_String s, char *a_, unsigned int _a_high);
DynamicStrings_String DynamicStrings_Mult (DynamicStrings_String s, unsigned int n);
DynamicStrings_String DynamicStrings_Slice (DynamicStrings_String s, int low, int high);
int DynamicStrings_Index (DynamicStrings_String s, char ch, unsigned int o);
int DynamicStrings_RIndex (DynamicStrings_String s, char ch, unsigned int o);
DynamicStrings_String DynamicStrings_RemoveComment (DynamicStrings_String s, char comment);
DynamicStrings_String DynamicStrings_RemoveWhitePrefix (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_RemoveWhitePostfix (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_ToUpper (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_ToLower (DynamicStrings_String s);
void DynamicStrings_CopyOut (char *a, unsigned int _a_high, DynamicStrings_String s);
char DynamicStrings_char (DynamicStrings_String s, int i);
void * DynamicStrings_string (DynamicStrings_String s);
DynamicStrings_String DynamicStrings_InitStringDB (char *a_, unsigned int _a_high, char *file_, unsigned int _file_high, unsigned int line);
DynamicStrings_String DynamicStrings_InitStringCharStarDB (void * a, char *file_, unsigned int _file_high, unsigned int line);
DynamicStrings_String DynamicStrings_InitStringCharDB (char ch, char *file_, unsigned int _file_high, unsigned int line);
DynamicStrings_String DynamicStrings_MultDB (DynamicStrings_String s, unsigned int n, char *file_, unsigned int _file_high, unsigned int line);
DynamicStrings_String DynamicStrings_DupDB (DynamicStrings_String s, char *file_, unsigned int _file_high, unsigned int line);
DynamicStrings_String DynamicStrings_SliceDB (DynamicStrings_String s, int low, int high, char *file_, unsigned int _file_high, unsigned int line);
void DynamicStrings_PushAllocation (void);
void DynamicStrings_PopAllocation (unsigned int halt);
DynamicStrings_String DynamicStrings_PopAllocationExemption (unsigned int halt, DynamicStrings_String e);
static void writeStringDesc (DynamicStrings_String s);
static void writeNspace (unsigned int n);
static void DumpStringInfo (DynamicStrings_String s, unsigned int i);
static void doDSdbEnter (void);
static void doDSdbExit (DynamicStrings_String s);
static void DSdbEnter (void);
static void DSdbExit (DynamicStrings_String s);
static unsigned int Capture (DynamicStrings_String s);
static unsigned int Min (unsigned int a, unsigned int b);
static unsigned int Max (unsigned int a, unsigned int b);
static void writeString (char *a_, unsigned int _a_high);
static void writeCstring (void * a);
static void writeCard (unsigned int c);
static void writeLongcard (long unsigned int l);
static void writeAddress (void * a);
static void writeLn (void);
static DynamicStrings_String AssignDebug (DynamicStrings_String s, char *file_, unsigned int _file_high, unsigned int line, char *proc_, unsigned int _proc_high);
static unsigned int IsOn (DynamicStrings_String list, DynamicStrings_String s);
static void AddTo (DynamicStrings_String *list, DynamicStrings_String s);
static void SubFrom (DynamicStrings_String *list, DynamicStrings_String s);
static void AddAllocated (DynamicStrings_String s);
static void AddDeallocated (DynamicStrings_String s);
static unsigned int IsOnAllocated (DynamicStrings_String s);
static unsigned int IsOnDeallocated (DynamicStrings_String s);
static void SubAllocated (DynamicStrings_String s);
static void SubDeallocated (DynamicStrings_String s);
static void SubDebugInfo (DynamicStrings_String s);
static void AddDebugInfo (DynamicStrings_String s);
static void ConcatContents (Contents *c, char *a_, unsigned int _a_high, unsigned int h, unsigned int o);
static void DeallocateCharStar (DynamicStrings_String s);
static DynamicStrings_String CheckPoisoned (DynamicStrings_String s);
static void MarkInvalid (DynamicStrings_String s);
static void ConcatContentsAddress (Contents *c, void * a, unsigned int h);
static DynamicStrings_String AddToGarbage (DynamicStrings_String a, DynamicStrings_String b);
static unsigned int IsOnGarbage (DynamicStrings_String e, DynamicStrings_String s);
static unsigned int IsWhite (char ch);
static void DumpState (DynamicStrings_String s);
static void DumpStringSynopsis (DynamicStrings_String s);
static void DumpString (DynamicStrings_String s);
static void Init (void);

static void writeStringDesc (DynamicStrings_String s)
{
  writeCstring (s->debug.file);
  writeString ((char *) ":", 1);
  writeCard (s->debug.line);
  writeString ((char *) ":", 1);
  writeCstring (s->debug.proc);
  writeString ((char *) " ", 1);
  writeAddress ((void *) s);
  writeString ((char *) " ", 1);
  switch (s->head->state)
    {
      case inuse:
        writeString ((char *) "still in use (", 14);
        writeCard (s->contents.len);
        writeString ((char *) ") characters", 12);
        break;

      case marked:
        writeString ((char *) "marked", 6);
        break;

      case onlist:
        writeString ((char *) "on a (lost) garbage list", 24);
        break;

      case poisoned:
        writeString ((char *) "poisoned", 8);
        break;


      default:
        writeString ((char *) "unknown state", 13);
        break;
    }
}

static void writeNspace (unsigned int n)
{
  while (n > 0)
    {
      writeString ((char *) " ", 1);
      n -= 1;
    }
}

static void DumpStringInfo (DynamicStrings_String s, unsigned int i)
{
  DynamicStrings_String t;

  if (s != NULL)
    {
      writeNspace (i);
      writeStringDesc (s);
      writeLn ();
      if (s->head->garbage != NULL)
        {
          writeNspace (i);
          writeString ((char *) "garbage list:", 13);
          writeLn ();
          do {
            s = s->head->garbage;
            DumpStringInfo (s, i+1);
            writeLn ();
          } while (! (s == NULL));
        }
    }
}

static void doDSdbEnter (void)
{
  if (CheckOn)
    DynamicStrings_PushAllocation ();
}

static void doDSdbExit (DynamicStrings_String s)
{
  if (CheckOn)
    s = DynamicStrings_PopAllocationExemption (TRUE, s);
}

static void DSdbEnter (void)
{
}

static void DSdbExit (DynamicStrings_String s)
{
}

static unsigned int Capture (DynamicStrings_String s)
{
  captured = s;
  return 1;
}

static unsigned int Min (unsigned int a, unsigned int b)
{
  if (a < b)
    return a;
  else
    return b;
}

static unsigned int Max (unsigned int a, unsigned int b)
{
  if (a > b)
    return a;
  else
    return b;
}

static void writeString (char *a_, unsigned int _a_high)
{
  int i;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  i = libc_write (1, &a, (int ) StrLib_StrLen ((char *) a, _a_high));
}

static void writeCstring (void * a)
{
  int i;

  if (a == NULL)
    writeString ((char *) "(null)", 6);
  else
    i = libc_write (1, a, libc_strlen (a));
}

static void writeCard (unsigned int c)
{
  char ch;
  int i;

  if (c > 9)
    {
      writeCard (c / 10);
      writeCard (c % 10);
    }
  else
    {
      ch = (char) (((unsigned int) ('0'))+c);
      i = libc_write (1, &ch, 1);
    }
}

static void writeLongcard (long unsigned int l)
{
  char ch;
  int i;

  if (l > 16)
    {
      writeLongcard (l / 16);
      writeLongcard (l % 16);
    }
  else if (l < 10)
    {
      ch = (char) (((unsigned int) ('0'))+((unsigned int ) (l)));
      i = libc_write (1, &ch, 1);
    }
  else if (l < 16)
    {
      ch = (char) ((((unsigned int) ('a'))+((unsigned int ) (l)))-10);
      i = libc_write (1, &ch, 1);
    }
}

static void writeAddress (void * a)
{
  writeLongcard ((long unsigned int ) (a));
}

static void writeLn (void)
{
  char ch;
  int i;

  ch = ASCII_lf;
  i = libc_write (1, &ch, 1);
}

static DynamicStrings_String AssignDebug (DynamicStrings_String s, char *file_, unsigned int _file_high, unsigned int line, char *proc_, unsigned int _proc_high)
{
  void * f;
  void * p;
  char file[_file_high+1];
  char proc[_proc_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (file, file_, _file_high+1);
  memcpy (proc, proc_, _proc_high+1);

  f = &file;
  p = &proc;
  Storage_ALLOCATE (&s->debug.file, (StrLib_StrLen ((char *) file, _file_high))+1);
  if ((libc_strncpy (s->debug.file, f, (StrLib_StrLen ((char *) file, _file_high))+1)) == NULL)
    {}  /* empty.  */
  s->debug.line = line;
  Storage_ALLOCATE (&s->debug.proc, (StrLib_StrLen ((char *) proc, _proc_high))+1);
  if ((libc_strncpy (s->debug.proc, p, (StrLib_StrLen ((char *) proc, _proc_high))+1)) == NULL)
    {}  /* empty.  */
  return s;
}

static unsigned int IsOn (DynamicStrings_String list, DynamicStrings_String s)
{
  while ((list != s) && (list != NULL))
    list = list->debug.next;
  return list == s;
}

static void AddTo (DynamicStrings_String *list, DynamicStrings_String s)
{
  if ((*list) == NULL)
    {
      (*list) = s;
      s->debug.next = NULL;
    }
  else
    {
      s->debug.next = (*list);
      (*list) = s;
    }
}

static void SubFrom (DynamicStrings_String *list, DynamicStrings_String s)
{
  DynamicStrings_String p;

  if ((*list) == s)
    (*list) = s->debug.next;
  else
    {
      p = (*list);
      while ((p->debug.next != NULL) && (p->debug.next != s))
        p = p->debug.next;
      if (p->debug.next == s)
        p->debug.next = s->debug.next;
      else
        return;
    }
  s->debug.next = NULL;
}

static void AddAllocated (DynamicStrings_String s)
{
  Init ();
  AddTo (&frameHead->alloc, s);
}

static void AddDeallocated (DynamicStrings_String s)
{
  Init ();
  AddTo (&frameHead->dealloc, s);
}

static unsigned int IsOnAllocated (DynamicStrings_String s)
{
  frame f;

  Init ();
  f = frameHead;
  do {
    if (IsOn (f->alloc, s))
      return TRUE;
    else
      f = f->next;
  } while (! (f == NULL));
  return FALSE;
}

static unsigned int IsOnDeallocated (DynamicStrings_String s)
{
  frame f;

  Init ();
  f = frameHead;
  do {
    if (IsOn (f->dealloc, s))
      return TRUE;
    else
      f = f->next;
  } while (! (f == NULL));
  return FALSE;
}

static void SubAllocated (DynamicStrings_String s)
{
  frame f;

  Init ();
  f = frameHead;
  do {
    if (IsOn (f->alloc, s))
      {
        SubFrom (&f->alloc, s);
        return;
      }
    else
      f = f->next;
  } while (! (f == NULL));
}

static void SubDeallocated (DynamicStrings_String s)
{
  frame f;

  Init ();
  f = frameHead;
  do {
    if (IsOn (f->dealloc, s))
      {
        SubFrom (&f->dealloc, s);
        return;
      }
    else
      f = f->next;
  } while (! (f == NULL));
}

static void SubDebugInfo (DynamicStrings_String s)
{
  if (IsOnDeallocated (s))
    {
      Assertion_Assert (! DebugOn);
      return;
    }
  if (IsOnAllocated (s))
    {
      SubAllocated (s);
      AddDeallocated (s);
    }
  else
    Assertion_Assert (! DebugOn);
}

static void AddDebugInfo (DynamicStrings_String s)
{
  s->debug.next = NULL;
  s->debug.file = NULL;
  s->debug.line = 0;
  s->debug.proc = NULL;
  if (CheckOn)
    AddAllocated (s);
}

static void ConcatContents (Contents *c, char *a_, unsigned int _a_high, unsigned int h, unsigned int o)
{
  unsigned int i;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  i = (*c).len;
  while ((o < h) && (i < MaxBuf))
    {
      (*c).buf.array[i] = a[o];
      o += 1;
      i += 1;
    }
  if (o < h)
    {
      (*c).len = MaxBuf;
      Storage_ALLOCATE ((void **) &(*c).next, sizeof (stringRecord));
      (*c).next->head = NULL;
      (*c).next->contents.len = 0;
      (*c).next->contents.next = NULL;
      ConcatContents (&(*c).next->contents, (char *) a, _a_high, h, o);
      AddDebugInfo ((*c).next);
      (*c).next = AssignDebug ((*c).next, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 714, (char *) "ConcatContents", 14);
    }
  else
    (*c).len = i;
}

static void DeallocateCharStar (DynamicStrings_String s)
{
  if ((s != NULL) && (s->head != NULL))
    {
      if (s->head->charStarUsed && (s->head->charStar != NULL))
        Storage_DEALLOCATE (&s->head->charStar, s->head->charStarSize);
      s->head->charStarUsed = FALSE;
      s->head->charStar = NULL;
      s->head->charStarSize = 0;
      s->head->charStarValid = FALSE;
    }
}

static DynamicStrings_String CheckPoisoned (DynamicStrings_String s)
{
  if (((PoisonOn && (s != NULL)) && (s->head != NULL)) && (s->head->state == poisoned))
    M2RTS_HALT (0);
  return s;
}

static void MarkInvalid (DynamicStrings_String s)
{
  if (PoisonOn)
    s = CheckPoisoned (s);
  if (s->head != NULL)
    s->head->charStarValid = FALSE;
}

static void ConcatContentsAddress (Contents *c, void * a, unsigned int h)
{
  unsigned int i;
  unsigned int j;
  char * p;

  j = 0;
  i = (*c).len;
  p = a;
  while ((j < h) && (i < MaxBuf))
    {
      (*c).buf.array[i] = (*p);
      i += 1;
      j += 1;
      p += 1;
    }
  if (j < h)
    {
      (*c).len = MaxBuf;
      Storage_ALLOCATE ((void **) &(*c).next, sizeof (stringRecord));
      (*c).next->head = NULL;
      (*c).next->contents.len = 0;
      (*c).next->contents.next = NULL;
      ConcatContentsAddress (&(*c).next->contents, (void *) p, h-j);
      AddDebugInfo ((*c).next);
      if (TraceOn)
        (*c).next = AssignDebug ((*c).next, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 909, (char *) "ConcatContentsAddress", 21);
    }
  else
    {
      (*c).len = i;
      (*c).next = NULL;
    }
}

static DynamicStrings_String AddToGarbage (DynamicStrings_String a, DynamicStrings_String b)
{
  DynamicStrings_String c;

  if (PoisonOn)
    {
      a = CheckPoisoned (a);
      b = CheckPoisoned (b);
    }
  if (((((a != b) && (a != NULL)) && (b != NULL)) && (b->head->state == marked)) && (a->head->state == inuse))
    {
      c = a;
      while (c->head->garbage != NULL)
        c = c->head->garbage;
      c->head->garbage = b;
      b->head->state = onlist;
      if (CheckOn)
        SubDebugInfo (b);
    }
  return a;
}

static unsigned int IsOnGarbage (DynamicStrings_String e, DynamicStrings_String s)
{
  if ((e != NULL) && (s != NULL))
    while (e->head->garbage != NULL)
      if (e->head->garbage == s)
        return TRUE;
      else
        e = e->head->garbage;
  return FALSE;
}

static unsigned int IsWhite (char ch)
{
  return (ch == ' ') || (ch == ASCII_tab);
}

static void DumpState (DynamicStrings_String s)
{
  switch (s->head->state)
    {
      case inuse:
        writeString ((char *) "still in use (", 14);
        writeCard (s->contents.len);
        writeString ((char *) ") characters", 12);
        break;

      case marked:
        writeString ((char *) "marked", 6);
        break;

      case onlist:
        writeString ((char *) "on a garbage list", 17);
        break;

      case poisoned:
        writeString ((char *) "poisoned", 8);
        break;


      default:
        writeString ((char *) "unknown state", 13);
        break;
    }
}

static void DumpStringSynopsis (DynamicStrings_String s)
{
  writeCstring (s->debug.file);
  writeString ((char *) ":", 1);
  writeCard (s->debug.line);
  writeString ((char *) ":", 1);
  writeCstring (s->debug.proc);
  writeString ((char *) " string ", 8);
  writeAddress ((void *) s);
  writeString ((char *) " ", 1);
  DumpState (s);
  if (IsOnAllocated (s))
    writeString ((char *) " globally allocated", 19);
  else if (IsOnDeallocated (s))
    writeString ((char *) " globally deallocated", 21);
  else
    writeString ((char *) " globally unknown", 17);
  writeLn ();
}

static void DumpString (DynamicStrings_String s)
{
  DynamicStrings_String t;

  if (s != NULL)
    {
      DumpStringSynopsis (s);
      if ((s->head != NULL) && (s->head->garbage != NULL))
        {
          writeString ((char *) "display chained strings on the garbage list", 43);
          writeLn ();
          t = s->head->garbage;
          while (t != NULL)
            {
              DumpStringSynopsis (t);
              t = t->head->garbage;
            }
        }
    }
}

static void Init (void)
{
  if (! Initialized)
    {
      Initialized = TRUE;
      frameHead = NULL;
      DynamicStrings_PushAllocation ();
    }
}

DynamicStrings_String DynamicStrings_InitString (char *a_, unsigned int _a_high)
{
  DynamicStrings_String s;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  Storage_ALLOCATE ((void **) &s, sizeof (stringRecord));
  s->contents.len = 0;
  s->contents.next = NULL;
  ConcatContents (&s->contents, (char *) a, _a_high, StrLib_StrLen ((char *) a, _a_high), 0);
  Storage_ALLOCATE ((void **) &s->head, sizeof (descriptor));
  s->head->charStarUsed = FALSE;
  s->head->charStar = NULL;
  s->head->charStarSize = 0;
  s->head->charStarValid = FALSE;
  s->head->garbage = NULL;
  s->head->state = inuse;
  AddDebugInfo (s);
  if (TraceOn)
    s = AssignDebug (s, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 750, (char *) "InitString", 10);
  return s;
}

DynamicStrings_String DynamicStrings_KillString (DynamicStrings_String s)
{
  DynamicStrings_String t;

  if (PoisonOn)
    s = CheckPoisoned (s);
  if (s != NULL)
    {
      if (CheckOn)
      {
        /* avoid gcc warning by using compound statement even if not strictly necessary.  */
        if (IsOnAllocated (s))
          SubAllocated (s);
        else if (IsOnDeallocated (s))
          SubDeallocated (s);
      }
      if (s->head != NULL)
        {
          s->head->state = poisoned;
          s->head->garbage = DynamicStrings_KillString (s->head->garbage);
          if (! PoisonOn)
            DeallocateCharStar (s);
          if (! PoisonOn)
            {
              Storage_DEALLOCATE ((void **) &s->head, sizeof (descriptor));
              s->head = NULL;
            }
        }
      t = DynamicStrings_KillString (s->contents.next);
      if (! PoisonOn)
        Storage_DEALLOCATE ((void **) &s, sizeof (stringRecord));
    }
  return NULL;
}

void DynamicStrings_Fin (DynamicStrings_String s)
{
  if ((DynamicStrings_KillString (s)) != NULL)
    M2RTS_HALT (0);
}

DynamicStrings_String DynamicStrings_InitStringCharStar (void * a)
{
  DynamicStrings_String s;

  Storage_ALLOCATE ((void **) &s, sizeof (stringRecord));
  s->contents.len = 0;
  s->contents.next = NULL;
  if (a != NULL)
    ConcatContentsAddress (&s->contents, a, (unsigned int ) libc_strlen (a));
  Storage_ALLOCATE ((void **) &s->head, sizeof (descriptor));
  s->head->charStarUsed = FALSE;
  s->head->charStar = NULL;
  s->head->charStarSize = 0;
  s->head->charStarValid = FALSE;
  s->head->garbage = NULL;
  s->head->state = inuse;
  AddDebugInfo (s);
  if (TraceOn)
    s = AssignDebug (s, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 949, (char *) "InitStringCharStar", 18);
  return s;
}

DynamicStrings_String DynamicStrings_InitStringChar (char ch)
{
  typedef struct _T2_a _T2;

  struct _T2_a { char array[1+1]; };
  _T2 a;
  DynamicStrings_String s;

  a.array[0] = ch;
  a.array[1] = ASCII_nul;
  s = DynamicStrings_InitString ((char *) &a.array[0], 1);
  if (TraceOn)
    s = AssignDebug (s, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 969, (char *) "InitStringChar", 14);
  return s;
}

DynamicStrings_String DynamicStrings_Mark (DynamicStrings_String s)
{
  if (PoisonOn)
    s = CheckPoisoned (s);
  if ((s != NULL) && (s->head->state == inuse))
    s->head->state = marked;
  return s;
}

unsigned int DynamicStrings_Length (DynamicStrings_String s)
{
  if (s == NULL)
    return 0;
  else
    return s->contents.len+(DynamicStrings_Length (s->contents.next));
}

DynamicStrings_String DynamicStrings_ConCat (DynamicStrings_String a, DynamicStrings_String b)
{
  DynamicStrings_String t;

  if (PoisonOn)
    {
      a = CheckPoisoned (a);
      b = CheckPoisoned (b);
    }
  if (a == b)
    return DynamicStrings_ConCat (a, DynamicStrings_Mark (DynamicStrings_Dup (b)));
  else if (a != NULL)
    {
      a = AddToGarbage (a, b);
      MarkInvalid (a);
      t = a;
      while (b != NULL)
        {
          while ((t->contents.len == MaxBuf) && (t->contents.next != NULL))
            t = t->contents.next;
          ConcatContents (&t->contents, (char *) &b->contents.buf.array[0], (MaxBuf-1), b->contents.len, 0);
          b = b->contents.next;
        }
    }
  if ((a == NULL) && (b != NULL))
    M2RTS_HALT (0);
  return a;
}

DynamicStrings_String DynamicStrings_ConCatChar (DynamicStrings_String a, char ch)
{
  typedef struct _T3_a _T3;

  struct _T3_a { char array[1+1]; };
  _T3 b;
  DynamicStrings_String t;

  if (PoisonOn)
    a = CheckPoisoned (a);
  b.array[0] = ch;
  b.array[1] = ASCII_nul;
  t = a;
  MarkInvalid (a);
  while ((t->contents.len == MaxBuf) && (t->contents.next != NULL))
    t = t->contents.next;
  ConcatContents (&t->contents, (char *) &b.array[0], 1, 1, 0);
  return a;
}

DynamicStrings_String DynamicStrings_Assign (DynamicStrings_String a, DynamicStrings_String b)
{
  if (PoisonOn)
    {
      a = CheckPoisoned (a);
      b = CheckPoisoned (b);
    }
  if ((a != NULL) && (b != NULL))
    {
      a->contents.next = DynamicStrings_KillString (a->contents.next);
      a->contents.len = 0;
    }
  return DynamicStrings_ConCat (a, b);
}

DynamicStrings_String DynamicStrings_Dup (DynamicStrings_String s)
{
  if (PoisonOn)
    s = CheckPoisoned (s);
  s = DynamicStrings_Assign (DynamicStrings_InitString ((char *) "", 0), s);
  if (TraceOn)
    s = AssignDebug (s, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 1165, (char *) "Dup", 3);
  return s;
}

DynamicStrings_String DynamicStrings_Add (DynamicStrings_String a, DynamicStrings_String b)
{
  if (PoisonOn)
    {
      a = CheckPoisoned (a);
      b = CheckPoisoned (b);
    }
  a = DynamicStrings_ConCat (DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "", 0), a), b);
  if (TraceOn)
    a = AssignDebug (a, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 1185, (char *) "Add", 3);
  return a;
}

unsigned int DynamicStrings_Equal (DynamicStrings_String a, DynamicStrings_String b)
{
  unsigned int i;

  if (PoisonOn)
    {
      a = CheckPoisoned (a);
      b = CheckPoisoned (b);
    }
  if ((DynamicStrings_Length (a)) == (DynamicStrings_Length (b)))
    {
      while ((a != NULL) && (b != NULL))
        {
          i = 0;
          Assertion_Assert (a->contents.len == b->contents.len);
          while (i < a->contents.len)
            {
              if (a->contents.buf.array[i] != a->contents.buf.array[i])
                M2RTS_HALT (0);
              if (b->contents.buf.array[i] != b->contents.buf.array[i])
                M2RTS_HALT (0);
              if (a->contents.buf.array[i] != b->contents.buf.array[i])
                return FALSE;
              i += 1;
            }
          a = a->contents.next;
          b = b->contents.next;
        }
      return TRUE;
    }
  else
    return FALSE;
}

unsigned int DynamicStrings_EqualCharStar (DynamicStrings_String s, void * a)
{
  DynamicStrings_String t;

  if (PoisonOn)
    s = CheckPoisoned (s);
  t = DynamicStrings_InitStringCharStar (a);
  if (TraceOn)
    t = AssignDebug (t, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 1250, (char *) "EqualCharStar", 13);
  t = AddToGarbage (t, s);
  if (DynamicStrings_Equal (t, s))
    {
      t = DynamicStrings_KillString (t);
      return TRUE;
    }
  else
    {
      t = DynamicStrings_KillString (t);
      return FALSE;
    }
}

unsigned int DynamicStrings_EqualArray (DynamicStrings_String s, char *a_, unsigned int _a_high)
{
  DynamicStrings_String t;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  if (PoisonOn)
    s = CheckPoisoned (s);
  t = DynamicStrings_InitString ((char *) a, _a_high);
  if (TraceOn)
    t = AssignDebug (t, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 1280, (char *) "EqualArray", 10);
  t = AddToGarbage (t, s);
  if (DynamicStrings_Equal (t, s))
    {
      t = DynamicStrings_KillString (t);
      return TRUE;
    }
  else
    {
      t = DynamicStrings_KillString (t);
      return FALSE;
    }
}

DynamicStrings_String DynamicStrings_Mult (DynamicStrings_String s, unsigned int n)
{
  if (PoisonOn)
    s = CheckPoisoned (s);
  if (n <= 0)
    s = AddToGarbage (DynamicStrings_InitString ((char *) "", 0), s);
  else
    s = DynamicStrings_ConCat (DynamicStrings_Mult (s, n-1), s);
  if (TraceOn)
    s = AssignDebug (s, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 1312, (char *) "Mult", 4);
  return s;
}

DynamicStrings_String DynamicStrings_Slice (DynamicStrings_String s, int low, int high)
{
  DynamicStrings_String d;
  DynamicStrings_String t;
  int start;
  int end;
  int o;

  if (PoisonOn)
    s = CheckPoisoned (s);
  if (low < 0)
    low = ((int ) (DynamicStrings_Length (s)))+low;
  if (high <= 0)
    high = ((int ) (DynamicStrings_Length (s)))+high;
  else
    high = Min (DynamicStrings_Length (s), (unsigned int ) high);
  d = DynamicStrings_InitString ((char *) "", 0);
  d = AddToGarbage (d, s);
  o = 0;
  t = d;
  while (s != NULL)
    if (low < (o+((int ) (s->contents.len))))
      if (o > high)
        s = NULL;
      else
        {
          if (low < o)
            start = 0;
          else
            start = low-o;
          end = Max (Min (MaxBuf, (unsigned int ) high-o), 0);
          while (t->contents.len == MaxBuf)
            {
              if (t->contents.next == NULL)
                {
                  Storage_ALLOCATE ((void **) &t->contents.next, sizeof (stringRecord));
                  t->contents.next->head = NULL;
                  t->contents.next->contents.len = 0;
                  AddDebugInfo (t->contents.next);
                  if (TraceOn)
                    t->contents.next = AssignDebug (t->contents.next, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 1380, (char *) "Slice", 5);
                }
              t = t->contents.next;
            }
          ConcatContentsAddress (&t->contents, &s->contents.buf.array[start], (unsigned int ) end-start);
          o += s->contents.len;
          s = s->contents.next;
        }
    else
      {
        o += s->contents.len;
        s = s->contents.next;
      }
  if (TraceOn)
    d = AssignDebug (d, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 1397, (char *) "Slice", 5);
  return d;
}

int DynamicStrings_Index (DynamicStrings_String s, char ch, unsigned int o)
{
  unsigned int i;
  unsigned int k;

  if (PoisonOn)
    s = CheckPoisoned (s);
  k = 0;
  while (s != NULL)
    {
      if ((k+s->contents.len) < o)
        k += s->contents.len;
      else
        {
          i = o-k;
          while (i < s->contents.len)
            {
              if (s->contents.buf.array[i] == ch)
                return k+i;
              i += 1;
            }
          k += i;
          o = k;
        }
      s = s->contents.next;
    }
  return -1;
}

int DynamicStrings_RIndex (DynamicStrings_String s, char ch, unsigned int o)
{
  unsigned int i;
  unsigned int k;
  int j;

  if (PoisonOn)
    s = CheckPoisoned (s);
  j = -1;
  k = 0;
  while (s != NULL)
    {
      if ((k+s->contents.len) < o)
        k += s->contents.len;
      else
        {
          if (o < k)
            i = 0;
          else
            i = o-k;
          while (i < s->contents.len)
            {
              if (s->contents.buf.array[i] == ch)
                j = k;
              k += 1;
              i += 1;
            }
        }
      s = s->contents.next;
    }
  return j;
}

DynamicStrings_String DynamicStrings_RemoveComment (DynamicStrings_String s, char comment)
{
  int i;

  i = DynamicStrings_Index (s, comment, 0);
  if (i == 0)
    s = DynamicStrings_InitString ((char *) "", 0);
  else if (i > 0)
    s = DynamicStrings_RemoveWhitePostfix (DynamicStrings_Slice (DynamicStrings_Mark (s), 0, i));
  if (TraceOn)
    s = AssignDebug (s, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 1509, (char *) "RemoveComment", 13);
  return s;
}

DynamicStrings_String DynamicStrings_RemoveWhitePrefix (DynamicStrings_String s)
{
  unsigned int i;

  i = 0;
  while (IsWhite (DynamicStrings_char (s, (int ) i)))
    i += 1;
  s = DynamicStrings_Slice (s, (int ) (i), 0);
  if (TraceOn)
    s = AssignDebug (s, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 1621, (char *) "RemoveWhitePrefix", 17);
  return s;
}

DynamicStrings_String DynamicStrings_RemoveWhitePostfix (DynamicStrings_String s)
{
  int i;

  i = ((int ) (DynamicStrings_Length (s)))-1;
  while ((i >= 0) && (IsWhite (DynamicStrings_char (s, i))))
    i -= 1;
  s = DynamicStrings_Slice (s, 0, i+1);
  if (TraceOn)
    s = AssignDebug (s, (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/DynamicStrings.mod", 51, 1643, (char *) "RemoveWhitePostfix", 18);
  return s;
}

DynamicStrings_String DynamicStrings_ToUpper (DynamicStrings_String s)
{
  char ch;
  unsigned int i;
  DynamicStrings_String t;

  if (s != NULL)
    {
      MarkInvalid (s);
      t = s;
      while (t != NULL)
        {
          i = 0;
          while (i < t->contents.len)
            {
              ch = t->contents.buf.array[i];
              if ((ch >= 'a') && (ch <= 'z'))
                t->contents.buf.array[i] = (char) ((((unsigned int) (ch))-((unsigned int) ('a')))+((unsigned int) ('A')));
              i += 1;
            }
          t = t->contents.next;
        }
    }
  return s;
}

DynamicStrings_String DynamicStrings_ToLower (DynamicStrings_String s)
{
  char ch;
  unsigned int i;
  DynamicStrings_String t;

  if (s != NULL)
    {
      MarkInvalid (s);
      t = s;
      while (t != NULL)
        {
          i = 0;
          while (i < t->contents.len)
            {
              ch = t->contents.buf.array[i];
              if ((ch >= 'A') && (ch <= 'Z'))
                t->contents.buf.array[i] = (char) ((((unsigned int) (ch))-((unsigned int) ('A')))+((unsigned int) ('a')));
              i += 1;
            }
          t = t->contents.next;
        }
    }
  return s;
}

void DynamicStrings_CopyOut (char *a, unsigned int _a_high, DynamicStrings_String s)
{
  unsigned int i;
  unsigned int l;

  l = Min ((_a_high)+1, DynamicStrings_Length (s));
  i = 0;
  while (i < l)
    {
      a[i] = DynamicStrings_char (s, (int ) i);
      i += 1;
    }
  if (i <= (_a_high))
    a[i] = ASCII_nul;
}

char DynamicStrings_char (DynamicStrings_String s, int i)
{
  unsigned int c;

  if (PoisonOn)
    s = CheckPoisoned (s);
  if (i < 0)
    c = (unsigned int ) (((int ) (DynamicStrings_Length (s)))+i);
  else
    c = i;
  while ((s != NULL) && (c >= s->contents.len))
    {
      c -= s->contents.len;
      s = s->contents.next;
    }
  if ((s == NULL) || (c >= s->contents.len))
    return ASCII_nul;
  else
    return s->contents.buf.array[c];
}

void * DynamicStrings_string (DynamicStrings_String s)
{
  DynamicStrings_String a;
  unsigned int l;
  unsigned int i;
  char * p;

  if (PoisonOn)
    s = CheckPoisoned (s);
  if (s == NULL)
    return NULL;
  else
    {
      if (! s->head->charStarValid)
        {
          l = DynamicStrings_Length (s);
          if (! (s->head->charStarUsed && (s->head->charStarSize > l)))
            {
              DeallocateCharStar (s);
              Storage_ALLOCATE (&s->head->charStar, l+1);
              s->head->charStarSize = l+1;
              s->head->charStarUsed = TRUE;
            }
          p = s->head->charStar;
          a = s;
          while (a != NULL)
            {
              i = 0;
              while (i < a->contents.len)
                {
                  (*p) = a->contents.buf.array[i];
                  i += 1;
                  p += 1;
                }
              a = a->contents.next;
            }
          (*p) = ASCII_nul;
          s->head->charStarValid = TRUE;
        }
      return s->head->charStar;
    }
}

DynamicStrings_String DynamicStrings_InitStringDB (char *a_, unsigned int _a_high, char *file_, unsigned int _file_high, unsigned int line)
{
  char a[_a_high+1];
  char file[_file_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (file, file_, _file_high+1);

  return AssignDebug (DynamicStrings_InitString ((char *) a, _a_high), (char *) file, _file_high, line, (char *) "InitString", 10);
}

DynamicStrings_String DynamicStrings_InitStringCharStarDB (void * a, char *file_, unsigned int _file_high, unsigned int line)
{
  char file[_file_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (file, file_, _file_high+1);

  return AssignDebug (DynamicStrings_InitStringCharStar (a), (char *) file, _file_high, line, (char *) "InitStringCharStar", 18);
}

DynamicStrings_String DynamicStrings_InitStringCharDB (char ch, char *file_, unsigned int _file_high, unsigned int line)
{
  char file[_file_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (file, file_, _file_high+1);

  return AssignDebug (DynamicStrings_InitStringChar (ch), (char *) file, _file_high, line, (char *) "InitStringChar", 14);
}

DynamicStrings_String DynamicStrings_MultDB (DynamicStrings_String s, unsigned int n, char *file_, unsigned int _file_high, unsigned int line)
{
  char file[_file_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (file, file_, _file_high+1);

  return AssignDebug (DynamicStrings_Mult (s, n), (char *) file, _file_high, line, (char *) "Mult", 4);
}

DynamicStrings_String DynamicStrings_DupDB (DynamicStrings_String s, char *file_, unsigned int _file_high, unsigned int line)
{
  char file[_file_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (file, file_, _file_high+1);

  return AssignDebug (DynamicStrings_Dup (s), (char *) file, _file_high, line, (char *) "Dup", 3);
}

DynamicStrings_String DynamicStrings_SliceDB (DynamicStrings_String s, int low, int high, char *file_, unsigned int _file_high, unsigned int line)
{
  char file[_file_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (file, file_, _file_high+1);

  DSdbEnter ();
  s = AssignDebug (DynamicStrings_Slice (s, low, high), (char *) file, _file_high, line, (char *) "Slice", 5);
  DSdbExit (s);
  return s;
}

void DynamicStrings_PushAllocation (void)
{
  frame f;

  if (CheckOn)
    {
      Init ();
      Storage_ALLOCATE ((void **) &f, sizeof (frameRec));
      f->next = frameHead;
      f->alloc = NULL;
      f->dealloc = NULL;
      frameHead = f;
    }
}

void DynamicStrings_PopAllocation (unsigned int halt)
{
  if ((DynamicStrings_PopAllocationExemption (halt, (DynamicStrings_String) NULL)) == NULL)
    {}  /* empty.  */
}

DynamicStrings_String DynamicStrings_PopAllocationExemption (unsigned int halt, DynamicStrings_String e)
{
  DynamicStrings_String s;
  frame f;
  unsigned int b;

  Init ();
  if (frameHead == NULL)
    writeString ((char *) "mismatched number of PopAllocation's compared to PushAllocation's", 65);
  else
    {
      if (frameHead->alloc != NULL)
        {
          b = FALSE;
          s = frameHead->alloc;
          while (s != NULL)
            {
              if (! (((e == s) || (IsOnGarbage (e, s))) || (IsOnGarbage (s, e))))
                {
                  if (! b)
                    {
                      writeString ((char *) "the following strings have been lost", 36);
                      writeLn ();
                      b = TRUE;
                    }
                  DumpStringInfo (s, 0);
                }
              s = s->debug.next;
            }
          if (b && halt)
            libc_exit (1);
        }
      frameHead = frameHead->next;
    }
  return e;
}

void _M2_DynamicStrings_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  Initialized = FALSE;
  Init ();
}

void _M2_DynamicStrings_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
