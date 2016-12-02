/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   if !defined (FALSE)
#      define FALSE (1==0)
#   endif

#include <stddef.h>
#include <string.h>
#include <limits.h>
#   include "GStorage.h"
#include "Gmcrts.h"
#define _RTExceptions_H
#define _RTExceptions_C

#   include "GASCII.h"
#   include "GStrLib.h"
#   include "GStorage.h"
#   include "GSYSTEM.h"
#   include "Glibc.h"
#   include "GM2RTS.h"
#   include "GSysExceptions.h"
#   include "GM2EXCEPTION.h"

typedef struct RTExceptions_ProcedureHandler_p RTExceptions_ProcedureHandler;

#   define MaxBuffer 4096
typedef struct handler_r handler;

typedef handler *Handler;

typedef struct ehblock_r ehblock;

typedef struct _T1_a _T1;

typedef char *PtrToChar;

typedef ehblock *RTExceptions_EHBlock;

typedef void (*RTExceptions_ProcedureHandler_t) (void);
struct RTExceptions_ProcedureHandler_p { RTExceptions_ProcedureHandler_t proc; };

struct _T1_a { char array[MaxBuffer+1]; };
struct handler_r {
                   RTExceptions_ProcedureHandler p;
                   unsigned int n;
                   Handler right;
                   Handler left;
                   Handler stack;
                 };

struct ehblock_r {
                   _T1 buffer;
                   unsigned int number;
                   Handler handlers;
                   RTExceptions_EHBlock right;
                 };

static unsigned int inException;
static Handler freeHandler;
static RTExceptions_EHBlock freeEHB;
static RTExceptions_EHBlock currentEHB;
static void * currentSource;
void RTExceptions_Raise (unsigned int number, void * file, unsigned int line, unsigned int column, void * function, void * message);
void RTExceptions_SetExceptionBlock (RTExceptions_EHBlock source);
RTExceptions_EHBlock RTExceptions_GetExceptionBlock (void);
void * RTExceptions_GetTextBuffer (RTExceptions_EHBlock e);
unsigned int RTExceptions_GetTextBufferSize (RTExceptions_EHBlock e);
unsigned int RTExceptions_GetNumber (RTExceptions_EHBlock source);
RTExceptions_EHBlock RTExceptions_InitExceptionBlock (void);
RTExceptions_EHBlock RTExceptions_KillExceptionBlock (RTExceptions_EHBlock e);
void RTExceptions_PushHandler (RTExceptions_EHBlock e, unsigned int number, RTExceptions_ProcedureHandler p);
void RTExceptions_PopHandler (RTExceptions_EHBlock e, unsigned int number);
void RTExceptions_DefaultErrorCatch (void);
void RTExceptions_BaseExceptionsThrow (void);
unsigned int RTExceptions_IsInExceptionState (void);
unsigned int RTExceptions_SetExceptionState (unsigned int to);
void RTExceptions_SwitchExceptionState (unsigned int *from, unsigned int to);
RTExceptions_EHBlock RTExceptions_GetBaseExceptionBlock (void);
void RTExceptions_SetExceptionSource (void * source);
void * RTExceptions_GetExceptionSource (void);
static void ErrorString (char *a_, unsigned int _a_high);
static Handler findHandler (RTExceptions_EHBlock e, unsigned int number);
static void InvokeHandler (void);
static void DoThrow (void);
static void addChar (char ch, unsigned int *i);
static void * stripPath (void * s);
static void addFile (void * s, unsigned int *i);
static void addStr (void * s, unsigned int *i);
static void addNum (unsigned int n, unsigned int *i);
static RTExceptions_EHBlock New (void);
static Handler NewHandler (void);
static Handler KillHandler (Handler h);
static Handler KillHandlers (Handler h);
static Handler InitHandler (Handler h, Handler l, Handler r, Handler s, unsigned int number, RTExceptions_ProcedureHandler proc);
static void SubHandler (Handler h);
static void AddHandler (RTExceptions_EHBlock e, Handler h);
static void indexf (void * a);
static void range (void * a);
static void casef (void * a);
static void invalidloc (void * a);
static void function (void * a);
static void wholevalue (void * a);
static void wholediv (void * a);
static void realvalue (void * a);
static void realdiv (void * a);
static void complexvalue (void * a);
static void complexdiv (void * a);
static void protection (void * a);
static void systemf (void * a);
static void coroutine (void * a);
static void exception (void * a);
static void Init (void);
static void TidyUp (void);

static void ErrorString (char *a_, unsigned int _a_high)
{
  int n;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  n = libc_write (2, &a, (int ) StrLib_StrLen ((char *) a, _a_high));
}

static Handler findHandler (RTExceptions_EHBlock e, unsigned int number)
{
  Handler h;

  h = e->handlers->right;
  while ((h != e->handlers) && (number != h->n))
    h = h->right;
  if (h == e->handlers)
    return NULL;
  else
    return h;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

static void InvokeHandler (void)
{
  Handler h;

  h = findHandler (currentEHB, currentEHB->number);
  if (h == NULL)
    throw (RTExceptions_GetNumber (RTExceptions_GetExceptionBlock ()));
  else
    (*h->p.proc) ();
}

static void DoThrow (void)
{
  throw (RTExceptions_GetNumber (RTExceptions_GetExceptionBlock ()));
}

static void addChar (char ch, unsigned int *i)
{
  if (((*i) <= MaxBuffer) && (currentEHB != NULL))
    {
      currentEHB->buffer.array[(*i)] = ch;
      (*i) += 1;
    }
}

static void * stripPath (void * s)
{
  PtrToChar f;
  PtrToChar p;

  p = s;
  f = s;
  while ((*p) != ASCII_nul)
    if ((*p) == '/')
      {
        p += 1;
        f = p;
      }
    else
      p += 1;
  return f;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

static void addFile (void * s, unsigned int *i)
{
  PtrToChar p;

  p = stripPath (s);
  while ((p != NULL) && ((*p) != ASCII_nul))
    {
      addChar ((*p), i);
      p += 1;
    }
}

static void addStr (void * s, unsigned int *i)
{
  PtrToChar p;

  p = s;
  while ((p != NULL) && ((*p) != ASCII_nul))
    {
      addChar ((*p), i);
      p += 1;
    }
}

static void addNum (unsigned int n, unsigned int *i)
{
  if (n < 10)
    addChar ((char) ((n % 10)+((unsigned int) ('0'))), i);
  else
    {
      addNum (n / 10, i);
      addNum (n % 10, i);
    }
}

static RTExceptions_EHBlock New (void)
{
  RTExceptions_EHBlock e;

  if (freeEHB == NULL)
    Storage_ALLOCATE ((void **) &e, sizeof (ehblock));
  else
    {
      e = freeEHB;
      freeEHB = freeEHB->right;
    }
  return e;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

static Handler NewHandler (void)
{
  Handler h;

  if (freeHandler == NULL)
    Storage_ALLOCATE ((void **) &h, sizeof (handler));
  else
    {
      h = freeHandler;
      freeHandler = freeHandler->right;
    }
  return h;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

static Handler KillHandler (Handler h)
{
  h->right = freeHandler;
  freeHandler = h;
  return NULL;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

static Handler KillHandlers (Handler h)
{
  h->left->right = freeHandler;
  freeHandler = h;
  return NULL;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

static Handler InitHandler (Handler h, Handler l, Handler r, Handler s, unsigned int number, RTExceptions_ProcedureHandler proc)
{
  h->p = proc;
  h->n = number;
  h->right = r;
  h->left = l;
  h->stack = s;
  return h;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

static void SubHandler (Handler h)
{
  h->right->left = h->left;
  h->left->right = h->right;
}

static void AddHandler (RTExceptions_EHBlock e, Handler h)
{
  h->right = e->handlers;
  h->left = e->handlers->left;
  e->handlers->left->right = h;
  e->handlers->left = h;
}

static void indexf (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_indexException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 603, 9, "indexf", "array index out of bounds");
}

static void range (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_rangeException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 615, 9, "range", "assignment out of range");
}

static void casef (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_caseSelectException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 627, 9, "casef", "case selector out of range");
}

static void invalidloc (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_invalidLocation), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 639, 9, "invalidloc", "invalid address referenced");
}

static void function (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_functionException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 651, 9, "function", "... function ... ");
}

static void wholevalue (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_wholeValueException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 663, 9, "wholevalue", "illegal whole value exception");
}

static void wholediv (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_wholeDivException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 675, 9, "wholediv", "illegal whole value exception");
}

static void realvalue (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_realValueException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 687, 9, "realvalue", "illegal real value exception");
}

static void realdiv (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_realDivException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 699, 9, "realdiv", "real number division by zero exception");
}

static void complexvalue (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_complexValueException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 711, 9, "complexvalue", "illegal complex value exception");
}

static void complexdiv (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_complexDivException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 723, 9, "complexdiv", "complex number division by zero exception");
}

static void protection (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_protException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 735, 9, "protection", "protection exception");
}

static void systemf (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_sysException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 747, 9, "systemf", "system exception");
}

static void coroutine (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_coException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 759, 9, "coroutine", "coroutine exception");
}

static void exception (void * a)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_exException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 771, 9, "exception", "exception exception");
}

static void Init (void)
{
  inException = FALSE;
  freeHandler = NULL;
  freeEHB = NULL;
  currentEHB = RTExceptions_InitExceptionBlock ();
  currentSource = NULL;
  RTExceptions_BaseExceptionsThrow ();
  SysExceptions_InitExceptionHandlers ((SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) indexf}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) range}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) casef}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) invalidloc}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) function}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) wholevalue}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) wholediv}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) realvalue}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) realdiv}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) complexvalue}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) complexdiv}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) protection}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) systemf}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) coroutine}, (SysExceptions_PROCEXCEPTION) {(SysExceptions_PROCEXCEPTION_t) exception});
}

static void TidyUp (void)
{
  Handler f;
  RTExceptions_EHBlock e;

  if (currentEHB != NULL)
    currentEHB = RTExceptions_KillExceptionBlock (currentEHB);
  while (freeHandler != NULL)
    {
      f = freeHandler;
      freeHandler = freeHandler->right;
      Storage_DEALLOCATE ((void **) &f, sizeof (handler));
    }
  while (freeEHB != NULL)
    {
      e = freeEHB;
      freeEHB = freeEHB->right;
      Storage_DEALLOCATE ((void **) &e, sizeof (ehblock));
    }
}

void RTExceptions_Raise (unsigned int number, void * file, unsigned int line, unsigned int column, void * function, void * message)
{
  unsigned int i;

  currentEHB->number = number;
  i = 0;
  addFile (file, &i);
  addChar (':', &i);
  addNum (line, &i);
  addChar (':', &i);
  addNum (column, &i);
  addChar (':', &i);
  addStr (message, &i);
  addChar (' ', &i);
  addChar ('i', &i);
  addChar ('n', &i);
  addChar (' ', &i);
  addStr (function, &i);
  addChar (ASCII_nl, &i);
  addChar (ASCII_nul, &i);
  InvokeHandler ();
}

void RTExceptions_SetExceptionBlock (RTExceptions_EHBlock source)
{
  currentEHB = source;
}

RTExceptions_EHBlock RTExceptions_GetExceptionBlock (void)
{
  return currentEHB;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

void * RTExceptions_GetTextBuffer (RTExceptions_EHBlock e)
{
  return &e->buffer;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

unsigned int RTExceptions_GetTextBufferSize (RTExceptions_EHBlock e)
{
  return sizeof (e->buffer);
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

unsigned int RTExceptions_GetNumber (RTExceptions_EHBlock source)
{
  return source->number;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

RTExceptions_EHBlock RTExceptions_InitExceptionBlock (void)
{
  RTExceptions_EHBlock e;

  e = New ();
  e->number = UINT_MAX;
  e->handlers = NewHandler ();
  e->handlers->right = e->handlers;
  e->handlers->left = e->handlers;
  e->right = e;
  return e;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

RTExceptions_EHBlock RTExceptions_KillExceptionBlock (RTExceptions_EHBlock e)
{
  e->handlers = KillHandlers (e->handlers);
  e->right = freeEHB;
  freeEHB = e;
  return NULL;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

void RTExceptions_PushHandler (RTExceptions_EHBlock e, unsigned int number, RTExceptions_ProcedureHandler p)
{
  Handler h;
  Handler i;

  h = findHandler (e, number);
  if (h == NULL)
    i = InitHandler (NewHandler (), (Handler) NULL, (Handler) NULL, (Handler) NULL, number, p);
  else
    {
      SubHandler (h);
      i = InitHandler (NewHandler (), (Handler) NULL, (Handler) NULL, h, number, p);
    }
  AddHandler (e, i);
}

void RTExceptions_PopHandler (RTExceptions_EHBlock e, unsigned int number)
{
  Handler h;
  Handler i;

  h = findHandler (e, number);
  if (h != NULL)
    {
      SubHandler (h);
      if (h->stack != NULL)
        AddHandler (e, h->stack);
      h = KillHandler (h);
    }
}

void RTExceptions_DefaultErrorCatch (void)
{
  RTExceptions_EHBlock e;
  int n;

  e = RTExceptions_GetExceptionBlock ();
  n = libc_write (2, RTExceptions_GetTextBuffer (e), libc_strlen (RTExceptions_GetTextBuffer (e)));
  M2RTS_HALT (0);
}

void RTExceptions_BaseExceptionsThrow (void)
{
  M2EXCEPTION_M2Exceptions i;

  for (i=M2EXCEPTION_indexException; i<=M2EXCEPTION_exException; i++)
    RTExceptions_PushHandler (RTExceptions_GetExceptionBlock (), (unsigned int ) (i), (RTExceptions_ProcedureHandler) {(RTExceptions_ProcedureHandler_t) DoThrow});
}

unsigned int RTExceptions_IsInExceptionState (void)
{
  return inException;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

unsigned int RTExceptions_SetExceptionState (unsigned int to)
{
  unsigned int old;

  old = inException;
  inException = to;
  return old;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

void RTExceptions_SwitchExceptionState (unsigned int *from, unsigned int to)
{
  (*from) = inException;
  inException = to;
}

RTExceptions_EHBlock RTExceptions_GetBaseExceptionBlock (void)
{
  if (currentEHB == NULL)
    M2RTS_Halt ((char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.mod", 49, 589, (char *) "GetBaseExceptionBlock", 21, (char *) "currentEHB has not been initialized yet", 39);
  else
    return currentEHB;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

void RTExceptions_SetExceptionSource (void * source)
{
  currentSource = source;
}

void * RTExceptions_GetExceptionSource (void)
{
  return currentSource;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def", 19, 0);
}

void _M2_RTExceptions_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  Init ();
}

void _M2_RTExceptions_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  TidyUp ();
}
