/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/Indexing.mod.  */

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
#define _Indexing_H
#define _Indexing_C

#   include "Glibc.h"
#   include "GStorage.h"
#   include "GSYSTEM.h"
#   include "GmcDebug.h"
#   include "GM2RTS.h"

typedef struct Indexing_IndexProcedure_p Indexing_IndexProcedure;

#   define MinSize 128
typedef struct _T1_r _T1;

typedef void * *PtrToAddress;

typedef _T1 *Indexing_Index;

typedef unsigned char *PtrToByte;

typedef void (*Indexing_IndexProcedure_t) (void *);
struct Indexing_IndexProcedure_p { Indexing_IndexProcedure_t proc; };

struct _T1_r {
               void *ArrayStart;
               unsigned int ArraySize;
               unsigned int Used;
               unsigned int Low;
               unsigned int High;
               unsigned int Debug;
               unsigned int Map;
             };

Indexing_Index Indexing_InitIndex (unsigned int low);
Indexing_Index Indexing_KillIndex (Indexing_Index i);
Indexing_Index Indexing_DebugIndex (Indexing_Index i);
unsigned int Indexing_InBounds (Indexing_Index i, unsigned int n);
unsigned int Indexing_HighIndice (Indexing_Index i);
unsigned int Indexing_LowIndice (Indexing_Index i);
void Indexing_PutIndice (Indexing_Index i, unsigned int n, void * a);
void * Indexing_GetIndice (Indexing_Index i, unsigned int n);
unsigned int Indexing_IsIndiceInIndex (Indexing_Index i, void * a);
void Indexing_RemoveIndiceFromIndex (Indexing_Index i, void * a);
void Indexing_DeleteIndice (Indexing_Index i, unsigned int j);
void Indexing_IncludeIndiceIntoIndex (Indexing_Index i, void * a);
void Indexing_ForeachIndiceInIndexDo (Indexing_Index i, Indexing_IndexProcedure p);

Indexing_Index Indexing_InitIndex (unsigned int low)
{
  Indexing_Index i;

  Storage_ALLOCATE ((void **) &i, sizeof (_T1));
  i->Low = low;
  i->High = 0;
  i->ArraySize = MinSize;
  Storage_ALLOCATE (&i->ArrayStart, MinSize);
  i->ArrayStart = libc_memset (i->ArrayStart, 0, i->ArraySize);
  i->Debug = FALSE;
  i->Used = 0;
  i->Map = (unsigned int) 0;
  return i;
}

Indexing_Index Indexing_KillIndex (Indexing_Index i)
{
  Storage_DEALLOCATE (&i->ArrayStart, i->ArraySize);
  Storage_DEALLOCATE ((void **) &i, sizeof (_T1));
  return NULL;
}

Indexing_Index Indexing_DebugIndex (Indexing_Index i)
{
  i->Debug = TRUE;
  return i;
}

unsigned int Indexing_InBounds (Indexing_Index i, unsigned int n)
{
  if (i == NULL)
    M2RTS_HALT (0);
  else
    return (n >= i->Low) && (n <= i->High);
}

unsigned int Indexing_HighIndice (Indexing_Index i)
{
  if (i == NULL)
    M2RTS_HALT (0);
  else
    return i->High;
}

unsigned int Indexing_LowIndice (Indexing_Index i)
{
  if (i == NULL)
    M2RTS_HALT (0);
  else
    return i->Low;
}

void Indexing_PutIndice (Indexing_Index i, unsigned int n, void * a)
{
  unsigned int oldSize;
  void * b;
  unsigned int * * p;

  if (! (Indexing_InBounds (i, n)))
    {
      /* avoid gcc warning by using compound statement even if not strictly necessary.  */
      if (n < i->Low)
        M2RTS_HALT (0);
      else
        {
          oldSize = i->ArraySize;
          while (((n-i->Low)*(sizeof (void *))) >= i->ArraySize)
            i->ArraySize = i->ArraySize*2;
          if (oldSize != i->ArraySize)
            {
              Storage_REALLOCATE (&i->ArrayStart, i->ArraySize);
              b = i->ArrayStart;
              b += oldSize;
              b = libc_memset (b, 0, i->ArraySize-oldSize);
            }
          i->High = n;
        }
    }
  b = i->ArrayStart;
  b += (n-i->Low)*(sizeof (void *));
  p = b;
  (*p) = a;
  i->Used += 1;
  if (i->Debug)
    if (n < 32)
      i->Map |= (1 << (n ));
}

void * Indexing_GetIndice (Indexing_Index i, unsigned int n)
{
  PtrToByte b;
  PtrToAddress p;

  if (! (Indexing_InBounds (i, n)))
    M2RTS_HALT (0);
  b = i->ArrayStart;
  b += (n-i->Low)*(sizeof (void *));
  p = (PtrToAddress) (b);
  if (i->Debug)
    if (((n < 32) && (! ((((1 << (n)) & (i->Map)) != 0)))) && ((*p) != NULL))
      M2RTS_HALT (0);
  return (*p);
}

unsigned int Indexing_IsIndiceInIndex (Indexing_Index i, void * a)
{
  unsigned int j;
  PtrToByte b;
  PtrToAddress p;

  j = i->Low;
  b = i->ArrayStart;
  while (j <= i->High)
    {
      p = (PtrToAddress) (b);
      if ((*p) == a)
        return TRUE;
      b += sizeof (void *);
      j += 1;
    }
  return FALSE;
}

void Indexing_RemoveIndiceFromIndex (Indexing_Index i, void * a)
{
  unsigned int j;
  unsigned int k;
  PtrToAddress p;
  PtrToByte b;

  j = i->Low;
  b = i->ArrayStart;
  while (j <= i->High)
    {
      p = (PtrToAddress) (b);
      b += sizeof (void *);
      if ((*p) == a)
        Indexing_DeleteIndice (i, j);
      j += 1;
    }
}

void Indexing_DeleteIndice (Indexing_Index i, unsigned int j)
{
  PtrToAddress p;
  PtrToByte b;

  if (Indexing_InBounds (i, j))
    {
      b = i->ArrayStart;
      b += (sizeof (void *))*(j-i->Low);
      p = (PtrToAddress) (b);
      b += sizeof (void *);
      p = libc_memmove ((void *) p, (void *) b, (i->High-j)*(sizeof (void *)));
      i->High -= 1;
      i->Used -= 1;
    }
  else
    M2RTS_HALT (0);
}

void Indexing_IncludeIndiceIntoIndex (Indexing_Index i, void * a)
{
  if (! (Indexing_IsIndiceInIndex (i, a)))
    {
      /* avoid gcc warning by using compound statement even if not strictly necessary.  */
      if (i->Used == 0)
        Indexing_PutIndice (i, Indexing_LowIndice (i), a);
      else
        Indexing_PutIndice (i, (Indexing_HighIndice (i))+1, a);
    }
}

void Indexing_ForeachIndiceInIndexDo (Indexing_Index i, Indexing_IndexProcedure p)
{
  unsigned int j;
  Indexing_IndexProcedure q;

  j = Indexing_LowIndice (i);
  q = p;
  while (j <= (Indexing_HighIndice (i)))
    {
      mcDebug_assert (q.proc == p.proc);
      (*p.proc) (Indexing_GetIndice (i, j));
      j += 1;
    }
}

void _M2_Indexing_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_Indexing_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
