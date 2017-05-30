/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/alists.mod.  */

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
#define _alists_H
#define _alists_C

#   include "GStorage.h"

typedef struct alists_performOperation_p alists_performOperation;

#   define MaxnoOfelements 5
typedef struct _T1_r _T1;

typedef struct _T2_a _T2;

typedef _T1 *alists_alist;

typedef void (*alists_performOperation_t) (void *);
struct alists_performOperation_p { alists_performOperation_t proc; };

struct _T2_a { void * array[MaxnoOfelements-1+1]; };
struct _T1_r {
               unsigned int noOfelements;
               _T2 elements;
               alists_alist next;
             };

alists_alist alists_initList (void);
void alists_killList (alists_alist *l);
void alists_putItemIntoList (alists_alist l, void * c);
void * alists_getItemFromList (alists_alist l, unsigned int n);
unsigned int alists_getIndexOfList (alists_alist l, void * c);
unsigned int alists_noOfItemsInList (alists_alist l);
void alists_includeItemIntoList (alists_alist l, void * c);
void alists_removeItemFromList (alists_alist l, void * c);
unsigned int alists_isItemInList (alists_alist l, void * c);
void alists_foreachItemInListDo (alists_alist l, alists_performOperation p);
alists_alist alists_duplicateList (alists_alist l);
static void removeItem (alists_alist p, alists_alist l, unsigned int i);

static void removeItem (alists_alist p, alists_alist l, unsigned int i)
{
  l->noOfelements -= 1;
  while (i <= l->noOfelements)
    {
      l->elements.array[i-1] = l->elements.array[i+1-1];
      i += 1;
    }
  if ((l->noOfelements == 0) && (p != NULL))
    {
      p->next = l->next;
      Storage_DEALLOCATE ((void **) &l, sizeof (_T1));
    }
}

alists_alist alists_initList (void)
{
  alists_alist l;

  Storage_ALLOCATE ((void **) &l, sizeof (_T1));
  l->noOfelements = 0;
  l->next = NULL;
  return l;
}

void alists_killList (alists_alist *l)
{
  if ((*l) != NULL)
    {
      if ((*l)->next != NULL)
        alists_killList (&(*l)->next);
      Storage_DEALLOCATE ((void **) &(*l), sizeof (_T1));
    }
}

void alists_putItemIntoList (alists_alist l, void * c)
{
  if (l->noOfelements < MaxnoOfelements)
    {
      l->noOfelements += 1;
      l->elements.array[l->noOfelements-1] = c;
    }
}

void * alists_getItemFromList (alists_alist l, unsigned int n)
{
  while (l != NULL)
    {
      if (n <= l->noOfelements)
        return l->elements.array[n-1];
      else
        n -= l->noOfelements;
      l = l->next;
    }
  return 0;
}

unsigned int alists_getIndexOfList (alists_alist l, void * c)
{
  unsigned int i;

  if (l == NULL)
    return 0;
  else
    {
      i = 1;
      while (i <= l->noOfelements)
        if (l->elements.array[i-1] == c)
          return i;
        else
          i += 1;
      return l->noOfelements+(alists_getIndexOfList (l->next, c));
    }
}

unsigned int alists_noOfItemsInList (alists_alist l)
{
  unsigned int t;

  if (l == NULL)
    return 0;
  else
    {
      t = 0;
      do {
        t += l->noOfelements;
        l = l->next;
      } while (! (l == NULL));
      return t;
    }
}

void alists_includeItemIntoList (alists_alist l, void * c)
{
  if (alists_isItemInList (l, c))
    alists_putItemIntoList (l, c);
}

void alists_removeItemFromList (alists_alist l, void * c)
{
  alists_alist p;
  unsigned int i;
  unsigned int found;

  if (l != NULL)
    {
      found = FALSE;
      p = NULL;
      do {
        i = 1;
        while ((i <= l->noOfelements) && (l->elements.array[i-1] != c))
          i += 1;
        if ((i <= l->noOfelements) && (l->elements.array[i-1] == c))
          found = TRUE;
        else
          {
            p = l;
            l = l->next;
          }
      } while (! ((l == NULL) || found));
      if (found)
        removeItem (p, l, i);
    }
}

unsigned int alists_isItemInList (alists_alist l, void * c)
{
  unsigned int i;

  do {
    i = 1;
    while (i <= l->noOfelements)
      if (l->elements.array[i-1] == c)
        return TRUE;
      else
        i += 1;
    l = l->next;
  } while (! (l == NULL));
  return FALSE;
}

void alists_foreachItemInListDo (alists_alist l, alists_performOperation p)
{
  unsigned int i;
  unsigned int n;

  n = alists_noOfItemsInList (l);
  i = 1;
  while (i <= n)
    {
      (*p.proc) (alists_getItemFromList (l, i));
      i += 1;
    }
}

alists_alist alists_duplicateList (alists_alist l)
{
  alists_alist m;
  unsigned int n;
  unsigned int i;

  m = alists_initList ();
  n = alists_noOfItemsInList (l);
  i = 1;
  while (i <= n)
    {
      alists_putItemIntoList (m, alists_getItemFromList (l, i));
      i += 1;
    }
  return m;
}

void _M2_alists_init (int argc, char *argv[])
{
}
