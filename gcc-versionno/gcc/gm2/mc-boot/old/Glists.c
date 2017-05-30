/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/lists.mod.  */

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
#define _lists_H
#define _lists_C

#   include "GStorage.h"

typedef struct symbolKey_performOperation_p symbolKey_performOperation;

#   define MaxnoOfelements 5
typedef struct _T1_r _T1;

typedef struct _T2_a _T2;

typedef _T1 *lists_list;

typedef void (*symbolKey_performOperation_t) (void *);
struct symbolKey_performOperation_p { symbolKey_performOperation_t proc; };

struct _T2_a { void * array[MaxnoOfelements-1+1]; };
struct _T1_r {
               unsigned int noOfelements;
               _T2 elements;
               lists_list next;
             };

lists_list lists_initList (void);
void lists_killList (lists_list *l);
void lists_putItemIntoList (lists_list l, void * c);
void * lists_getItemFromList (lists_list l, unsigned int n);
unsigned int lists_getIndexOfList (lists_list l, void * c);
unsigned int lists_noOfItemsInList (lists_list l);
void lists_includeItemIntoList (lists_list l, void * c);
void lists_removeItemFromList (lists_list l, void * c);
unsigned int lists_isItemInList (lists_list l, void * c);
void lists_foreachItemInListDo (lists_list l, symbolKey_performOperation p);
lists_list lists_duplicateList (lists_list l);
static void removeItem (lists_list p, lists_list l, unsigned int i);

static void removeItem (lists_list p, lists_list l, unsigned int i)
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

lists_list lists_initList (void)
{
  lists_list l;

  Storage_ALLOCATE ((void **) &l, sizeof (_T1));
  l->noOfelements = 0;
  l->next = NULL;
  return l;
}

void lists_killList (lists_list *l)
{
  if ((*l) != NULL)
    {
      if ((*l)->next != NULL)
        lists_killList (&(*l)->next);
      Storage_DEALLOCATE ((void **) &(*l), sizeof (_T1));
    }
}

void lists_putItemIntoList (lists_list l, void * c)
{
  if (l->noOfelements < MaxnoOfelements)
    {
      l->noOfelements += 1;
      l->elements.array[l->noOfelements-1] = c;
    }
}

void * lists_getItemFromList (lists_list l, unsigned int n)
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

unsigned int lists_getIndexOfList (lists_list l, void * c)
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
      return l->noOfelements+(lists_getIndexOfList (l->next, c));
    }
}

unsigned int lists_noOfItemsInList (lists_list l)
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

void lists_includeItemIntoList (lists_list l, void * c)
{
  if (lists_isItemInList (l, c))
    lists_putItemIntoList (l, c);
}

void lists_removeItemFromList (lists_list l, void * c)
{
  lists_list p;
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

unsigned int lists_isItemInList (lists_list l, void * c)
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

void lists_foreachItemInListDo (lists_list l, symbolKey_performOperation p)
{
  unsigned int i;
  unsigned int n;

  n = lists_noOfItemsInList (l);
  i = 1;
  while (i <= n)
    {
      (*p.proc) (lists_getItemFromList (l, i));
      i += 1;
    }
}

lists_list lists_duplicateList (lists_list l)
{
  lists_list m;
  unsigned int n;
  unsigned int i;

  m = lists_initList ();
  n = lists_noOfItemsInList (l);
  i = 1;
  while (i <= n)
    {
      lists_putItemIntoList (m, lists_getItemFromList (l, i));
      i += 1;
    }
  return m;
}

void _M2_lists_init (int argc, char *argv[])
{
}
