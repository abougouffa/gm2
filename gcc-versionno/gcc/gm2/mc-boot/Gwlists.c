/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/wlists.mod.  */

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
#define _wlists_H
#define _wlists_C

#   include "GStorage.h"

typedef struct wlists_performOperation_p wlists_performOperation;

#   define maxNoOfElements 5
typedef struct _T1_r _T1;

typedef struct _T2_a _T2;

typedef _T1 *wlists_wlist;

typedef void (*wlists_performOperation_t) (unsigned int);
struct wlists_performOperation_p { wlists_performOperation_t proc; };

struct _T2_a { unsigned int array[maxNoOfElements-1+1]; };
struct _T1_r {
               unsigned int noOfElements;
               _T2 elements;
               wlists_wlist next;
             };


/*
   initList - creates a new wlist, l.
*/

wlists_wlist wlists_initList (void);

/*
   killList - deletes the complete wlist, l.
*/

void wlists_killList (wlists_wlist *l);

/*
   putItemIntoList - places an WORD, c, into wlist, l.
*/

void wlists_putItemIntoList (wlists_wlist l, unsigned int c);

/*
   getItemFromList - retrieves the nth WORD from wlist, l.
*/

unsigned int wlists_getItemFromList (wlists_wlist l, unsigned int n);

/*
   getIndexOfList - returns the index for WORD, c, in wlist, l.
                    If more than one WORD, c, exists the index
                    for the first is returned.
*/

unsigned int wlists_getIndexOfList (wlists_wlist l, unsigned int c);

/*
   noOfItemsInList - returns the number of items in wlist, l.
*/

unsigned int wlists_noOfItemsInList (wlists_wlist l);

/*
   includeItemIntoList - adds an WORD, c, into a wlist providing
                         the value does not already exist.
*/

void wlists_includeItemIntoList (wlists_wlist l, unsigned int c);

/*
   removeItemFromList - removes a WORD, c, from a wlist.
                        It assumes that this value only appears once.
*/

void wlists_removeItemFromList (wlists_wlist l, unsigned int c);

/*
   replaceItemInList - replace the nth WORD in wlist, l.
                       The first item in a wlists is at index, 1.
                       If the index, n, is out of range nothing is changed.
*/

void wlists_replaceItemInList (wlists_wlist l, unsigned int n, unsigned int w);

/*
   isItemInList - returns true if a WORD, c, was found in wlist, l.
*/

unsigned int wlists_isItemInList (wlists_wlist l, unsigned int c);

/*
   foreachItemInListDo - calls procedure, P, foreach item in wlist, l.
*/

void wlists_foreachItemInListDo (wlists_wlist l, wlists_performOperation p);

/*
   duplicateList - returns a duplicate wlist derived from, l.
*/

wlists_wlist wlists_duplicateList (wlists_wlist l);

/*
   removeItem - remove an element at index, i, from the wlist data type.
*/

static void removeItem (wlists_wlist p, wlists_wlist l, unsigned int i);


/*
   removeItem - remove an element at index, i, from the wlist data type.
*/

static void removeItem (wlists_wlist p, wlists_wlist l, unsigned int i)
{
  l->noOfElements -= 1;
  while (i <= l->noOfElements)
    {
      l->elements.array[i-1] = l->elements.array[i+1-1];
      i += 1;
    }
  if ((l->noOfElements == 0) && (p != NULL))
    {
      p->next = l->next;
      Storage_DEALLOCATE ((void **) &l, sizeof (_T1));
    }
}


/*
   initList - creates a new wlist, l.
*/

wlists_wlist wlists_initList (void)
{
  wlists_wlist l;

  Storage_ALLOCATE ((void **) &l, sizeof (_T1));
  l->noOfElements = 0;
  l->next = NULL;
  return l;
}


/*
   killList - deletes the complete wlist, l.
*/

void wlists_killList (wlists_wlist *l)
{
  if ((*l) != NULL)
    {
      if ((*l)->next != NULL)
        wlists_killList (&(*l)->next);
      Storage_DEALLOCATE ((void **) &(*l), sizeof (_T1));
    }
}


/*
   putItemIntoList - places an WORD, c, into wlist, l.
*/

void wlists_putItemIntoList (wlists_wlist l, unsigned int c)
{
  if (l->noOfElements < maxNoOfElements)
    {
      l->noOfElements += 1;
      l->elements.array[l->noOfElements-1] = c;
    }
  else if (l->next != NULL)
    wlists_putItemIntoList (l->next, c);
  else
    {
      l->next = wlists_initList ();
      wlists_putItemIntoList (l->next, c);
    }
}


/*
   getItemFromList - retrieves the nth WORD from wlist, l.
*/

unsigned int wlists_getItemFromList (wlists_wlist l, unsigned int n)
{
  while (l != NULL)
    {
      if (n <= l->noOfElements)
        return l->elements.array[n-1];
      else
        n -= l->noOfElements;
      l = l->next;
    }
  return 0;
}


/*
   getIndexOfList - returns the index for WORD, c, in wlist, l.
                    If more than one WORD, c, exists the index
                    for the first is returned.
*/

unsigned int wlists_getIndexOfList (wlists_wlist l, unsigned int c)
{
  unsigned int i;

  if (l == NULL)
    return 0;
  else
    {
      i = 1;
      while (i <= l->noOfElements)
        if (l->elements.array[i-1] == c)
          return i;
        else
          i += 1;
      return l->noOfElements+(wlists_getIndexOfList (l->next, c));
    }
}


/*
   noOfItemsInList - returns the number of items in wlist, l.
*/

unsigned int wlists_noOfItemsInList (wlists_wlist l)
{
  unsigned int t;

  if (l == NULL)
    return 0;
  else
    {
      t = 0;
      do {
        t += l->noOfElements;
        l = l->next;
      } while (! (l == NULL));
      return t;
    }
}


/*
   includeItemIntoList - adds an WORD, c, into a wlist providing
                         the value does not already exist.
*/

void wlists_includeItemIntoList (wlists_wlist l, unsigned int c)
{
  if (! (wlists_isItemInList (l, c)))
    wlists_putItemIntoList (l, c);
}


/*
   removeItemFromList - removes a WORD, c, from a wlist.
                        It assumes that this value only appears once.
*/

void wlists_removeItemFromList (wlists_wlist l, unsigned int c)
{
  wlists_wlist p;
  unsigned int i;
  unsigned int found;

  if (l != NULL)
    {
      found = FALSE;
      p = NULL;
      do {
        i = 1;
        while ((i <= l->noOfElements) && (l->elements.array[i-1] != c))
          i += 1;
        if ((i <= l->noOfElements) && (l->elements.array[i-1] == c))
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


/*
   replaceItemInList - replace the nth WORD in wlist, l.
                       The first item in a wlists is at index, 1.
                       If the index, n, is out of range nothing is changed.
*/

void wlists_replaceItemInList (wlists_wlist l, unsigned int n, unsigned int w)
{
  while (l != NULL)
    {
      if (n <= l->noOfElements)
        l->elements.array[n-1] = w;
      else
        n -= l->noOfElements;
      l = l->next;
    }
}


/*
   isItemInList - returns true if a WORD, c, was found in wlist, l.
*/

unsigned int wlists_isItemInList (wlists_wlist l, unsigned int c)
{
  unsigned int i;

  do {
    i = 1;
    while (i <= l->noOfElements)
      if (l->elements.array[i-1] == c)
        return TRUE;
      else
        i += 1;
    l = l->next;
  } while (! (l == NULL));
  return FALSE;
}


/*
   foreachItemInListDo - calls procedure, P, foreach item in wlist, l.
*/

void wlists_foreachItemInListDo (wlists_wlist l, wlists_performOperation p)
{
  unsigned int i;
  unsigned int n;

  n = wlists_noOfItemsInList (l);
  i = 1;
  while (i <= n)
    {
      (*p.proc) (wlists_getItemFromList (l, i));
      i += 1;
    }
}


/*
   duplicateList - returns a duplicate wlist derived from, l.
*/

wlists_wlist wlists_duplicateList (wlists_wlist l)
{
  wlists_wlist m;
  unsigned int n;
  unsigned int i;

  m = wlists_initList ();
  n = wlists_noOfItemsInList (l);
  i = 1;
  while (i <= n)
    {
      wlists_putItemIntoList (m, wlists_getItemFromList (l, i));
      i += 1;
    }
  return m;
}

void _M2_wlists_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_wlists_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}