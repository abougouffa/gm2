/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcStack.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#include <stddef.h>
#   include "GStorage.h"
#define _mcStack_H
#define _mcStack_C

#   include "GStorage.h"
#   include "GIndexing.h"
#   include "GM2RTS.h"

typedef struct _T1_r _T1;

typedef _T1 *mcStack_stack;

struct _T1_r {
               Indexing_Index list;
               unsigned int count;
             };


/*
   init - create and return a stack.
*/

mcStack_stack mcStack_init (void);

/*
   kill - deletes stack, s.
*/

void mcStack_kill (mcStack_stack *s);

/*
   push - an address, a, onto the stack, s.
          It returns, a.
*/

void * mcStack_push (mcStack_stack s, void * a);

/*
   pop - and return the top element from stack, s.
*/

void * mcStack_pop (mcStack_stack s);

/*
   replace - performs a pop; push (a); return a.
*/

void * mcStack_replace (mcStack_stack s, void * a);

/*
   depth - returns the depth of the stack.
*/

unsigned int mcStack_depth (mcStack_stack s);

/*
   access - returns the, i, th stack element.
            The top of stack is defined by:

            access (s, depth (s)).
*/

void * mcStack_access (mcStack_stack s, unsigned int i);


/*
   init - create and return a stack.
*/

mcStack_stack mcStack_init (void)
{
  mcStack_stack s;

  Storage_ALLOCATE ((void **) &s, sizeof (_T1));
  s->list = Indexing_InitIndex (1);
  s->count = 0;
  return s;
}


/*
   kill - deletes stack, s.
*/

void mcStack_kill (mcStack_stack *s)
{
  (*s)->list = Indexing_KillIndex ((*s)->list);
  Storage_DEALLOCATE ((void **) &(*s), sizeof (_T1));
  (*s) = NULL;
}


/*
   push - an address, a, onto the stack, s.
          It returns, a.
*/

void * mcStack_push (mcStack_stack s, void * a)
{
  if (s->count == 0)
    Indexing_PutIndice (s->list, Indexing_LowIndice (s->list), a);
  else
    Indexing_PutIndice (s->list, (Indexing_HighIndice (s->list))+1, a);
  s->count += 1;
  return a;
}


/*
   pop - and return the top element from stack, s.
*/

void * mcStack_pop (mcStack_stack s)
{
  void * a;

  if (s->count == 0)
    M2RTS_HALT (0);
  else
    {
      s->count -= 1;
      a = Indexing_GetIndice (s->list, Indexing_HighIndice (s->list));
      Indexing_DeleteIndice (s->list, Indexing_HighIndice (s->list));
      return a;
    }
}


/*
   replace - performs a pop; push (a); return a.
*/

void * mcStack_replace (mcStack_stack s, void * a)
{
  void * b;

  b = mcStack_pop (s);
  return mcStack_push (s, a);
}


/*
   depth - returns the depth of the stack.
*/

unsigned int mcStack_depth (mcStack_stack s)
{
  return s->count;
}


/*
   access - returns the, i, th stack element.
            The top of stack is defined by:

            access (s, depth (s)).
*/

void * mcStack_access (mcStack_stack s, unsigned int i)
{
  if ((i > s->count) || (i == 0))
    M2RTS_HALT (0);
  else
    return Indexing_GetIndice (s->list, i);
}

void _M2_mcStack_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_mcStack_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}