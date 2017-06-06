/* automatically created by mc from ../../gcc-versionno/gcc/gm2/mc/varargs.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#include <stddef.h>
#include <string.h>
#include <limits.h>
#   include "GStorage.h"
#define _varargs_H
#define _varargs_C

#   include "GStorage.h"
#   include "Glibc.h"
#   include "GSYSTEM.h"
#   include "GM2RTS.h"

#   define MaxArg 4
typedef struct argDesc_r argDesc;

typedef struct _T1_r _T1;

typedef struct _T2_a _T2;

typedef _T1 *varargs_vararg;

struct argDesc_r {
                   void *ptr;
                   unsigned int len;
                 };

struct _T2_a { argDesc array[MaxArg+1]; };
struct _T1_r {
               unsigned int nArgs;
               unsigned int i;
               void *contents;
               unsigned int size;
               _T2 arg;
             };


/*
   nargs - returns the number of arguments wrapped in, v.
*/

unsigned int varargs_nargs (varargs_vararg v);

/*
   arg - fills in, a, with the next argument.  The size of, a, must be an exact
         match with the original vararg parameter.
*/

void varargs_arg (varargs_vararg v, unsigned char *a, unsigned int _a_high);

/*
   next - assigns the next arg to be collected as, i.
*/

void varargs_next (varargs_vararg v, unsigned int i);

/*
   copy - returns a copy of, v.
*/

varargs_vararg varargs_copy (varargs_vararg v);

/*
   replace - fills the next argument with, a.  The size of, a,
             must be an exact match with the original vararg
             parameter.
*/

void varargs_replace (varargs_vararg v, unsigned char *a, unsigned int _a_high);

/*
   end - destructor for vararg, v.
*/

void varargs_end (varargs_vararg *v);

/*
   start1 - wraps up argument, a, into a vararg.
*/

varargs_vararg varargs_start1 (unsigned char *a_, unsigned int _a_high);

/*
   start2 - wraps up arguments, a, b, into a vararg.
*/

varargs_vararg varargs_start2 (unsigned char *a_, unsigned int _a_high, unsigned char *b_, unsigned int _b_high);

/*
   start3 - wraps up arguments, a, b, c, into a vararg.
*/

varargs_vararg varargs_start3 (unsigned char *a_, unsigned int _a_high, unsigned char *b_, unsigned int _b_high, unsigned char *c_, unsigned int _c_high);

/*
   start4 - wraps up arguments, a, b, c, d, into a vararg.
*/

varargs_vararg varargs_start4 (unsigned char *a_, unsigned int _a_high, unsigned char *b_, unsigned int _b_high, unsigned char *c_, unsigned int _c_high, unsigned char *d_, unsigned int _d_high);


/*
   nargs - returns the number of arguments wrapped in, v.
*/

unsigned int varargs_nargs (varargs_vararg v)
{
  return v->nArgs;
}


/*
   arg - fills in, a, with the next argument.  The size of, a, must be an exact
         match with the original vararg parameter.
*/

void varargs_arg (varargs_vararg v, unsigned char *a, unsigned int _a_high)
{
  unsigned int j;
  unsigned char * p;

  if (v->i == v->nArgs)
    M2RTS_HALT (0);
  else
    {
      if (((_a_high)+1) == v->arg.array[v->i].len)
        {
          p = v->arg.array[v->i].ptr;
          j = 0;
          while (j <= (_a_high))
            {
              a[j] = (*p);
              p += 1;
              j += 1;
            }
        }
      else
        M2RTS_HALT (0);
      v->i += 1;
    }
}


/*
   next - assigns the next arg to be collected as, i.
*/

void varargs_next (varargs_vararg v, unsigned int i)
{
  v->i = i;
}


/*
   copy - returns a copy of, v.
*/

varargs_vararg varargs_copy (varargs_vararg v)
{
  varargs_vararg c;
  unsigned int j;
  unsigned int offset;

  Storage_ALLOCATE ((void **) &c, sizeof (_T1));
  c->i = v->i;
  c->nArgs = v->nArgs;
  c->size = v->size;
  Storage_ALLOCATE (&c->contents, c->size);
  c->contents = libc_memcpy (c->contents, v->contents, c->size);
  for (j=0; j<=c->nArgs; j++)
    {
      offset = v->contents-v->arg.array[j].ptr;
      c->arg.array[j].ptr = c->contents+offset;
      c->arg.array[j].len = v->arg.array[j].len;
    }
  return c;
}


/*
   replace - fills the next argument with, a.  The size of, a,
             must be an exact match with the original vararg
             parameter.
*/

void varargs_replace (varargs_vararg v, unsigned char *a, unsigned int _a_high)
{
  unsigned int j;
  unsigned char * p;

  if (v->i == v->nArgs)
    M2RTS_HALT (0);
  else
    if (((_a_high)+1) == v->arg.array[v->i].len)
      {
        p = v->arg.array[v->i].ptr;
        j = 0;
        while (j <= (_a_high))
          {
            (*p) = a[j];
            p += 1;
            j += 1;
          }
      }
    else
      M2RTS_HALT (0);
}


/*
   end - destructor for vararg, v.
*/

void varargs_end (varargs_vararg *v)
{
  if ((*v) != NULL)
    {
      Storage_DEALLOCATE (&(*v)->contents, (unsigned int) sizeof (varargs_vararg));
      Storage_DEALLOCATE ((void **) &(*v), sizeof (_T1));
    }
}


/*
   start1 - wraps up argument, a, into a vararg.
*/

varargs_vararg varargs_start1 (unsigned char *a_, unsigned int _a_high)
{
  varargs_vararg v;
  unsigned char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  Storage_ALLOCATE ((void **) &v, sizeof (_T1));
  v->i = 0;
  v->nArgs = 1;
  v->size = (_a_high)+1;
  Storage_ALLOCATE (&v->contents, v->size);
  v->contents = libc_memcpy (v->contents, &a, v->size);
  v->arg.array[0].ptr = v->contents;
  v->arg.array[0].len = v->size;
  return v;
}


/*
   start2 - wraps up arguments, a, b, into a vararg.
*/

varargs_vararg varargs_start2 (unsigned char *a_, unsigned int _a_high, unsigned char *b_, unsigned int _b_high)
{
  varargs_vararg v;
  unsigned char * p;
  unsigned char a[_a_high+1];
  unsigned char b[_b_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (b, b_, _b_high+1);

  Storage_ALLOCATE ((void **) &v, sizeof (_T1));
  v->i = 0;
  v->nArgs = 2;
  v->size = ((_a_high)+(_b_high))+2;
  Storage_ALLOCATE (&v->contents, v->size);
  p = libc_memcpy (v->contents, &a, (_a_high)+1);
  v->arg.array[0].ptr = p;
  v->arg.array[0].len = (_a_high)+1;
  p += v->arg.array[0].len;
  p = libc_memcpy ((void *) p, &b, (_b_high)+1);
  v->arg.array[1].ptr = p;
  v->arg.array[1].len = (_b_high)+1;
  return v;
}


/*
   start3 - wraps up arguments, a, b, c, into a vararg.
*/

varargs_vararg varargs_start3 (unsigned char *a_, unsigned int _a_high, unsigned char *b_, unsigned int _b_high, unsigned char *c_, unsigned int _c_high)
{
  varargs_vararg v;
  unsigned char * p;
  unsigned char a[_a_high+1];
  unsigned char b[_b_high+1];
  unsigned char c[_c_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (b, b_, _b_high+1);
  memcpy (c, c_, _c_high+1);

  Storage_ALLOCATE ((void **) &v, sizeof (_T1));
  v->i = 0;
  v->nArgs = 3;
  v->size = (((_a_high)+(_b_high))+(_c_high))+3;
  Storage_ALLOCATE (&v->contents, v->size);
  p = libc_memcpy (v->contents, &a, (_a_high)+1);
  v->arg.array[0].ptr = p;
  v->arg.array[0].len = (_a_high)+1;
  p += v->arg.array[0].len;
  p = libc_memcpy ((void *) p, &b, (_b_high)+1);
  v->arg.array[1].ptr = p;
  v->arg.array[1].len = (_b_high)+1;
  p += v->arg.array[1].len;
  p = libc_memcpy ((void *) p, &c, (_c_high)+1);
  v->arg.array[2].ptr = p;
  v->arg.array[2].len = (_c_high)+1;
  return v;
}


/*
   start4 - wraps up arguments, a, b, c, d, into a vararg.
*/

varargs_vararg varargs_start4 (unsigned char *a_, unsigned int _a_high, unsigned char *b_, unsigned int _b_high, unsigned char *c_, unsigned int _c_high, unsigned char *d_, unsigned int _d_high)
{
  varargs_vararg v;
  unsigned char * p;
  unsigned char a[_a_high+1];
  unsigned char b[_b_high+1];
  unsigned char c[_c_high+1];
  unsigned char d[_d_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (b, b_, _b_high+1);
  memcpy (c, c_, _c_high+1);
  memcpy (d, d_, _d_high+1);

  Storage_ALLOCATE ((void **) &v, sizeof (_T1));
  v->i = 0;
  v->nArgs = 4;
  v->size = ((((_a_high)+(_b_high))+(_c_high))+(_d_high))+4;
  Storage_ALLOCATE (&v->contents, v->size);
  p = libc_memcpy (v->contents, &a, (_a_high)+1);
  v->arg.array[0].len = (_a_high)+1;
  p += v->arg.array[0].len;
  p = libc_memcpy ((void *) p, &b, (_b_high)+1);
  v->arg.array[1].ptr = p;
  v->arg.array[1].len = (_b_high)+1;
  p += v->arg.array[1].len;
  p = libc_memcpy ((void *) p, &c, (_c_high)+1);
  v->arg.array[2].ptr = p;
  v->arg.array[2].len = (_c_high)+1;
  p += v->arg.array[2].len;
  p = libc_memcpy ((void *) p, &c, (_c_high)+1);
  v->arg.array[3].ptr = p;
  v->arg.array[3].len = (_c_high)+1;
  return v;
}

void _M2_varargs_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_varargs_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
