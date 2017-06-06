/* automatically created by mc from ../../gcc-versionno/gcc/gm2/gm2-libs/MemUtils.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#define _MemUtils_H
#define _MemUtils_C

#   include "GSYSTEM.h"


/*
   MemCopy - copys a region of memory to the required destination.
*/

void MemUtils_MemCopy (void * from, unsigned int length, void * to);

/*
   MemZero - sets a region of memory: a..a+length to zero.
*/

void MemUtils_MemZero (void * a, unsigned int length);


/*
   MemCopy - copys a region of memory to the required destination.
*/

void MemUtils_MemCopy (void * from, unsigned int length, void * to)
{
  unsigned int * pwb;
  unsigned int * pwa;
  unsigned char * pbb;
  unsigned char * pba;

  while (length >= (sizeof (unsigned int)))
    {
      pwa = from;
      pwb = to;
      (*pwb) = (*pwa);
      from += sizeof (unsigned int);
      to += sizeof (unsigned int);
      length -= sizeof (unsigned int);
    }
  while (length > 0)
    {
      pba = from;
      pbb = to;
      (*pbb) = (*pba);
      from += sizeof (unsigned char);
      to += sizeof (unsigned char);
      length -= sizeof (unsigned char);
    }
}


/*
   MemZero - sets a region of memory: a..a+length to zero.
*/

void MemUtils_MemZero (void * a, unsigned int length)
{
  unsigned int * pwa;
  unsigned char * pba;

  pwa = a;
  while (length >= (sizeof (unsigned int)))
    {
      (*pwa) = (unsigned int ) (0);
      pwa += sizeof (unsigned int);
      length -= sizeof (unsigned int);
    }
  pba = (void *) (pwa);
  while (length >= (sizeof (unsigned char)))
    {
      (*pba) = (unsigned char ) (0);
      pba += sizeof (unsigned char);
      length -= sizeof (unsigned char);
    }
}

void _M2_MemUtils_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_MemUtils_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
