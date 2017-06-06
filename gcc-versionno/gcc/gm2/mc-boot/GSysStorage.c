/* automatically created by mc from ../../gcc-versionno/gcc/gm2/gm2-libs/SysStorage.mod.  */

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
#define _SysStorage_H
#define _SysStorage_C

#   include "Glibc.h"
#   include "GDebug.h"

void SysStorage_ALLOCATE (void * *a, unsigned int Size);
void SysStorage_DEALLOCATE (void * *a, unsigned int Size);

/*
   REALLOCATE - attempts to reallocate storage. The address,
                a, should either be NIL in which case ALLOCATE
                is called, or alternatively it should have already
                been initialized by ALLOCATE. The allocated storage
                is resized accordingly.
*/

void SysStorage_REALLOCATE (void * *a, unsigned int Size);

/*
   REALLOCATE - attempts to reallocate storage. The address,
                a, should either be NIL in which case ALLOCATE
                is called, or alternatively it should have already
                been initialized by ALLOCATE. The allocated storage
                is resized accordingly.
*/

unsigned int SysStorage_Available (unsigned int Size);

/*
   Init - 
*/

void SysStorage_Init (void);

void SysStorage_ALLOCATE (void * *a, unsigned int Size)
{
  (*a) = libc_malloc (Size);
  if ((*a) == NULL)
    Debug_Halt ((char *) "out of memory error", 19, 31, (char *) "../../gcc-versionno/gcc/gm2/gm2-libs/SysStorage.mod", 51);
}

void SysStorage_DEALLOCATE (void * *a, unsigned int Size)
{
  libc_free ((*a));
  (*a) = NULL;
}


/*
   REALLOCATE - attempts to reallocate storage. The address,
                a, should either be NIL in which case ALLOCATE
                is called, or alternatively it should have already
                been initialized by ALLOCATE. The allocated storage
                is resized accordingly.
*/

void SysStorage_REALLOCATE (void * *a, unsigned int Size)
{
  if ((*a) == NULL)
    SysStorage_ALLOCATE (a, Size);
  else
    {
      (*a) = libc_realloc ((*a), Size);
      if ((*a) == NULL)
        Debug_Halt ((char *) "out of memory error", 19, 60, (char *) "../../gcc-versionno/gcc/gm2/gm2-libs/SysStorage.mod", 51);
    }
}


/*
   REALLOCATE - attempts to reallocate storage. The address,
                a, should either be NIL in which case ALLOCATE
                is called, or alternatively it should have already
                been initialized by ALLOCATE. The allocated storage
                is resized accordingly.
*/

unsigned int SysStorage_Available (unsigned int Size)
{
  void * a;

  a = libc_malloc (Size);
  if (a == NULL)
    return FALSE;
  else
    {
      libc_free (a);
      return TRUE;
    }
}


/*
   Init - 
*/

void SysStorage_Init (void)
{
}

void _M2_SysStorage_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_SysStorage_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
