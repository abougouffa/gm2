/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/Storage.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#define _Storage_H
#define _Storage_C

#   include "GSysStorage.h"

void Storage_ALLOCATE (void * *a, unsigned int Size);
void Storage_DEALLOCATE (void * *a, unsigned int Size);
void Storage_REALLOCATE (void * *a, unsigned int Size);
unsigned int Storage_Available (unsigned int Size);

void Storage_ALLOCATE (void * *a, unsigned int Size)
{
  SysStorage_ALLOCATE (a, Size);
}

void Storage_DEALLOCATE (void * *a, unsigned int Size)
{
  SysStorage_DEALLOCATE (a, Size);
}

void Storage_REALLOCATE (void * *a, unsigned int Size)
{
  SysStorage_REALLOCATE (a, Size);
}

unsigned int Storage_Available (unsigned int Size)
{
  return SysStorage_Available (Size);
}

void _M2_Storage_init (int argc, char *argv[])
{
}

void _M2_Storage_finish (int argc, char *argv[])
{
}
