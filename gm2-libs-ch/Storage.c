/* Output from p2c, the Pascal-to-C translator */
/* From input file "Storage.md" */


#include <p2c/p2c.h>


#define StorageG
#include "GStorage.h"


#ifndef SysStorageH
#include "GSysStorage.h"
#endif


void Storage_ALLOCATE(void **a, unsigned int Size)
{
  SysStorage_ALLOCATE(a, Size);
}


void Storage_DEALLOCATE(void **a, unsigned int Size)
{
  SysStorage_DEALLOCATE(a, Size);
}


void Storage_REALLOCATE(void **a, unsigned int Size)
{
  SysStorage_REALLOCATE(a, Size);
}

BOOLEAN Storage_Available(unsigned int Size)
{
  return (SysStorage_Available(Size));
}


void _M2_Storage_init(void)
{
}
/* p2c: Note: Remember to call _M2_Storage_init() in main program [215] */



/* End. */
