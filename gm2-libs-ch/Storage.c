/* Output from p2c, the Pascal-to-C translator */
/* From input file "Storage.md" */


#include <p2c/p2c.h>


#define StorageG
#include "Storage.h"


#ifndef SysStorageH
#include "SysStorage.h"
#endif


void Storage_ALLOCATE(void **a, unsigned long Size)
{
  SysStorage_ALLOCATE(a, Size);
}


void Storage_DEALLOCATE(void **a, unsigned long Size)
{
  SysStorage_DEALLOCATE(a, Size);
}


BOOLEAN Storage_Available(unsigned long Size)
{
  return (SysStorage_Available(Size));
}


void _M2_Storage_init(void)
{
}
/* p2c: Note: Remember to call _M2_Storage_init() in main program [215] */



/* End. */
