#include <errno.h>

int errno_geterrno (void)
{
  return errno;
}

void _M2_errno_init (void) 
{
}

void _M2_errno_finish (void) 
{
}
