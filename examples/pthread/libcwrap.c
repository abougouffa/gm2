#include <stdio.h>
#include <errno.h>

int libcwrap_errno (void)
{
  return errno;
}

void _M2_libcwrap_init (void)
{
}

