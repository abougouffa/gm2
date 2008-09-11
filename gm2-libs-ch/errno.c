#if defined(HAVE_SYS_ERRNO_H)
#  include <sys/errno.h>
#endif

#if defined(HAVE_ERRNO_H)
#  include <errno.h>
#endif

int errno_geterrno (void)
{
#if defined(HAVE_ERRNO_H) || defined(HAVE_SYS_ERRNO_H)
  return errno;
#else
  return -1;
#endif
}

void _M2_errno_init (void) 
{
}

void _M2_errno_finish (void) 
{
}
