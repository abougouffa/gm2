/*
 *   ldtoa.c - this is a hack to allow the libraries to compile and link.
 *             It will work as long as long double precision is not required.
 *             Its only purpose is to allow development of the libraries and
 *             the implementation of LONGCOMPLEX to continue without having to
 *             implement ldtoa yet..
 */

#define GM2

#include <p2c/p2c.h>

#if defined(HAVE_STRINGS)
#  include <strings.h>
#endif

#if defined(HAVE_STRING)
#  include <string.h>
#endif


#if !defined(TRUE)
#  define TRUE (1==1)
#endif
#if !defined(FALSE)
#  define FALSE (1==0)
#endif

extern double dtoa_strtod (const char *s, int high, int *error);
extern char *dtoa_dtoa (double d, int mode, int ndigits, int *decpt, char *sign);
extern void dtoa_bfree (void *v);


long double ldtoa_strtold (const char *s, int high, int *error)
{
  return (long double)dtoa_strtod(s, high, error);
}

long double ldtoa_strtold_string (const char *s, int *error)
{
  int len = strlen(s);

  if (len>0)
    return dtoa_strtod(s, len-1, error);
  else
    *error = TRUE;
}

char *ldtoa_ldtoa (long double d, int mode, int ndigits, int *decpt, char *sign)
{
  return dtoa_dtoa((double)d, mode, ndigits, decpt, sign);
}

#if defined(GM2)
/*
 *  GNU Modula-2 hooks
 */

void _M2_ldtoa_init (void) {}
void _M2_ldtoa_finish (void) {}
#endif
