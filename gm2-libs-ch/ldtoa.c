/*
 *   ldtoa.c - 
 */

#define GM2

#include <p2c/p2c.h>

#include "gm2-libs-host.h"

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

#if defined(HAVE_STDLIB_H)
#  if !defined(_ISOC99_SOURCE)
#     define _ISOC99_SOURCE
#  endif
#  include <stdlib.h>
#endif

#define MAX_FP_DIGITS 500

typedef enum Mode { maxsignicant, decimaldigits } Mode;

/*
 *  maxsignicant:  return a string containing max(1,ndigits) significant
 *                 digits.  The return string contains the string produced
 *                 by ecvt.
 *  decimaldigits: return a string produced by fcvt.  The string will
 *                 contain ndigits past the decimal point
 *                 (ndigits may be negative).
 */



long double ldtoa_strtold (const char *s, int *error)
{
  char *endp;
  long double d;

#if defined(HAVE_STRTOLD)
  errno = 0;
  d = strtold (s, &endp);
#else
# error "you need to build on a system which can support strtold"
#endif
  if (endp != NULL && (*endp == '\0'))
    *error = (errno != 0);
  else
    *error = TRUE;
  return d;
}

char *ldtoa_ldtoa (long double d, int mode, int ndigits, int *decpt, int *sign)
{
  char *p;
  switch (mode) {

  case maxsignicant:
    p = malloc (ndigits+1);
    qecvt_r (d, ndigits, decpt, sign, p, ndigits+1);
    return p;
  case decimaldigits:
    p = malloc (MAX_FP_DIGITS);
    qfcvt_r (d, ndigits, decpt, sign, p, MAX_FP_DIGITS);
    return p;
  default:
    abort();
  }
}

#if defined(GM2)
/*
 *  GNU Modula-2 hooks
 */

void _M2_ldtoa_init (void) {}
void _M2_ldtoa_finish (void) {}
#endif
