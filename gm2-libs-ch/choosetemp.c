#include <libiberty.h>
#include "../gm2-libiberty/choosetemp.h"

/* Return a temporary file name (as a string) or NIL if unable to create
   one.  */

void *choosetemp_make_temp_file(void *suffix)
{
  return (void *)make_temp_file((const char *)suffix);
}

/* to satisfy the GM2 linker */
void
_M2_choosetemp_init (void) {}





