#include <stdio.h>
#include <stdlib.h>

extern "C" void cpptry (void)
{
  throw (int)9;
}

