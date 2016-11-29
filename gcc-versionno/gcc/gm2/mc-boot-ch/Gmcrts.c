#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

void CaseException (char *s, unsigned int high, unsigned int lineno)
{
  fprintf (stderr, "%s:%d:case statement has no matching selection\n", s, lineno);
  exit (1);
}

void throw (int n)
{
  fprintf (stderr, "throw called (%d)\n", n);
  exit (1);
}
