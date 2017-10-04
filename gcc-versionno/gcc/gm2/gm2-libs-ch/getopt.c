
#include <unistd.h>

char *getopt_optarg;
int getopt_optind;
int getopt_opterr;
int getopt_optopt;


char
getopt_getopt (int argc, char *argv[], char *optstring)
{
  char r = getopt (argc, argv, optstring);

  getopt_optarg = optarg;
  getopt_optind = optind;
  getopt_opterr = opterr;
  getopt_optopt = optopt;

  if (r == (char)-1)
    return (char)0;
  return r;
}


/*
 *  GNU Modula-2 linking fodder.
 */

void _M2_getopt_init (void)
{
}


void _M2_getopt_finish (void)
{
}
