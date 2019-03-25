/* getopt.c provide access to the C getopt library.

Copyright (C) 2017-2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  */

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

int
getopt_getopt_long (int argc, char *argv[], char *optstring, void *longopts,
                    int *longindex)
{
  int r = getopt_long (argc, argv, optstring, longopts, longindex);

  getopt_optarg = optarg;
  getopt_optind = optind;
  getopt_opterr = opterr;
  getopt_optopt = optopt;

  return r;
}

int
getopt_getopt_long_only (int argc, char *argv[], char *optstring,
                         void *longopts, int *longindex)
{
  int r = getopt_long_only (argc, argv, optstring, longopts, longindex);

  getopt_optarg = optarg;
  getopt_optind = optind;
  getopt_opterr = opterr;
  getopt_optopt = optopt;

  return r;
}

/* GNU Modula-2 linking fodder.  */

void
_M2_getopt_init (void)
{
}

void
_M2_getopt_finish (void)
{
}
