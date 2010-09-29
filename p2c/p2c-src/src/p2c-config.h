/* Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
                 Free Software Foundation, Inc.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


#include "auto-host.h"
#include "ansidecl.h"
#include <stdio.h>

#if defined(HAVE_STDLIB_H)
#  include <stdlib.h>
#endif

#if defined(HAVE_UNISTD_H)
#  include <unistd.h>
#endif

#if defined(HAVE_STRINGS_H)
#  include <strings.h>
#endif

#if defined(HAVE_STRING_H)
#  include <string.h>
#endif

#if defined(HAVE_TIME_H)
#  include <time.h>
#endif


#ifdef FILE       /* a #define in BSD, a typedef in SYSV (hp-ux, at least) */
# ifndef BSD
#  define BSD 1
# endif
#endif



