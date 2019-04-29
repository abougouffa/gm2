/* top.mod main top level program module for mc.

Copyright (C) 2015-2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GmcOptions.h"
#   include "GmcComp.h"
#   include "GM2RTS.h"


/*
   init - translate the source file after handling all the
          program arguments.
*/

static void init (void);

/*
   init - translate the source file after handling all the
          program arguments.
*/

static void init (void);


/*
   init - translate the source file after handling all the
          program arguments.
*/

static void init (void)
{
  M2RTS_ExitOnHalt (1);
  mcComp_compile (mcOptions_handleOptions ());
}

void _M2_top_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  init ();
}

void _M2_top_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
