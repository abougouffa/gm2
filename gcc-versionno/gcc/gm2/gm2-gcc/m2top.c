/* Copyright (C) 2012, 2013, 2014, 2015.
 * Free Software Foundation, Inc.
 *
 *  Gaius Mulley <gaius@glam.ac.uk> constructed this file.
 */

/*
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
along with GNU Modula-2; see the file COPYING.  If not, write to the
Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.
*/

#include "gcc-consolidation.h"

#include "../gm2-tree.h"
#include "../gm2-lang.h"

#include "m2decl.h"
#include "m2assert.h"
#include "m2expr.h"
#include "m2type.h"
#include "m2tree.h"
#include "m2block.h"
#define m2top_c
#include "m2top.h"


/*
 *  StartGlobalContext - initializes a dummy function for the global scope.
 */

void m2top_StartGlobalContext (void)
{
#if 0
  assert (cfun == NULL));
  init_dummy_function_start ();
#endif
}


/*
 *  EndGlobalContext - ends the dummy function for the global scope.
 */

void m2top_EndGlobalContext (void)
{
#if 0
  expand_dummy_function_end ();
#endif
}


/*
 *  FinishBackend - flushes all outstanding functions held in the GCC backend
 *                  out to the assembly file.
 */

void m2top_FinishBackend (void)
{
}


/*
 *  SetFlagUnitAtATime - sets GCC flag_unit_at_a_time to b.
 */

void m2top_SetFlagUnitAtATime (int b)
{
  flag_unit_at_a_time = b;
}
