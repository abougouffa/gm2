/* Copyright (C) 2012
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


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "toplev.h"
#include "tm_p.h"
#include "flags.h"
#include <stdio.h>


/*
 *  utilize some of the C build routines
 */

#include "c-tree.h"
#include "rtl.h"
#include "function.h"
#include "expr.h"
#include "output.h"
#include "ggc.h"
#include "intl.h"
#include "convert.h"
#include "target.h"
#include "debug.h"
#include "diagnostic.h"
#include "except.h"
#include "libfuncs.h"
#include "../gm2-tree.h"
#include "tree-iterator.h"


#define m2misc_c
#include "m2tree.h"
#include "m2misc.h"
#include "m2block.h"


/*
 *  DebugTree - display the tree, t.
 */

void
m2misc_DebugTree (tree t)
{
  debug_tree(t);
}


/*
 *  DebugTree - display the tree, t.
 */


void
m2misc_DebugTreeChain (tree t)
{
  for (; (t != NULL); t = TREE_CHAIN (t))
    debug_tree(t);
}


/*
 *  DebugTree - display the tree, t.
 */



void
m2misc_printStmt (void)
{
  if (m2block_cur_stmt_list () != NULL)
    debug_tree (m2block_cur_stmt_list ());
}
