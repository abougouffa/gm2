/* Copyright (C) 2015
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
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "options.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "stor-layout.h"
#include "attribs.h"
#include "intl.h"
#include "tree-iterator.h"
#include "diagnostic.h"
#include "wide-int-print.h"
#include "real.h"
#include "float.h"

#include <stdio.h>

#if !defined(GM2TOOLS)
/*
 *  utilize some of the C build routines
 */

#include "fold-const.h"
#include "varasm.h"
#include "hashtab.h"
#include "hard-reg-set.h"
#include "function.h"

#include "hash-map.h"
#include "langhooks.h"
#include "timevar.h"
#include "dumpfile.h"
#include "target.h"
#include "dominance.h"
#include "cfg.h"
#include "cfganal.h"
#include "predict.h"
#include "basic-block.h"
#include "df.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-ssa.h"
#include "gimplify.h"
#include "stringpool.h"
#include "print-tree.h"
#include "except.h"
#include "toplev.h"
#include "convert.h"
#include "tree-dump.h"
#include "plugin-api.h"
#include "hard-reg-set.h"
#include "function.h"
#include "ipa-ref.h"
#include "cgraph.h"
#include "stmt.h"

#endif
