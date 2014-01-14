/* Language-dependent hooks for GNU Modula-2.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
 *               Free Software Foundation, Inc.
   Contributed by Gaius Mulley <gaius@glam.ac.uk>

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#if !defined(GM2_LANG_H)
#  define GM2_LANG_H

#if defined(GM2_LANG_C)
#  define EXTERN
#else
#  define EXTERN extern
#endif
#include "config.h"
#include "system.h"
#include "ansidecl.h"
#include "coretypes.h"
#include "opts.h"
#include "tree.h"
#include "gimple.h"


/* Language-dependent contents of a type.  */

struct GTY(()) lang_type
{
  char dummy;
};

/* Language-dependent contents of a decl.  */

struct GTY(()) lang_decl
{
  char dummy;
};

/* Language-dependent contents of an identifier.  */

/* The limbo_value is used for block level extern declarations, which need
   to be type checked against subsequent extern declarations.  They can't
   be referenced after they fall out of scope, so they can't be global.

   The rid_code field is used for keywords.  It is in all
   lang_identifier nodes, because some keywords are only special in a
   particular context.  */

struct GTY(()) lang_identifier
{
  struct tree_identifier common;
};

/* The resulting tree type.  */

union GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
	   chain_next ("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN (&%h.generic)) : NULL")))
lang_tree_node
{
  union tree_node GTY((tag ("0"),
		       desc ("tree_node_structure (&%h)"))) generic;
  struct lang_identifier GTY((tag ("1"))) identifier;
};

/* Structure giving our language-specific hooks.  */

struct GTY(()) language_function
{
  /* While we are parsing the function, this contains information
     about the statement-tree that we are building.  */
/* struct stmt_tree_s stmt_tree; */
    tree stmt_tree;
};

EXTERN enum gimplify_status  gm2_gimplify_expr (tree *, tree *, tree *);
EXTERN bool gm2_mark_addressable (tree);
EXTERN tree gm2_type_for_size             (unsigned int bits, int unsignedp);
EXTERN tree gm2_type_for_mode             (enum machine_mode mode, int unsignedp);
EXTERN bool gm2_langhook_init (void);
EXTERN bool gm2_langhook_handle_option (size_t scode, const char *arg,
					int value,
					int kind ATTRIBUTE_UNUSED,
					location_t loc ATTRIBUTE_UNUSED,
					const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED);
EXTERN void gm2_langhook_init_options (unsigned int decoded_options_count,
				       struct cl_decoded_option *decoded_options);



#undef EXTERN
#endif
