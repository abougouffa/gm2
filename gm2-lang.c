/* Language-dependent hooks for GNU Modula-2.
   Copyright 2002 Free Software Foundation, Inc.
   Contributed by Gaius Mulley <gaius@glam.ac.uk>

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "c-common.h"
#include "c-tree.h"
#include "toplev.h"
#include "langhooks.h"
#include "langhooks-def.h"

static HOST_WIDE_INT gm2_get_alias_set PARAMS ((tree));

extern const char *gm2_init		PARAMS ((const char *));
extern void gm2_init_options		PARAMS ((void));
extern int gm2_decode_option		PARAMS ((int, char **));
static HOST_WIDE_INT gm2_get_alias_set	PARAMS ((tree));
extern void gm2_print_decl		PARAMS ((FILE *, tree, int));
extern void gm2_print_type		PARAMS ((FILE *, tree, int));
extern void gm2_init_decl_processing	PARAMS ((void));

/* Structure giving our language-specific hooks.  */

#undef  LANG_HOOKS_NAME
#define LANG_HOOKS_NAME			"GNU Modula-2"
#undef  LANG_HOOKS_INIT
#define LANG_HOOKS_INIT			gm2_init
#if 0
#undef  LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS		gm2_init_options
#endif
#undef  LANG_HOOKS_DECODE_OPTION
#define LANG_HOOKS_DECODE_OPTION	gm2_decode_option
#undef LANG_HOOKS_HONOR_READONLY
#define LANG_HOOKS_HONOR_READONLY	1
#undef LANG_HOOKS_GET_ALIAS_SET
#define LANG_HOOKS_GET_ALIAS_SET	gm2_get_alias_set

const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* Return the typed-based alias set for T, which may be an expression
   or a type.  Return -1 if we don't do anything special.  */

static HOST_WIDE_INT
gm2_get_alias_set (t)
     tree t ATTRIBUTE_UNUSED;
{
  return -1;
}
