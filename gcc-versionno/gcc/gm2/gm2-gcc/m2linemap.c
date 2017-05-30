/* Copyright (C) 2012, 2013, 2014, 2015
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

/*
 *  utilize some of the C build routines
 */

#include "../gm2-tree.h"
#include "../gm2-lang.h"

#include "m2decl.h"
#include "m2assert.h"
#include "m2expr.h"
#include "m2type.h"
#include "m2tree.h"
#include "m2block.h"
#define m2linemap_c
#include "m2linemap.h"


static int inFile = FALSE;

#if defined (__cplusplus)
#   define EXTERN extern "C"
#else
#   define EXTERN
#endif


/*
 *  Start getting locations from a new file.
 */

EXTERN
void
m2linemap_StartFile (void *filename, unsigned int linebegin)
{
  if (inFile)
    m2linemap_EndFile ();
  linemap_add (line_table, LC_ENTER, 0, xstrdup (reinterpret_cast<char *> (filename)), linebegin);
  inFile = TRUE;
}


/*
 *  Tell the line table the file has ended.
 */

EXTERN
void
m2linemap_EndFile (void)
{
  linemap_add (line_table, LC_LEAVE, 0, NULL, 0);
  inFile = FALSE;
}


/*
 *  Indicate that there is a new source file line number with a maximum
 *  width.
 */

EXTERN
void
m2linemap_StartLine (unsigned int linenumber, unsigned int linesize)
{
  linemap_line_start (line_table, linenumber, linesize);
}


/*
 *  GetLocationColumn - returns a location_t based on the current line
 *                      number and column.
 */

EXTERN
location_t
m2linemap_GetLocationColumn (unsigned int column)
{
  return linemap_position_for_column (line_table, column);
}


/*
 *  UnknownLocation - return the predefined location representing an unknown location.
 */

EXTERN
location_t
m2linemap_UnknownLocation (void)
{
  return UNKNOWN_LOCATION;
}

/*
 *  BuiltinsLocation - return the predefined location representing a builtin.
 */

EXTERN
location_t
m2linemap_BuiltinsLocation (void)
{
  return BUILTINS_LOCATION;
}
