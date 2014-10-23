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

#if !defined(m2decl_h)

#   define m2decl_h
#   if defined(m2decl_c)
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN 
#      endif
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif


EXTERN tree m2decl_GetDeclContext (tree t);
EXTERN tree m2decl_BuildStringConstant (const char *string, int length);
EXTERN tree m2decl_BuildConstLiteralNumber (const char *str, unsigned int base);
EXTERN void m2decl_DetermineSizeOfConstant (const char *str, unsigned int base,
					    int *needsLong, int *needsUnsigned);
EXTERN void m2decl_RememberVariables (tree l);
            
EXTERN tree m2decl_BuildEndFunctionDeclaration (location_t location_begin, location_t location_end,
						const char *name, tree returntype,
						int isexternal, int isnested, int ispublic);
EXTERN void m2decl_BuildStartFunctionDeclaration (int uses_varargs);
EXTERN tree m2decl_BuildParameterDeclaration (location_t location, char *name, tree type,
					      int isreference);
EXTERN tree m2decl_DeclareKnownConstant (location_t location, tree type, tree value);
EXTERN tree m2decl_DeclareKnownVariable (location_t location, char *name, tree type, int exported,
					 int imported, int istemporary,
					 int isglobal, tree scope);

EXTERN tree m2decl_BuildCharConstant (char *string);
EXTERN tree m2decl_BuildStringConstantType (int length, const char *string, tree type);
EXTERN tree m2decl_BuildIntegerConstant (int value);

EXTERN int m2decl_GetBitsPerWord (void);
EXTERN int m2decl_GetBitsPerUnit (void);
EXTERN int m2decl_GetBitsPerInt (void);
EXTERN int m2decl_GetBitsPerBitset (void);

#  undef EXTERN
#endif
