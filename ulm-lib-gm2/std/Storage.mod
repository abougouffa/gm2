(* Ulm's Modula-2 Library
   Copyright (C) 1984-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either version
   2 of the License, or (at your option) any later version.

   Ulm's Modula-2 Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
   ----------------------------------------------------------------------------
   E-mail contact: gm2@glam.ac.uk
   ----------------------------------------------------------------------------
   $Id: Storage.mod,v 1.5 2005/11/21 21:50:38 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: Storage.mod,v $
   Revision 1.5  2005/11/21 21:50:38  gaius
   * fixed many Copyright dates and GPL, LGPL and FDL license
     issues.
   * modified gm2/ulm-lib-gm2/std/Storage.mod to use malloc and
     free. This in turn fixes a runtime regression test (hello world)
     now works with the Ulm libraries.
   * fixed gm2/gm2.texi to include FDL notice and also fixed all
     included texi files in the same way.
   * added GPL, Modula-2 and Copyright notices to all gm2/tools-src
     files.

   Revision 1.4  2005/11/21 12:09:59  gaius
   updated Copyright notices and dates

   Revision 1.3  2004/07/02 21:07:42  gaius
   fixed many IMPORT bugs in inner modules

   Revision 1.2  2004/06/29 08:51:42  gaius
   * made flex lexical analysers ignore carriage return
   * fixed bug in M2Quads.mod checking parameter of
     a const var before value was known.
   * fixed local MODULEs so that they can FROM mod IMPORT
   * tidied up some ulm implementation modules in ulm-lib-gm2/std

   Revision 1.1  2003/12/27 00:16:05  gaius
   added ulm libraries into the gm2 tree. Currently these
   are only used when regression testing, but later they
   will be accessible by users of gm2.

   Revision 0.2  1997/02/28  15:50:36  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:36  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)
IMPLEMENTATION MODULE Storage;

   FROM SYSTEM IMPORT WORD, ADDRESS, ADR, TSIZE;
   FROM libc IMPORT malloc, free ;
   FROM SysPanic IMPORT Panic;

   VAR
      Mode: (returnNIL, abort);

   PROCEDURE DEALLOCATE(VAR ptr: ADDRESS; size: CARDINAL);
   BEGIN
      free(ptr)
   END DEALLOCATE;

   PROCEDURE ALLOCATE(VAR ptr: ADDRESS; size: CARDINAL);
   BEGIN
      ptr := malloc(size);
      IF ptr=NIL
      THEN
         IF Mode#returnNIL
         THEN
            Panic("No space available.")
         END
      END
   END ALLOCATE;

   PROCEDURE Setmode(m: CARDINAL);
   BEGIN
      CASE m OF
        1: Mode := abort;
      | 2: Mode := returnNIL;
      ELSE
         (* nothing *)
      END;
   END Setmode;

BEGIN
   Mode := abort
END Storage.
