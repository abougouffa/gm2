(* Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
                 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

This file was originally part of the University of Ulm library
*)


(* Ulm's Modula-2 Library
   Copyright (C) 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991,
   1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003, 2004, 2005
   by University of Ulm, SAI, D-89069 Ulm, Germany
*)

IMPLEMENTATION MODULE StrSpec;		(* gsk 1/85 *)

   (* bug in StrPos fixed: afb 5/86 *)

   FROM SYSTEM  IMPORT SIZE ;
   FROM ASCII   IMPORT nul;
   FROM Strings IMPORT StrLen, StrCat;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;


   CONST
      MaxIndice = MAX(INTEGER) ;  (* the maximum GCC can access is INTEGER sized arrays.  *)



   PROCEDURE StrPartCpy ( VAR target           : ARRAY OF CHAR;
			      source           : ARRAY OF CHAR;
			      position, number : CARDINAL );

      VAR
	index     : CARDINAL;
	sourceLen : CARDINAL;

   BEGIN
      IF number = 0 THEN
	 target [ 0 ] := nul;
      ELSE
	 sourceLen := StrLen ( source );
	 index := 0;
	 WHILE ( index < number ) AND ( position < sourceLen )
		AND ( index <= HIGH ( target ) )
		AND ( source [ position ] <> nul ) DO
	    target [ index ] := source [ position ];
	    INC ( index );
	    INC ( position )
	 END;
	 IF index <= HIGH ( target ) THEN
	    target [ index ] := nul
	 END
      END
   END StrPartCpy;


   PROCEDURE StrDel ( VAR target           : ARRAY OF CHAR ;
			  position, number : CARDINAL );

      VAR
	index     : CARDINAL;
	targetLen : CARDINAL;

   BEGIN
      targetLen := StrLen ( target );
      IF ( number > 0 ) AND ( position < targetLen ) THEN
	 index := position;
	 WHILE index + number < targetLen DO
	    target [ index ] := target [ index+number ];
	    INC ( index )
	 END;
	 target [ index ] := nul
      END
   END StrDel;


   PROCEDURE StrIns ( VAR target      : ARRAY OF CHAR;
			  insertion   : ARRAY OF CHAR;
			  position    : CARDINAL );

      VAR
	lauf, index : CARDINAL;
	StoreIt     : POINTER TO ARRAY [ 0 .. MaxIndice ] OF CHAR;
	insertLen,
	targetLen   : CARDINAL;

   BEGIN
      targetLen := StrLen ( target );
      IF position > targetLen THEN
	 StrCat ( target, insertion )
      ELSE
	 insertLen := StrLen ( insertion ) - 1;
	 ALLOCATE ( StoreIt, SIZE(target) );
	 StrPartCpy ( StoreIt^, target, position, targetLen-position );
	 index := position;
	 lauf := 0;
	 WHILE ( lauf <= insertLen ) AND ( index <= HIGH(target) ) DO
	    target [ index ] := insertion [ lauf ];
	    INC ( lauf );
	    INC ( index )
	 END;
	 lauf := 0;
	 WHILE ( StoreIt^ [ lauf ] <> nul ) AND ( index <= HIGH ( target ) ) DO
	    target [ index ] := StoreIt^ [ lauf ];
	    INC ( index );
	    INC ( lauf )
	 END;
	 IF index < HIGH ( target ) THEN
	    target [ index ] := nul
	 END;
	 DEALLOCATE ( StoreIt, SIZE(target) )
      END
   END StrIns;


   PROCEDURE StrPos ( source, search : ARRAY OF CHAR ) : CARDINAL;

      VAR
	lauf, index, position : CARDINAL;
	sourceLen, searchLen  : CARDINAL;
	diff : CARDINAL; (* |sourceLen - searchLen| *) (* rev afb *)

   BEGIN
      sourceLen := StrLen ( source ) - 1;
      searchLen := StrLen ( search ) - 1;
      IF ( search[0] = nul ) OR ( source[0] = nul ) THEN
	 RETURN HIGH ( source ) + 1
      ELSE
	 index := 0;
	 IF sourceLen > searchLen THEN (* rev afb *)
	    diff := sourceLen - searchLen;
	 ELSE
	    diff := searchLen - sourceLen;
	 END;
	 WHILE index <= diff DO (* rev afb *)
	    WHILE ( index <= diff ) AND ( source [ index ] <> search [ 0 ] ) DO
	       INC ( index )
	    END;
	    position := index;
	    lauf := 0;
	    WHILE  ( index <= sourceLen )
		AND ( ( lauf <= searchLen )
		OR ( ( lauf <= HIGH(search) ) AND ( search [ lauf ] <> nul ) ) )
		AND ( source [ index ] = search [ lauf ] )
		AND ( source [ index ] <> nul ) DO
	       INC ( lauf );
	       INC ( index )
	    END;
	    IF ( lauf > searchLen ) OR ( ( lauf = searchLen )
					AND ( index <= sourceLen )
					AND ( source[index] = search[lauf] ) )
				    THEN
	       RETURN position
	    END;
	    index := position + 1
	 END;
	 RETURN HIGH ( source ) + 1
      END
   END StrPos;


END StrSpec.
