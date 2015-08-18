(* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                 2012, 2013, 2014, 2015
                 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA *)

IMPLEMENTATION MODULE SCmdArgs ;

FROM ASCII IMPORT cr, nul ;
FROM DynamicStrings IMPORT Length, Slice, char ;

CONST
   esc    = '\' ;
   space  = ' ' ;
   squote = "'" ;
   dquote = '"' ;
   tab    = ' ' ;


(*
   isWhite - 
*)

PROCEDURE isWhite (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch=space) OR (ch=tab)
END isWhite ;


(*
   skipWhite - 
*)

PROCEDURE skipWhite (s: String; i, e: INTEGER) : INTEGER ;
VAR
   ch: CHAR ;
BEGIN
   WHILE i<e DO
      ch := char(s, i) ;
      IF isWhite(ch)
      THEN
         INC(i)
      ELSE
         RETURN( i )
      END
   END ;
   RETURN( i )
END skipWhite ;


(*
   skipOverWhite - 
*)

PROCEDURE skipOverWhite (s: String; start, end: INTEGER) : INTEGER ;
BEGIN
   INC(start) ;
   WHILE (start<end) AND (NOT isWhite(char(s, start))) DO
      INC(start)
   END ;
   RETURN( start )
END skipOverWhite ;


(*
   skipOver - 
*)

PROCEDURE skipOver (s: String; start, end: INTEGER; ch: CHAR) : INTEGER ;
BEGIN
   INC(start) ;
   WHILE (start<end) AND (char(s, start)#ch) DO
      INC(start)
   END ;
   RETURN( start )
END skipOver ;


(*
   skipNextArg - 
*)

PROCEDURE skipNextArg (s: String; start, end: INTEGER) : INTEGER ;
VAR
   ch: CHAR ;
BEGIN
   IF start<end
   THEN
      ch := char(s, start) ;
      IF ch=dquote
      THEN
         end := skipOver(s, start, end, dquote)
      ELSIF ch=squote
      THEN
         end := skipOver(s, start, end, squote)
      ELSE
         end := skipOverWhite(s, start, end)
      END
   END ;
   RETURN( end )
END skipNextArg ;


(*
   GetArg - takes a command line and attempts to extract argument, n,
            from CmdLine. The resulting argument is placed into, a.
            The result of the operation is returned.
*)

PROCEDURE GetArg (CmdLine: String;
                  n: CARDINAL; VAR Argi: String) : BOOLEAN ;
VAR
   i         : CARDINAL ;
   sn,
   start, end: INTEGER ;
   ch        : CHAR ;
BEGIN
   i := 0 ;
   start := 0 ;
   end := Length(CmdLine) ;
   WHILE i<n DO
      start := skipWhite(CmdLine, start, end) ;
      sn := skipNextArg(CmdLine, start, end) ;
      IF sn<end
      THEN
         start := sn ;
         INC(i)
      ELSE
         RETURN( FALSE )
      END
   END ;
   start := skipWhite(CmdLine, start, end) ;
   sn := skipNextArg(CmdLine, start, end) ;
   Argi := Slice(CmdLine, start, sn) ;
   RETURN( TRUE )
END GetArg ;


(*
   Narg - returns the number of arguments available from
          command line, CmdLine.
*)

PROCEDURE Narg (CmdLine: String) : CARDINAL ;
VAR
   n         : CARDINAL ;
   s,
   start, end: INTEGER ;
BEGIN
   n := 0 ;
   start := 0 ;
   end := Length(CmdLine) ;
   LOOP
      start := skipWhite(CmdLine, start, end) ;
      s := skipNextArg(CmdLine, start, end) ;
      IF s<end
      THEN
         start := s ;
         INC(n)
      ELSE
         RETURN( n )
      END
   END
END Narg ;


PROCEDURE Escape (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( ch=esc )
END Escape ;


PROCEDURE Space (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch=space) OR (ch=tab) )
END Space ;


PROCEDURE DoubleQuote (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( ch=dquote )
END DoubleQuote ;


PROCEDURE SingleQuote (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( ch=squote )
END SingleQuote ;


END SCmdArgs.
