(* Display.mod provides a Logitech 3.0 compatible Display module.

Copyright (C) 2004-2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE Display ;

FROM FIO IMPORT StdOut, WriteChar ;
FROM ASCII IMPORT EOL, nl, bs, del ;


(*
   Write - display a character to the stdout.
           ASCII.EOL moves to the beginning of the next line.
           ASCII.del erases the character to the left of the cursor.
*)

PROCEDURE Write (ch: CHAR) ;
BEGIN
   CASE ch OF

   EOL:   WriteChar(StdOut, nl) |
   del:   WriteChar(StdOut, bs) ;
          WriteChar(StdOut, ' ') ;
          WriteChar(StdOut, bs)

   ELSE
      WriteChar(StdOut, ch)
   END
END Write ;


END Display.
