(* test.mod a tiny test program writing 'a' reversed color to the screen.

Copyright (C) 2003-2019 Free Software Foundation, Inc.
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

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

MODULE test ;

FROM ncurses IMPORT WINDOW, chtype,
                    initscr, endwin, wclear, wrefresh, waddch, wmove ;
FROM SYSTEM IMPORT BITSET ;

CONST
   A_REVERSE = 10 ;

VAR
   w: WINDOW ;
   r: INTEGER ;
   ch: chtype ;
BEGIN
   w := initscr () ;
   r := wclear (w) ;
   r := wrefresh (w) ;
   r := wmove (w, 10, 10) ;
   ch := ORD ('a') ;
   ch := VAL (chtype, VAL (BITSET, ch) + BITSET {A_REVERSE+8}) ;
   r := waddch (w, ch) ;
   r := wrefresh (w) ;
   r := endwin ()
END test.
