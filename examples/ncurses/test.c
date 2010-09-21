/* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc. */
/* This file is part of GNU Modula-2.

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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA */
#include <ncurses.h>

#define ADD_CHAR(C,P)  ((P)*0x100+(C))

main()
{
  WINDOW *w = initscr() ;
  int     r = start_color();
  int     pair = 1;

  r = wclear(w) ;
  r = wrefresh(w) ;
  r = cbreak() ;
  r = noecho();
  r = nonl() ;
  r = wrefresh(w);

  r = wmove(w, 1, 1);
  r = wrefresh(w);
  pair++;
  r = init_pair(pair, 0, 7) ;
  r = waddch(w, ADD_CHAR('a', pair));
  r = wrefresh(w);


  r = wmove(w, 2, 2);
  r = wrefresh(w);
  pair++;
  r = init_pair(pair, 0, 7) ;
  r = waddch(w, ADD_CHAR('b', pair));
  r = wrefresh(w);

  while (1)
    ;
}
