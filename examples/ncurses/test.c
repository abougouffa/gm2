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
