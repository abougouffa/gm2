/* Copyright (C) 2005, 2006 Free Software Foundation, Inc. */
/* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */
int *p, ch;


extern first func (void (*arrayfunc2)(int, char));

#define MYTYPE  void *

extern MYTYPE h;

#define LIMIT  1000
#define FOOBAR (1+MAXIMUM)



extern int numbers[FOOBAR];

typedef enum xyz {red, yellow, green} lights;

struct complex {
  enum xyz this;
  int *a[100];
  void (*arrayfunc[5])(int, char);
} myfirst;  


enum this {
  black,
  blue,
  green,
  red,
  yellow
} colors;

extern void foobarmumble (void);
extern myfunc (char *, int, void *);

struct mumble {
  char *name;
  void *addr;
  struct mumble *next;
  void (*arrayfunc[5])(int, char);
} *mylist;

extern struct mine {
  char *p[10], q;
  int x;
  int y;
} foobar;

struct mystruct {
  char *name;
  void *addr;
  struct mystruct *next;
} mystruct;

extern char *p, q;

extern char *strdup (char *p);
extern void (*arrayfunc[5])(int, char);
extern void (*myfunc)(int, char);
extern int *bar;
extern int *foo[100];
extern int matrix[4][4];
extern int (*statemachine[4][4])(int, int);

extern char myarray[];
extern char array[101];
extern int lastfunc (long int z);

extern void firstfunc (int x, float y);
extern int foo;


