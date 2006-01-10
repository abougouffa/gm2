/* Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc. */
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

#include <stdio.h>
#include <stdarg.h>
#include "gm2-libs-host.h"

#if defined(HAVE_STDLIB_H)
  #include <stdlib.h>
#endif

#if defined(HAVE_MALLOC_H)
  #include <malloc.h>
#endif


typedef void (*PROC)(void);

#if defined(HAVE_SIGNAL_H)
#  include <signal.h>

struct plist {
  PROC          proc;
  struct plist *next;
};

static struct plist *head = NULL;


/*
 *  localHandler - dismisses the parameter, p, and invokes
 *                 the GNU Modula-2 handler.
 */

static void localHandler (int p)
{
  if (head != NULL)
    head->proc();
}

/*
 *  EnableBreak - enable the current break handler.
 */

void Break_EnableBreak (void)
{
  signal(SIGINT, localHandler);
}

/*
 *  DisableBreak - disable the current break handler (and all
 *                 installed handlers).
 */

void Break_DisableBreak (void)
{
  signal(SIGINT, SIG_IGN);
}

/*
 *  InstallBreak - installs a procedure, p, to be invoked when
 *                 a ctrl-c is caught. Any number of these
 *                 procedures may be stacked. Only the top
 *                 procedure is run when ctrl-c is caught.
 */

void Break_InstallBreak (PROC p)
{
  struct plist *q = (struct plist *)malloc(sizeof(struct plist));

  if (q == NULL) {
    perror("out of memory error in module Break");
    exit(1);
  }
  q->next = head;
  head = q;
  head->proc = p;
}

/*
 *  UnInstallBreak - pops the break handler stack.
 */

void Break_UnInstallBreak (void)
{
  struct plist *q = head;

  if (head != NULL) {
    head = head->next;
    free(q);
  }
}
#else
void Break_EnableBreak (void) {}
void Break_DisableBreak (void) {}
void Break_InstallBreak (PROC *p) {}
void Break_UnInstallBreak (void) {}
#endif
