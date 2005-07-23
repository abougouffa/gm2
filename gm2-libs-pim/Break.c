#include <stdio.h>
#include <stdarg.h>
#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
  #include <stdlib.h>
#else
  #include <malloc.h>
#endif
#include "gm2-libs-host.h"

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
