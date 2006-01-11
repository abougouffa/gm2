/* Copyright (C) 2005, 2006 Free Software Foundation, Inc. */
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
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <malloc.h>
#include <signal.h>

#define PORTSTART     7000
#define NOOFTRIES      100
#define MAXHOSTNAME    256

#if !defined(TRUE)
#  define TRUE  (1==1)
#endif
#if !defined(FALSE)
#  define FALSE (1==0)
#endif

#define ERROR(X)   { printf("%s:%d:%s - %s\n", __FILE__, __LINE__, \
                                X, strerror(errno)); \
                                exit(1); }

#define ASSERT(X)  { if (! (X)) { printf("%s:%d: assert(%s) failed\n", \
					 __FILE__, __LINE__, #X ); exit(1); }}


typedef struct {
  char                hostname[MAXHOSTNAME];
  struct hostent     *hp;
  struct sockaddr_in  sa, isa;
  int                 sockFd;
  int                 portNo;
} tcpServerState;


/*
 *  tcpServerEstablishPort - returns a tcpState containing the relevant
 *                           information about a socket declared to recieve
 *                           tcp connections. This method attempts to use
 *                           the port specified by the parameter.
 */

tcpServerState *tcpServerEstablishPort (int portNo)
{
  tcpServerState *s = (tcpServerState *)malloc(sizeof(tcpServerState));
  int b, p, n;

  if (s == NULL)
    ERROR("no more memory");

  /* remove SIGPIPE which is raised on the server if the client is killed */
  signal(SIGPIPE, SIG_IGN);
  
  if (gethostname(s->hostname, MAXHOSTNAME) < 0)
    ERROR("cannot find our hostname");

  s->hp = gethostbyname(s->hostname);
  if (s->hp == NULL)
    ERROR("cannot get host name");

  p = -1;
  n =  0;
  do {
    p++;
    /*
     * Open a TCP socket (an Internet stream socket)
     */
    
    s->sockFd = socket(s->hp->h_addrtype, SOCK_STREAM, 0);
    if (s->sockFd < 0)
      ERROR("socket");
    
    bzero((char *)&s->sa, sizeof(s->sa));
    ASSERT((s->hp->h_addrtype == AF_INET));
    s->sa.sin_family      = s->hp->h_addrtype;
    s->sa.sin_addr.s_addr = htonl(INADDR_ANY);
    s->sa.sin_port        = htons(portNo+p);
    
    b = bind(s->sockFd, (struct sockaddr *)&s->sa, sizeof(s->sa));
  } while ((b < 0) && (n<NOOFTRIES));

  if (b < 0)
    ERROR("bind");

  s->portNo = portNo+p;
  printf("The receiving host is: %s, the port is %d\n", s->hostname, s->portNo);
  listen(s->sockFd, 1);
  return s;
}


/*
 *  tcpServerEstablish - returns a tcpServerState containing the relevant
 *                       information about a socket declared to recieve
 *                       tcp connections.
 */

tcpServerState *tcpServerEstablish (void)
{
  return tcpServerEstablishPort (PORTSTART);
}

/*
 *  tcpServerAccept - returns a file descriptor once a client has connected and
 *                    been accepted.
 */

int tcpServerAccept (tcpServerState *s)
{
  int i = sizeof(s->isa);
  int t;

  printf("before accept\n");
  t = accept(s->sockFd, (struct sockaddr *)&s->isa, &i);
  if (t < 0)
    ERROR("accept");
  printf("sockFd = %d and accept returns %d\n", s->sockFd, t);
  return t;
}

/*
 *  tcpServerPortNo - returns the portNo from structure, s.
 */

int tcpServerPortNo (tcpServerState *s)
{
  return s->portNo;
}

/*
 *  tcpServerSocketFd - returns the sockFd from structure, s.
 */

int tcpServerSocketFd (tcpServerState *s)
{
  return s->sockFd;
}

/*
 *  tcpServerIP - returns the IP address from structure, s.
 */

int tcpServerIP (tcpServerState *s)
{
  return s->sa.sin_addr.s_addr;
}

/*
****************************************************************
***             C L I E N T     R O U T I N E S
****************************************************************
 */

typedef struct {
  char                hostname[MAXHOSTNAME];
  struct hostent     *hp;
  struct sockaddr_in  sa;
  int                 sockFd;
  int                 portNo;
} tcpClientState;


/*
 *  tcpClientSocket - returns a file descriptor (socket) which has
 *                    connected to, serverName:portNo.
 */

tcpClientState *tcpClientSocket (char *serverName, int portNo)
{
  tcpClientState *s = (tcpClientState *)malloc(sizeof(tcpClientState));

  if (s == NULL)
    ERROR("no more memory");

  /* remove SIGPIPE which is raised on the server if the client is killed */
  signal(SIGPIPE, SIG_IGN);

  s->hp = gethostbyname(serverName);
  if (s->hp == NULL) {
    fprintf(stderr, "cannot find host: %s\n", serverName);
    exit(1);
  }

  bzero((char *)&s->sa, sizeof(s->sa));
  s->sa.sin_family = AF_INET;
  bcopy((char *)s->hp->h_addr, (char *)&s->sa.sin_addr, s->hp->h_length);
  s->portNo        = portNo;
  s->sa.sin_port   = htons(portNo);

  /*
   * Open a TCP socket (an Internet stream socket)
   */

  s->sockFd = socket(s->hp->h_addrtype, SOCK_STREAM, 0);
  return s;
}

/*
 *  tcpClientConnect - returns the file descriptor associated with, s,
 *                     once a connect has been performed.
 */

int tcpClientConnect (tcpClientState *s)
{
  if (connect(s->sockFd, (struct sockaddr *)&s->sa, sizeof(s->sa)) < 0)
    ERROR("failed to connect to the TCP server");

  return s->sockFd;
}

/*
 *  tcpClientPortNo - returns the portNo from structure, s.
 */

int tcpClientPortNo (tcpClientState *s)
{
  return s->portNo;
}

/*
 *  tcpClientSocketFd - returns the sockFd from structure, s.
 */

int tcpClientSocketFd (tcpClientState *s)
{
  return s->sockFd;
}

/*
 *  tcpClientIP - returns the sockFd from structure, s.
 */

int tcpClientIP (tcpClientState *s)
{
  return s->sa.sin_addr.s_addr;
}
