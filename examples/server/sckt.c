
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
} tcpState;


/*
 *  tcpServerEstablish - returns a tcpState containing the relevant
 *                       information about a socket declared to recieve
 *                       tcp connections.
 */

tcpState *tcpServerEstablish (void)
{
  tcpState *s = (tcpState *)malloc(sizeof(tcpState));
  int b, p, n;

  /* remove SIGPIPE which is raised on the server if the client is killed */
  signal(SIGPIPE, SIG_IGN);
  if (s == NULL)
    ERROR("no more memory");
  
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
    s->sa.sin_port        = htons(PORTSTART+p);
    
    b = bind(s->sockFd, (struct sockaddr *)&s->sa, sizeof(s->sa));
  } while ((b < 0) && (n<NOOFTRIES));

  if (b < 0)
    ERROR("bind");

  s->portNo = PORTSTART+p;
  printf("The receiving host is: %s, the port is %d\n", s->hostname, s->portNo);
  listen(s->sockFd, 1);
  return s;
}

/*
 *  tcpServerAccept - returns a file descriptor once a client has connected and
 *                    been accepted.
 */

int tcpServerAccept (tcpState *s)
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
 *  tcpPortNo - returns the portNo from structure, s.
 */

int tcpPortNo (tcpState *s)
{
  return s->portNo;
}

/*
 *  tcpSocketFd - returns the sockFd from structure, s.
 */

int tcpSocketFd (tcpState *s)
{
  return s->sockFd;
}
