/*
**  GNU Pth - The GNU Portable Threads
**  Copyright (c) 1999-2002 Ralf S. Engelschall <rse@engelschall.com>
**
**  This file is part of GNU Pth, a non-preemptive thread scheduling
**  library which can be found at http://www.gnu.org/software/pth/.
**
**  This library is free software; you can redistribute it and/or
**  modify it under the terms of the GNU Lesser General Public
**  License as published by the Free Software Foundation; either
**  version 2.1 of the License, or (at your option) any later version.
**
**  This library is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
**  Lesser General Public License for more details.
**
**  You should have received a copy of the GNU Lesser General Public
**  License along with this library; if not, write to the Free Software
**  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
**  USA, or contact Ralf S. Engelschall <rse@engelschall.com>.
**
**  pth.h: Pth public API definitions
*/
                             /* ``What you see is all you get.''
                                          -- Brian Kernighan      */
#ifndef _PTH_H_
#define _PTH_H_

    /* the library version */
#ifndef PTH_VERSION_STR
#define PTH_VERSION_STR "1.4.1 (27-Jan-2002)"
#endif
#ifndef PTH_VERSION_HEX
#define PTH_VERSION_HEX 0x104201
#endif
#ifndef PTH_VERSION
#define PTH_VERSION PTH_VERSION_HEX
#endif

    /* essential headers */
/* #include <sys/types.h>     /* for ssize_t, off_t */
#include <sys/time.h>      /* for struct timeval */
#if 0
/*  #include <sys/socket.h>    /* for sockaddr       */
/* #include <sys/signal.h>    /* for sigset_t       */
/* #include <sys/select.h> */
#endif
struct sockaddr;
typedef void *sigset_t;

    /* the user-space context structure */
typedef struct pth_uctx_st *pth_uctx_t;
struct pth_uctx_st;

    /* fallbacks for essential typedefs */
typedef int pid_t;
/* typedef unsigned int size_t; */
typedef unsigned int ssize_t;
typedef unsigned int socklen_t;
typedef int off_t;
typedef int sig_atomic_t;

typedef unsigned long int nfds_t;



    /* bitmask generation */
/* #define _BIT(n) (1<<(n)) */

    /* C++ support */
#ifdef __cplusplus
#define BEGIN_DECLARATION extern "C" {
#define END_DECLARATION   }
#else
#define BEGIN_DECLARATION /*nop*/
#define END_DECLARATION   /*nop*/
#endif

BEGIN_DECLARATION

    /* some global constants */
#define PTH_KEY_MAX                  256
#define PTH_ATFORK_MAX               128
#define PTH_DESTRUCTOR_ITERATIONS    4

    /* system call mapping support type (soft variant can be overridden) */
#define PTH_SYSCALL_HARD 0
#ifndef PTH_SYSCALL_SOFT
#define PTH_SYSCALL_SOFT 0
#endif

    /* the time value structure */
/* typedef struct timeval pth_time_t; */
typedef void * pth_time_t;

    /* the unique thread id/handle */

typedef void *pth_t;
/* typedef struct pth_st *pth_t; */
/* struct pth_st; */

    /* thread states */
typedef enum pth_state_en {
    PTH_STATE_SCHEDULER,             /* the special scheduler thread only       */
    PTH_STATE_NEW,                   /* spawned, but still not dispatched       */
    PTH_STATE_READY,                 /* ready, waiting to be dispatched         */
    PTH_STATE_WAITING,               /* suspended, waiting until event occurred */
    PTH_STATE_DEAD                   /* terminated, waiting to be joined        */
} pth_state_t;

    /* thread priority values */
#define PTH_PRIO_MAX                 +5
#define PTH_PRIO_STD                  0
#define PTH_PRIO_MIN                 -5

    /* the thread attribute structure */
typedef void *pth_attr_t;
/* typedef struct pth_attr_st *pth_attr_t; */
/* struct pth_attr_st; */

    /* attribute set/get commands for pth_attr_{get,set}() */

enum {
    PTH_ATTR_PRIO,           /* RW [int]               priority of thread                */
    PTH_ATTR_NAME,           /* RW [char *]            name of thread                    */
    PTH_ATTR_JOINABLE,       /* RW [int]               thread detachment type            */
    PTH_ATTR_CANCEL_STATE,   /* RW [unsigned int]      thread cancellation state         */
    PTH_ATTR_STACK_SIZE,     /* RW [unsigned int]      stack size                        */
    PTH_ATTR_STACK_ADDR,     /* RW [char *]            stack lower address               */
    PTH_ATTR_TIME_SPAWN,     /* RO [pth_time_t]        time thread was spawned           */
    PTH_ATTR_TIME_LAST,      /* RO [pth_time_t]        time thread was last dispatched   */
    PTH_ATTR_TIME_RAN,       /* RO [pth_time_t]        time thread was running           */
    PTH_ATTR_START_FUNC,     /* RO [void *(*)(void *)] thread start function             */
    PTH_ATTR_START_ARG,      /* RO [void *]            thread start argument             */
    PTH_ATTR_STATE,          /* RO [pth_state_t]       scheduling state                  */
    PTH_ATTR_EVENTS,         /* RO [pth_event_t]       events the thread is waiting for  */
    PTH_ATTR_BOUND           /* RO [int]               whether object is bound to thread */
};

    /* default thread attribute */
#define PTH_ATTR_DEFAULT (0)

    /* the event structure */
typedef void *pth_event_t;


    /* event deallocation types */
enum { PTH_FREE_THIS, PTH_FREE_ALL };

    /* event walking directions */

    /* the key type and init value */
typedef int pth_key_t;
#define PTH_KEY_INIT (-1)

    /* the once structure and init value */
typedef int pth_once_t;
#define PTH_ONCE_INIT FALSE

    /* general ring structure */
typedef struct pth_ringnode_st pth_ringnode_t;
struct pth_ringnode_st {
    pth_ringnode_t *rn_next;
    pth_ringnode_t *rn_prev;
};
typedef struct pth_ring_st pth_ring_t;
struct pth_ring_st {
    pth_ringnode_t *r_hook;
    unsigned int    r_nodes;
};


    /* the message port structure */
typedef void *pth_msgport_t;

    /* the message structure */
typedef struct pth_message_st pth_message_t;
struct pth_message_st { /* not hidden to allow inclusion */
    pth_ringnode_t m_node;
    pth_msgport_t  m_replyport;
    unsigned int   m_size;
    void          *m_data;
};

    /* the mutex structure */
typedef struct pth_mutex_st pth_mutex_t;
struct pth_mutex_st { /* not hidden to avoid destructor */
    pth_ringnode_t mx_node;
    int            mx_state;
    pth_t          mx_owner;
    unsigned long  mx_count;
};

    /* the read-write lock structure */
typedef struct pth_rwlock_st pth_rwlock_t;
struct pth_rwlock_st { /* not hidden to avoid destructor */
    int            rw_state;
    unsigned int   rw_mode;
    unsigned long  rw_readers;
    pth_mutex_t    rw_mutex_rd;
    pth_mutex_t    rw_mutex_rw;
};

    /* the condition variable structure */
typedef struct pth_cond_st pth_cond_t;
struct pth_cond_st { /* not hidden to avoid destructor */
    unsigned long cn_state;
    unsigned int  cn_waiters;
};

    /* the barrier variable structure */
typedef struct pth_barrier_st pth_barrier_t;
struct pth_barrier_st { /* not hidden to avoid destructor */
    unsigned long br_state;
    int           br_threshold;
    int           br_count;
    int           br_cycle;
    pth_cond_t    br_cond;
    pth_mutex_t   br_mutex;
};

/* fake a poll(2) environment */
#define POLLIN      0x0001      /* any readable data available   */
#define POLLPRI     0x0002      /* OOB/Urgent readable data      */
#define POLLOUT     0x0004      /* file descriptor is writeable  */
#define POLLERR     0x0008      /* some poll error occurred      */
#define POLLHUP     0x0010      /* file descriptor was "hung up" */
#define POLLNVAL    0x0020      /* requested events "invalid"    */
#define POLLRDNORM  POLLIN
#define POLLRDBAND  POLLIN
#define POLLWRNORM  POLLOUT
#define POLLWRBAND  POLLOUT
#define INFTIM      (-1)        /* poll infinite */


struct pollfd {
    int fd;                     /* which file descriptor to poll */
    short events;               /* events we are interested in   */
    short revents;              /* events found on return        */
};

#define PTH_FAKE_RWV 0
/* fake a readv(2)/writev(2) environment */
struct iovec {
    void  *iov_base;  /* memory base address */
    size_t iov_len;   /* memory chunk length */
};
#ifndef UIO_MAXIOV
#define UIO_MAXIOV 1024

    /* extension support */
#define PTH_EXT_SFIO 0

typedef void *Sfdisc_t;

    /* global functions */
extern int            pth_init(void);
extern int            pth_kill(void);
extern long           pth_ctrl(unsigned long, ...);
extern long           pth_version(void);

    /* thread attribute functions */
extern pth_attr_t     pth_attr_of(pth_t);
extern pth_attr_t     pth_attr_new(void);
extern int            pth_attr_init(pth_attr_t);
extern int            pth_attr_set(pth_attr_t, int, ...);
extern int            pth_attr_get(pth_attr_t, int, ...);
extern int            pth_attr_destroy(pth_attr_t);

    /* thread functions */
extern pth_t          pth_spawn(pth_attr_t, void *(*)(void *), void *);
extern int            pth_once(pth_once_t *, void (*)(void *), void *);
extern pth_t          pth_self(void);
extern int            pth_suspend(pth_t);
extern int            pth_resume(pth_t);
extern int            pth_yield(pth_t);
extern int            pth_nap(pth_time_t);
extern int            pth_wait(pth_event_t);
extern int            pth_cancel(pth_t);
extern int            pth_abort(pth_t);
extern int            pth_raise(pth_t, int);
extern int            pth_join(pth_t, void **);
extern void           pth_exit(void *);

    /* utility functions */
extern int            pth_fdmode(int, int);
extern pth_time_t     pth_time(long, long);
extern pth_time_t     pth_timeout(long, long);

    /* cancellation functions */
extern void           pth_cancel_state(int, int *);
extern void           pth_cancel_point(void);

    /* event functions */
extern pth_event_t    pth_event(unsigned long, ...);
extern unsigned long  pth_event_typeof(pth_event_t);
extern int            pth_event_extract(pth_event_t ev, ...);
extern pth_event_t    pth_event_concat(pth_event_t, ...);
extern pth_event_t    pth_event_isolate(pth_event_t);
extern pth_event_t    pth_event_walk(pth_event_t, unsigned int);
extern int            pth_event_occurred(pth_event_t);
extern int            pth_event_free(pth_event_t, int);

    /* key-based storage functions */
extern int            pth_key_create(pth_key_t *, void (*)(void *));
extern int            pth_key_delete(pth_key_t);
extern int            pth_key_setdata(pth_key_t, const void *);
extern void          *pth_key_getdata(pth_key_t);

    /* message port functions */
extern pth_msgport_t  pth_msgport_create(const char *);
extern void           pth_msgport_destroy(pth_msgport_t);
extern pth_msgport_t  pth_msgport_find(const char *);
extern int            pth_msgport_pending(pth_msgport_t);
extern int            pth_msgport_put(pth_msgport_t, pth_message_t *);
extern pth_message_t *pth_msgport_get(pth_msgport_t);
extern int            pth_msgport_reply(pth_message_t *);

    /* cleanup handler functions */
extern int            pth_cleanup_push(void (*)(void *), void *);
extern int            pth_cleanup_pop(int);

    /* process forking functions */
extern int            pth_atfork_push(void (*)(void *), void (*)(void *), void (*)(void *), void *);
extern int            pth_atfork_pop(void);
extern pid_t          pth_fork(void);

    /* synchronization functions */
extern int            pth_mutex_init(pth_mutex_t *);
extern int            pth_mutex_acquire(pth_mutex_t *, int, pth_event_t);
extern int            pth_mutex_release(pth_mutex_t *);
extern int            pth_rwlock_init(pth_rwlock_t *);
extern int            pth_rwlock_acquire(pth_rwlock_t *, int, int, pth_event_t);
extern int            pth_rwlock_release(pth_rwlock_t *);
extern int            pth_cond_init(pth_cond_t *);
extern int            pth_cond_await(pth_cond_t *, pth_mutex_t *, pth_event_t);
extern int            pth_cond_notify(pth_cond_t *, int);
extern int            pth_barrier_init(pth_barrier_t *, int);
extern int            pth_barrier_reach(pth_barrier_t *);

    /* user-space context functions */
extern int            pth_uctx_create(pth_uctx_t *);
extern int            pth_uctx_make(pth_uctx_t, char *, size_t, const sigset_t *, void (*)(void *), void *, pth_uctx_t);
extern int            pth_uctx_save(pth_uctx_t);
extern int            pth_uctx_restore(pth_uctx_t);
extern int            pth_uctx_switch(pth_uctx_t, pth_uctx_t);
extern int            pth_uctx_destroy(pth_uctx_t);

    /* extension functions */
extern Sfdisc_t      *pth_sfiodisc(void);

    /* generalized variants of replacement functions */
extern int            pth_sigwait_ev(const sigset_t *, int *, pth_event_t);
extern int            pth_connect_ev(int, const struct sockaddr *, socklen_t, pth_event_t);
extern int            pth_accept_ev(int, struct sockaddr *, socklen_t *, pth_event_t);
extern int            pth_select_ev(int, fd_set *, fd_set *, fd_set *, struct timeval *, pth_event_t);
extern int            pth_poll_ev(struct pollfd *, nfds_t, int, pth_event_t);
extern ssize_t        pth_read_ev(int, void *, size_t, pth_event_t);
extern ssize_t        pth_write_ev(int, const void *, size_t, pth_event_t);
extern ssize_t        pth_readv_ev(int, const struct iovec *, int, pth_event_t);
extern ssize_t        pth_writev_ev(int, const struct iovec *, int, pth_event_t);
extern ssize_t        pth_recv_ev(int, void *, size_t, int, pth_event_t);
extern ssize_t        pth_send_ev(int, const void *, size_t, int, pth_event_t);
extern ssize_t        pth_recvfrom_ev(int, void *, size_t, int, struct sockaddr *, socklen_t *, pth_event_t);
extern ssize_t        pth_sendto_ev(int, const void *, size_t, int, const struct sockaddr *, socklen_t, pth_event_t);

    /* standard replacement functions */
extern int            pth_usleep(unsigned int);
extern unsigned int   pth_sleep(unsigned int);
extern pid_t          pth_waitpid(pid_t, int *, int);
extern int            pth_system(const char *);
extern int            pth_sigmask(int, const sigset_t *, sigset_t *);
extern int            pth_sigwait(const sigset_t *, int *);
extern int            pth_connect(int, const struct sockaddr *, socklen_t);
extern int            pth_accept(int, struct sockaddr *, socklen_t *);
extern int            pth_select(int, fd_set *, fd_set *, fd_set *, struct timeval *);
extern int            pth_poll(struct pollfd *, nfds_t, int);
extern ssize_t        pth_read(int, void *, size_t);
extern ssize_t        pth_write(int, const void *, size_t);
extern ssize_t        pth_readv(int, const struct iovec *, int);
extern ssize_t        pth_writev(int, const struct iovec *, int);
extern ssize_t        pth_recv(int, void *, size_t, int);
extern ssize_t        pth_send(int, const void *, size_t, int);
extern ssize_t        pth_recvfrom(int, void *, size_t, int, struct sockaddr *, socklen_t *);
extern ssize_t        pth_sendto(int, const void *, size_t, int, const struct sockaddr *, socklen_t);
extern ssize_t        pth_pread(int, void *, size_t, off_t);
extern ssize_t        pth_pwrite(int, const void *, size_t, off_t);

#endif /* _PTH_H_ */

