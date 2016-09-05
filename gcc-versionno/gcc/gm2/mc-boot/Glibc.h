/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/libc.def.  */


#if !defined (_libc_H)
#   define _libc_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_libc_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

typedef long int libc_time_t;

typedef struct libc_tm_r libc_tm;

typedef struct libc_timeb_r libc_timeb;

typedef libc_tm *libc_ptrToTM;

struct libc_tm_r {
                   int tm_sec;
                   int tm_min;
                   int tm_hour;
                   int tm_mday;
                   int tm_mon;
                   int tm_year;
                   int tm_wday;
                   int tm_yday;
                   int tm_isdst;
                   long int tm_gmtoff;
                   void *tm_zone;
                 };

struct libc_timeb_r {
                      libc_time_t time;
                      short unsigned int millitm;
                      short unsigned int timezone;
                      short unsigned int dstflag;
                    };

EXTERN int libc_write (int d, void * buf, int nbytes);
EXTERN int libc_read (int d, void * buf, int nbytes);
EXTERN int libc_system (void * a);
EXTERN void libc_abort (void);
EXTERN void * libc_malloc (unsigned int size);
EXTERN void libc_free (void * ptr);
EXTERN void * libc_realloc (void * ptr, unsigned int size);
EXTERN int libc_isatty (int fd);
EXTERN void libc_exit (int r);
EXTERN void * libc_getenv (void * s);
EXTERN int libc_getpid (void);
EXTERN int libc_dup (int d);
EXTERN int libc_close (int d);
EXTERN int libc_open (void * filename, int oflag, ...);
EXTERN int libc_creat (void * filename, unsigned int mode);
EXTERN long int libc_lseek (int fd, long int offset, int whence);
EXTERN void libc_perror (char *string_, unsigned int _string_high);
EXTERN int libc_readv (int fd, void * v, int n);
EXTERN int libc_writev (int fd, void * v, int n);
EXTERN void * libc_getcwd (void * buf, int size);
EXTERN int libc_chown (void * filename, int uid, int gid);
EXTERN int libc_strlen (void * a);
EXTERN void * libc_strcpy (void * dest, void * src);
EXTERN void * libc_strncpy (void * dest, void * src, unsigned int n);
EXTERN int libc_unlink (void * file);
EXTERN void * libc_memcpy (void * dest, void * src, unsigned int size);
EXTERN void * libc_memset (void * s, int c, unsigned int size);
EXTERN void * libc_memmove (void * dest, void * src, unsigned int size);
EXTERN int libc_printf (char *format_, unsigned int _format_high, ...);
EXTERN int libc_setenv (void * name, void * value, int overwrite);
EXTERN void libc_srand (int seed);
EXTERN int libc_rand (void);
EXTERN libc_time_t libc_time (void * a);
EXTERN void * libc_localtime (libc_time_t *t);
EXTERN int libc_ftime (libc_timeb *t);
EXTERN int libc_shutdown (int s, int how);
EXTERN int libc_rename (void * oldpath, void * newpath);
EXTERN int libc_setjmp (void * env);
EXTERN void libc_longjmp (void * env, int val);
EXTERN void libc_atexit (PROC proc);
EXTERN void * libc_ttyname (int filedes);
EXTERN unsigned int libc_sleep (unsigned int seconds);

#   undef EXTERN
#endif
