
typedef void * fd_set;
typedef long  __time_t;
typedef unsigned int __useconds_t;
typedef long __suseconds_t;
typedef unsigned int size_t;


struct timeval
{
    __time_t tv_sec;		/* Seconds.  */
    __suseconds_t tv_usec;	/* Microseconds.  */
};
