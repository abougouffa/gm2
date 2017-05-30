/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/IO.def.  */


#if !defined (_IO_H)
#   define _IO_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_IO_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void IO_Read (char *ch);
EXTERN void IO_Write (char ch);
EXTERN void IO_Error (char ch);
EXTERN void IO_UnBufferedMode (int fd, unsigned int input);
EXTERN void IO_BufferedMode (int fd, unsigned int input);
EXTERN void IO_EchoOn (int fd, unsigned int input);
EXTERN void IO_EchoOff (int fd, unsigned int input);

#   undef EXTERN
#endif
