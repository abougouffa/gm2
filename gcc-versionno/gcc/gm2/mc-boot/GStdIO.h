/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/StdIO.def.  */


#if !defined (_StdIO_H)
#   define _StdIO_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_StdIO_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

typedef struct StdIO_ProcWrite_p StdIO_ProcWrite;

typedef struct StdIO_ProcRead_p StdIO_ProcRead;

typedef void (*StdIO_ProcWrite_t) (char);
struct StdIO_ProcWrite_p { StdIO_ProcWrite_t proc; };

typedef void (*StdIO_ProcRead_t) (char *);
struct StdIO_ProcRead_p { StdIO_ProcRead_t proc; };

EXTERN void StdIO_Read (char *ch);
EXTERN void StdIO_Write (char ch);
EXTERN void StdIO_PushOutput (StdIO_ProcWrite p);
EXTERN void StdIO_PopOutput (void);
EXTERN StdIO_ProcWrite StdIO_GetCurrentOutput (void);
EXTERN void StdIO_PushInput (StdIO_ProcRead p);
EXTERN void StdIO_PopInput (void);
EXTERN StdIO_ProcRead StdIO_GetCurrentInput (void);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
