/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcDebug.def.  */


#if !defined (_mcDebug_H)
#   define _mcDebug_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_mcDebug_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   assert - tests the boolean, q. If false then an error is reported
            and the execution is terminated.
*/

EXTERN void mcDebug_assert (unsigned int q);

/*
   writeDebug - only writes a string if the debugging mode is on.
*/

EXTERN void mcDebug_writeDebug (char *a_, unsigned int _a_high);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
