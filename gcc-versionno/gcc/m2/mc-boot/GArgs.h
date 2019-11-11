

#if !defined (_Args_H)
#   define _Args_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_Args_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   GetArg - returns the nth argument from the command line.
            The success of the operation is returned.
*/

EXTERN unsigned int Args_GetArg (char *a, unsigned int _a_high, unsigned int i);

/*
   Narg - returns the number of arguments available from
          command line.
*/

EXTERN unsigned int Args_Narg (void);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
