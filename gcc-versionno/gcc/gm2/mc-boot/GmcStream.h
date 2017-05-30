/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcStream.def.  */


#if !defined (_mcStream_H)
#   define _mcStream_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GFIO.h"

#   if defined (_mcStream_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   openFrag - create and open fragment, id, and return the file.
              The file should not be closed by the user.
*/

EXTERN FIO_File mcStream_openFrag (unsigned int id);

/*
   setDest - informs the stream module and all fragments must be copied
             info, f.
*/

EXTERN void mcStream_setDest (FIO_File f);

/*
   combine - closes all fragments and then writes them in
             order to the destination file.  The dest file
             is returned.
*/

EXTERN FIO_File mcStream_combine (void);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
