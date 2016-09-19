/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/Indexing.def.  */


#if !defined (_Indexing_H)
#   define _Indexing_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_Indexing_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

#if !defined (Indexing_Index_D)
#  define Indexing_Index_D
   typedef void *Indexing_Index;
#endif

typedef struct Indexing_IndexProcedure_p Indexing_IndexProcedure;

typedef void (*Indexing_IndexProcedure_t) (void *);
struct Indexing_IndexProcedure_p { Indexing_IndexProcedure_t proc; };

EXTERN Indexing_Index Indexing_InitIndex (unsigned int low);
EXTERN Indexing_Index Indexing_KillIndex (Indexing_Index i);
EXTERN Indexing_Index Indexing_DebugIndex (Indexing_Index i);
EXTERN unsigned int Indexing_InBounds (Indexing_Index i, unsigned int n);
EXTERN unsigned int Indexing_HighIndice (Indexing_Index i);
EXTERN unsigned int Indexing_LowIndice (Indexing_Index i);
EXTERN void Indexing_PutIndice (Indexing_Index i, unsigned int n, void * a);
EXTERN void * Indexing_GetIndice (Indexing_Index i, unsigned int n);
EXTERN unsigned int Indexing_IsIndiceInIndex (Indexing_Index i, void * a);
EXTERN void Indexing_RemoveIndiceFromIndex (Indexing_Index i, void * a);
EXTERN void Indexing_DeleteIndice (Indexing_Index i, unsigned int j);
EXTERN void Indexing_IncludeIndiceIntoIndex (Indexing_Index i, void * a);
EXTERN void Indexing_ForeachIndiceInIndexDo (Indexing_Index i, Indexing_IndexProcedure p);

#   undef EXTERN
#endif
