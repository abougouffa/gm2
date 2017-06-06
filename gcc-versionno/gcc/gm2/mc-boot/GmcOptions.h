/* automatically created by mc from ../../gcc-versionno/gcc/gm2/mc/mcOptions.def.  */


#if !defined (_mcOptions_H)
#   define _mcOptions_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_mcOptions_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   handleOptions - iterates over all options setting appropriate
                   values and returns the single source file
                   if found at the end of the arguments.
*/

EXTERN DynamicStrings_String mcOptions_handleOptions (void);

/*
   getQuiet - return the value of quiet.
*/

EXTERN unsigned int mcOptions_getQuiet (void);

/*
   getVerbose - return the value of verbose.
*/

EXTERN unsigned int mcOptions_getVerbose (void);

/*
   getInternalDebugging - return the value of internalDebugging.
*/

EXTERN unsigned int mcOptions_getInternalDebugging (void);
EXTERN DynamicStrings_String mcOptions_getCppCommandLine (void);

/*
   getOutputFile - sets the output filename to output.
*/

EXTERN DynamicStrings_String mcOptions_getOutputFile (void);

/*
   getExtendedOpaque - return the extendedOpaque value.
*/

EXTERN unsigned int mcOptions_getExtendedOpaque (void);

/*
   setDebugTopological - sets the flag debugTopological to value.
*/

EXTERN void mcOptions_setDebugTopological (unsigned int value);

/*
   getDebugTopological - returns the flag value of the command
                         line option --debug-top.
*/

EXTERN unsigned int mcOptions_getDebugTopological (void);

/*
   getHPrefix - saves the H file prefix.
*/

EXTERN DynamicStrings_String mcOptions_getHPrefix (void);

/*
   getIgnoreFQ - returns the ignorefq flag.
*/

EXTERN unsigned int mcOptions_getIgnoreFQ (void);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
