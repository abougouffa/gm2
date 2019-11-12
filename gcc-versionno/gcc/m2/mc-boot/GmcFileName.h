/* 
    Title      : mcFileName
    Author     : Gaius Mulley
    System     : GNU Modula-2
    Date       : Thu Nov 26 12:59:30 2015
    Revision   : $Version$
    Description: Provides a procedure to calculate a system file name.
  */


#if !defined (_mcFileName_H)
#   define _mcFileName_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_mcFileName_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   calculateFileName - calculates and returns a new string filename
                       given a module and an extension. This file name
                       length will be operating system specific.
                       String, Extension, is concatenated onto
                       Module and thus it is safe to `Mark' the extension
                       for garbage collection.
*/

EXTERN DynamicStrings_String mcFileName_calculateFileName (DynamicStrings_String module, DynamicStrings_String extension);

/*
   calculateStemName - calculates the stem name for given a module.
                       This name length will be operating system and
      	       	       compiler specific.
*/

EXTERN DynamicStrings_String mcFileName_calculateStemName (DynamicStrings_String module);

/*
   extractExtension - given a, filename, return the filename without
                      the extension, Ext.
*/

EXTERN DynamicStrings_String mcFileName_extractExtension (DynamicStrings_String filename, DynamicStrings_String ext);

/*
   extractModule - given a, filename, return the module name including any
                   extension. A new string is returned.
*/

EXTERN DynamicStrings_String mcFileName_extractModule (DynamicStrings_String filename);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
