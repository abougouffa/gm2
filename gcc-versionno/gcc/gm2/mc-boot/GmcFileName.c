/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcFileName.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#define _mcFileName_H
#define _mcFileName_C

#   include "GASCII.h"
#   include "GDynamicStrings.h"

#   define MaxFileName 0
#   define MaxStemName 0
#   define Directory '/'
DynamicStrings_String mcFileName_calculateFileName (DynamicStrings_String module, DynamicStrings_String extension);
DynamicStrings_String mcFileName_calculateStemName (DynamicStrings_String module);
DynamicStrings_String mcFileName_extractExtension (DynamicStrings_String filename, DynamicStrings_String ext);
DynamicStrings_String mcFileName_extractModule (DynamicStrings_String filename);

DynamicStrings_String mcFileName_calculateFileName (DynamicStrings_String module, DynamicStrings_String extension)
{
  if (MaxFileName == 0)
    return DynamicStrings_ConCat (DynamicStrings_ConCatChar (DynamicStrings_Slice (module, 0, MaxFileName), '.'), extension);
  else
    return DynamicStrings_ConCat (DynamicStrings_ConCatChar (DynamicStrings_Slice (module, 0, (MaxFileName-(DynamicStrings_Length (extension)))-1), '.'), extension);
}

DynamicStrings_String mcFileName_calculateStemName (DynamicStrings_String module)
{
  return DynamicStrings_Slice (module, 0, MaxStemName);
}

DynamicStrings_String mcFileName_extractExtension (DynamicStrings_String filename, DynamicStrings_String ext)
{
  if (DynamicStrings_Equal (ext, DynamicStrings_Mark (DynamicStrings_Slice (filename, (int) -(DynamicStrings_Length (ext)), 0))))
    return DynamicStrings_Slice (filename, 0, (int) -(DynamicStrings_Length (ext)));
  else
    return filename;
}

DynamicStrings_String mcFileName_extractModule (DynamicStrings_String filename)
{
  int i;

  i = DynamicStrings_Index (filename, Directory, 0);
  if (i == -1)
    return DynamicStrings_Dup (filename);
  else
    return DynamicStrings_Slice (filename, i+1, 0);
}

void _M2_mcFileName_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_mcFileName_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
