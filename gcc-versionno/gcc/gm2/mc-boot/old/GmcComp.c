/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcComp.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   if !defined (TRUE)
#      define TRUE (1==1)
#   endif

#   if !defined (FALSE)
#      define FALSE (1==0)
#   endif

#include <stddef.h>
#include <string.h>
#include <limits.h>
#define _mcComp_H
#define _mcComp_C

#   include "GFIO.h"
#   include "Glibc.h"
#   include "Gdecl.h"
#   include "GsymbolKey.h"
#   include "GSYSTEM.h"
#   include "GmcReserved.h"
#   include "GmcSearch.h"
#   include "GmcLexBuf.h"
#   include "GmcFileName.h"
#   include "GmcPreprocess.h"
#   include "GFormatStrings.h"
#   include "Gmcflex.h"
#   include "Gmcp1.h"
#   include "Gmcp2.h"
#   include "Gmcp3.h"
#   include "Gmcp4.h"
#   include "Gmcp5.h"
#   include "GmcError.h"
#   include "GnameKey.h"
#   include "GmcPrintf.h"
#   include "GmcQuiet.h"
#   include "GDynamicStrings.h"
#   include "GmcOptions.h"

#   define Debugging FALSE
typedef struct parserFunction_p parserFunction;

typedef struct openFunction_p openFunction;

typedef unsigned int (*parserFunction_t) (void);
struct parserFunction_p { parserFunction_t proc; };

typedef unsigned int (*openFunction_t) (decl_node, unsigned int);
struct openFunction_p { openFunction_t proc; };

static unsigned int currentPass;
void mcComp_compile (DynamicStrings_String s);
static void doCompile (DynamicStrings_String s);
static decl_node examineCompilationUnit (void);
static decl_node peepInto (DynamicStrings_String s);
static decl_node initParser (DynamicStrings_String s);
static void p1 (decl_node n);
static void p2 (decl_node n);
static void p3 (decl_node n);
static void p4 (decl_node n);
static void p5 (decl_node n);
static unsigned int doOpen (decl_node n, DynamicStrings_String symName, DynamicStrings_String fileName, unsigned int exitOnFailure);
static unsigned int openDef (decl_node n, unsigned int exitOnFailure);
static unsigned int openMod (decl_node n, unsigned int exitOnFailure);
static void pass (unsigned int no, decl_node n, parserFunction f, decl_isNodeF isnode, openFunction open);
static void doPass (unsigned int parseDefs, unsigned int parseMain, unsigned int no, symbolKey_performOperation p, char *desc_, unsigned int _desc_high);
static void setToPassNo (unsigned int n);
static void init (void);

static void doCompile (DynamicStrings_String s)
{
  decl_node n;

  n = initParser (s);
  doPass (TRUE, TRUE, 1, (symbolKey_performOperation) {(symbolKey_performOperation_t) p1}, (char *) "lexical analysis, modules, root decls and C preprocessor", 56);
  doPass (TRUE, TRUE, 2, (symbolKey_performOperation) {(symbolKey_performOperation_t) p2}, (char *) "[all modules] type equivalence and enumeration types", 52);
  doPass (TRUE, TRUE, 3, (symbolKey_performOperation) {(symbolKey_performOperation_t) p3}, (char *) "[all modules] import lists, types, variables and procedure declarations", 71);
  doPass (TRUE, TRUE, 4, (symbolKey_performOperation) {(symbolKey_performOperation_t) p4}, (char *) "[all modules] constant expressions", 34);
  if (decl_isDef (n))
    if (decl_isImp (n))
      {
        mcQuiet_qprintf0 ((char *) "Parse implementation module\\", 29);
        doPass (FALSE, TRUE, 5, (symbolKey_performOperation) {(symbolKey_performOperation_t) p5}, (char *) "[implementation module] build code tree for all procedures and module initialisations", 85);
      }
    else
      {
        mcQuiet_qprintf0 ((char *) "Parse program module\\", 22);
        doPass (FALSE, TRUE, 5, (symbolKey_performOperation) {(symbolKey_performOperation_t) p5}, (char *) "[program module] build code tree for all procedures and module initialisations", 78);
      }
  mcQuiet_qprintf0 ((char *) "walk tree converting it to C/C++\\", 34);
  decl_out ();
}

static decl_node examineCompilationUnit (void)
{
  while (((mcLexBuf_currenttoken != mcReserved_eoftok) && (mcLexBuf_currenttoken != mcReserved_semicolontok)) && (mcLexBuf_currenttoken != mcReserved_lsbratok))
    {
      if (mcLexBuf_currenttoken == mcReserved_definitiontok)
        {
          mcLexBuf_getToken ();
          if (mcLexBuf_currenttoken == mcReserved_moduletok)
            {
              mcLexBuf_getToken ();
              if (mcLexBuf_currenttoken == mcReserved_fortok)
                {
                  mcLexBuf_getToken ();
                  if (mcLexBuf_currenttoken == mcReserved_stringtok)
                    mcLexBuf_getToken ();
                  else
                    {
                      mcflex_mcError (DynamicStrings_string (DynamicStrings_InitString ((char *) "expecting language string after FOR keyword", 43)));
                      libc_exit (1);
                    }
                }
              if (mcLexBuf_currenttoken == mcReserved_identtok)
                return decl_lookupDef (nameKey_makekey (mcLexBuf_currentstring));
            }
          else
            mcflex_mcError (DynamicStrings_string (DynamicStrings_InitString ((char *) "MODULE missing after DEFINITION keyword", 39)));
        }
      mcLexBuf_getToken ();
    }
  mcflex_mcError (DynamicStrings_string (DynamicStrings_InitString ((char *) "failed to find module name", 26)));
  libc_exit (1);
}

static decl_node peepInto (DynamicStrings_String s)
{
  decl_node n;
  DynamicStrings_String fileName;

  fileName = mcPreprocess_preprocessModule (s);
  if (mcLexBuf_openSource (fileName))
    {
      n = examineCompilationUnit ();
      decl_setSource (n, nameKey_makekey (DynamicStrings_string (fileName)));
      decl_setMainModule (n);
      mcLexBuf_closeSource ();
      mcLexBuf_reInitialize ();
      return n;
    }
  else
    {
      mcPrintf_fprintf1 (FIO_StdErr, (char *) "failed to open %s\\", 19, (unsigned char *) &s, sizeof (s));
      libc_exit (1);
    }
}

static decl_node initParser (DynamicStrings_String s)
{
  mcQuiet_qprintf1 ((char *) "Compiling: %s\\", 15, (unsigned char *) &s, sizeof (s));
  return peepInto (s);
}

static void p1 (decl_node n)
{
  if (decl_isDef (n))
    {
      pass (1, n, (parserFunction) {(parserFunction_t) mcp1_CompilationUnit}, (decl_isNodeF) {(decl_isNodeF_t) decl_isDef}, (openFunction) {(openFunction_t) openDef});
      if ((decl_hasHidden (n)) && (mcOptions_getExtendedOpaque ()))
        pass (1, decl_lookupImp (decl_getSymName (n)), (parserFunction) {(parserFunction_t) mcp1_CompilationUnit}, (decl_isNodeF) {(decl_isNodeF_t) decl_isImp}, (openFunction) {(openFunction_t) openMod});
    }
  else
    pass (1, n, (parserFunction) {(parserFunction_t) mcp1_CompilationUnit}, (decl_isNodeF) {(decl_isNodeF_t) decl_isImpOrModule}, (openFunction) {(openFunction_t) openMod});
}

static void p2 (decl_node n)
{
  if (decl_isDef (n))
    {
      pass (2, n, (parserFunction) {(parserFunction_t) mcp2_CompilationUnit}, (decl_isNodeF) {(decl_isNodeF_t) decl_isDef}, (openFunction) {(openFunction_t) openDef});
      if ((decl_hasHidden (n)) && (mcOptions_getExtendedOpaque ()))
        pass (2, decl_lookupImp (decl_getSymName (n)), (parserFunction) {(parserFunction_t) mcp2_CompilationUnit}, (decl_isNodeF) {(decl_isNodeF_t) decl_isImp}, (openFunction) {(openFunction_t) openMod});
    }
  else
    pass (2, n, (parserFunction) {(parserFunction_t) mcp2_CompilationUnit}, (decl_isNodeF) {(decl_isNodeF_t) decl_isImpOrModule}, (openFunction) {(openFunction_t) openMod});
}

static void p3 (decl_node n)
{
  if (decl_isDef (n))
    {
      pass (3, n, (parserFunction) {(parserFunction_t) mcp3_CompilationUnit}, (decl_isNodeF) {(decl_isNodeF_t) decl_isDef}, (openFunction) {(openFunction_t) openDef});
      if ((decl_hasHidden (n)) && (mcOptions_getExtendedOpaque ()))
        pass (3, decl_lookupImp (decl_getSymName (n)), (parserFunction) {(parserFunction_t) mcp3_CompilationUnit}, (decl_isNodeF) {(decl_isNodeF_t) decl_isImp}, (openFunction) {(openFunction_t) openMod});
    }
  else
    pass (3, n, (parserFunction) {(parserFunction_t) mcp3_CompilationUnit}, (decl_isNodeF) {(decl_isNodeF_t) decl_isImpOrModule}, (openFunction) {(openFunction_t) openMod});
}

static void p4 (decl_node n)
{
  if (decl_isDef (n))
    {
      pass (4, n, (parserFunction) {(parserFunction_t) mcp4_CompilationUnit}, (decl_isNodeF) {(decl_isNodeF_t) decl_isDef}, (openFunction) {(openFunction_t) openDef});
      if ((decl_hasHidden (n)) && (mcOptions_getExtendedOpaque ()))
        pass (4, decl_lookupImp (decl_getSymName (n)), (parserFunction) {(parserFunction_t) mcp4_CompilationUnit}, (decl_isNodeF) {(decl_isNodeF_t) decl_isImp}, (openFunction) {(openFunction_t) openMod});
    }
  else
    pass (4, n, (parserFunction) {(parserFunction_t) mcp4_CompilationUnit}, (decl_isNodeF) {(decl_isNodeF_t) decl_isImpOrModule}, (openFunction) {(openFunction_t) openMod});
}

static void p5 (decl_node n)
{
  pass (5, n, (parserFunction) {(parserFunction_t) mcp5_CompilationUnit}, (decl_isNodeF) {(decl_isNodeF_t) decl_isImpOrModule}, (openFunction) {(openFunction_t) openMod});
}

static unsigned int doOpen (decl_node n, DynamicStrings_String symName, DynamicStrings_String fileName, unsigned int exitOnFailure)
{
  DynamicStrings_String postProcessed;

  mcQuiet_qprintf2 ((char *) "   Module %-20s : %s\\", 22, (unsigned char *) &symName, sizeof (symName), (unsigned char *) &fileName, sizeof (fileName));
  postProcessed = mcPreprocess_preprocessModule (fileName);
  decl_setSource (n, nameKey_makekey (DynamicStrings_string (postProcessed)));
  decl_setCurrentModule (n);
  if (mcLexBuf_openSource (postProcessed))
    return TRUE;
  mcPrintf_fprintf1 (FIO_StdErr, (char *) "failed to open %s\\", 19, (unsigned char *) &fileName, sizeof (fileName));
  if (exitOnFailure)
    libc_exit (1);
  return FALSE;
}

static unsigned int openDef (decl_node n, unsigned int exitOnFailure)
{
  nameKey_Name sourceName;
  DynamicStrings_String symName;
  DynamicStrings_String fileName;

  sourceName = decl_getSource (n);
  symName = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
  if (sourceName == nameKey_NulName)
  {
    /* avoid dangling else.  */
    if (mcSearch_findSourceDefFile (symName, &fileName))
      {
        mcPrintf_fprintf1 (FIO_StdErr, (char *) "failed to find definition module %s.def\\", 41, (unsigned char *) &symName, sizeof (symName));
        if (exitOnFailure)
          libc_exit (1);
      }
  }
  else
    fileName = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (sourceName));
  return doOpen (n, symName, fileName, exitOnFailure);
}

static unsigned int openMod (decl_node n, unsigned int exitOnFailure)
{
  nameKey_Name sourceName;
  DynamicStrings_String symName;
  DynamicStrings_String fileName;

  sourceName = decl_getSource (n);
  symName = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (decl_getSymName (n)));
  if (sourceName == nameKey_NulName)
  {
    /* avoid dangling else.  */
    if (mcSearch_findSourceModFile (symName, &fileName))
      {
        if (decl_isImp (n))
          mcPrintf_fprintf1 (FIO_StdErr, (char *) "failed to find implementation module %s.mod\\", 45, (unsigned char *) &symName, sizeof (symName));
        else
          mcPrintf_fprintf1 (FIO_StdErr, (char *) "failed to find program module %s.mod\\", 38, (unsigned char *) &symName, sizeof (symName));
        if (exitOnFailure)
          libc_exit (1);
      }
  }
  else
    fileName = DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (sourceName));
  return doOpen (n, symName, fileName, exitOnFailure);
}

static void pass (unsigned int no, decl_node n, parserFunction f, decl_isNodeF isnode, openFunction open)
{
  if (((*isnode.proc) (n)) && (decl_isVisited (n)))
    {
      decl_setVisited (n);
      if ((*open.proc) (n, TRUE))
        {
          if ((*f.proc) ())
            {
              mcError_writeFormat0 ((char *) "compilation failed", 18);
              mcLexBuf_closeSource ();
              return;
            }
          mcLexBuf_closeSource ();
        }
    }
}

static void doPass (unsigned int parseDefs, unsigned int parseMain, unsigned int no, symbolKey_performOperation p, char *desc_, unsigned int _desc_high)
{
  DynamicStrings_String descs;
  char desc[_desc_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (desc, desc_, _desc_high);

  setToPassNo (no);
  descs = DynamicStrings_InitString ((char *) desc, _desc_high);
  mcQuiet_qprintf2 ((char *) "Pass %d: %s\\", 13, (unsigned char *) &no, sizeof (no), (unsigned char *) &descs, sizeof (descs));
  decl_foreachDefModuleDo ((symbolKey_performOperation) {(symbolKey_performOperation_t) decl_unsetVisited});
  decl_foreachModModuleDo ((symbolKey_performOperation) {(symbolKey_performOperation_t) decl_unsetVisited});
  if (parseMain)
    {
      decl_unsetVisited (decl_getMainModule ());
      if (parseDefs && (decl_isImp (decl_getMainModule ())))
        (*p.proc) ((void *) decl_lookupDef (decl_getSymName (decl_getMainModule ())));
      (*p.proc) ((void *) decl_getMainModule ());
    }
  if (parseDefs)
    decl_foreachDefModuleDo (p);
  mcError_flushWarnings ();
  mcError_flushErrors ();
  setToPassNo (0);
}

static void setToPassNo (unsigned int n)
{
  currentPass = n;
}

static void init (void)
{
  setToPassNo (0);
}

void mcComp_compile (DynamicStrings_String s)
{
  if (s != NULL)
    doCompile (s);
}

void _M2_mcComp_init (int argc, char *argv[])
{
  init ();
}