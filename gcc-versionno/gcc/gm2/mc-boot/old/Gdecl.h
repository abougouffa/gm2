/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/decl.def.  */


#if !defined (_decl_H)
#   define _decl_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GnameKey.h"
#   include "GsymbolKey.h"
#   include "GmcReserved.h"

#   if defined (_decl_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

#if !defined (decl_node_D)
#  define decl_node_D
   typedef void *decl_node;
#endif

typedef struct decl_isNodeF_p decl_isNodeF;

typedef unsigned int (*decl_isNodeF_t) (decl_node);
struct decl_isNodeF_p { decl_isNodeF_t proc; };

EXTERN unsigned int decl_getDeclaredMod (decl_node n);
EXTERN unsigned int decl_getDeclaredDef (decl_node n);
EXTERN unsigned int decl_getFirstUsed (decl_node n);
EXTERN unsigned int decl_isDef (decl_node n);
EXTERN unsigned int decl_isImp (decl_node n);
EXTERN unsigned int decl_isImpOrModule (decl_node n);
EXTERN unsigned int decl_isVisited (decl_node n);
EXTERN void decl_unsetVisited (decl_node n);
EXTERN void decl_setVisited (decl_node n);
EXTERN void decl_setEnumsComplete (decl_node n);
EXTERN unsigned int decl_getEnumsComplete (decl_node n);
EXTERN void decl_resetEnumPos (decl_node n);
EXTERN decl_node decl_getNextEnum (void);
EXTERN unsigned int decl_isModule (decl_node n);
EXTERN unsigned int decl_isMainModule (decl_node n);
EXTERN void decl_setMainModule (decl_node n);
EXTERN void decl_setCurrentModule (decl_node n);
EXTERN decl_node decl_lookupDef (nameKey_Name n);
EXTERN decl_node decl_lookupImp (nameKey_Name n);
EXTERN decl_node decl_lookupModule (nameKey_Name n);
EXTERN void decl_putDefForC (decl_node n);
EXTERN decl_node decl_lookupInScope (decl_node scope, nameKey_Name n);
EXTERN unsigned int decl_isConst (decl_node n);
EXTERN unsigned int decl_isType (decl_node n);
EXTERN void decl_putType (decl_node des, decl_node exp);
EXTERN decl_node decl_getType (decl_node n);
EXTERN decl_node decl_skipType (decl_node n);
EXTERN void decl_putTypeHidden (decl_node des);
EXTERN unsigned int decl_isTypeHidden (decl_node n);
EXTERN unsigned int decl_hasHidden (decl_node n);
EXTERN unsigned int decl_isVar (decl_node n);
EXTERN unsigned int decl_isTemporary (decl_node n);
EXTERN unsigned int decl_isExported (decl_node n);
EXTERN decl_node decl_getDeclScope (void);
EXTERN decl_node decl_getScope (decl_node n);
EXTERN unsigned int decl_isLiteral (decl_node n);
EXTERN unsigned int decl_isConstSet (decl_node n);
EXTERN unsigned int decl_isEnumerationField (decl_node n);
EXTERN unsigned int decl_isEnumeration (decl_node n);
EXTERN unsigned int decl_isUnbounded (decl_node n);
EXTERN unsigned int decl_isParameter (decl_node n);
EXTERN unsigned int decl_isVarParam (decl_node n);
EXTERN unsigned int decl_isParam (decl_node n);
EXTERN unsigned int decl_isNonVarParam (decl_node n);
EXTERN decl_node decl_addOptParameter (decl_node proc, nameKey_Name id, decl_node type, decl_node init);
EXTERN unsigned int decl_isOptarg (decl_node n);
EXTERN unsigned int decl_isRecord (decl_node n);
EXTERN unsigned int decl_isRecordField (decl_node n);
EXTERN unsigned int decl_isVarientField (decl_node n);
EXTERN unsigned int decl_isArray (decl_node n);
EXTERN unsigned int decl_isProcType (decl_node n);
EXTERN unsigned int decl_isPointer (decl_node n);
EXTERN unsigned int decl_isProcedure (decl_node n);
EXTERN unsigned int decl_isVarient (decl_node n);
EXTERN unsigned int decl_isSet (decl_node n);
EXTERN unsigned int decl_isSubrange (decl_node n);
EXTERN unsigned int decl_isZtype (decl_node n);
EXTERN unsigned int decl_isRtype (decl_node n);
EXTERN decl_node decl_makeConst (nameKey_Name n);
EXTERN void decl_putConst (decl_node n, decl_node v);
EXTERN decl_node decl_makeType (nameKey_Name n);
EXTERN decl_node decl_makeTypeImp (nameKey_Name n);
EXTERN decl_node decl_makeVar (nameKey_Name n);
EXTERN void decl_putVar (decl_node var, decl_node type, decl_node decl);
EXTERN decl_node decl_makeVarDecl (decl_node i, decl_node type);
EXTERN decl_node decl_makeEnum (void);
EXTERN decl_node decl_makeEnumField (decl_node e, nameKey_Name n);
EXTERN decl_node decl_makeSubrange (decl_node low, decl_node high);
EXTERN void decl_putSubrangeType (decl_node sub, decl_node type);
EXTERN decl_node decl_makePointer (decl_node type);
EXTERN decl_node decl_makeSet (decl_node type);
EXTERN decl_node decl_makeArray (decl_node subr, decl_node type);
EXTERN void decl_putUnbounded (decl_node n);
EXTERN decl_node decl_makeRecord (void);
EXTERN decl_node decl_makeVarient (decl_node r);
EXTERN decl_node decl_addFieldsToRecord (decl_node r, decl_node v, decl_node i, decl_node t);
EXTERN void decl_buildVarientSelector (decl_node r, decl_node v, nameKey_Name tag, decl_node type);
EXTERN decl_node decl_buildVarientFieldRecord (decl_node v, decl_node p);
EXTERN nameKey_Name decl_getSymName (decl_node n);
EXTERN decl_node decl_import (decl_node m, decl_node n);
EXTERN decl_node decl_lookupExported (decl_node n, nameKey_Name i);
EXTERN decl_node decl_lookupSym (nameKey_Name n);
EXTERN void decl_addImportedModule (decl_node m, decl_node i, unsigned int scoped);
EXTERN void decl_setSource (decl_node n, nameKey_Name s);
EXTERN nameKey_Name decl_getSource (decl_node n);
EXTERN decl_node decl_getMainModule (void);
EXTERN decl_node decl_getCurrentModule (void);
EXTERN void decl_foreachDefModuleDo (symbolKey_performOperation p);
EXTERN void decl_foreachModModuleDo (symbolKey_performOperation p);
EXTERN void decl_enterScope (decl_node n);
EXTERN void decl_leaveScope (void);
EXTERN decl_node decl_makeProcedure (nameKey_Name n);
EXTERN decl_node decl_makeProcType (void);
EXTERN void decl_putProcTypeOptReturn (decl_node proc);
EXTERN void decl_putReturnType (decl_node p, decl_node type);
EXTERN decl_node decl_makeVarParameter (decl_node l, decl_node type);
EXTERN decl_node decl_makeNonVarParameter (decl_node l, decl_node type);
EXTERN void decl_paramEnter (decl_node n);
EXTERN void decl_paramLeave (decl_node n);
EXTERN decl_node decl_makeIdentList (void);
EXTERN unsigned int decl_putIdent (decl_node n, nameKey_Name i);
EXTERN void decl_addVarParameters (decl_node n, decl_node i, decl_node type);
EXTERN void decl_addNonVarParameters (decl_node n, decl_node i, decl_node type);
EXTERN decl_node decl_makeVarargs (void);
EXTERN unsigned int decl_isVarargs (decl_node n);
EXTERN void decl_addParameter (decl_node proc, decl_node param);
EXTERN decl_node decl_makeBinaryTok (mcReserved_toktype op, decl_node l, decl_node r);
EXTERN decl_node decl_makeUnaryTok (mcReserved_toktype op, decl_node e);
EXTERN decl_node decl_makeComponentRef (decl_node rec, decl_node field);
EXTERN decl_node decl_makePointerRef (decl_node ptr, decl_node field);
EXTERN unsigned int decl_isPointerRef (decl_node n);
EXTERN decl_node decl_makeDeRef (decl_node n);
EXTERN decl_node decl_makeArrayRef (decl_node array, decl_node index);
EXTERN decl_node decl_getLastOp (decl_node n);
EXTERN decl_node decl_getCardinal (void);
EXTERN decl_node decl_makeLiteralInt (nameKey_Name n);
EXTERN decl_node decl_makeLiteralReal (nameKey_Name n);
EXTERN decl_node decl_makeString (nameKey_Name n);
EXTERN decl_node decl_makeSetValue (void);
EXTERN unsigned int decl_isSetValue (decl_node n);
EXTERN decl_node decl_putSetValue (decl_node n, decl_node t);
EXTERN decl_node decl_includeSetValue (decl_node n, decl_node l, decl_node h);
EXTERN decl_node decl_getBuiltinConst (nameKey_Name n);
EXTERN decl_node decl_makeExpList (void);
EXTERN unsigned int decl_isExpList (decl_node n);
EXTERN void decl_putExpList (decl_node n, decl_node e);
EXTERN decl_node decl_makeConstExp (void);
EXTERN decl_node decl_getNextConstExp (void);
EXTERN void decl_setConstExpComplete (decl_node n);
EXTERN decl_node decl_fixupConstExp (decl_node c, decl_node e);
EXTERN void decl_resetConstExpPos (decl_node n);
EXTERN decl_node decl_makeFuncCall (decl_node c, decl_node n);
EXTERN decl_node decl_makeStatementSequence (void);
EXTERN unsigned int decl_isStatementSequence (decl_node n);
EXTERN void decl_addStatement (decl_node s, decl_node n);
EXTERN decl_node decl_makeReturn (void);
EXTERN unsigned int decl_isReturn (decl_node n);
EXTERN void decl_putReturn (decl_node n, decl_node e);
EXTERN decl_node decl_makeWhile (void);
EXTERN void decl_putWhile (decl_node n, decl_node e, decl_node s);
EXTERN unsigned int decl_isWhile (decl_node n);
EXTERN decl_node decl_makeAssignment (decl_node d, decl_node e);
EXTERN void decl_putBegin (decl_node b, decl_node s);
EXTERN decl_node decl_makeExit (decl_node l, unsigned int n);
EXTERN unsigned int decl_isExit (decl_node n);
EXTERN decl_node decl_makeLoop (void);
EXTERN unsigned int decl_isLoop (decl_node n);
EXTERN void decl_putLoop (decl_node l, decl_node s);
EXTERN decl_node decl_makeComment (char *a_, unsigned int _a_high);
EXTERN decl_node decl_makeIf (decl_node e, decl_node s);
EXTERN unsigned int decl_isIf (decl_node n);
EXTERN decl_node decl_makeElsif (decl_node i, decl_node e, decl_node s);
EXTERN unsigned int decl_isElsif (decl_node n);
EXTERN void decl_putElse (decl_node i, decl_node s);
EXTERN decl_node decl_makeFor (void);
EXTERN unsigned int decl_isFor (decl_node n);
EXTERN void decl_putFor (decl_node f, decl_node i, decl_node s, decl_node e, decl_node b, decl_node sq);
EXTERN decl_node decl_makeRepeat (void);
EXTERN unsigned int decl_isRepeat (decl_node n);
EXTERN void decl_putRepeat (decl_node n, decl_node s, decl_node e);
EXTERN decl_node decl_makeCase (void);
EXTERN unsigned int decl_isCase (decl_node n);
EXTERN decl_node decl_putCaseExpression (decl_node n, decl_node e);
EXTERN decl_node decl_putCaseElse (decl_node n, decl_node e);
EXTERN decl_node decl_putCaseStatement (decl_node n, decl_node l, decl_node s);
EXTERN decl_node decl_makeCaseLabelList (decl_node l, decl_node s);
EXTERN unsigned int decl_isCaseLabelList (decl_node n);
EXTERN decl_node decl_makeCaseList (void);
EXTERN unsigned int decl_isCaseList (decl_node n);
EXTERN decl_node decl_putCaseRange (decl_node n, decl_node lo, decl_node hi);
EXTERN decl_node decl_makeRange (decl_node lo, decl_node hi);
EXTERN unsigned int decl_isRange (decl_node n);
EXTERN decl_node decl_dupExpr (decl_node n);
EXTERN void decl_setLangC (void);
EXTERN void decl_setLangCP (void);
EXTERN void decl_setLangM2 (void);
EXTERN void decl_out (void);

#   undef EXTERN
#endif
