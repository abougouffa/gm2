/* automatically created by mc from gm2/gm2-auto/mcp1.mod.  */

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
#define _mcp1_H
#define _mcp1_C

#   include "GDynamicStrings.h"
#   include "GmcError.h"
#   include "GnameKey.h"
#   include "GmcPrintf.h"
#   include "GmcDebug.h"
#   include "GmcReserved.h"
#   include "GmcLexBuf.h"
#   include "Gdecl.h"

#   define Pass1 TRUE
#   define Debugging FALSE
typedef unsigned int stop0;

typedef unsigned int SetOfStop0;

typedef unsigned int stop1;

typedef unsigned int SetOfStop1;

typedef unsigned int stop2;

typedef unsigned int SetOfStop2;

static unsigned int WasNoError;
static nameKey_Name curident;
static decl_node curproc;
static decl_node curmodule;
unsigned int mcp1_CompilationUnit (void);
static void ErrorString (DynamicStrings_String s);
static void ErrorArray (char *a_, unsigned int _a_high);
static void checkEndName (decl_node module, nameKey_Name name, char *desc_, unsigned int _desc_high);
static DynamicStrings_String DescribeStop (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void DescribeError (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SyntaxError (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SyntaxCheck (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void WarnMissingToken (mcReserved_toktype t);
static void MissingToken (mcReserved_toktype t);
static unsigned int CheckAndInsert (mcReserved_toktype t, SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static unsigned int InStopSet (mcReserved_toktype t, SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void PeepToken (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Expect (mcReserved_toktype t, SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Ident (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void string (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Integer (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Real (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void registerImport (nameKey_Name ident, unsigned int scoped);
static void FileUnit (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ProgramModule (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ImplementationModule (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ImplementationOrProgramModule (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Number (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Qualident (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ConstExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Relation (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SimpleConstExpr (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void UnaryOrConstTerm (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void AddOperator (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ConstTerm (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void MulOperator (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ConstFactor (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ConstString (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ComponentElement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ComponentValue (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ArraySetRecordValue (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Constructor (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ConstSetOrQualidentOrFunction (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ConstActualParameters (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ConstAttribute (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ConstAttributeExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Element (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ByteAlignment (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void OptAlignmentExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void AlignmentExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Alignment (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void TypeDeclaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Type (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SimpleType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Enumeration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void IdentList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SubrangeType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ArrayType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void RecordType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void DefaultRecordAttributes (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void RecordFieldPragma (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void FieldPragmaExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void PragmaConstExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void AttributeExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void FieldListSequence (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void FieldListStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void FieldList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void TagIdent (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void CaseTag (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Varient (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void VarientCaseLabelList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void VarientCaseLabels (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentConstExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentRelation (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentSimpleConstExpr (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentUnaryOrConstTerm (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentAddOperator (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentConstTerm (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentMulOperator (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentConstFactor (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentConstString (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentConstAttribute (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentConstAttributeExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentComponentElement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentComponentValue (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentArraySetRecordValue (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentConstructor (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentConstSetOrQualidentOrFunction (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentElement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentActualParameters (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SilentExpList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SetType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void PointerType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ProcedureType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void FormalTypeList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void FormalReturn (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void OptReturnType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ProcedureParameters (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ProcedureParameter (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void VarIdent (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void VarIdentList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void VariableDeclaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Designator (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SubDesignator (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ArrayExpList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ExpList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Expression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SimpleExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void UnaryOrTerm (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Term (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Factor (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SetOrDesignatorOrFunction (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void SimpleDes (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ActualParameters (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ExitStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ReturnStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Statement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void RetryStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void AssignmentOrProcedureCall (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void StatementSequence (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void IfStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void CaseStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void CaseEndStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Case (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void CaseLabelList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void CaseLabels (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void WhileStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void RepeatStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ForStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void LoopStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void WithStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ProcedureDeclaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void DefProcedureIdent (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ProcedureIdent (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void DefineBuiltinProcedure (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ProcedureHeading (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Builtin (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void DefProcedureHeading (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ProcedureBlock (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Block (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void InitialBlock (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void FinalBlock (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void InitialBlockBody (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void FinalBlockBody (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ProcedureBlockBody (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void NormalPart (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ExceptionalPart (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Declaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void DefFormalParameters (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void DefMultiFPSection (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void FormalParameters (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void MultiFPSection (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void FPSection (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void DefExtendedFP (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ExtendedFP (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void VarFPSection (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void NonVarFPSection (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void OptArg (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void DefOptArg (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void FormalType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ModuleDeclaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Priority (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Export (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void FromImport (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ImportModuleList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void WithoutFromImport (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Import (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void DefinitionModule (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void DefTypeDeclaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void ConstantDeclaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void Definition (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void AsmStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void AsmOperands (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void AsmOperandSpec (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void AsmList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void NamedOperand (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void AsmOperandName (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void AsmElement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);
static void TrashList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2);

static void ErrorString (DynamicStrings_String s)
{
  mcError_errorStringAt (s, mcLexBuf_getTokenNo ());
  WasNoError = FALSE;
}

static void ErrorArray (char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high);

  ErrorString (DynamicStrings_InitString ((char *) a, _a_high));
}

static void checkEndName (decl_node module, nameKey_Name name, char *desc_, unsigned int _desc_high)
{
  DynamicStrings_String s;
  char desc[_desc_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (desc, desc_, _desc_high);

  if ((decl_getSymName (module)) != name)
    {
      s = DynamicStrings_InitString ((char *) "inconsistent module name found with this ", 41);
      s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitString ((char *) desc, _desc_high)));
      ErrorString (s);
    }
}

static DynamicStrings_String DescribeStop (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  unsigned int n;
  DynamicStrings_String str;
  DynamicStrings_String message;

  n = 0;
  message = DynamicStrings_InitString ((char *) "", 0);
  if ((((1 << (mcReserved_stringtok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`string'", 8)));
      n += 1;
    }
  if ((((1 << (mcReserved_realtok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`real number'", 13)));
      n += 1;
    }
  if ((((1 << (mcReserved_identtok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`identifier'", 12)));
      n += 1;
    }
  if ((((1 << (mcReserved_integertok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`integer number'", 16)));
      n += 1;
    }
  if ((((1 << (mcReserved_inlinetok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`__INLINE__'", 12)));
      n += 1;
    }
  if ((((1 << (mcReserved_builtintok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`__BUILTIN__'", 13)));
      n += 1;
    }
  if ((((1 << (mcReserved_attributetok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`__ATTRIBUTE__'", 15)));
      n += 1;
    }
  if ((((1 << (mcReserved_filetok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`__FILE__'", 10)));
      n += 1;
    }
  if ((((1 << (mcReserved_linetok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`__LINE__'", 10)));
      n += 1;
    }
  if ((((1 << (mcReserved_datetok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`__DATE__'", 10)));
      n += 1;
    }
  if ((((1 << (mcReserved_periodperiodperiodtok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`...'", 5)));
      n += 1;
    }
  if ((((1 << (mcReserved_volatiletok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`VOLATILE'", 10)));
      n += 1;
    }
  if ((((1 << (mcReserved_asmtok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`ASM'", 5)));
      n += 1;
    }
  if ((((1 << (mcReserved_withtok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`WITH'", 6)));
      n += 1;
    }
  if ((((1 << (mcReserved_whiletok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`WHILE'", 7)));
      n += 1;
    }
  if ((((1 << (mcReserved_vartok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`VAR'", 5)));
      n += 1;
    }
  if ((((1 << (mcReserved_untiltok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`UNTIL'", 7)));
      n += 1;
    }
  if ((((1 << (mcReserved_typetok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`TYPE'", 6)));
      n += 1;
    }
  if ((((1 << (mcReserved_totok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`TO'", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_thentok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`THEN'", 6)));
      n += 1;
    }
  if ((((1 << (mcReserved_settok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`SET'", 5)));
      n += 1;
    }
  if ((((1 << (mcReserved_returntok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`RETURN'", 8)));
      n += 1;
    }
  if ((((1 << (mcReserved_retrytok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`RETRY'", 7)));
      n += 1;
    }
  if ((((1 << (mcReserved_repeattok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`REPEAT'", 8)));
      n += 1;
    }
  if ((((1 << (mcReserved_remtok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`REM'", 5)));
      n += 1;
    }
  if ((((1 << (mcReserved_recordtok-mcReserved_recordtok)) & (stopset2)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`RECORD'", 8)));
      n += 1;
    }
  if ((((1 << (mcReserved_unqualifiedtok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`UNQUALIFIED'", 13)));
      n += 1;
    }
  if ((((1 << (mcReserved_qualifiedtok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`QUALIFIED'", 11)));
      n += 1;
    }
  if ((((1 << (mcReserved_proceduretok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`PROCEDURE'", 11)));
      n += 1;
    }
  if ((((1 << (mcReserved_pointertok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`POINTER'", 9)));
      n += 1;
    }
  if ((((1 << (mcReserved_packedsettok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`PACKEDSET'", 11)));
      n += 1;
    }
  if ((((1 << (mcReserved_ortok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`OR'", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_oftok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`OF'", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_nottok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`NOT'", 5)));
      n += 1;
    }
  if ((((1 << (mcReserved_moduletok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`MODULE'", 8)));
      n += 1;
    }
  if ((((1 << (mcReserved_modtok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`MOD'", 5)));
      n += 1;
    }
  if ((((1 << (mcReserved_looptok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`LOOP'", 6)));
      n += 1;
    }
  if ((((1 << (mcReserved_intok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`IN'", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_importtok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`IMPORT'", 8)));
      n += 1;
    }
  if ((((1 << (mcReserved_implementationtok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`IMPLEMENTATION'", 16)));
      n += 1;
    }
  if ((((1 << (mcReserved_iftok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`IF'", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_fromtok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`FROM'", 6)));
      n += 1;
    }
  if ((((1 << (mcReserved_fortok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`FOR'", 5)));
      n += 1;
    }
  if ((((1 << (mcReserved_finallytok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`FINALLY'", 9)));
      n += 1;
    }
  if ((((1 << (mcReserved_exporttok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`EXPORT'", 8)));
      n += 1;
    }
  if ((((1 << (mcReserved_exittok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`EXIT'", 6)));
      n += 1;
    }
  if ((((1 << (mcReserved_excepttok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`EXCEPT'", 8)));
      n += 1;
    }
  if ((((1 << (mcReserved_endtok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`END'", 5)));
      n += 1;
    }
  if ((((1 << (mcReserved_elsiftok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`ELSIF'", 7)));
      n += 1;
    }
  if ((((1 << (mcReserved_elsetok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`ELSE'", 6)));
      n += 1;
    }
  if ((((1 << (mcReserved_dotok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`DO'", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_divtok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`DIV'", 5)));
      n += 1;
    }
  if ((((1 << (mcReserved_definitiontok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`DEFINITION'", 12)));
      n += 1;
    }
  if ((((1 << (mcReserved_consttok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`CONST'", 7)));
      n += 1;
    }
  if ((((1 << (mcReserved_casetok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`CASE'", 6)));
      n += 1;
    }
  if ((((1 << (mcReserved_bytok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`BY'", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_begintok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`BEGIN'", 7)));
      n += 1;
    }
  if ((((1 << (mcReserved_arraytok-mcReserved_arraytok)) & (stopset1)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`ARRAY'", 7)));
      n += 1;
    }
  if ((((1 << (mcReserved_andtok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`AND'", 5)));
      n += 1;
    }
  if ((((1 << (mcReserved_colontok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`:'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_periodperiodtok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`..'", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_rdirectivetok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`*>'", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_ldirectivetok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`<*'", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_greaterequaltok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`>='", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_lessequaltok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`<='", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_lessgreatertok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`<>'", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_hashtok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`#'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_equaltok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`='", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_uparrowtok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`^'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_semicolontok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`;'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_commatok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`,'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_periodtok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`.'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_ambersandtok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`&'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_dividetok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`/'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_timestok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`*'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_minustok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`-'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_plustok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`+'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_doublequotestok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCatChar (DynamicStrings_ConCatChar (DynamicStrings_ConCatChar (DynamicStrings_ConCatChar (DynamicStrings_ConCatChar (message, ' '), '`'), '\\'), '\''), ',');
      n += 1;
    }
  if ((((1 << (mcReserved_singlequotetok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCatChar (DynamicStrings_ConCatChar (DynamicStrings_ConCatChar (DynamicStrings_ConCatChar (DynamicStrings_ConCatChar (message, ' '), '\\'), '\''), '\\'), ',');
      n += 1;
    }
  if ((((1 << (mcReserved_greatertok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`>'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_lesstok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`<'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_rparatok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`)'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_lparatok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`('", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_rcbratok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`}'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_lcbratok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`{'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_rsbratok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`]'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_lsbratok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`['", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_bartok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`|'", 3)));
      n += 1;
    }
  if ((((1 << (mcReserved_becomestok-mcReserved_eoftok)) & (stopset0)) != 0))
    {
      message = DynamicStrings_ConCat (DynamicStrings_ConCatChar (message, ' '), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "`:='", 4)));
      n += 1;
    }
  if ((((1 << (mcReserved_eoftok-mcReserved_eoftok)) & (stopset0)) != 0))
    ;  /* empty.  */
  if (n == 0)
    {
      str = DynamicStrings_InitString ((char *) " syntax error", 13);
      message = DynamicStrings_KillString (message);
    }
  return str;
}

static void DescribeError (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  DynamicStrings_String str;

  str = DynamicStrings_InitString ((char *) "", 0);
  switch (mcLexBuf_currenttoken)
    {
      case mcReserved_stringtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `string'", 28), DynamicStrings_Mark (str));
        break;

      case mcReserved_realtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `real number'", 33), DynamicStrings_Mark (str));
        break;

      case mcReserved_identtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `identifier'", 32), DynamicStrings_Mark (str));
        break;

      case mcReserved_integertok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `integer number'", 36), DynamicStrings_Mark (str));
        break;

      case mcReserved_inlinetok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `__INLINE__'", 32), DynamicStrings_Mark (str));
        break;

      case mcReserved_builtintok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `__BUILTIN__'", 33), DynamicStrings_Mark (str));
        break;

      case mcReserved_attributetok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `__ATTRIBUTE__'", 35), DynamicStrings_Mark (str));
        break;

      case mcReserved_filetok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `__FILE__'", 30), DynamicStrings_Mark (str));
        break;

      case mcReserved_linetok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `__LINE__'", 30), DynamicStrings_Mark (str));
        break;

      case mcReserved_datetok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `__DATE__'", 30), DynamicStrings_Mark (str));
        break;

      case mcReserved_periodperiodperiodtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `...'", 25), DynamicStrings_Mark (str));
        break;

      case mcReserved_volatiletok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `VOLATILE'", 30), DynamicStrings_Mark (str));
        break;

      case mcReserved_asmtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `ASM'", 25), DynamicStrings_Mark (str));
        break;

      case mcReserved_withtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `WITH'", 26), DynamicStrings_Mark (str));
        break;

      case mcReserved_whiletok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `WHILE'", 27), DynamicStrings_Mark (str));
        break;

      case mcReserved_vartok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `VAR'", 25), DynamicStrings_Mark (str));
        break;

      case mcReserved_untiltok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `UNTIL'", 27), DynamicStrings_Mark (str));
        break;

      case mcReserved_typetok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `TYPE'", 26), DynamicStrings_Mark (str));
        break;

      case mcReserved_totok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `TO'", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_thentok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `THEN'", 26), DynamicStrings_Mark (str));
        break;

      case mcReserved_settok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `SET'", 25), DynamicStrings_Mark (str));
        break;

      case mcReserved_returntok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `RETURN'", 28), DynamicStrings_Mark (str));
        break;

      case mcReserved_retrytok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `RETRY'", 27), DynamicStrings_Mark (str));
        break;

      case mcReserved_repeattok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `REPEAT'", 28), DynamicStrings_Mark (str));
        break;

      case mcReserved_remtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `REM'", 25), DynamicStrings_Mark (str));
        break;

      case mcReserved_recordtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `RECORD'", 28), DynamicStrings_Mark (str));
        break;

      case mcReserved_unqualifiedtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `UNQUALIFIED'", 33), DynamicStrings_Mark (str));
        break;

      case mcReserved_qualifiedtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `QUALIFIED'", 31), DynamicStrings_Mark (str));
        break;

      case mcReserved_proceduretok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `PROCEDURE'", 31), DynamicStrings_Mark (str));
        break;

      case mcReserved_pointertok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `POINTER'", 29), DynamicStrings_Mark (str));
        break;

      case mcReserved_packedsettok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `PACKEDSET'", 31), DynamicStrings_Mark (str));
        break;

      case mcReserved_ortok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `OR'", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_oftok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `OF'", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_nottok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `NOT'", 25), DynamicStrings_Mark (str));
        break;

      case mcReserved_moduletok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `MODULE'", 28), DynamicStrings_Mark (str));
        break;

      case mcReserved_modtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `MOD'", 25), DynamicStrings_Mark (str));
        break;

      case mcReserved_looptok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `LOOP'", 26), DynamicStrings_Mark (str));
        break;

      case mcReserved_intok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `IN'", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_importtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `IMPORT'", 28), DynamicStrings_Mark (str));
        break;

      case mcReserved_implementationtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `IMPLEMENTATION'", 36), DynamicStrings_Mark (str));
        break;

      case mcReserved_iftok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `IF'", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_fromtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `FROM'", 26), DynamicStrings_Mark (str));
        break;

      case mcReserved_fortok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `FOR'", 25), DynamicStrings_Mark (str));
        break;

      case mcReserved_finallytok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `FINALLY'", 29), DynamicStrings_Mark (str));
        break;

      case mcReserved_exporttok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `EXPORT'", 28), DynamicStrings_Mark (str));
        break;

      case mcReserved_exittok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `EXIT'", 26), DynamicStrings_Mark (str));
        break;

      case mcReserved_excepttok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `EXCEPT'", 28), DynamicStrings_Mark (str));
        break;

      case mcReserved_endtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `END'", 25), DynamicStrings_Mark (str));
        break;

      case mcReserved_elsiftok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `ELSIF'", 27), DynamicStrings_Mark (str));
        break;

      case mcReserved_elsetok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `ELSE'", 26), DynamicStrings_Mark (str));
        break;

      case mcReserved_dotok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `DO'", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_divtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `DIV'", 25), DynamicStrings_Mark (str));
        break;

      case mcReserved_definitiontok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `DEFINITION'", 32), DynamicStrings_Mark (str));
        break;

      case mcReserved_consttok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `CONST'", 27), DynamicStrings_Mark (str));
        break;

      case mcReserved_casetok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `CASE'", 26), DynamicStrings_Mark (str));
        break;

      case mcReserved_bytok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `BY'", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_begintok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `BEGIN'", 27), DynamicStrings_Mark (str));
        break;

      case mcReserved_arraytok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `ARRAY'", 27), DynamicStrings_Mark (str));
        break;

      case mcReserved_andtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `AND'", 25), DynamicStrings_Mark (str));
        break;

      case mcReserved_colontok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `:'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_periodperiodtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `..'", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_rdirectivetok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `*>'", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_ldirectivetok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `<*'", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_greaterequaltok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `>='", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_lessequaltok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `<='", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_lessgreatertok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `<>'", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_hashtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `#'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_equaltok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `='", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_uparrowtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `^'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_semicolontok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `;'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_commatok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `,'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_periodtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `.'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_ambersandtok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `&'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_dividetok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `/'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_timestok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `*'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_minustok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `-'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_plustok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `+'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_doublequotestok:
        str = DynamicStrings_ConCat (DynamicStrings_ConCatChar (DynamicStrings_ConCatChar (DynamicStrings_InitString ((char *) "syntax error, found '", 21), '\\'), '\''), DynamicStrings_Mark (str));
        break;

      case mcReserved_singlequotetok:
        str = DynamicStrings_ConCat (DynamicStrings_ConCatChar (DynamicStrings_ConCatChar (DynamicStrings_InitString ((char *) "syntax error, found \"", 21), '\''), '\\'), DynamicStrings_Mark (str));
        break;

      case mcReserved_greatertok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `>'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_lesstok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `<'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_rparatok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `)'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_lparatok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `('", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_rcbratok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `}'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_lcbratok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `{'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_rsbratok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `]'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_lsbratok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `['", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_bartok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `|'", 23), DynamicStrings_Mark (str));
        break;

      case mcReserved_becomestok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `:='", 24), DynamicStrings_Mark (str));
        break;

      case mcReserved_eoftok:
        str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error, found `'", 22), DynamicStrings_Mark (str));
        break;


      default:
        break;
    }
  ErrorString (str);
}

static void SyntaxError (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  DescribeError (stopset0, stopset1, stopset2);
  if (Debugging)
    mcPrintf_printf0 ((char *) "\\", 21);
  while ((((((unsigned int) (mcLexBuf_currenttoken)) < 32) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & (stopset0)) != 0))) || (((((unsigned int) (mcLexBuf_currenttoken)) >= 32) && (((unsigned int) (mcLexBuf_currenttoken)) < 64)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & (stopset1)) != 0)))) || ((((unsigned int) (mcLexBuf_currenttoken)) >= 64) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & (stopset2)) != 0))))
    mcLexBuf_getToken ();
  if (Debugging)
    mcPrintf_printf0 ((char *) " ***\\", 6);
}

static void SyntaxCheck (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((((((unsigned int) (mcLexBuf_currenttoken)) < 32) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & (stopset0)) != 0))) || (((((unsigned int) (mcLexBuf_currenttoken)) >= 32) && (((unsigned int) (mcLexBuf_currenttoken)) < 64)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & (stopset1)) != 0)))) || ((((unsigned int) (mcLexBuf_currenttoken)) >= 64) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & (stopset2)) != 0))))
    SyntaxError (stopset0, stopset1, stopset2);
}

static void WarnMissingToken (mcReserved_toktype t)
{
  SetOfStop0 s0;
  SetOfStop1 s1;
  SetOfStop2 s2;
  DynamicStrings_String str;

  s0 = (SetOfStop0) 0;
  s1 = (SetOfStop1) 0;
  s2 = (SetOfStop2) 0;
  if (((unsigned int) (t)) < 32)
    s0 = (SetOfStop0) ((1 << (t-mcReserved_eoftok)));
  str = DescribeStop (s0, s1, s2);
  str = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "syntax error,", 13), DynamicStrings_Mark (str));
  mcError_errorStringAt (str, mcLexBuf_getTokenNo ());
}

static void MissingToken (mcReserved_toktype t)
{
  WarnMissingToken (t);
  if ((((t != mcReserved_identtok) && (t != mcReserved_integertok)) && (t != mcReserved_realtok)) && (t != mcReserved_stringtok))
    {
      if (Debugging)
        mcPrintf_printf0 ((char *) "inserting token\\", 17);
      mcLexBuf_insertToken (t);
    }
}

static unsigned int CheckAndInsert (mcReserved_toktype t, SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((((((unsigned int) (t)) < 32) && ((((1 << (t-mcReserved_eoftok)) & (stopset0)) != 0))) || (((((unsigned int) (t)) >= 32) && (((unsigned int) (t)) < 64)) && ((((1 << (t-mcReserved_arraytok)) & (stopset1)) != 0)))) || ((((unsigned int) (t)) >= 64) && ((((1 << (t-mcReserved_recordtok)) & (stopset2)) != 0))))
    {
      WarnMissingToken (t);
      mcLexBuf_insertTokenAndRewind (t);
      return TRUE;
    }
  else
    return FALSE;
}

static unsigned int InStopSet (mcReserved_toktype t, SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((((((unsigned int) (t)) < 32) && ((((1 << (t-mcReserved_eoftok)) & (stopset0)) != 0))) || (((((unsigned int) (t)) >= 32) && (((unsigned int) (t)) < 64)) && ((((1 << (t-mcReserved_arraytok)) & (stopset1)) != 0)))) || ((((unsigned int) (t)) >= 64) && ((((1 << (t-mcReserved_recordtok)) & (stopset2)) != 0))))
    return TRUE;
  else
    return FALSE;
}

static void PeepToken (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (((((((unsigned int) (mcLexBuf_currenttoken)) < 32) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & (stopset0)) != 0))) || (((((unsigned int) (mcLexBuf_currenttoken)) >= 32) && (((unsigned int) (mcLexBuf_currenttoken)) < 64)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & (stopset1)) != 0)))) || ((((unsigned int) (mcLexBuf_currenttoken)) >= 64) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & (stopset2)) != 0)))) && (InStopSet ((mcReserved_toktype) mcReserved_identtok, stopset0, stopset1, stopset2)))
    if ((((((((CheckAndInsert ((mcReserved_toktype) mcReserved_semicolontok, stopset0, stopset1, stopset2)) || (CheckAndInsert ((mcReserved_toktype) mcReserved_rsbratok, stopset0, stopset1, stopset2))) || (CheckAndInsert ((mcReserved_toktype) mcReserved_rparatok, stopset0, stopset1, stopset2))) || (CheckAndInsert ((mcReserved_toktype) mcReserved_rcbratok, stopset0, stopset1, stopset2))) || (CheckAndInsert ((mcReserved_toktype) mcReserved_periodtok, stopset0, stopset1, stopset2))) || (CheckAndInsert ((mcReserved_toktype) mcReserved_oftok, stopset0, stopset1, stopset2))) || (CheckAndInsert ((mcReserved_toktype) mcReserved_endtok, stopset0, stopset1, stopset2))) || (CheckAndInsert ((mcReserved_toktype) mcReserved_commatok, stopset0, stopset1, stopset2)))
      ;  /* empty.  */
}

static void Expect (mcReserved_toktype t, SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == t)
    {
      mcLexBuf_getToken ();
      if (Pass1)
        PeepToken (stopset0, stopset1, stopset2);
    }
  else
    MissingToken (t);
  SyntaxCheck (stopset0, stopset1, stopset2);
}

static void Ident (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  curident = nameKey_makekey (mcLexBuf_currentstring);
  Expect ((mcReserved_toktype) mcReserved_identtok, stopset0, stopset1, stopset2);
}

static void string (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_stringtok, stopset0, stopset1, stopset2);
}

static void Integer (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_integertok, stopset0, stopset1, stopset2);
}

static void Real (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_realtok, stopset0, stopset1, stopset2);
}

static void registerImport (nameKey_Name ident, unsigned int scoped)
{
  decl_node n;

  n = decl_lookupDef (ident);
  decl_addImportedModule (decl_getCurrentModule (), n, scoped);
}

static void FileUnit (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_definitiontok)
    DefinitionModule (stopset0, stopset1, stopset2);
}

static void ProgramModule (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_moduletok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1, stopset2);
  curmodule = decl_lookupModule (curident);
  decl_enterScope (curmodule);
  if (mcLexBuf_currenttoken == mcReserved_lsbratok)
    Priority (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_semicolontok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_importtok-mcReserved_arraytok)) | (1 << (mcReserved_fromtok-mcReserved_arraytok)) | (1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_finallytok-mcReserved_arraytok)) | (1 << (mcReserved_begintok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_moduletok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_typetok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))));
  while (((mcLexBuf_currenttoken >= mcReserved_arraytok) && (mcLexBuf_currenttoken < mcReserved_recordtok)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & ((SetOfStop1) ((1 << (mcReserved_fromtok-mcReserved_arraytok)) | (1 << (mcReserved_importtok-mcReserved_arraytok))))) != 0)))
    Import (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_fromtok-mcReserved_arraytok)) | (1 << (mcReserved_importtok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_begintok-mcReserved_arraytok)) | (1 << (mcReserved_finallytok-mcReserved_arraytok)) | (1 << (mcReserved_moduletok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_vartok-mcReserved_recordtok)) | (1 << (mcReserved_typetok-mcReserved_recordtok))));
  Block (stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_periodtok-mcReserved_eoftok))), stopset1, stopset2);
  checkEndName (curmodule, curident, (char *) "program module", 14);
  decl_leaveScope ();
  Expect ((mcReserved_toktype) mcReserved_periodtok, stopset0, stopset1, stopset2);
}

static void ImplementationModule (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_implementationtok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_moduletok-mcReserved_arraytok))), stopset2);
  Expect ((mcReserved_toktype) mcReserved_moduletok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1, stopset2);
  curmodule = decl_lookupImp (curident);
  decl_enterScope (decl_lookupDef (curident));
  decl_enterScope (curmodule);
  if (mcLexBuf_currenttoken == mcReserved_lsbratok)
    Priority (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_semicolontok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_importtok-mcReserved_arraytok)) | (1 << (mcReserved_fromtok-mcReserved_arraytok)) | (1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_finallytok-mcReserved_arraytok)) | (1 << (mcReserved_begintok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_moduletok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_typetok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))));
  while (((mcLexBuf_currenttoken >= mcReserved_arraytok) && (mcLexBuf_currenttoken < mcReserved_recordtok)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & ((SetOfStop1) ((1 << (mcReserved_fromtok-mcReserved_arraytok)) | (1 << (mcReserved_importtok-mcReserved_arraytok))))) != 0)))
    Import (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_fromtok-mcReserved_arraytok)) | (1 << (mcReserved_importtok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_begintok-mcReserved_arraytok)) | (1 << (mcReserved_finallytok-mcReserved_arraytok)) | (1 << (mcReserved_moduletok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_vartok-mcReserved_recordtok)) | (1 << (mcReserved_typetok-mcReserved_recordtok))));
  Block (stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_periodtok-mcReserved_eoftok))), stopset1, stopset2);
  checkEndName (curmodule, curident, (char *) "implementation module", 21);
  decl_leaveScope ();
  decl_leaveScope ();
  Expect ((mcReserved_toktype) mcReserved_periodtok, stopset0, stopset1, stopset2);
}

static void ImplementationOrProgramModule (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_implementationtok)
    ImplementationModule (stopset0, stopset1, stopset2);
}

static void Number (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_integertok)
    Integer (stopset0, stopset1, stopset2);
}

static void Qualident (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_periodtok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_periodtok)
    {
      Expect ((mcReserved_toktype) mcReserved_periodtok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_periodtok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void ConstExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  SimpleConstExpr (stopset0+(SetOfStop0) ((1 << (mcReserved_greaterequaltok-mcReserved_eoftok)) | (1 << (mcReserved_greatertok-mcReserved_eoftok)) | (1 << (mcReserved_lessequaltok-mcReserved_eoftok)) | (1 << (mcReserved_lesstok-mcReserved_eoftok)) | (1 << (mcReserved_lessgreatertok-mcReserved_eoftok)) | (1 << (mcReserved_hashtok-mcReserved_eoftok)) | (1 << (mcReserved_equaltok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_intok-mcReserved_arraytok))), stopset2);
  if (((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_equaltok-mcReserved_eoftok)) | (1 << (mcReserved_hashtok-mcReserved_eoftok)) | (1 << (mcReserved_lessgreatertok-mcReserved_eoftok)) | (1 << (mcReserved_lesstok-mcReserved_eoftok)) | (1 << (mcReserved_lessequaltok-mcReserved_eoftok)) | (1 << (mcReserved_greatertok-mcReserved_eoftok)) | (1 << (mcReserved_greaterequaltok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_intok))
    {
      Relation (stopset0+(SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))));
      SimpleConstExpr (stopset0, stopset1, stopset2);
    }
}

static void Relation (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_equaltok)
    Expect ((mcReserved_toktype) mcReserved_equaltok, stopset0, stopset1, stopset2);
}

static void SimpleConstExpr (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  UnaryOrConstTerm (stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_ortok-mcReserved_arraytok))), stopset2);
  while (((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_ortok))
    {
      AddOperator (stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))));
      ConstTerm (stopset0+(SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_ortok-mcReserved_arraytok))), stopset2);
    }
}

static void UnaryOrConstTerm (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_plustok)
    {
      Expect ((mcReserved_toktype) mcReserved_plustok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))));
      ConstTerm (stopset0, stopset1, stopset2);
    }
}

static void AddOperator (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_plustok)
    Expect ((mcReserved_toktype) mcReserved_plustok, stopset0, stopset1, stopset2);
}

static void ConstTerm (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  ConstFactor (stopset0+(SetOfStop0) ((1 << (mcReserved_ambersandtok-mcReserved_eoftok)) | (1 << (mcReserved_andtok-mcReserved_eoftok)) | (1 << (mcReserved_dividetok-mcReserved_eoftok)) | (1 << (mcReserved_timestok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_modtok-mcReserved_arraytok)) | (1 << (mcReserved_divtok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_remtok-mcReserved_recordtok))));
  while ((((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_timestok-mcReserved_eoftok)) | (1 << (mcReserved_dividetok-mcReserved_eoftok)) | (1 << (mcReserved_andtok-mcReserved_eoftok)) | (1 << (mcReserved_ambersandtok-mcReserved_eoftok))))) != 0))) || (((mcLexBuf_currenttoken >= mcReserved_arraytok) && (mcLexBuf_currenttoken < mcReserved_recordtok)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & ((SetOfStop1) ((1 << (mcReserved_divtok-mcReserved_arraytok)) | (1 << (mcReserved_modtok-mcReserved_arraytok))))) != 0)))) || (mcLexBuf_currenttoken == mcReserved_remtok))
    {
      MulOperator (stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      ConstFactor (stopset0+(SetOfStop0) ((1 << (mcReserved_timestok-mcReserved_eoftok)) | (1 << (mcReserved_dividetok-mcReserved_eoftok)) | (1 << (mcReserved_andtok-mcReserved_eoftok)) | (1 << (mcReserved_ambersandtok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_divtok-mcReserved_arraytok)) | (1 << (mcReserved_modtok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_remtok-mcReserved_recordtok))));
    }
}

static void MulOperator (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_timestok)
    Expect ((mcReserved_toktype) mcReserved_timestok, stopset0, stopset1, stopset2);
}

static void ConstFactor (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok))))) != 0)))
    Number (stopset0, stopset1, stopset2);
}

static void ConstString (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  string (stopset0, stopset1, stopset2);
}

static void ComponentElement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  ConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_periodperiodtok-mcReserved_eoftok))), stopset1, stopset2);
  if (mcLexBuf_currenttoken == mcReserved_periodperiodtok)
    {
      Expect ((mcReserved_toktype) mcReserved_periodperiodtok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      ConstExpression (stopset0, stopset1, stopset2);
    }
}

static void ComponentValue (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  ComponentElement (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_bytok-mcReserved_arraytok))), stopset2);
  if (mcLexBuf_currenttoken == mcReserved_bytok)
    {
      Expect ((mcReserved_toktype) mcReserved_bytok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      ConstExpression (stopset0, stopset1, stopset2);
    }
}

static void ArraySetRecordValue (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  ComponentValue (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      ComponentValue (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void Constructor (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lcbratok, stopset0+(SetOfStop0) ((1 << (mcReserved_rcbratok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))));
  if ((((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_nottok)) || ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))))) != 0))))
    ArraySetRecordValue (stopset0+(SetOfStop0) ((1 << (mcReserved_rcbratok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rcbratok, stopset0, stopset1, stopset2);
}

static void ConstSetOrQualidentOrFunction (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_identtok)
    {
      Qualident (stopset0+(SetOfStop0) ((1 << (mcReserved_lcbratok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
      if ((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))))) != 0)))
        if (mcLexBuf_currenttoken == mcReserved_lcbratok)
          Constructor (stopset0, stopset1, stopset2);
    }
}

static void ConstActualParameters (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  ActualParameters (stopset0, stopset1, stopset2);
}

static void ConstAttribute (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_attributetok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_builtintok-mcReserved_recordtok))));
  Expect ((mcReserved_toktype) mcReserved_builtintok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_lesstok-mcReserved_eoftok))), stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  ConstAttributeExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0, stopset1, stopset2);
}

static void ConstAttributeExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_identtok)
    Ident (stopset0, stopset1, stopset2);
}

static void Element (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  ConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_periodperiodtok-mcReserved_eoftok))), stopset1, stopset2);
  if (mcLexBuf_currenttoken == mcReserved_periodperiodtok)
    {
      Expect ((mcReserved_toktype) mcReserved_periodperiodtok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      ConstExpression (stopset0, stopset1, stopset2);
    }
}

static void ByteAlignment (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_ldirectivetok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  AttributeExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_rdirectivetok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rdirectivetok, stopset0, stopset1, stopset2);
}

static void OptAlignmentExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_lparatok)
    AlignmentExpression (stopset0, stopset1, stopset2);
}

static void AlignmentExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
  ConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0, stopset1, stopset2);
}

static void Alignment (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_ldirectivetok)
    ByteAlignment (stopset0, stopset1, stopset2);
}

static void TypeDeclaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  decl_node n;

  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_equaltok-mcReserved_eoftok))), stopset1, stopset2);
  n = decl_makeTypeImp (curident);
  Expect ((mcReserved_toktype) mcReserved_equaltok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_pointertok-mcReserved_arraytok)) | (1 << (mcReserved_packedsettok-mcReserved_arraytok)) | (1 << (mcReserved_oftok-mcReserved_arraytok)) | (1 << (mcReserved_arraytok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_settok-mcReserved_recordtok)) | (1 << (mcReserved_recordtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  Type (stopset0+(SetOfStop0) ((1 << (mcReserved_ldirectivetok-mcReserved_eoftok))), stopset1, stopset2);
  Alignment (stopset0, stopset1, stopset2);
}

static void Type (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_identtok))
    SimpleType (stopset0, stopset1, stopset2);
}

static void SimpleType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_identtok)
    {
      Qualident (stopset0+(SetOfStop0) ((1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1, stopset2);
      if (mcLexBuf_currenttoken == mcReserved_lsbratok)
        SubrangeType (stopset0, stopset1, stopset2);
    }
}

static void Enumeration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  IdentList (stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0, stopset1, stopset2);
}

static void IdentList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void SubrangeType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lsbratok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
  ConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_periodperiodtok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_periodperiodtok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
  ConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_rsbratok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rsbratok, stopset0, stopset1, stopset2);
}

static void ArrayType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_arraytok, stopset0+(SetOfStop0) ((1 << (mcReserved_lsbratok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  SimpleType (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_oftok-mcReserved_arraytok))), stopset2);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0+(SetOfStop0) ((1 << (mcReserved_lsbratok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      SimpleType (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_oftok-mcReserved_arraytok))), stopset2);
    }
  Expect ((mcReserved_toktype) mcReserved_oftok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_pointertok-mcReserved_arraytok)) | (1 << (mcReserved_packedsettok-mcReserved_arraytok)) | (1 << (mcReserved_oftok-mcReserved_arraytok)) | (1 << (mcReserved_arraytok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_settok-mcReserved_recordtok)) | (1 << (mcReserved_recordtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  Type (stopset0, stopset1, stopset2);
}

static void RecordType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_recordtok, stopset0+(SetOfStop0) ((1 << (mcReserved_ldirectivetok-mcReserved_eoftok)) | (1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  if (mcLexBuf_currenttoken == mcReserved_ldirectivetok)
    DefaultRecordAttributes (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  FieldListSequence (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok))), stopset2);
  Expect ((mcReserved_toktype) mcReserved_endtok, stopset0, stopset1, stopset2);
}

static void DefaultRecordAttributes (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_ldirectivetok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  AttributeExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_rdirectivetok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rdirectivetok, stopset0, stopset1, stopset2);
}

static void RecordFieldPragma (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_ldirectivetok)
    {
      Expect ((mcReserved_toktype) mcReserved_ldirectivetok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      FieldPragmaExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok)) | (1 << (mcReserved_rdirectivetok-mcReserved_eoftok))), stopset1, stopset2);
      while (mcLexBuf_currenttoken == mcReserved_commatok)
        {
          Expect ((mcReserved_toktype) mcReserved_commatok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
          FieldPragmaExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_rdirectivetok-mcReserved_eoftok)) | (1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
        }
      Expect ((mcReserved_toktype) mcReserved_rdirectivetok, stopset0, stopset1, stopset2);
    }
}

static void FieldPragmaExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
  PragmaConstExpression (stopset0, stopset1, stopset2);
}

static void PragmaConstExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_lparatok)
    {
      Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      ConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
      Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0, stopset1, stopset2);
    }
}

static void AttributeExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
  ConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0, stopset1, stopset2);
}

static void FieldListSequence (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  FieldListStatement (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_semicolontok)
    {
      Expect ((mcReserved_toktype) mcReserved_semicolontok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_casetok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      FieldListStatement (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void FieldListStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((mcLexBuf_currenttoken == mcReserved_casetok) || (mcLexBuf_currenttoken == mcReserved_identtok))
    FieldList (stopset0, stopset1, stopset2);
}

static void FieldList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_identtok)
    {
      IdentList (stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
      Expect ((mcReserved_toktype) mcReserved_colontok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_pointertok-mcReserved_arraytok)) | (1 << (mcReserved_packedsettok-mcReserved_arraytok)) | (1 << (mcReserved_oftok-mcReserved_arraytok)) | (1 << (mcReserved_arraytok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_settok-mcReserved_recordtok)) | (1 << (mcReserved_recordtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      Type (stopset0+(SetOfStop0) ((1 << (mcReserved_ldirectivetok-mcReserved_eoftok))), stopset1, stopset2);
      RecordFieldPragma (stopset0, stopset1, stopset2);
    }
}

static void TagIdent (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_identtok)
    Ident (stopset0, stopset1, stopset2);
}

static void CaseTag (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  TagIdent (stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
  if (mcLexBuf_currenttoken == mcReserved_colontok)
    {
      Expect ((mcReserved_toktype) mcReserved_colontok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      Qualident (stopset0, stopset1, stopset2);
    }
}

static void Varient (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_nottok)) || ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))))) != 0))))
    {
      VarientCaseLabelList (stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
      Expect ((mcReserved_toktype) mcReserved_colontok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_casetok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      FieldListSequence (stopset0, stopset1, stopset2);
    }
}

static void VarientCaseLabelList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  VarientCaseLabels (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0+(SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))));
      VarientCaseLabels (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void VarientCaseLabels (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  ConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_periodperiodtok-mcReserved_eoftok))), stopset1, stopset2);
  if (mcLexBuf_currenttoken == mcReserved_periodperiodtok)
    {
      Expect ((mcReserved_toktype) mcReserved_periodperiodtok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      ConstExpression (stopset0, stopset1, stopset2);
    }
}

static void SilentConstExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  SilentSimpleConstExpr (stopset0+(SetOfStop0) ((1 << (mcReserved_greaterequaltok-mcReserved_eoftok)) | (1 << (mcReserved_greatertok-mcReserved_eoftok)) | (1 << (mcReserved_lessequaltok-mcReserved_eoftok)) | (1 << (mcReserved_lesstok-mcReserved_eoftok)) | (1 << (mcReserved_lessgreatertok-mcReserved_eoftok)) | (1 << (mcReserved_hashtok-mcReserved_eoftok)) | (1 << (mcReserved_equaltok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_intok-mcReserved_arraytok))), stopset2);
  if (((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_equaltok-mcReserved_eoftok)) | (1 << (mcReserved_hashtok-mcReserved_eoftok)) | (1 << (mcReserved_lessgreatertok-mcReserved_eoftok)) | (1 << (mcReserved_lesstok-mcReserved_eoftok)) | (1 << (mcReserved_lessequaltok-mcReserved_eoftok)) | (1 << (mcReserved_greatertok-mcReserved_eoftok)) | (1 << (mcReserved_greaterequaltok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_intok))
    {
      SilentRelation (stopset0+(SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))));
      SilentSimpleConstExpr (stopset0, stopset1, stopset2);
    }
}

static void SilentRelation (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_equaltok)
    Expect ((mcReserved_toktype) mcReserved_equaltok, stopset0, stopset1, stopset2);
}

static void SilentSimpleConstExpr (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  SilentUnaryOrConstTerm (stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_ortok-mcReserved_arraytok))), stopset2);
  while (((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_ortok))
    {
      SilentAddOperator (stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))));
      SilentConstTerm (stopset0+(SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_ortok-mcReserved_arraytok))), stopset2);
    }
}

static void SilentUnaryOrConstTerm (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_plustok)
    {
      Expect ((mcReserved_toktype) mcReserved_plustok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))));
      SilentConstTerm (stopset0, stopset1, stopset2);
    }
}

static void SilentAddOperator (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_plustok)
    Expect ((mcReserved_toktype) mcReserved_plustok, stopset0, stopset1, stopset2);
}

static void SilentConstTerm (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  SilentConstFactor (stopset0+(SetOfStop0) ((1 << (mcReserved_ambersandtok-mcReserved_eoftok)) | (1 << (mcReserved_andtok-mcReserved_eoftok)) | (1 << (mcReserved_dividetok-mcReserved_eoftok)) | (1 << (mcReserved_timestok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_modtok-mcReserved_arraytok)) | (1 << (mcReserved_divtok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_remtok-mcReserved_recordtok))));
  while ((((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_timestok-mcReserved_eoftok)) | (1 << (mcReserved_dividetok-mcReserved_eoftok)) | (1 << (mcReserved_andtok-mcReserved_eoftok)) | (1 << (mcReserved_ambersandtok-mcReserved_eoftok))))) != 0))) || (((mcLexBuf_currenttoken >= mcReserved_arraytok) && (mcLexBuf_currenttoken < mcReserved_recordtok)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & ((SetOfStop1) ((1 << (mcReserved_divtok-mcReserved_arraytok)) | (1 << (mcReserved_modtok-mcReserved_arraytok))))) != 0)))) || (mcLexBuf_currenttoken == mcReserved_remtok))
    {
      SilentMulOperator (stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      SilentConstFactor (stopset0+(SetOfStop0) ((1 << (mcReserved_timestok-mcReserved_eoftok)) | (1 << (mcReserved_dividetok-mcReserved_eoftok)) | (1 << (mcReserved_andtok-mcReserved_eoftok)) | (1 << (mcReserved_ambersandtok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_divtok-mcReserved_arraytok)) | (1 << (mcReserved_modtok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_remtok-mcReserved_recordtok))));
    }
}

static void SilentMulOperator (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_timestok)
    Expect ((mcReserved_toktype) mcReserved_timestok, stopset0, stopset1, stopset2);
}

static void SilentConstFactor (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok))))) != 0)))
    Number (stopset0, stopset1, stopset2);
}

static void SilentConstString (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  string (stopset0, stopset1, stopset2);
}

static void SilentConstAttribute (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_attributetok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_builtintok-mcReserved_recordtok))));
  Expect ((mcReserved_toktype) mcReserved_builtintok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_lesstok-mcReserved_eoftok))), stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  SilentConstAttributeExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0, stopset1, stopset2);
}

static void SilentConstAttributeExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_identtok)
    Ident (stopset0, stopset1, stopset2);
}

static void SilentComponentElement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  SilentConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_periodperiodtok-mcReserved_eoftok))), stopset1, stopset2);
  if (mcLexBuf_currenttoken == mcReserved_periodperiodtok)
    {
      Expect ((mcReserved_toktype) mcReserved_periodperiodtok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      SilentConstExpression (stopset0, stopset1, stopset2);
    }
}

static void SilentComponentValue (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  SilentComponentElement (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_bytok-mcReserved_arraytok))), stopset2);
  if (mcLexBuf_currenttoken == mcReserved_bytok)
    {
      Expect ((mcReserved_toktype) mcReserved_bytok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      SilentConstExpression (stopset0, stopset1, stopset2);
    }
}

static void SilentArraySetRecordValue (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  SilentComponentValue (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      SilentComponentValue (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void SilentConstructor (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lcbratok, stopset0+(SetOfStop0) ((1 << (mcReserved_rcbratok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))));
  if ((((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_nottok)) || ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))))) != 0))))
    SilentArraySetRecordValue (stopset0+(SetOfStop0) ((1 << (mcReserved_rcbratok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rcbratok, stopset0, stopset1, stopset2);
}

static void SilentConstSetOrQualidentOrFunction (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_lcbratok)
    SilentConstructor (stopset0, stopset1, stopset2);
}

static void SilentElement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  SilentConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_periodperiodtok-mcReserved_eoftok))), stopset1, stopset2);
  if (mcLexBuf_currenttoken == mcReserved_periodperiodtok)
    {
      Expect ((mcReserved_toktype) mcReserved_periodperiodtok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      SilentConstExpression (stopset0, stopset1, stopset2);
    }
}

static void SilentActualParameters (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))));
  if ((((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_nottok)) || ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))))) != 0))))
    SilentExpList (stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0, stopset1, stopset2);
}

static void SilentExpList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  SilentConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      SilentConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void SetType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_settok)
    Expect ((mcReserved_toktype) mcReserved_settok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_oftok-mcReserved_arraytok))), stopset2);
  Expect ((mcReserved_toktype) mcReserved_oftok, stopset0+(SetOfStop0) ((1 << (mcReserved_lsbratok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  SimpleType (stopset0, stopset1, stopset2);
}

static void PointerType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_pointertok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_totok-mcReserved_recordtok))));
  Expect ((mcReserved_toktype) mcReserved_totok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_pointertok-mcReserved_arraytok)) | (1 << (mcReserved_packedsettok-mcReserved_arraytok)) | (1 << (mcReserved_oftok-mcReserved_arraytok)) | (1 << (mcReserved_arraytok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_settok-mcReserved_recordtok)) | (1 << (mcReserved_recordtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  Type (stopset0, stopset1, stopset2);
}

static void ProcedureType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_proceduretok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
  if (mcLexBuf_currenttoken == mcReserved_lparatok)
    FormalTypeList (stopset0, stopset1, stopset2);
}

static void FormalTypeList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_arraytok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_periodperiodperiodtok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  if (mcLexBuf_currenttoken == mcReserved_rparatok)
    {
      Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
      FormalReturn (stopset0, stopset1, stopset2);
    }
}

static void FormalReturn (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_colontok)
    {
      Expect ((mcReserved_toktype) mcReserved_colontok, stopset0+(SetOfStop0) ((1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      OptReturnType (stopset0, stopset1, stopset2);
    }
}

static void OptReturnType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_lsbratok)
    {
      Expect ((mcReserved_toktype) mcReserved_lsbratok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      Qualident (stopset0+(SetOfStop0) ((1 << (mcReserved_rsbratok-mcReserved_eoftok))), stopset1, stopset2);
      Expect ((mcReserved_toktype) mcReserved_rsbratok, stopset0, stopset1, stopset2);
    }
}

static void ProcedureParameters (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  ProcedureParameter (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_arraytok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_vartok-mcReserved_recordtok)) | (1 << (mcReserved_periodperiodperiodtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      ProcedureParameter (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void ProcedureParameter (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_periodperiodperiodtok)
    Expect ((mcReserved_toktype) mcReserved_periodperiodperiodtok, stopset0, stopset1, stopset2);
}

static void VarIdent (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  decl_node n;

  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1, stopset2);
  n = decl_makeVar (curident);
  if (mcLexBuf_currenttoken == mcReserved_lsbratok)
    {
      Expect ((mcReserved_toktype) mcReserved_lsbratok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      ConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_rsbratok-mcReserved_eoftok))), stopset1, stopset2);
      Expect ((mcReserved_toktype) mcReserved_rsbratok, stopset0, stopset1, stopset2);
    }
}

static void VarIdentList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  VarIdent (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      VarIdent (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void VariableDeclaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  VarIdentList (stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_colontok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_pointertok-mcReserved_arraytok)) | (1 << (mcReserved_packedsettok-mcReserved_arraytok)) | (1 << (mcReserved_oftok-mcReserved_arraytok)) | (1 << (mcReserved_arraytok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_settok-mcReserved_recordtok)) | (1 << (mcReserved_recordtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  Type (stopset0+(SetOfStop0) ((1 << (mcReserved_ldirectivetok-mcReserved_eoftok))), stopset1, stopset2);
  Alignment (stopset0, stopset1, stopset2);
}

static void Designator (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Qualident (stopset0+(SetOfStop0) ((1 << (mcReserved_uparrowtok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok)) | (1 << (mcReserved_periodtok-mcReserved_eoftok))), stopset1, stopset2);
  while ((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_periodtok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok)) | (1 << (mcReserved_uparrowtok-mcReserved_eoftok))))) != 0)))
    SubDesignator (stopset0+(SetOfStop0) ((1 << (mcReserved_periodtok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok)) | (1 << (mcReserved_uparrowtok-mcReserved_eoftok))), stopset1, stopset2);
}

static void SubDesignator (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_periodtok)
    {
      Expect ((mcReserved_toktype) mcReserved_periodtok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      Ident (stopset0, stopset1, stopset2);
    }
}

static void ArrayExpList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expression (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      Expression (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void ExpList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expression (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      Expression (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void Expression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  SimpleExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_greaterequaltok-mcReserved_eoftok)) | (1 << (mcReserved_greatertok-mcReserved_eoftok)) | (1 << (mcReserved_lessequaltok-mcReserved_eoftok)) | (1 << (mcReserved_lesstok-mcReserved_eoftok)) | (1 << (mcReserved_lessgreatertok-mcReserved_eoftok)) | (1 << (mcReserved_hashtok-mcReserved_eoftok)) | (1 << (mcReserved_equaltok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_intok-mcReserved_arraytok))), stopset2);
  if (((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_equaltok-mcReserved_eoftok)) | (1 << (mcReserved_hashtok-mcReserved_eoftok)) | (1 << (mcReserved_lessgreatertok-mcReserved_eoftok)) | (1 << (mcReserved_lesstok-mcReserved_eoftok)) | (1 << (mcReserved_lessequaltok-mcReserved_eoftok)) | (1 << (mcReserved_greatertok-mcReserved_eoftok)) | (1 << (mcReserved_greaterequaltok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_intok))
    {
      Relation (stopset0+(SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      SimpleExpression (stopset0, stopset1, stopset2);
    }
}

static void SimpleExpression (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  UnaryOrTerm (stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_ortok-mcReserved_arraytok))), stopset2);
  while (((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_ortok))
    {
      AddOperator (stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      Term (stopset0+(SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_ortok-mcReserved_arraytok))), stopset2);
    }
}

static void UnaryOrTerm (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_plustok)
    {
      Expect ((mcReserved_toktype) mcReserved_plustok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      Term (stopset0, stopset1, stopset2);
    }
}

static void Term (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Factor (stopset0+(SetOfStop0) ((1 << (mcReserved_ambersandtok-mcReserved_eoftok)) | (1 << (mcReserved_andtok-mcReserved_eoftok)) | (1 << (mcReserved_dividetok-mcReserved_eoftok)) | (1 << (mcReserved_timestok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_modtok-mcReserved_arraytok)) | (1 << (mcReserved_divtok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_remtok-mcReserved_recordtok))));
  while ((((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_timestok-mcReserved_eoftok)) | (1 << (mcReserved_dividetok-mcReserved_eoftok)) | (1 << (mcReserved_andtok-mcReserved_eoftok)) | (1 << (mcReserved_ambersandtok-mcReserved_eoftok))))) != 0))) || (((mcLexBuf_currenttoken >= mcReserved_arraytok) && (mcLexBuf_currenttoken < mcReserved_recordtok)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & ((SetOfStop1) ((1 << (mcReserved_divtok-mcReserved_arraytok)) | (1 << (mcReserved_modtok-mcReserved_arraytok))))) != 0)))) || (mcLexBuf_currenttoken == mcReserved_remtok))
    {
      MulOperator (stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      Factor (stopset0+(SetOfStop0) ((1 << (mcReserved_timestok-mcReserved_eoftok)) | (1 << (mcReserved_dividetok-mcReserved_eoftok)) | (1 << (mcReserved_andtok-mcReserved_eoftok)) | (1 << (mcReserved_ambersandtok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_divtok-mcReserved_arraytok)) | (1 << (mcReserved_modtok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_remtok-mcReserved_recordtok))));
    }
}

static void Factor (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok))))) != 0)))
    Number (stopset0, stopset1, stopset2);
}

static void SetOrDesignatorOrFunction (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_identtok)
    {
      Qualident (stopset0+(SetOfStop0) ((1 << (mcReserved_lcbratok-mcReserved_eoftok)) | (1 << (mcReserved_periodtok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok)) | (1 << (mcReserved_uparrowtok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
      if ((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_uparrowtok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok)) | (1 << (mcReserved_periodtok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))))) != 0)))
        if (mcLexBuf_currenttoken == mcReserved_lcbratok)
          Constructor (stopset0, stopset1, stopset2);
    }
}

static void SimpleDes (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  while ((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_periodtok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok)) | (1 << (mcReserved_uparrowtok-mcReserved_eoftok))))) != 0)))
    SubDesignator (stopset0+(SetOfStop0) ((1 << (mcReserved_periodtok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok)) | (1 << (mcReserved_uparrowtok-mcReserved_eoftok))), stopset1, stopset2);
}

static void ActualParameters (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  if ((((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_nottok)) || ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))))) != 0))))
    ExpList (stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0, stopset1, stopset2);
}

static void ExitStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_exittok, stopset0, stopset1, stopset2);
}

static void ReturnStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_returntok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  if ((((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_nottok)) || ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))))) != 0))))
    Expression (stopset0, stopset1, stopset2);
}

static void Statement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((((mcLexBuf_currenttoken >= mcReserved_arraytok) && (mcLexBuf_currenttoken < mcReserved_recordtok)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & ((SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok))))) != 0))) || ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_retrytok-mcReserved_recordtok)) | (1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))))) != 0))))
    if (mcLexBuf_currenttoken == mcReserved_identtok)
      AssignmentOrProcedureCall (stopset0, stopset1, stopset2);
}

static void RetryStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_retrytok, stopset0, stopset1, stopset2);
}

static void AssignmentOrProcedureCall (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Designator (stopset0+(SetOfStop0) ((1 << (mcReserved_becomestok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
  if (mcLexBuf_currenttoken == mcReserved_becomestok)
    {
      Expect ((mcReserved_toktype) mcReserved_becomestok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      Expression (stopset0, stopset1, stopset2);
    }
}

static void StatementSequence (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Statement (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_semicolontok)
    {
      Expect ((mcReserved_toktype) mcReserved_semicolontok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      Statement (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void IfStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_iftok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  Expression (stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_thentok-mcReserved_recordtok))));
  Expect ((mcReserved_toktype) mcReserved_thentok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_elsiftok-mcReserved_arraytok)) | (1 << (mcReserved_elsetok-mcReserved_arraytok)) | (1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok))));
  StatementSequence (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_elsiftok-mcReserved_arraytok)) | (1 << (mcReserved_elsetok-mcReserved_arraytok)) | (1 << (mcReserved_endtok-mcReserved_arraytok))), stopset2);
  while (mcLexBuf_currenttoken == mcReserved_elsiftok)
    {
      Expect ((mcReserved_toktype) mcReserved_elsiftok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      Expression (stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_thentok-mcReserved_recordtok))));
      Expect ((mcReserved_toktype) mcReserved_thentok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_elsetok-mcReserved_arraytok)) | (1 << (mcReserved_elsiftok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok))));
      StatementSequence (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_elsetok-mcReserved_arraytok)) | (1 << (mcReserved_elsiftok-mcReserved_arraytok))), stopset2);
    }
  if (mcLexBuf_currenttoken == mcReserved_elsetok)
    {
      Expect ((mcReserved_toktype) mcReserved_elsetok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok))));
      StatementSequence (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok))), stopset2);
    }
  Expect ((mcReserved_toktype) mcReserved_endtok, stopset0, stopset1, stopset2);
}

static void CaseStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_casetok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  Expression (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_oftok-mcReserved_arraytok))), stopset2);
  Expect ((mcReserved_toktype) mcReserved_oftok, stopset0+(SetOfStop0) ((1 << (mcReserved_bartok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_elsetok-mcReserved_arraytok)) | (1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))));
  Case (stopset0+(SetOfStop0) ((1 << (mcReserved_bartok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_elsetok-mcReserved_arraytok)) | (1 << (mcReserved_endtok-mcReserved_arraytok))), stopset2);
  while (mcLexBuf_currenttoken == mcReserved_bartok)
    {
      Expect ((mcReserved_toktype) mcReserved_bartok, stopset0+(SetOfStop0) ((1 << (mcReserved_bartok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_elsetok-mcReserved_arraytok)) | (1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))));
      Case (stopset0+(SetOfStop0) ((1 << (mcReserved_bartok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_elsetok-mcReserved_arraytok))), stopset2);
    }
  CaseEndStatement (stopset0, stopset1, stopset2);
}

static void CaseEndStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_endtok)
    Expect ((mcReserved_toktype) mcReserved_endtok, stopset0, stopset1, stopset2);
}

static void Case (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((((mcLexBuf_currenttoken < mcReserved_arraytok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_eoftok)) & ((SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))))) != 0))) || (mcLexBuf_currenttoken == mcReserved_nottok)) || ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))))) != 0))))
    {
      CaseLabelList (stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
      Expect ((mcReserved_toktype) mcReserved_colontok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok))));
      StatementSequence (stopset0, stopset1, stopset2);
    }
}

static void CaseLabelList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  CaseLabels (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0+(SetOfStop0) ((1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))));
      CaseLabels (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void CaseLabels (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  ConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_periodperiodtok-mcReserved_eoftok))), stopset1, stopset2);
  if (mcLexBuf_currenttoken == mcReserved_periodperiodtok)
    {
      Expect ((mcReserved_toktype) mcReserved_periodperiodtok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      ConstExpression (stopset0, stopset1, stopset2);
    }
}

static void WhileStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_whiletok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  Expression (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_dotok-mcReserved_arraytok))), stopset2);
  Expect ((mcReserved_toktype) mcReserved_dotok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok))));
  StatementSequence (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok))), stopset2);
  Expect ((mcReserved_toktype) mcReserved_endtok, stopset0, stopset1, stopset2);
}

static void RepeatStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_repeattok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_untiltok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok))));
  StatementSequence (stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_untiltok-mcReserved_recordtok))));
  Expect ((mcReserved_toktype) mcReserved_untiltok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  Expression (stopset0, stopset1, stopset2);
}

static void ForStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_fortok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_becomestok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_becomestok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  Expression (stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_totok-mcReserved_recordtok))));
  Expect ((mcReserved_toktype) mcReserved_totok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  Expression (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_bytok-mcReserved_arraytok)) | (1 << (mcReserved_dotok-mcReserved_arraytok))), stopset2);
  if (mcLexBuf_currenttoken == mcReserved_bytok)
    {
      Expect ((mcReserved_toktype) mcReserved_bytok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      ConstExpression (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_dotok-mcReserved_arraytok))), stopset2);
    }
  Expect ((mcReserved_toktype) mcReserved_dotok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok))));
  StatementSequence (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok))), stopset2);
  Expect ((mcReserved_toktype) mcReserved_endtok, stopset0, stopset1, stopset2);
}

static void LoopStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_looptok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok))));
  StatementSequence (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok))), stopset2);
  Expect ((mcReserved_toktype) mcReserved_endtok, stopset0, stopset1, stopset2);
}

static void WithStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_withtok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Designator (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_dotok-mcReserved_arraytok))), stopset2);
  Expect ((mcReserved_toktype) mcReserved_dotok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok))));
  StatementSequence (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok))), stopset2);
  Expect ((mcReserved_toktype) mcReserved_endtok, stopset0, stopset1, stopset2);
}

static void ProcedureDeclaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  ProcedureHeading (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_semicolontok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_begintok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_moduletok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_typetok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))));
  ProcedureBlock (stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Ident (stopset0, stopset1, stopset2);
}

static void DefProcedureIdent (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Ident (stopset0, stopset1, stopset2);
  curproc = decl_makeProcedure (curident);
}

static void ProcedureIdent (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Ident (stopset0, stopset1, stopset2);
  curproc = decl_lookupSym (curident);
  if (curproc == NULL)
    curproc = decl_makeProcedure (curident);
}

static void DefineBuiltinProcedure (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_inlinetok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok))))) != 0)))
    if (mcLexBuf_currenttoken == mcReserved_attributetok)
      {
        Expect ((mcReserved_toktype) mcReserved_attributetok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_builtintok-mcReserved_recordtok))));
        Expect ((mcReserved_toktype) mcReserved_builtintok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
        Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
        Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
        Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
        Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
        Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0, stopset1, stopset2);
      }
}

static void ProcedureHeading (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_proceduretok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_inlinetok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  DefineBuiltinProcedure (stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  ProcedureIdent (stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
  decl_enterScope (curproc);
  if (mcLexBuf_currenttoken == mcReserved_lparatok)
    FormalParameters (stopset0, stopset1, stopset2);
}

static void Builtin (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_inlinetok-mcReserved_recordtok)) | (1 << (mcReserved_builtintok-mcReserved_recordtok))))) != 0)))
    if (mcLexBuf_currenttoken == mcReserved_builtintok)
      Expect ((mcReserved_toktype) mcReserved_builtintok, stopset0, stopset1, stopset2);
}

static void DefProcedureHeading (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_proceduretok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_inlinetok-mcReserved_recordtok)) | (1 << (mcReserved_builtintok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  Builtin (stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  DefProcedureIdent (stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
  if (mcLexBuf_currenttoken == mcReserved_lparatok)
    DefFormalParameters (stopset0, stopset1, stopset2);
}

static void ProcedureBlock (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  while ((((mcLexBuf_currenttoken >= mcReserved_arraytok) && (mcLexBuf_currenttoken < mcReserved_recordtok)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & ((SetOfStop1) ((1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_moduletok-mcReserved_arraytok))))) != 0))) || ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_typetok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))))) != 0))))
    Declaration (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_begintok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_moduletok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_typetok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))));
  if (mcLexBuf_currenttoken == mcReserved_begintok)
    {
      Expect ((mcReserved_toktype) mcReserved_begintok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_excepttok-mcReserved_arraytok)) | (1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok))));
      ProcedureBlockBody (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok))), stopset2);
    }
  Expect ((mcReserved_toktype) mcReserved_endtok, stopset0, stopset1, stopset2);
  decl_leaveScope ();
}

static void Block (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  while ((((mcLexBuf_currenttoken >= mcReserved_arraytok) && (mcLexBuf_currenttoken < mcReserved_recordtok)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & ((SetOfStop1) ((1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_moduletok-mcReserved_arraytok))))) != 0))) || ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_typetok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))))) != 0))))
    Declaration (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_finallytok-mcReserved_arraytok)) | (1 << (mcReserved_begintok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_moduletok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_typetok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))));
  InitialBlock (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_finallytok-mcReserved_arraytok))), stopset2);
  FinalBlock (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok))), stopset2);
  Expect ((mcReserved_toktype) mcReserved_endtok, stopset0, stopset1, stopset2);
}

static void InitialBlock (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_begintok)
    {
      Expect ((mcReserved_toktype) mcReserved_begintok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_excepttok-mcReserved_arraytok)) | (1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok))));
      InitialBlockBody (stopset0, stopset1, stopset2);
    }
}

static void FinalBlock (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_finallytok)
    {
      Expect ((mcReserved_toktype) mcReserved_finallytok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_excepttok-mcReserved_arraytok)) | (1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok))));
      FinalBlockBody (stopset0, stopset1, stopset2);
    }
}

static void InitialBlockBody (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  NormalPart (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_excepttok-mcReserved_arraytok))), stopset2);
  if (mcLexBuf_currenttoken == mcReserved_excepttok)
    {
      Expect ((mcReserved_toktype) mcReserved_excepttok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      ExceptionalPart (stopset0, stopset1, stopset2);
    }
}

static void FinalBlockBody (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  NormalPart (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_excepttok-mcReserved_arraytok))), stopset2);
  if (mcLexBuf_currenttoken == mcReserved_excepttok)
    {
      Expect ((mcReserved_toktype) mcReserved_excepttok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      ExceptionalPart (stopset0, stopset1, stopset2);
    }
}

static void ProcedureBlockBody (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  NormalPart (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_excepttok-mcReserved_arraytok))), stopset2);
  if (mcLexBuf_currenttoken == mcReserved_excepttok)
    {
      Expect ((mcReserved_toktype) mcReserved_excepttok, stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_exittok-mcReserved_arraytok)) | (1 << (mcReserved_fortok-mcReserved_arraytok)) | (1 << (mcReserved_looptok-mcReserved_arraytok)) | (1 << (mcReserved_casetok-mcReserved_arraytok)) | (1 << (mcReserved_iftok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_returntok-mcReserved_recordtok)) | (1 << (mcReserved_retrytok-mcReserved_recordtok)) | (1 << (mcReserved_asmtok-mcReserved_recordtok)) | (1 << (mcReserved_withtok-mcReserved_recordtok)) | (1 << (mcReserved_repeattok-mcReserved_recordtok)) | (1 << (mcReserved_whiletok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
      ExceptionalPart (stopset0, stopset1, stopset2);
    }
}

static void NormalPart (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  StatementSequence (stopset0, stopset1, stopset2);
}

static void ExceptionalPart (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  StatementSequence (stopset0, stopset1, stopset2);
}

static void Declaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_consttok)
    {
      Expect ((mcReserved_toktype) mcReserved_consttok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      while (mcLexBuf_currenttoken == mcReserved_identtok)
        {
          ConstantDeclaration (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
          Expect ((mcReserved_toktype) mcReserved_semicolontok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
        }
    }
}

static void DefFormalParameters (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_periodperiodperiodtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))));
  if ((mcLexBuf_currenttoken == mcReserved_lsbratok) || ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_periodperiodperiodtok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))))) != 0))))
    DefMultiFPSection (stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
  FormalReturn (stopset0, stopset1, stopset2);
}

static void DefMultiFPSection (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((mcLexBuf_currenttoken == mcReserved_lsbratok) || (mcLexBuf_currenttoken == mcReserved_periodperiodperiodtok))
    DefExtendedFP (stopset0, stopset1, stopset2);
}

static void FormalParameters (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_periodperiodperiodtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))));
  if ((mcLexBuf_currenttoken == mcReserved_lsbratok) || ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_periodperiodperiodtok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))))) != 0))))
    MultiFPSection (stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
  FormalReturn (stopset0, stopset1, stopset2);
}

static void MultiFPSection (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((mcLexBuf_currenttoken == mcReserved_lsbratok) || (mcLexBuf_currenttoken == mcReserved_periodperiodperiodtok))
    ExtendedFP (stopset0, stopset1, stopset2);
}

static void FPSection (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_identtok)
    NonVarFPSection (stopset0, stopset1, stopset2);
}

static void DefExtendedFP (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_lsbratok)
    DefOptArg (stopset0, stopset1, stopset2);
}

static void ExtendedFP (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_lsbratok)
    OptArg (stopset0, stopset1, stopset2);
}

static void VarFPSection (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_vartok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  IdentList (stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_colontok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_arraytok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  FormalType (stopset0, stopset1, stopset2);
}

static void NonVarFPSection (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  IdentList (stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_colontok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_arraytok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  FormalType (stopset0, stopset1, stopset2);
}

static void OptArg (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lsbratok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_colontok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_arraytok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  FormalType (stopset0+(SetOfStop0) ((1 << (mcReserved_equaltok-mcReserved_eoftok)) | (1 << (mcReserved_rsbratok-mcReserved_eoftok))), stopset1, stopset2);
  if (mcLexBuf_currenttoken == mcReserved_equaltok)
    {
      Expect ((mcReserved_toktype) mcReserved_equaltok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
      ConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_rsbratok-mcReserved_eoftok))), stopset1, stopset2);
    }
  Expect ((mcReserved_toktype) mcReserved_rsbratok, stopset0, stopset1, stopset2);
}

static void DefOptArg (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lsbratok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_colontok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_arraytok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  FormalType (stopset0+(SetOfStop0) ((1 << (mcReserved_equaltok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_equaltok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
  ConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_rsbratok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rsbratok, stopset0, stopset1, stopset2);
}

static void FormalType (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  while (mcLexBuf_currenttoken == mcReserved_arraytok)
    {
      Expect ((mcReserved_toktype) mcReserved_arraytok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_oftok-mcReserved_arraytok))), stopset2);
      Expect ((mcReserved_toktype) mcReserved_oftok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_arraytok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
    }
  Qualident (stopset0, stopset1, stopset2);
}

static void ModuleDeclaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_moduletok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1, stopset2);
  if (mcLexBuf_currenttoken == mcReserved_lsbratok)
    Priority (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_semicolontok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_importtok-mcReserved_arraytok)) | (1 << (mcReserved_fromtok-mcReserved_arraytok)) | (1 << (mcReserved_exporttok-mcReserved_arraytok)) | (1 << (mcReserved_finallytok-mcReserved_arraytok)) | (1 << (mcReserved_begintok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_moduletok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_typetok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))));
  while (((mcLexBuf_currenttoken >= mcReserved_arraytok) && (mcLexBuf_currenttoken < mcReserved_recordtok)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & ((SetOfStop1) ((1 << (mcReserved_fromtok-mcReserved_arraytok)) | (1 << (mcReserved_importtok-mcReserved_arraytok))))) != 0)))
    Import (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_begintok-mcReserved_arraytok)) | (1 << (mcReserved_finallytok-mcReserved_arraytok)) | (1 << (mcReserved_exporttok-mcReserved_arraytok)) | (1 << (mcReserved_fromtok-mcReserved_arraytok)) | (1 << (mcReserved_importtok-mcReserved_arraytok)) | (1 << (mcReserved_moduletok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_vartok-mcReserved_recordtok)) | (1 << (mcReserved_typetok-mcReserved_recordtok))));
  if (mcLexBuf_currenttoken == mcReserved_exporttok)
    Export (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_begintok-mcReserved_arraytok)) | (1 << (mcReserved_finallytok-mcReserved_arraytok)) | (1 << (mcReserved_moduletok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_vartok-mcReserved_recordtok)) | (1 << (mcReserved_typetok-mcReserved_recordtok))));
  Block (stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Ident (stopset0, stopset1, stopset2);
}

static void Priority (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lsbratok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
  ConstExpression (stopset0+(SetOfStop0) ((1 << (mcReserved_rsbratok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rsbratok, stopset0, stopset1, stopset2);
}

static void Export (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_exporttok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_qualifiedtok-mcReserved_arraytok)) | (1 << (mcReserved_unqualifiedtok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  if (mcLexBuf_currenttoken == mcReserved_qualifiedtok)
    {
      Expect ((mcReserved_toktype) mcReserved_qualifiedtok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      IdentList (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
    }
  Expect ((mcReserved_toktype) mcReserved_semicolontok, stopset0, stopset1, stopset2);
}

static void FromImport (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_fromtok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Ident (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_importtok-mcReserved_arraytok))), stopset2);
  registerImport (curident, FALSE);
  Expect ((mcReserved_toktype) mcReserved_importtok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  IdentList (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_semicolontok, stopset0, stopset1, stopset2);
}

static void ImportModuleList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
  registerImport (curident, TRUE);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
      registerImport (curident, TRUE);
    }
}

static void WithoutFromImport (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_importtok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  ImportModuleList (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_semicolontok, stopset0, stopset1, stopset2);
}

static void Import (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_fromtok)
    FromImport (stopset0, stopset1, stopset2);
}

static void DefinitionModule (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  unsigned int c;

  c = FALSE;
  Expect ((mcReserved_toktype) mcReserved_definitiontok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_moduletok-mcReserved_arraytok))), stopset2);
  Expect ((mcReserved_toktype) mcReserved_moduletok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_fortok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  if (mcLexBuf_currenttoken == mcReserved_fortok)
    {
      Expect ((mcReserved_toktype) mcReserved_fortok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok))));
      string (stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      c = TRUE;
    }
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_semicolontok, stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_importtok-mcReserved_arraytok)) | (1 << (mcReserved_fromtok-mcReserved_arraytok)) | (1 << (mcReserved_exporttok-mcReserved_arraytok)) | (1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_vartok-mcReserved_recordtok)) | (1 << (mcReserved_typetok-mcReserved_recordtok))));
  curmodule = decl_lookupDef (curident);
  if (c)
    decl_putDefForC (curmodule);
  decl_enterScope (curmodule);
  while (((mcLexBuf_currenttoken >= mcReserved_arraytok) && (mcLexBuf_currenttoken < mcReserved_recordtok)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & ((SetOfStop1) ((1 << (mcReserved_fromtok-mcReserved_arraytok)) | (1 << (mcReserved_importtok-mcReserved_arraytok))))) != 0)))
    Import (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_exporttok-mcReserved_arraytok)) | (1 << (mcReserved_fromtok-mcReserved_arraytok)) | (1 << (mcReserved_importtok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_typetok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))));
  if (mcLexBuf_currenttoken == mcReserved_exporttok)
    Export (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_typetok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))));
  while ((((mcLexBuf_currenttoken >= mcReserved_arraytok) && (mcLexBuf_currenttoken < mcReserved_recordtok)) && ((((1 << (mcLexBuf_currenttoken-mcReserved_arraytok)) & ((SetOfStop1) ((1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok))))) != 0))) || ((mcLexBuf_currenttoken >= mcReserved_recordtok) && ((((1 << (mcLexBuf_currenttoken-mcReserved_recordtok)) & ((SetOfStop2) ((1 << (mcReserved_typetok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))))) != 0))))
    Definition (stopset0, stopset1+(SetOfStop1) ((1 << (mcReserved_endtok-mcReserved_arraytok)) | (1 << (mcReserved_consttok-mcReserved_arraytok)) | (1 << (mcReserved_proceduretok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_typetok-mcReserved_recordtok)) | (1 << (mcReserved_vartok-mcReserved_recordtok))));
  Expect ((mcReserved_toktype) mcReserved_endtok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_periodtok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_periodtok, stopset0, stopset1, stopset2);
  checkEndName (curmodule, curident, (char *) "definition module", 17);
  decl_leaveScope ();
}

static void DefTypeDeclaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  decl_node n;

  while (mcLexBuf_currenttoken == mcReserved_identtok)
    {
      Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok)) | (1 << (mcReserved_equaltok-mcReserved_eoftok))), stopset1, stopset2);
      n = decl_makeType (curident);
      if (mcLexBuf_currenttoken == mcReserved_semicolontok)
        {
          Expect ((mcReserved_toktype) mcReserved_semicolontok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
          decl_putTypeHidden (n);
        }
    }
}

static void ConstantDeclaration (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  decl_node n;

  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_equaltok-mcReserved_eoftok))), stopset1, stopset2);
  n = decl_makeConst (curident);
  Expect ((mcReserved_toktype) mcReserved_equaltok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_attributetok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok)) | (1 << (mcReserved_stringtok-mcReserved_recordtok))));
  ConstExpression (stopset0, stopset1, stopset2);
}

static void Definition (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_consttok)
    {
      Expect ((mcReserved_toktype) mcReserved_consttok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
      while (mcLexBuf_currenttoken == mcReserved_identtok)
        {
          ConstantDeclaration (stopset0+(SetOfStop0) ((1 << (mcReserved_semicolontok-mcReserved_eoftok))), stopset1, stopset2);
          Expect ((mcReserved_toktype) mcReserved_semicolontok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
        }
    }
}

static void AsmStatement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_asmtok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_volatiletok-mcReserved_recordtok))));
  if (mcLexBuf_currenttoken == mcReserved_volatiletok)
    Expect ((mcReserved_toktype) mcReserved_volatiletok, stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok))));
  AsmOperands (stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0, stopset1, stopset2);
}

static void AsmOperands (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  string (stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
  if (mcLexBuf_currenttoken == mcReserved_colontok)
    AsmOperandSpec (stopset0, stopset1, stopset2);
}

static void AsmOperandSpec (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_colontok)
    {
      Expect ((mcReserved_toktype) mcReserved_colontok, stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok)) | (1 << (mcReserved_commatok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok))));
      AsmList (stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
      if (mcLexBuf_currenttoken == mcReserved_colontok)
        {
          Expect ((mcReserved_toktype) mcReserved_colontok, stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok)) | (1 << (mcReserved_commatok-mcReserved_eoftok)) | (1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok))));
          AsmList (stopset0+(SetOfStop0) ((1 << (mcReserved_colontok-mcReserved_eoftok))), stopset1, stopset2);
          if (mcLexBuf_currenttoken == mcReserved_colontok)
            {
              Expect ((mcReserved_toktype) mcReserved_colontok, stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok))));
              TrashList (stopset0, stopset1, stopset2);
            }
        }
    }
}

static void AsmList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if ((mcLexBuf_currenttoken == mcReserved_lsbratok) || (mcLexBuf_currenttoken == mcReserved_stringtok))
    AsmElement (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0+(SetOfStop0) ((1 << (mcReserved_lsbratok-mcReserved_eoftok))), stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok))));
      AsmElement (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

static void NamedOperand (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  Expect ((mcReserved_toktype) mcReserved_lsbratok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_identtok-mcReserved_recordtok))));
  Ident (stopset0+(SetOfStop0) ((1 << (mcReserved_rsbratok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rsbratok, stopset0, stopset1, stopset2);
}

static void AsmOperandName (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_lsbratok)
    NamedOperand (stopset0, stopset1, stopset2);
}

static void AsmElement (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  AsmOperandName (stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok))));
  string (stopset0+(SetOfStop0) ((1 << (mcReserved_lparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_lparatok, stopset0+(SetOfStop0) ((1 << (mcReserved_minustok-mcReserved_eoftok)) | (1 << (mcReserved_plustok-mcReserved_eoftok)) | (1 << (mcReserved_lparatok-mcReserved_eoftok)) | (1 << (mcReserved_lcbratok-mcReserved_eoftok))), stopset1+(SetOfStop1) ((1 << (mcReserved_nottok-mcReserved_arraytok))), stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok)) | (1 << (mcReserved_integertok-mcReserved_recordtok)) | (1 << (mcReserved_realtok-mcReserved_recordtok)) | (1 << (mcReserved_identtok-mcReserved_recordtok))));
  Expression (stopset0+(SetOfStop0) ((1 << (mcReserved_rparatok-mcReserved_eoftok))), stopset1, stopset2);
  Expect ((mcReserved_toktype) mcReserved_rparatok, stopset0, stopset1, stopset2);
}

static void TrashList (SetOfStop0 stopset0, SetOfStop1 stopset1, SetOfStop2 stopset2)
{
  if (mcLexBuf_currenttoken == mcReserved_stringtok)
    string (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
  while (mcLexBuf_currenttoken == mcReserved_commatok)
    {
      Expect ((mcReserved_toktype) mcReserved_commatok, stopset0, stopset1, stopset2+(SetOfStop2) ((1 << (mcReserved_stringtok-mcReserved_recordtok))));
      string (stopset0+(SetOfStop0) ((1 << (mcReserved_commatok-mcReserved_eoftok))), stopset1, stopset2);
    }
}

unsigned int mcp1_CompilationUnit (void)
{
  WasNoError = TRUE;
  FileUnit ((SetOfStop0) ((1 << (mcReserved_eoftok-mcReserved_eoftok))), (SetOfStop1) 0, (SetOfStop2) 0);
  return WasNoError;
}

void _M2_mcp1_init (int argc, char *argv[])
{
}