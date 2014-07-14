
#if !defined(m2expr_h)
#   define m2expr_h
#   if defined(m2expr_c)
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN 
#      endif
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void m2expr_BuildBinaryForeachWordDo (location_t location,
					     tree type, tree op1, tree op2, tree op3,
					     tree (*binop)(location_t, tree, tree, int),
					     int is_op1lvalue, int is_op2lvalue, int is_op3lvalue,
					     int is_op1const, int is_op2const, int is_op3const);
EXTERN tree m2expr_BuildCmplx (tree type, tree real, tree imag);
EXTERN tree m2expr_BuildIm (tree op1);
EXTERN tree m2expr_BuildRe (tree op1);
EXTERN tree m2expr_BuildAbs (location_t location, tree t);
EXTERN tree m2expr_BuildCap (location_t location, tree t);
EXTERN int m2expr_DetermineSign (tree e);
EXTERN int m2expr_AreRealOrComplexConstantsEqual (tree e1, tree e2);
EXTERN int m2expr_AreConstantsEqual (tree e1, tree e2);
EXTERN int m2expr_IsFalse (tree t);
EXTERN int m2expr_IsTrue (tree t);
EXTERN tree m2expr_BuildIndirect (location_t location, tree target, tree type);
EXTERN tree m2expr_BuildComponentRef (tree record, tree field);
EXTERN tree m2expr_BuildArray (tree type, tree array, tree index, tree lowIndice);
EXTERN void m2expr_BuildIfNotInRangeGoto (location_t location, tree var, tree low, tree high, char *label);
EXTERN void m2expr_BuildIfInRangeGoto (location_t location, tree var, tree low, tree high, char *label);
EXTERN void m2expr_BuildForeachWordInSetDoIfExpr (location_t location, tree type, tree op1, tree op2,
						  int is_op1lvalue, int is_op2lvalue,
						  int  is_op1const, int is_op2const,
						  tree (*expr) (location_t, tree, tree),
						  char *label);
EXTERN void m2expr_BuildIfNotVarInVar (location_t location, tree type, tree varset, tree varel,
				       int is_lvalue,
				       tree  low,
				       tree  high ATTRIBUTE_UNUSED,
				       char *label);
EXTERN void m2expr_BuildIfVarInVar (location_t location, tree type, tree varset, tree varel,
				    int is_lvalue,
				    tree  low,
				    tree  high ATTRIBUTE_UNUSED,
				    char *label);
EXTERN void m2expr_BuildIfNotConstInVar (location_t location, tree type, tree varset, tree constel,
					 int is_lvalue, int fieldno,
					 char *label);
EXTERN void m2expr_BuildIfConstInVar (location_t location, tree type, tree varset, tree constel,
				      int is_lvalue, int fieldno,
				      char *label);
EXTERN tree m2expr_BuildIsNotSubset (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildIsSubset (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildIsNotSuperset (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildIsSuperset (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildNotEqualTo (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildEqualTo (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildGreaterThanOrEqual (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildLessThanOrEqual (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildGreaterThan (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildLessThan (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildLogicalDifference (location_t location, tree op1, tree op2, int needconvert);
EXTERN tree m2expr_BuildSymmetricDifference (location_t location, tree op1, tree op2,  int  needconvert);
EXTERN tree m2expr_BuildLogicalAnd (location_t location, tree op1, tree op2, int needconvert);
EXTERN tree m2expr_BuildLogicalOr (location_t location, tree op1, tree op2, int needconvert);
EXTERN tree m2expr_BuildLogicalOrAddress (location_t location, tree op1, tree op2, int needconvert);
EXTERN tree m2expr_BuildOffset (location_t location, tree record, tree field, int needconvert ATTRIBUTE_UNUSED);
EXTERN tree m2expr_BuildOffset1 (location_t location, tree field, int needconvert ATTRIBUTE_UNUSED);
EXTERN tree m2expr_BuildAddr (location_t location, tree op1, int needconvert);
EXTERN tree m2expr_BuildSize (location_t location, tree op1, int needconvert ATTRIBUTE_UNUSED);
EXTERN tree m2expr_BuildTBitSize (location_t location, tree type);
EXTERN tree m2expr_BuildSetNegate (location_t location, tree op1, int needconvert);
EXTERN tree m2expr_BuildNegate (location_t location, tree op1, int needconvert);
EXTERN tree m2expr_BuildTrunc (tree op1);
EXTERN tree m2expr_BuildCoerce (location_t location, tree des, tree type, tree expr);
EXTERN tree m2expr_RemoveOverflow (tree t);
EXTERN int m2expr_TreeOverflow (tree t);

EXTERN unsigned int m2expr_StringLength (tree string);
EXTERN tree m2expr_FoldAndStrip (tree t);
EXTERN int m2expr_interpret_integer (const char *str, unsigned int base,
				     unsigned HOST_WIDE_INT *low, HOST_WIDE_INT *high);
EXTERN int m2expr_interpret_m2_integer (const char *str, unsigned int base,
					unsigned int *low, int *high);


EXTERN tree m2expr_BuildAdd (location_t location, tree op1, tree op2, int needconvert);
EXTERN tree m2expr_BuildSub (location_t location, tree op1, tree op2, int needconvert);
EXTERN tree m2expr_BuildDivTrunc (location_t location, tree op1, tree op2, int needconvert);
EXTERN tree m2expr_BuildModTrunc (location_t location, tree op1, tree op2, int needconvert);

EXTERN tree m2expr_BuildDivFloor (location_t location, tree op1, tree op2, int needconvert);

EXTERN tree m2expr_BuildModFloor (location_t location, tree op1, tree op2, int needconvert);

EXTERN tree m2expr_BuildLSL (location_t location, tree op1, tree op2, int needconvert);

EXTERN tree m2expr_BuildLSR (location_t location, tree op1, tree op2, int needconvert);

EXTERN void m2expr_BuildLogicalShift (location_t location, tree op1, tree op2, tree op3,
				      tree nBits ATTRIBUTE_UNUSED, int needconvert);

EXTERN tree m2expr_BuildLRL (location_t location, tree op1, tree op2, int needconvert);

EXTERN tree m2expr_BuildLRR (location_t location, tree op1, tree op2, int needconvert);
EXTERN tree m2expr_BuildMult (location_t location, tree op1, tree op2, int needconvert);

EXTERN tree m2expr_BuildMask (location_t location, tree nBits, int needconvert);
EXTERN tree m2expr_BuildLRLn (location_t location, tree op1, tree op2, tree nBits, int  needconvert);
EXTERN tree m2expr_BuildLRRn (location_t location, tree op1, tree op2, tree nBits, int needconvert);
EXTERN void m2expr_BuildLogicalRotate (location_t location, tree op1, tree op2, tree op3, tree nBits, int needconvert);
EXTERN void m2expr_BuildBinarySetDo (location_t location, tree settype, tree op1, tree op2, tree op3,
				     void (*binop)(location_t, tree, tree, tree, tree, int),
				     int is_op1lvalue, int is_op2lvalue, int is_op3lvalue,
				     tree nBits,
				     tree unbounded,
				     tree varproc, tree leftproc, tree rightproc);

EXTERN tree m2expr_GetSizeOf (location_t location, tree type);
EXTERN tree m2expr_GetSizeOfInBits (tree type);

EXTERN tree m2expr_GetCardinalZero (void);
EXTERN tree m2expr_GetCardinalOne (void);
EXTERN tree m2expr_GetIntegerZero (void);
EXTERN tree m2expr_GetIntegerOne (void);
EXTERN tree m2expr_GetWordZero (void);
EXTERN tree m2expr_GetWordOne (void);
EXTERN tree m2expr_GetPointerZero (void);
EXTERN tree m2expr_GetPointerOne (void);

#if 0
EXTERN tree m2expr_GetBooleanTrue (void);
EXTERN tree m2expr_GetBooleanFalse (void);
#endif

EXTERN int m2expr_CompareTrees (tree e1, tree e2);
EXTERN tree m2expr_build_unary_op (location_t location ATTRIBUTE_UNUSED,
				   enum tree_code code, tree arg, int flag ATTRIBUTE_UNUSED);
EXTERN tree m2expr_build_binary_op (location_t location, enum tree_code code, tree op1, tree op2, int convert);
EXTERN void m2expr_ConstantExpressionWarning (tree value);
EXTERN tree m2expr_BuildAddAddress (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildRDiv (location_t location, tree op1, tree op2, int needconvert);

EXTERN void m2expr_init (location_t location);


#undef EXTERN
#endif
