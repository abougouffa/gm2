(* Copyright (C) 2015, 2016
   Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE decl ;

FROM ASCII IMPORT lf, tab ;
FROM symbolKey IMPORT symbolTree, initTree, getSymKey, putSymKey, foreachNodeDo ;
FROM mcDebug IMPORT assert ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM nameKey IMPORT NulName, makeKey, lengthKey, makekey, keyToCharStar ;
FROM SFIO IMPORT OpenToWrite, WriteS ;
FROM FIO IMPORT File, Close, FlushBuffer, StdOut, WriteLine, WriteChar ;
FROM DynamicStrings IMPORT String, InitString, EqualArray, InitStringCharStar, KillString ;
FROM mcOptions IMPORT getOutputFile ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2 ;
FROM libc IMPORT printf ;
FROM mcMetaError IMPORT metaError1, metaError2, metaErrors1, metaErrors2 ;
FROM StrLib IMPORT StrEqual ;

FROM mcPretty IMPORT pretty, initPretty, dupPretty, killPretty, print, prints,
                     setNeedSpace, noSpace, setindent, getindent ;

FROM Indexing IMPORT Index, InitIndex, ForeachIndiceInIndexDo,
                     IncludeIndiceIntoIndex, IsIndiceInIndex,
		     HighIndice, LowIndice, GetIndice, RemoveIndiceFromIndex ;

IMPORT DynamicStrings ;
IMPORT alists, wlists ;

FROM alists IMPORT alist ;
FROM wlists IMPORT wlist ;


CONST
   indentation = 3 ;
   ignoreFQ = TRUE ;

TYPE
   language = (ansiC, ansiCP, pim4) ;

   nodeT = (
            (* base constants.  *)
	    nil, true, false,
            (* system types.  *)
   	    address, byte, word,
            (* base types.  *)
	    char,
	    cardinal, longcard, shortcard,
            integer, longint, shortint,
	    real, longreal, shortreal,
	    bitset, boolean, proc,
	    ztype, rtype,
	    (* language features and compound type attributes.  *)
            type, record, varient, var, enumeration,
            subrange, array, subscript,
            string, const, literal, varparam, param, varargs,
	    pointer, recordfield, varientfield, enumerationfield,
            set, proctype,
	    (* blocks.  *)
	    procedure, def, imp, module,
	    (* statements.  *)
            loop, while, for, repeat,
	    assignment, call,
	    if, elsif,
	    (* expressions.  *)
	    neg,
	    cast, val,
	    plus, sub, div, mod,
	    adr, size, ord,
	    componentref, indirect,
	    equal, notequal, less, greater, greequal, lessequal,
	    lsl, lsr, lor, land, lnot, lxor,
	    and, or, not, identlist, vardecl) ;

    node = POINTER TO RECORD
                         CASE kind: nodeT OF

                         (* base constants.  *)
                         nil,
			 true,
			 false,
			 (* system types.  *)
                         address,
                         byte,
			 word            :  |
                         (* base types.  *)
			 boolean,
 			 proc,
                         char,
                         integer,
                         cardinal,
                         longcard,
                         shortcard,
                         longint,
                         shortint,
                         real,
                         longreal,
			 shortreal,
                         bitset,
                         ztype,
                         rtype           :  |
                         (* language features and compound type attributes.  *)
                         type            :  typeF            : typeT |
                         record          :  recordF          : recordT |
                         varient         :  varientF         : varientT |
                         var             :  varF             : varT |
                         enumeration     :  enumerationF     : enumerationT |
                         subrange        :  subrangeF        : subrangeT |
                         subscript       :  subscriptF       : subscriptT |
                         array           :  arrayF           : arrayT |
                         string          :  stringF          : stringT |
                         const           :  constF           : constT |
                         literal         :  literalF         : literalT |
			 varparam        :  varparamF        : varparamT |
                         param           :  paramF           : paramT |
			 varargs         :  varargsF         : varargsT |
			 pointer         :  pointerF         : pointerT |
                         recordfield     :  recordfieldF     : recordfieldT |
			 varientfield    :  varientfieldF    : varientfieldT |
			 enumerationfield:  enumerationfieldF: enumerationfieldT |
                         set             :  setF             : setT |
			 proctype        :  proctypeF        : proctypeT |
                         (* blocks.  *)
                         procedure       :  procedureF       : procedureT |
                         def             :  defF             : defT |
			 imp             :  impF             : impT |
			 module          :  moduleF          : moduleT |
                         (* statements.  *)
                         loop            :  loopF            : loopT |
                         while           :  whileF           : whileT |
                         for             :  forF             : forT |
                         repeat          :  repeatF          : repeatT |
                         if              :  ifF              : ifT |
                         elsif           :  elsifF           : elsifT |
			 assignment      :  assignmentF      : assignmentT |
                         (* expressions.  *)
                         equal,
                         notequal,
			 less,
			 greater,
			 greequal,
			 lessequal,
                         componentref,
                         cast,
                         val,
                         plus,
                         sub,
                         div,
			 mod             :  binaryF          : binaryT |
			 indirect,
                         ord,
			 not,
			 neg,
                         adr,
                         size            :  unaryF           : unaryT |
			 identlist       :  identlistF       : identlistT |
			 vardecl         :  vardeclF         : vardeclT

                         END ;
                         at:  where ;
                      END ;

       identlistT = RECORD
                       names:  wlist ;
                    END ;

       vardeclT = RECORD
                     names:  wlist ;
		     type :  node ;
		     scope:  node ;
                  END ;

       typeT = RECORD
                  name    :  Name ;
		  type    :  node ;
		  scope   :  node ;
		  isHidden:  BOOLEAN ;
               END ;

       recordT = RECORD
		    localSymbols:  symbolTree ;
                    listOfSons  :  Index ;
		    scope       :  node ;
                 END ;

       varientT = RECORD
		     listOfSons:  Index ;
		     varient   :  node ;
		     tag       :  node ;
		     scope     :  node ;
                  END ;

       varT = RECORD
                 name         :  Name ;
		 type         :  node ;
		 decl         :  node ;
		 scope        :  node ;
		 isInitialised:  BOOLEAN ;
              END ;

       enumerationT = RECORD
			 noOfElements: CARDINAL ;
                         localSymbols: symbolTree ;
			 listOfSons  : Index ;
			 scope       : node ;
                      END ;

       subrangeT = RECORD
		      low,
		      high :  node ;
		      type :  node ;
		      scope:  node ;
                   END ;

       subscriptT = RECORD
                       type:  node ;
		       expr:  node ;
                    END ;

       arrayT = RECORD
		   subr       :  node ;
		   type,
		   scope      :  node ;
		   isUnbounded:  BOOLEAN ;
                END ;

       stringT = RECORD
                    name            :  Name ;
		    length          :  CARDINAL ;
		    isCharCompatible:  BOOLEAN ;
                 END ;

       literalT = RECORD
                     name :  Name ;
		     type :  node ;
                  END ;

       constT = RECORD
                   name :  Name ;
		   type :  node ;
                   value:  node ;
                   scope:  node ;
                END ;

       varparamT = RECORD
                      namelist   :  node ;
                      type       :  node ;
		      scope      :  node ;
                      isUnbounded:  BOOLEAN ;
                   END ;

       paramT = RECORD
                   namelist   :  node ;
                   type       :  node ;
                   scope      :  node ;
                   isUnbounded:  BOOLEAN ;
                END ;

       varargsT = RECORD
                     scope    :  node ;
                  END ;

       pointerT = RECORD
                     type :  node ;
                     scope:  node ;
                  END ;

       recordfieldT = RECORD
                         name   :  Name ;
			 type   :  node ;
			 tag    :  BOOLEAN ;
			 parent :  node ;
			 varient:  node ;
			 scope  :  node ;
                      END ;

       varientfieldT = RECORD
                          name      :  Name ;
			  parent    :  node ;
			  varient   :  node ;
			  listOfSons:  Index ;
			  scope     :  node ;
                       END ;

       enumerationfieldT = RECORD
                              name :  Name ;
                              type :  node ;
			      scope:  node ;
                              value:  CARDINAL ;
                           END ;

       setT = RECORD
                 type :  node ;
                 scope:  node ;
              END ;

       componentT = RECORD
                       rec       :  node ;
                       field     :  node ;
		       resultType:  node ;
                    END ;

       assignmentT = RECORD
                        des,
                        expr:  node ;
                     END ;

       ifT = RECORD
                expr,
                elsif,     (* either else or elsif must be NIL.  *)
                then,
                else:  node ;
             END ;

       elsifT = RECORD
                   expr,
                   elsif,     (* either else or elsif must be NIL.  *)
                   then,
                   else:  node ;
                END ;

       loopT = RECORD
                  statements:  node ;
               END ;

       whileT = RECORD
                   expr,
                   statements:  node ;
                END ;

       repeatT = RECORD
                   expr,
                   statements:  node ;
                END ;

       forT = RECORD
                 assignment,
                 expr,
                 increment,
                 statements:  node ;
              END ;

       statementT = RECORD
                       sequence:  Index ;
                    END ;

       scopeT = RECORD
                   symbols   :  symbolTree ;
                   constants,
                   types,
		   procedures,
                   variables :  Index ;
                END ;

       procedureT = RECORD
                       name      :  Name ;
                       decls     :  scopeT ;
                       scope     :  node ;
                       parameters:  Index ;
                       returnopt,
		       optarg,
		       vararg    :  BOOLEAN ;
		       returnType:  node ;
                       statements:  node ;
                    END ;

       proctypeT = RECORD
                      parameters:  Index ;
                      returnopt,
                      optarg,
                      vararg    :  BOOLEAN ;
		      scope     :  node ;
                      returnType:  node ;
                   END ;

       binaryT = RECORD
                    left,
		    right,
                    resultType:  node ;
                 END ;

       unaryT = RECORD
                   arg,
                   resultType:  node ;
                END ;

       moduleT = RECORD
                    name             :  Name ;
                    source           :  Name ;
                    importedModules,
                    enums            :  Index ;
                    enumCount        :  CARDINAL ;
                    decls            :  scopeT ;
                    beginStatements,
                    finallyStatements:  node ;
                 END ;

       defT = RECORD
                 name             :  Name ;
                 source           :  Name ;
                 forC             :  BOOLEAN ;
                 exported,
                 importedModules,
                 enums            :  Index ;
                 enumCount        :  CARDINAL ;
                 decls            :  scopeT ;
              END ;

       impT = RECORD
                 name             :  Name ;
                 source           :  Name ;
                 importedModules,
                 enums            :  Index ;
                 enumCount        :  CARDINAL ;
                 beginStatements,
                 finallyStatements:  node ;
		 definitionModule :  node ;
                 decls            :  scopeT ;
              END ;

       where = RECORD
                  defDeclared,
                  modDeclared,
                  firstUsed  :  CARDINAL ;
               END ;

       outputStates = (text, punct, space) ;

       nodeProcedure = PROCEDURE (node) ;

       dependentState = (completed, blocked, recursive) ;


VAR
   outputFile    : File ;
   lang          : language ;
   bitsperunitN,
   bitsperwordN,
   bitspercharN,
   unitsperwordN,
   mainModule,
   currentModule,
   systemN,
   addressN,
   byteN,
   wordN,
   booleanN,
   procN,
   charN,
   integerN,
   cardinalN,
   longcardN,
   shortcardN,
   longintN,
   shortintN,
   bitsetN,
   ztypeN,
   rtypeN,
   realN,
   longrealN,
   shortrealN,
   nilN,
   trueN,
   falseN        : node ;
   scopeStack,
   defUniverseI,
   modUniverseI  : Index ;
   modUniverse,
   defUniverse   : symbolTree ;
   baseSymbols   : symbolTree ;
   outputState   : outputStates ;
   doP           : pretty ;
   todoQ,
   partialQ,
   doneQ         : alist ;
   procUsed,
   simplified    : BOOLEAN ;
   tempCount     : CARDINAL ;


(*
   newNode -
*)

PROCEDURE newNode (k: nodeT) : node ;
VAR
   d: node ;
BEGIN
   NEW (d) ;
   IF d=NIL
   THEN
      HALT
   ELSE
      d^.kind := k ;
      RETURN d
   END
END newNode ;


(*
   getDeclaredDef - returns the token number associated with the nodes declaration
                    in the definition module.
*)

PROCEDURE getDeclaredDef (n: node) : CARDINAL ;
BEGIN
   RETURN n^.at.defDeclared
END getDeclaredDef ;


(*
   getDeclaredMod - returns the token number associated with the nodes declaration
                    in the implementation or program module.
*)

PROCEDURE getDeclaredMod (n: node) : CARDINAL ;
BEGIN
   RETURN n^.at.modDeclared
END getDeclaredMod ;


(*
   getFirstUsed - returns the token number associated with the first use of
                  node, n.
*)

PROCEDURE getFirstUsed (n: node) : CARDINAL ;
BEGIN
   RETURN n^.at.firstUsed
END getFirstUsed ;


(*
   isDef - return TRUE if node, n, is a definition module.
*)

PROCEDURE isDef (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = def
END isDef ;


(*
   isImp - return TRUE if node, n, is an implementation module.
*)

PROCEDURE isImp (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = imp
END isImp ;


(*
   isModule - return TRUE if node, n, is a program module.
*)

PROCEDURE isModule (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = module
END isModule ;


(*
   isImpOrModule - returns TRUE if, n, is a program module or implementation module.
*)

PROCEDURE isImpOrModule (n: node) : BOOLEAN ;
BEGIN
   RETURN isImp (n) OR isModule (n)
END isImpOrModule ;


(*
   isProcedure - returns TRUE if node, n, is a procedure.
*)

PROCEDURE isProcedure (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = procedure
END isProcedure ;


(*
   isConst - returns TRUE if node, n, is a const.
*)

PROCEDURE isConst (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = const
END isConst ;


(*
   isType - returns TRUE if node, n, is a type.
*)

PROCEDURE isType (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = type
END isType ;


(*
   isVar - returns TRUE if node, n, is a type.
*)

PROCEDURE isVar (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = var
END isVar ;


(*
   isTemporary - returns TRUE if node, n, is a variable and temporary.
*)

PROCEDURE isTemporary (n: node) : BOOLEAN ;
BEGIN
   RETURN FALSE
END isTemporary ;


(*
   isExported - returns TRUE if symbol, n, is exported from
                the definition module.
*)

PROCEDURE isExported (n: node) : BOOLEAN ;
VAR
   s: node ;
BEGIN
   s := getScope (n) ;
   IF s#NIL
   THEN
      CASE s^.kind OF

      def:  RETURN IsIndiceInIndex (s^.defF.exported, n)

      ELSE
         RETURN FALSE
      END
   END ;
   RETURN FALSE
END isExported ;


(*
   lookupExported - attempts to lookup a node named, i, from definition
                    module, n.  The node is returned if found.
                    NIL is returned if not found.
*)

PROCEDURE lookupExported (n: node; i: Name) : node ;
VAR
   r: node ;
BEGIN
   assert (isDef (n)) ;
   r := getSymKey (n^.defF.decls.symbols, i) ;
   IF (r#NIL) AND isExported (r)
   THEN
      RETURN r
   END ;
   RETURN NIL
END lookupExported ;


(*
   importEnumFields - if, n, is an enumeration type import the all fields into module, m.
*)

PROCEDURE importEnumFields (m, n: node) ;
VAR
   r, e: node ;
   i, h: CARDINAL ;
BEGIN
   assert (isDef (m) OR isModule (m) OR isImp (m)) ;
   IF isEnumeration (n)
   THEN
      i := LowIndice (n^.enumerationF.listOfSons) ;
      h := HighIndice (n^.enumerationF.listOfSons) ;
      WHILE i<=h DO
         e := GetIndice (n^.enumerationF.listOfSons, i) ;
         r := import (m, e) ;
         IF e#r
         THEN
            metaError2 ('enumeration field {%1ad} cannot be imported implicitly into {%2d} due to a name clash',
                        e, m)
         END ;
         INC (i)
      END
   END
END importEnumFields ;


(*
   import - attempts to add node, n, into the scope of module, m.
            It might fail due to a name clash in which case the
            previous named symbol is returned.  On success, n,
            is returned.
*)

PROCEDURE import (m, n: node) : node ;
VAR
   name: Name ;
   r   : node ;
BEGIN
   assert (isDef (m) OR isModule (m) OR isImp (m)) ;
   name := getSymName (n) ;
   r := lookupInScope (m, name) ;
   IF r=NIL
   THEN
      CASE m^.kind OF

      def   :  putSymKey (m^.defF.decls.symbols, name, n) |
      imp   :  putSymKey (m^.impF.decls.symbols, name, n) |
      module:  putSymKey (m^.moduleF.decls.symbols, name, n)

      END ;
      importEnumFields (m, n) ;
      RETURN n
   END ;
   RETURN r
END import ;


(*
   isZtype - returns TRUE if, n, is the Z type.
*)

PROCEDURE isZtype (n: node) : BOOLEAN ;
BEGIN
   RETURN n = ztypeN
END isZtype ;


(*
   isRtype - returns TRUE if, n, is the R type.
*)

PROCEDURE isRtype (n: node) : BOOLEAN ;
BEGIN
   RETURN n = rtypeN
END isRtype ;


(*
   isLiteral - returns TRUE if, n, is a literal.
*)

PROCEDURE isLiteral (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = literal
END isLiteral ;


(*
   isConstSet - returns TRUE if, n, is a constant set.
*)

PROCEDURE isConstSet (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   IF isLiteral (n) OR isConst (n)
   THEN
      RETURN isSet (skipType (getType (n)))
   END ;
   RETURN FALSE
END isConstSet ;


(*
   isEnumerationField - returns TRUE if, n, is an enumeration field.
*)

PROCEDURE isEnumerationField (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = enumerationfield
END isEnumerationField ;


(*
   isUnbounded - returns TRUE if, n, is an unbounded array.
*)

PROCEDURE isUnbounded (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN (n^.kind = array) AND (n^.arrayF.isUnbounded)
END isUnbounded ;


(*
   isParameter - returns TRUE if, n, is a parameter.
*)

PROCEDURE isParameter (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN (n^.kind = param) OR (n^.kind = varparam)
END isParameter ;


(*
   isVarParam - returns TRUE if, n, is a var parameter.
*)

PROCEDURE isVarParam (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = varparam
END isVarParam ;


(*
   isParam - returns TRUE if, n, is a non var parameter.
*)

PROCEDURE isParam (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = param
END isParam ;


(*
   isNonVarParam - is an alias to isParam.
*)

PROCEDURE isNonVarParam (n: node) : BOOLEAN ;
BEGIN
   RETURN isParam (n)
END isNonVarParam ;


(*
   isRecord - returns TRUE if, n, is a record.
*)

PROCEDURE isRecord (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = record
END isRecord ;


(*
   isRecordField - returns TRUE if, n, is a record field.
*)

PROCEDURE isRecordField (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = recordfield
END isRecordField ;


(*
   isArray - returns TRUE if, n, is an array.
*)

PROCEDURE isArray (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = array
END isArray ;


(*
   isProcType - returns TRUE if, n, is a procedure type.
*)

PROCEDURE isProcType (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = proctype
END isProcType ;


(*
   isProcedure - returns TRUE if, n, is a procedure.
*)

PROCEDURE isProcedure (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = procedure
END isProcedure ;


(*
   isPointer - returns TRUE if, n, is a pointer.
*)

PROCEDURE isPointer (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = pointer
END isPointer ;


(*
   isVarient - returns TRUE if, n, is a varient record.
*)

PROCEDURE isVarient (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = varient
END isVarient ;


(*
   isVarientField - returns TRUE if, n, is a varient field.
*)

PROCEDURE isVarientField (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = varientfield
END isVarientField ;


(*
   isSet - returns TRUE if, n, is a set type.
*)

PROCEDURE isSet (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = set
END isSet ;


(*
   isSubrange - returns TRUE if, n, is a subrange type.
*)

PROCEDURE isSubrange (n: node) : BOOLEAN ;
BEGIN
   assert (n # NIL) ;
   RETURN n^.kind = subrange
END isSubrange ;


(*
   isMainModule - return TRUE if node, n, is the main module specified
                  by the source file.  This might be a definition,
                  implementation or program module.
*)

PROCEDURE isMainModule (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n = mainModule
END isMainModule ;


(*
   setMainModule - sets node, n, as the main module to be compiled.
*)

PROCEDURE setMainModule (n: node) ;
BEGIN
   assert (n#NIL) ;
   mainModule := n
END setMainModule ;


(*
   getMainModule - returns the main module node.
*)

PROCEDURE getMainModule () : node ;
BEGIN
   RETURN mainModule
END getMainModule ;


(*
   setCurrentModule - sets node, n, as the current module being compiled.
*)

PROCEDURE setCurrentModule (n: node) ;
BEGIN
   assert (n#NIL) ;
   currentModule := n
END setCurrentModule ;


(*
   getCurrentModule - returns the current module being compiled.
*)

PROCEDURE getCurrentModule () : node ;
BEGIN
   RETURN currentModule
END getCurrentModule ;


(*
   makeDef - returns a definition module node named, n.
*)

PROCEDURE makeDef (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := newNode (def) ;
   WITH d^ DO
      defF.name := n ;
      defF.source := NulName ;
      defF.forC := FALSE ;
      defF.exported := InitIndex (1) ;
      defF.importedModules := InitIndex (1) ;
      defF.enums := InitIndex (1) ;
      defF.enumCount := 0 ;
      initDecls (defF.decls)
   END ;
   RETURN d
END makeDef ;


(*
   makeImp - returns an implementation module node named, n.
*)

PROCEDURE makeImp (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := newNode (imp) ;
   WITH d^ DO
      impF.name := n ;
      impF.source := NulName ;
      impF.importedModules := InitIndex (1) ;
      impF.enums := InitIndex (1) ;
      impF.enumCount := 0 ;
      initDecls (impF.decls) ;
      impF.beginStatements := NIL ;
      impF.finallyStatements := NIL ;
      impF.definitionModule := NIL
   END ;
   RETURN d
END makeImp ;


(*
   makeModule - returns a module node named, n.
*)

PROCEDURE makeModule (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := newNode (mod) ;
   WITH d^ DO
      moduleF.name := n ;
      moduleF.source := NulName ;
      moduleF.importedModules := InitIndex (1) ;
      moduleF.enums := InitIndex (1) ;
      moduleF.enumCount := 0 ;
      initDecls (moduleF.decls) ;
      moduleF.beginStatements := NIL ;
      moduleF.finallyStatements := NIL
   END ;
   RETURN d
END makeModule ;


(*
   lookupDef - returns a definition module node named, n.
*)

PROCEDURE lookupDef (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := getSymKey (defUniverse, n) ;
   IF d=NIL
   THEN
      d := makeDef (n) ;
      putSymKey (defUniverse, n, d) ;
      IncludeIndiceIntoIndex (defUniverseI, d)
   END ;
   RETURN d
END lookupDef ;


(*
   lookupImp - returns an implementation module node named, n.
*)

PROCEDURE lookupImp (n: Name) : node ;
VAR
   m: node ;
BEGIN
   m := getSymKey (modUniverse, n) ;
   IF m=NIL
   THEN
      m := makeImp (n) ;
      putSymKey (modUniverse, n, m) ;
      IncludeIndiceIntoIndex (modUniverseI, m)
   END ;
   assert (NOT isModule (m)) ;
   RETURN m
END lookupImp ;


(*
   lookupModule - returns a module node named, n.
*)

PROCEDURE lookupModule (n: Name) : node ;
VAR
   m: node ;
BEGIN
   m := getSymKey (modUniverse, n) ;
   IF m=NIL
   THEN
      m := makeModule (n) ;
      putSymKey (modUniverse, n, m) ;
      IncludeIndiceIntoIndex (modUniverseI, m)
   END ;
   assert (NOT isImp (m)) ;
   RETURN m
END lookupModule ;


(*
   setSource - sets the source filename for module, n, to s.
*)

PROCEDURE setSource (n: node; s: Name) ;
BEGIN
   WITH n^ DO
      CASE kind OF

      def:  defF.source := s |
      mod:  moduleF.source := s |
      imp:  impF.source := s

      END
   END
END setSource ;


(*
   getSource - returns the source filename for module, n.
*)

PROCEDURE getSource (n: node) : Name ;
BEGIN
   WITH n^ DO
      CASE kind OF

      def:  RETURN defF.source |
      mod:  RETURN moduleF.source |
      imp:  RETURN impF.source

      END
   END
END getSource ;


(*
   initDecls - initialize the decls, scopeT.
*)

PROCEDURE initDecls (VAR decls: scopeT) ;
BEGIN
   decls.symbols := initTree () ;
   decls.constants := InitIndex (1) ;
   decls.types := InitIndex (1) ;
   decls.procedures := InitIndex (1) ;
   decls.variables := InitIndex (1)
END initDecls ;


(*
   enterScope - pushes symbol, n, to the scope stack.
*)

PROCEDURE enterScope (n: node) ;
BEGIN
   IF IsIndiceInIndex (scopeStack, n)
   THEN
      HALT
   ELSE
      IncludeIndiceIntoIndex (scopeStack, n)
   END
END enterScope ;


(*
   leaveScope - removes the top level scope.
*)

PROCEDURE leaveScope ;
VAR
   i: CARDINAL ;
   n: node ;
BEGIN
   i := HighIndice (scopeStack) ;
   n := GetIndice (scopeStack, i) ;
   RemoveIndiceFromIndex (scopeStack, n)
END leaveScope ;


(*
   getDeclScope - returns the node representing the
                  current declaration scope.
*)

PROCEDURE getDeclScope () : node ;
VAR
   i: CARDINAL ;
BEGIN
   i := HighIndice (scopeStack) ;
   RETURN GetIndice (scopeStack, i)
END getDeclScope ;


(*
   addTo - adds node, d, to scope decls and returns, d.
           It stores, d, in the symbols tree associated with decls.
*)

PROCEDURE addTo (VAR decls: scopeT; d: node) : node ;
VAR
   n: Name ;
BEGIN
   n := getSymName (d) ;
   IF n#NulName
   THEN
      IF getSymKey (decls.symbols, n)=NIL
      THEN
         putSymKey (decls.symbols, n, d)
      ELSE
         metaError1 ('{%1DMad} was declared', d) ;
         metaError1 ('{%1k} and is being declared again', n)
      END
   END ;
   IF isConst (d)
   THEN
      IncludeIndiceIntoIndex (decls.constants, d)
   ELSIF isVar (d)
   THEN
      IncludeIndiceIntoIndex (decls.variables, d)
   ELSIF isType (d)
   THEN
      IncludeIndiceIntoIndex (decls.types, d)
   ELSIF isProcedure (d)
   THEN
      IncludeIndiceIntoIndex (decls.procedures, d)
   END ;
   RETURN d
END addTo ;


(*
   export - export node, n, from definition module, d.
*)

PROCEDURE export (d, n: node) ;
BEGIN
   assert (isDef (d)) ;
   IncludeIndiceIntoIndex (d^.defF.exported, n)
END export ;


(*
   addToScope - adds node, n, to the current scope and returns, n.
*)

PROCEDURE addToScope (n: node) : node ;
VAR
   s: node ;
   i: CARDINAL ;
BEGIN
   i := HighIndice (scopeStack) ;
   s := GetIndice (scopeStack, i) ;
   IF isProcedure (s)
   THEN
      RETURN addTo (s^.procedureF.decls, n)
   ELSIF isModule (s)
   THEN
      RETURN addTo (s^.moduleF.decls, n)
   ELSIF isDef (s)
   THEN
      export (s, n) ;
      RETURN addTo (s^.defF.decls, n)
   ELSIF isImp (s)
   THEN
      RETURN addTo (s^.impF.decls, n)
   END ;
   HALT
END addToScope ;


(*
   addImportedModule - add module, i, to be imported by, m.
*)

PROCEDURE addImportedModule (m, i: node) ;
BEGIN
   assert (isDef (i) OR isModule (i)) ;
   IF isDef (m)
   THEN
      IncludeIndiceIntoIndex (m^.defF.importedModules, i)
   ELSIF isImp (m)
   THEN
      IncludeIndiceIntoIndex (m^.impF.importedModules, i)
   ELSIF isModule (m)
   THEN
      IncludeIndiceIntoIndex (m^.moduleF.importedModules, i)
   ELSE
      HALT
   END
END addImportedModule ;


(*
   makeConst - create, initialise and return a const node.
*)

PROCEDURE makeConst (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := newNode (const) ;
   WITH d^ DO
      constF.name := n ;
      constF.type := NIL ;
      constF.scope := getDeclScope () ;
      constF.value := NIL
   END ;
   RETURN addToScope (d)
END makeConst ;


(*
   makeType - create, initialise and return a type node.
*)

PROCEDURE makeType (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := newNode (type) ;
   WITH d^ DO
      typeF.name := n ;
      typeF.type := NIL ;
      typeF.scope := getDeclScope () ;
      typeF.isHidden := FALSE
   END ;
   RETURN addToScope (d)
END makeType ;


(*
   makeVar - create, initialise and return a var node.
*)

PROCEDURE makeVar (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := newNode (var) ;
   WITH d^ DO
      varF.name := n ;
      varF.type := NIL ;
      varF.decl := NIL ;
      varF.scope := getDeclScope () ;
      varF.isInitialised := FALSE
   END ;
   RETURN addToScope (d)
END makeVar ;


(*
   putVar - places, type, as the type for var.
*)

PROCEDURE putVar (var, type, decl: node) ;
BEGIN
   assert (var#NIL) ;
   assert (isVar (var)) ;
   var^.varF.type := type ;
   var^.varF.decl := decl
END putVar ;


(*
   makeVarDecl -
*)

PROCEDURE makeVarDecl (i: node; type: node) : node ;
VAR
   d, v: node ;
   j, n: CARDINAL ;
BEGIN
   d := newNode (vardecl) ;
   WITH d^ DO
      vardeclF.names := i^.identlistF.names ;
      vardeclF.type := type ;
      vardeclF.scope := getDeclScope ()
   END ;
   n := wlists.noOfItemsInList (d^.vardeclF.names) ;
   j := 1 ;
   WHILE j<=n DO
      v := lookupSym (wlists.getItemFromList (d^.vardeclF.names, j)) ;
      assert (isVar (v)) ;
      putVar (v, type, d) ;
      INC (j)
   END ;
   RETURN d
END makeVarDecl ;


(*
   makeProcedure - create, initialise and return a procedure node.
*)

PROCEDURE makeProcedure (n: Name) : node ;
VAR
   d: node ;
BEGIN
   d := newNode (procedure) ;
   WITH d^ DO
      procedureF.name := n ;
      initDecls (procedureF.decls) ;
      procedureF.scope := getDeclScope () ;
      procedureF.parameters := InitIndex (1) ;
      procedureF.returnopt := FALSE ;
      procedureF.optarg := FALSE ;
      procedureF.vararg := FALSE ;
      procedureF.returnType := NIL ;
      procedureF.statements := NIL
   END ;
   RETURN addToScope (d)
END makeProcedure ;


(*
   putReturnType - assigns, type, to the procedure or proctype, p.
*)

PROCEDURE putReturnType (p: node; type: node) ;
BEGIN
   assert (p#NIL) ;
   assert (isProcedure (p) OR isProcType (p)) ;
   IF p^.kind = procedure
   THEN
      p^.procedureF.returnType := type
   ELSE
      p^.proctypeF.returnType := type
   END
END putReturnType ;


(*
   makeProcType - returns a proctype node.
*)

PROCEDURE makeProcType () : node ;
VAR
   d: node ;
BEGIN
   d := newNode (proctype) ;
   WITH d^ DO
      proctypeF.scope := getDeclScope () ;
      proctypeF.parameters := InitIndex (1) ;
      proctypeF.returnopt := FALSE ;
      proctypeF.optarg := FALSE ;
      proctypeF.vararg := FALSE ;
      proctypeF.returnType := NIL
   END ;
   RETURN d
END makeProcType ;


(*
   putProcTypeReturn - sets the return type of, proc, to, type.
*)

PROCEDURE putProcTypeReturn (proc, type: node) ;
BEGIN
   proc^.proctypeF.returnType := type
END putProcTypeReturn ;


(*
   putProcTypeOptReturn - sets, proc, to have an optional return type.
*)

PROCEDURE putProcTypeOptReturn (proc: node) ;
BEGIN
   proc^.proctypeF.returnopt := TRUE
END putProcTypeOptReturn ;


(*
   makeNonVarParameter - returns a non var parameter node with, name: type.
*)

PROCEDURE makeNonVarParameter (l: node; type: node) : node ;
VAR
   d: node ;
BEGIN
   assert ((l=NIL) OR isIdentList (l)) ;
   d := newNode (param) ;
   d^.paramF.namelist := l ;
   d^.paramF.type := type ;
   d^.paramF.scope := NIL ;
   d^.paramF.isUnbounded := FALSE ;
   RETURN d
END makeNonVarParameter ;


(*
   makeVarParameter - returns a var parameter node with, name: type.
*)

PROCEDURE makeVarParameter (l: node; type: node) : node ;
VAR
   d: node ;
BEGIN
   assert ((l=NIL) OR isIdentList (l)) ;
   d := newNode (varparam) ;
   d^.varparamF.namelist := l ;
   d^.varparamF.type := type ;
   d^.varparamF.scope := NIL ;
   d^.varparamF.isUnbounded := FALSE ;
   RETURN d
END makeVarParameter ;


(*
   makeVarargs - returns a varargs node.
*)

PROCEDURE makeVarargs () : node ;
VAR
   d: node ;
BEGIN
   d := newNode (varargs) ;
   d^.varargsF.scope := NIL ;
   RETURN d
END makeVarargs ;


(*
   isVarargs - returns TRUE if, n, is a varargs node.
*)

PROCEDURE isVarargs (n: node) : BOOLEAN ;
BEGIN
   RETURN n^.kind = varargs
END isVarargs ;


(*
   addParameter - adds a parameter, param, to procedure or proctype, proc.
*)

PROCEDURE addParameter (proc, param: node) ;
BEGIN
   assert (isVarargs (param) OR isParam (param) OR isVarParam (param)) ;
   CASE proc^.kind OF

   procedure:  IncludeIndiceIntoIndex (proc^.procedureF.parameters, param) ;
               IF isVarargs (param)
               THEN
                  proc^.procedureF.vararg := TRUE
               END |
   proctype :  IncludeIndiceIntoIndex (proc^.proctypeF.parameters, param) ;
               IF isVarargs (param)
               THEN
                  proc^.proctypeF.vararg := TRUE
               END

   END
END addParameter ;


(*
   makeIdentList - returns a node which will be used to maintain an ident list.
*)

PROCEDURE makeIdentList () : node ;
VAR
   n: node ;
BEGIN
   n := newNode (identlist) ;
   n^.identlistF.names := wlists.initList () ;
   RETURN n
END makeIdentList ;


(*
   isIdentList - returns TRUE if, n, is an identlist.
*)

PROCEDURE isIdentList (n: node) : BOOLEAN ;
BEGIN
   RETURN n^.kind = identlist
END isIdentList ;


(*
   putIdent - places ident, i, into identlist, n.  It returns TRUE if
              ident, i, is unique.
*)

PROCEDURE putIdent (n: node; i: Name) : BOOLEAN ;
BEGIN
   assert (isIdentList (n)) ;
   IF wlists.isItemInList (n^.identlistF.names, i)
   THEN
      RETURN FALSE
   ELSE
      wlists.putItemIntoList (n^.identlistF.names, i) ;
      RETURN TRUE
   END
END putIdent ;


(*
   addVarParameters - adds the identlist, i, of, type, to be VAR parameters
                      in procedure, n.
*)

PROCEDURE addVarParameters (n: node; i: node; type: node) ;
VAR
   p: node ;
BEGIN
   assert (isIdentList (i)) ;
   assert (isProcedure (n)) ;
   p := makeVarParameter (i, type) ;
   IncludeIndiceIntoIndex (n^.procedureF.parameters, p)
END addVarParameters ;


(*
   addNonVarParameters - adds the identlist, i, of, type, to be parameters
                         in procedure, n.
*)

PROCEDURE addNonVarParameters (n: node; i: node; type: node) ;
VAR
   p: node ;
BEGIN
   assert (isIdentList (i)) ;
   assert (isProcedure (n)) ;
   p := makeNonVarParameter (i, type) ;
   IncludeIndiceIntoIndex (n^.procedureF.parameters, p)
END addNonVarParameters ;


(*
   makeSubrange - returns a subrange node, built from range: low..high.
*)

PROCEDURE makeSubrange (low, high: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (subrange) ;
   n^.subrangeF.low := low ;
   n^.subrangeF.high := high ;
   n^.subrangeF.type := NIL ;
   n^.subrangeF.scope := getDeclScope () ;
   RETURN n
END makeSubrange ;


(*
   putSubrangeType - assigns, type, to the subrange type, sub.
*)

PROCEDURE putSubrangeType (sub, type: node) ;
BEGIN
   assert (isSubrange (sub)) ;
   sub^.subrangeF.type := type
END putSubrangeType ;


(*
   makeSet - returns a set of, type, node.
*)

PROCEDURE makeSet (type: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (set) ;
   n^.setF.type := type ;
   n^.setF.scope := getDeclScope () ;
   RETURN n
END makeSet ;


(*
   makePointer - returns a pointer of, type, node.
*)

PROCEDURE makePointer (type: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (pointer) ;
   n^.pointerF.type := type ;
   n^.pointerF.scope := getDeclScope () ;
   RETURN n
END makePointer ;


(*
   makeArray - returns a node representing ARRAY subr OF type.
*)

PROCEDURE makeArray (subr, type: node) : node ;
VAR
   n: node ;
BEGIN
   assert (isSubrange (subr) OR isOrdinal (subr)) ;
   n := newNode (array) ;
   n^.arrayF.subr := subr ;
   n^.arrayF.type := type ;
   n^.arrayF.scope := getDeclScope () ;
   n^.arrayF.isUnbounded := FALSE ;
   RETURN n
END makeArray ;


(*
   makeRecord - creates and returns a record node.
*)

PROCEDURE makeRecord () : node ;
VAR
   n: node ;
BEGIN
   n := newNode (record) ;
   n^.recordF.localSymbols := initTree () ;
   n^.recordF.listOfSons := InitIndex (1) ;
   n^.recordF.scope := getDeclScope () ;
   RETURN n
END makeRecord ;


(*
   addFieldsToRecord - adds fields, i, of type, t, into a record, r.
                       It returns, r.
*)

PROCEDURE addFieldsToRecord (r, v, i, t: node) : node ;
VAR
   p, fj: node ;
   j, n : CARDINAL ;
   fn   : Name ;
BEGIN
   IF isRecord (r)
   THEN
      p := r ;
      v := NIL
   ELSE
      p := getRecord (getParent (r)) ;
      assert (isVarientField (r)) ;
      assert (isVarient (v)) ;
      putFieldVarient (r, v)
   END ;
   n := wlists.noOfItemsInList (i^.identlistF.names) ;
   j := 1 ;
   WHILE j<=n DO
      fn := wlists.getItemFromList (i^.identlistF.names, j) ;
      fj := getSymKey (p^.recordF.localSymbols, n) ;
      IF fj=NIL
      THEN
         fj := putFieldRecord (r, fn, t, v)
      ELSE
         metaErrors2 ('record field {%1ad} has already been declared inside a {%2Dd} {%2a}',
                      'attempting to declare a duplicate record field', fj, p)
      END ;
      INC (j)
   END ;
   RETURN r;
END addFieldsToRecord ;


(*
   makeVarient - creates a new symbol, a varient symbol for record or varient field
                 symbol, r.
*)

PROCEDURE makeVarient (r: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (varient) ;
   WITH n^ DO
      varientF.listOfSons := InitIndex (1) ;
      (* do we need to remember our parent (r) ?  *)
      (* if so use this   n^.varientF.parent := r  *)
      IF isRecord (r)
      THEN
         varientF.varient := NIL
      ELSE
         varientF.varient := r
      END ;
      varientF.tag := NIL ;
      varientF.scope := getDeclScope () ;
   END ;
   (* now add, n, to the record/varient, r, field list *)
   WITH r^ DO
      CASE kind OF

      record      :  IncludeIndiceIntoIndex (recordF.listOfSons, n) |
      varientfield:  IncludeIndiceIntoIndex (varientfieldF.listOfSons, n)

      END
   END ;
   RETURN n
END makeVarient ;


(*
   buildVarientFieldRecord - builds a varient field into a varient symbol, v.
                             The varient field is returned.
*)

PROCEDURE buildVarientFieldRecord (v: node) : node ;
VAR
   f: node ;
BEGIN
   f := makeVarientField (v) ;
   assert (isVarientField (f)) ;
   putFieldVarient (f, v) ;
   RETURN f
END buildVarientFieldRecord ;


(*
   makeVarientField - create a varient field within varient, v,
                      The new varient field is returned.
*)

PROCEDURE makeVarientField (v: node) : node ;
VAR
   n: node ;
BEGIN
   n := newNode (varientfield) ;
   WITH n^.varientfieldF DO
      name := NulName ;
      parent := NIL ;
      varient := v ;
      listOfSons := InitIndex (1) ;
      scope := getDeclScope ()
   END ;
   RETURN n
END makeVarientField ;


(*
   putFieldVarient - places the field varient, f, as a brother to, the
                     varient symbol, v, and also tells, f, that its varient
                     parent is, v.
*)

PROCEDURE putFieldVarient (f, v: node) ;
BEGIN
   assert (isVarient (v)) ;
   assert (isVarientField (f)) ;
   WITH v^ DO
      CASE kind OF

      varient:  IncludeIndiceIntoIndex (varientF.listOfSons, f)

      END
   END ;
   WITH f^ DO
      CASE kind OF

      varientfield:  varientfieldF.varient := v

      END
   END
END putFieldVarient ;


(*
   putFieldRecord -
*)

PROCEDURE putFieldRecord (r: node; tag: Name; type, v: node) : node ;
VAR
   f, n, p: node ;
BEGIN
   n := newNode (recordfield) ;
   WITH r^ DO
      CASE kind OF

      record:  IncludeIndiceIntoIndex (recordF.listOfSons, n) ;
               (* ensure that field, n, is in the parents Local Symbols.  *)
               IF tag#NulName
               THEN
                  IF getSymKey (recordF.localSymbols, tag) = NulName
                  THEN
                     putSymKey (recordF.localSymbols, tag, n)
                  ELSE
                     f := getSymKey (recordF.localSymbols, tag) ;
                     metaErrors1 ('field record {%1Dad} has already been declared',
                                  'field record duplicate', f)
                  END
               END |
      varientfield:  IncludeIndiceIntoIndex (varientfieldF.listOfSons, n) ;
                     p := getParent (r) ;
                     assert (p^.kind=record) ;
                     IF tag#NulName
                     THEN
                        putSymKey (p^.recordF.localSymbols, tag, n)
                     END

      END
   END ;
   (* fill in, n.  *)
   n^.recordfieldF.type := type ;
   n^.recordfieldF.name := tag ;
   n^.recordfieldF.parent := r ;
   n^.recordfieldF.varient := v ;
   n^.recordfieldF.tag := FALSE ;
   RETURN n
END putFieldRecord ;


(*
   buildVarientSelector - builds a field of name, tag, of, type, t, varient, r.
*)

PROCEDURE buildVarientSelector (r, v: node; tag: Name; type: node) ;
VAR
   f: node ;
BEGIN
   assert (NOT isRecord (r)) ;
   IF isVarient (r)
   THEN
      f := getParent (r) ;
      IF (type=NIL) AND (tag=NulName)
      THEN
         metaError1 ('expecting a tag field in the declaration of a varient record {%1Ua}', r)
      ELSIF type=NIL
      THEN
         putVarientTag (r, lookupSym (tag))
      ELSE
         f := putFieldRecord (f, tag, type, v) ;
         putVarientTag (r, f)
      END
   ELSE
      assert (isVarientField (r)) ;
      assert (isVarient (v)) ;
      putFieldVarient (r, v) ;
      IF (type=NIL) AND (tag=NulName)
      THEN
         metaError1 ('expecting a tag field in the declaration of a varient record {%1Ua}', r)
      ELSIF type=NIL
      THEN
         putVarientTag (v, lookupSym (tag))
      ELSE
         f := putFieldRecord (r, tag, type, v) ;
         putVarientTag (v, f)
      END
   END
END buildVarientSelector ;


(*
   ensureOrder - ensures that, a, and, b, exist in, i, and also
                 ensure that, a, is before, b.
*)

PROCEDURE ensureOrder (i: Index; a, b: node) ;
BEGIN
   assert (IsIndiceInIndex (i, a)) ;
   assert (IsIndiceInIndex (i, b)) ;
   RemoveIndiceFromIndex (i, a) ;
   RemoveIndiceFromIndex (i, b) ;
   IncludeIndiceIntoIndex (i, a) ;
   IncludeIndiceIntoIndex (i, b) ;
   assert (IsIndiceInIndex (i, a)) ;
   assert (IsIndiceInIndex (i, b))
END ensureOrder ;


(*
   putVarientTag -
*)

PROCEDURE putVarientTag (v: node; tag: node) ;
VAR
   p: node ;
BEGIN
   assert (isVarient (v)) ;
   CASE v^.kind OF

   varient:  v^.varientF.tag := tag

   END ;
   (* now ensure that if tag is a recordfield then it must be
      placed before the varient symbol in its parent listOfSons.
      This keeps the record fields in order as we want the tag
      before the C union.
   *)
   IF isRecordField (tag)
   THEN
      tag^.recordfieldF.tag := TRUE ;
      p := getParent(v) ;
      CASE p^.kind OF

      varient     : ensureOrder (p^.varientF.listOfSons, tag, v) |
      varientfield: ensureOrder (p^.varientfieldF.listOfSons, tag, v) |
      record      : ensureOrder (p^.recordF.listOfSons, tag, v)

      END
   END
END putVarientTag ;


(*
   getParent - returns the parent field of recordfield or varientfield symbol, n.
*)

PROCEDURE getParent (n: node) : node ;
BEGIN
   CASE n^.kind OF

   recordfield:  RETURN n^.recordfieldF.parent |
   varientfield:  RETURN n^.varientfieldF.parent

   END
END getParent ;


(*
   getRecord - returns the record associated with node, n.
               (Parental record).
*)

PROCEDURE getRecord (n: node) : node ;
BEGIN
   assert (n^.kind # varient) ;  (* if this fails then we need to add parent field to varient.  *)
   CASE n^.kind OF

   record      :  RETURN n |
(*   varient    :  RETURN getRecord (getParent (n)) | *)
   varientfield:  RETURN getRecord (getParent (n))

   END
END getRecord ;


(*
   putUnbounded - sets array, n, as unbounded.
*)

PROCEDURE putUnbounded (n: node) ;
BEGIN
   assert (n^.kind = array) ;
   n^.arrayF.isUnbounded := TRUE
END putUnbounded ;


(*
   addEnumToModule - adds enumeration type, e, into the list of enums
                     in module, m.
*)

PROCEDURE addEnumToModule (m, e: node) ;
BEGIN
   assert (isEnumeration (e)) ;
   assert (isModule (m) OR isDef (m) OR isImp (m)) ;
   IF isModule (m)
   THEN
      IncludeIndiceIntoIndex (m^.moduleF.enums, e)
   ELSIF isDef (m)
   THEN
      IncludeIndiceIntoIndex (m^.defF.enums, e)
   ELSIF isImp (m)
   THEN
      IncludeIndiceIntoIndex (m^.impF.enums, e)
   END
END addEnumToModule ;


(*
   getNextEnum - returns the next enumeration node.
*)

PROCEDURE getNextEnum () : node ;
VAR
   n: node ;
BEGIN
   assert (isDef (currentModule) OR isImp (currentModule) OR isModule (currentModule)) ;
   WITH currentModule^ DO
      IF isDef (currentModule)
      THEN
         INC (defF.enumCount) ;
         n := GetIndice (defF.enums, defF.enumCount)
      ELSIF isImp (currentModule)
      THEN
         INC (impF.enumCount) ;
         n := GetIndice (impF.enums, impF.enumCount)
      ELSIF isModule (currentModule)
      THEN
         INC (moduleF.enumCount) ;
         n := GetIndice (moduleF.enums, moduleF.enumCount)
      END
   END ;
   RETURN n
END getNextEnum ;


(*
   resetEnumPos - resets the index into the saved list of enums inside
                  module, n.
*)

PROCEDURE resetEnumPos (n: node) ;
BEGIN
   assert (isDef (n) OR isImp (n) OR isModule (n)) ;
   IF isDef (n)
   THEN
      n^.defF.enumCount := 0
   ELSIF isImp (n)
   THEN
      n^.impF.enumCount := 0
   ELSIF isModule (n)
   THEN
      n^.moduleF.enumCount := 0
   END
END resetEnumPos ;


(*
   makeEnum - creates an enumerated type and returns the node.
*)

PROCEDURE makeEnum () : node ;
VAR
   e: node ;
BEGIN
   e := newNode (enumeration) ;
   WITH e^ DO
      enumerationF.noOfElements := 0 ;
      enumerationF.localSymbols := initTree () ;
      enumerationF.scope := getDeclScope () ;
      enumerationF.listOfSons := InitIndex (1)
   END ;
   addEnumToModule (currentModule, e) ;
   RETURN e
END makeEnum ;


(*
   makeEnumField - returns an enumeration field, named, n.
*)

PROCEDURE makeEnumField (e: node; n: Name) : node ;
VAR
   f: node ;
BEGIN
   assert (isEnumeration (e)) ;
   f := getSymKey (e^.enumerationF.localSymbols, n) ;
   IF f=NIL
   THEN
      f := newNode (enumerationfield) ;
      putSymKey (e^.enumerationF.localSymbols, n, f) ;
      IncludeIndiceIntoIndex (e^.enumerationF.listOfSons, f) ;
      WITH f^ DO
         enumerationfieldF.name := n ;
         enumerationfieldF.type := e ;
         enumerationfieldF.scope := getDeclScope () ;
         enumerationfieldF.value := e^.enumerationF.noOfElements
      END ;
      INC (e^.enumerationF.noOfElements) ;
      assert (GetIndice (e^.enumerationF.listOfSons, e^.enumerationF.noOfElements) = f) ;
      RETURN f
   ELSE
      metaErrors2 ('cannot create enumeration field {%1k} as the name is already in use',
                   '{%2DMad} was declared elsewhere', n, f)
   END
END makeEnumField ;


(*
   isEnumeration - returns TRUE if node, n, is an enumeration type.
*)

PROCEDURE isEnumeration (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind = enumeration
END isEnumeration ;


(*
   putType - places, exp, as the type alias to des.
             TYPE des = exp ;
*)

PROCEDURE putType (des, exp: node) ;
BEGIN
   assert (des#NIL) ;
   assert (isType (des)) ;
   des^.typeF.type := exp
END putType ;


(*
   putTypeHidden - marks type, des, as being a hidden type.
                   TYPE des ;
*)

PROCEDURE putTypeHidden (des: node) ;
BEGIN
   assert (des#NIL) ;
   assert (isType (des)) ;
   des^.typeF.isHidden := TRUE
END putTypeHidden ;


(*
   isTypeHidden - returns TRUE if type, n, is hidden.
*)

PROCEDURE isTypeHidden (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   assert (isType (n)) ;
   RETURN n^.typeF.isHidden
END isTypeHidden ;


(*
   putConst - places value, v, into node, n.
*)

PROCEDURE putConst (n: node; v: node) ;
BEGIN
   assert (isConst (n)) ;
   n^.constF.value := v
END putConst ;


(*
   makeLiteralInt - creates and returns a literal node based on an integer type.
*)

PROCEDURE makeLiteralInt (n: Name) : node ;
VAR
   m: node ;
   s: String ;
BEGIN
   m := newNode (literal) ;
   s := InitStringCharStar (keyToCharStar (n)) ;
   WITH m^ DO
      literalF.name := n ;
      IF DynamicStrings.char (s, -1)='C'
      THEN
         literalF.type := charN
      ELSE
         literalF.type := ztypeN
      END
   END ;
   s := KillString (s) ;
   RETURN m
END makeLiteralInt ;


(*
   makeLiteralReal - creates and returns a literal node based on a real type.
*)

PROCEDURE makeLiteralReal (n: Name) : node ;
VAR
   m: node ;
BEGIN
   m := newNode (literal) ;
   WITH m^ DO
      literalF.name := n ;
      literalF.type := rtypeN
   END ;
   RETURN m
END makeLiteralReal ;


(*
   makeString - creates and returns a node containing string, n.
*)

PROCEDURE makeString (n: Name) : node ;
VAR
   m: node ;
BEGIN
   m := newNode (string) ;
   WITH m^ DO
      stringF.name := n ;
      stringF.length := lengthKey (n) ;
      stringF.isCharCompatible := (stringF.length = 1)
   END ;
   RETURN m
END makeString ;


(*
   getBuiltinConst - creates and returns a builtin const if available.
*)

PROCEDURE getBuiltinConst (n: Name) : node ;
BEGIN
   IF n=makeKey ('BITS_PER_UNIT')
   THEN
      RETURN bitsperunitN
   ELSIF n=makeKey ('BITS_PER_WORD')
   THEN
      RETURN bitsperwordN
   ELSIF n=makeKey ('BITS_PER_CHAR')
   THEN
      RETURN bitspercharN
   ELSIF n=makeKey ('UNITS_PER_WORD')
   THEN
      RETURN unitsperwordN
   ELSE
      RETURN NIL
   END
END getBuiltinConst ;


(*
   lookupInScope - looks up a symbol named, n, from, scope.
*)

PROCEDURE lookupInScope (scope: node; n: Name) : node ;
BEGIN
   CASE scope^.kind OF

   def      :  RETURN getSymKey (scope^.defF.decls.symbols, n) |
   module   :  RETURN getSymKey (scope^.moduleF.decls.symbols, n) |
   imp      :  RETURN getSymKey (scope^.impF.decls.symbols, n) |
   procedure:  RETURN getSymKey (scope^.procedureF.decls.symbols, n)

   END
END lookupInScope ;


(*
   lookupBase -
*)

PROCEDURE lookupBase (n: Name) : node ;
VAR
   m: node ;
BEGIN
   m := getSymKey (baseSymbols, n) ;
   IF m=procN
   THEN
      procUsed := TRUE
   END ;
   RETURN m
END lookupBase ;


(*
   lookupSym - returns the symbol named, n, from the scope stack.
*)

PROCEDURE lookupSym (n: Name) : node ;
VAR
   s, m: node ;
   l, h: CARDINAL ;
BEGIN
   l := LowIndice (scopeStack) ;
   h := HighIndice (scopeStack) ;
   WHILE h>=l DO
      s := GetIndice (scopeStack, h) ;
      m := lookupInScope (s, n) ;
      IF m#NIL
      THEN
         RETURN m
      END ;
      DEC (h)
   END ;
   RETURN lookupBase (n)
END lookupSym ;


(*
   getSymName - returns the name of symbol, n.
*)

PROCEDURE getSymName (n: node) : Name ;
BEGIN
   WITH n^ DO
      CASE kind OF

      address         :  RETURN makeKey ('ADDRESS') |
      byte            :  RETURN makeKey ('BYTE') |
      word            :  RETURN makeKey ('WORD') |
      (* base types.  *)
      boolean         :  RETURN makeKey ('BOOLEAN') |
      proc            :  RETURN makeKey ('PROC') |
      char            :  RETURN makeKey ('CHAR') |
      cardinal        :  RETURN makeKey ('CARDINAL') |
      longcard        :  RETURN makeKey ('LONGCARD') |
      shortcard       :  RETURN makeKey ('SHORTCARD') |
      integer         :  RETURN makeKey ('INTEGER') |
      longint         :  RETURN makeKey ('LONGINT') |
      shortint        :  RETURN makeKey ('SHORTINT') |
      real            :  RETURN makeKey ('REAL') |
      longreal        :  RETURN makeKey ('LONGREAL') |
      shortreal       :  RETURN makeKey ('SHORTREAL') |
      bitset          :  RETURN makeKey ('BITSET') |
      ztype           :  RETURN makeKey ('_ZTYPE') |
      rtype           :  RETURN makeKey ('_RTYPE') |
      (* language features and compound type attributes.  *)
      type            :  RETURN typeF.name |
      record          :  RETURN NulName |
      varient         :  RETURN NulName |
      var             :  RETURN varF.name |
      enumeration     :  HALT |
      subrange        :  HALT |
      pointer         :  HALT |
      array           :  HALT |
      string          :  RETURN stringF.name |
      const           :  RETURN constF.name |
      literal         :  RETURN literalF.name |
      varparam        :  HALT |
      param           :  HALT |
      recordfield     :  RETURN recordfieldF.name |
      varientfield    :  RETURN varientfieldF.name |
      enumerationfield:  RETURN enumerationfieldF.name |
      set             :  HALT |
      proctype        :  HALT |
      subscript       :  HALT |
      (* blocks.  *)
      procedure       :  RETURN procedureF.name |
      def             :  RETURN defF.name |
      imp             :  RETURN impF.name |
      module          :  RETURN moduleF.name |
      (* statements.  *)
      loop,
      while,
      for,
      repeat,
      if,
      elsif,
      assignment      :  RETURN NulName |
      (* expressions.  *)
      componentref,
      ord,
      cast,
      val,
      plus,
      sub,
      div,
      mod,
      neg,
      adr,
      size,
      indirect,
      equal,
      notequal,
      less,
      greater,
      greequal,
      lessequal       :  RETURN NulName

      END
   END
END getSymName ;


(*
   makeUnary - create a unary expression node with, e, as the argument
               and res as the return type.
*)

PROCEDURE makeUnary (k: nodeT; e: node; res: node) : node ;
VAR
   n: node ;
BEGIN
   NEW (n) ;
   WITH n^ DO
      kind := k ;
      CASE kind OF

      not,
      neg,
      adr,
      size:  WITH unaryF DO
                arg := e ;
                resultType := res
             END

      END
   END ;
   RETURN n
END makeUnary ;


(*
   makeBinary - create a binary node with left/right/result type:  l, r and res.
*)

PROCEDURE makeBinary (k: nodeT; l, r: node; res: node) : node ;
VAR
   n: node ;
BEGIN
   NEW (n) ;
   WITH n^ DO
      kind := k;
      CASE kind OF

      equal,
      notequal,
      less,
      greater,
      greequal,
      lessequal,
      and,
      or,
      componentref,
      ord,
      cast,
      val,
      plus,
      sub,
      div,
      mod,
      neg :  WITH binaryF DO
                left := l ;
                right := r ;
                resultType := res
             END

      END
   END ;
   RETURN n
END makeBinary ;


(*
   makeBase - create a base type or constant.
              It only supports the base types and constants
              enumerated below.
*)

PROCEDURE makeBase (k: nodeT) : node ;
VAR
   n: node ;
BEGIN
   NEW (n) ;
   WITH n^ DO
      kind := k ;
      CASE k OF

      nil,
      true,
      false,
      address,
      byte,
      word,
      char,
      cardinal,
      longcard,
      shortcard,
      integer,
      longint,
      shortint,
      bitset,
      ztype,
      rtype   :  (* legal kind.  *) |

      END
   END ;
   RETURN n
END makeBase ;


(*
   makeBinaryTok - creates and returns a boolean type node with,
                   l, and, r, nodes.
*)

PROCEDURE makeBinaryTok (op: toktype; l, r: node) : node ;
BEGIN
   IF op=lesstok
   THEN
      RETURN makeBinary (lessequal, l, r, booleanN)
   ELSIF op=equal
   THEN
      RETURN makeBinary (less, l, r, booleanN)
   ELSIF (op=hashtok) OR (op=lessgreatertok)
   THEN
      RETURN makeBinary (notequal, l, r, booleanN)
   ELSIF op=lesstok
   THEN
      RETURN makeBinary (less, l, r, booleanN)
   ELSIF op=greatertok
   THEN
      RETURN makeBinary (greater, l, r, booleanN)
   ELSIF op=greaterequaltok
   THEN
      RETURN makeBinary (greequal, l, r, booleanN)
   ELSIF op=andtok
   THEN
      RETURN makeBinary (and, l, r, booleanN)
   ELSIF op=ortok
   THEN
      RETURN makeBinary (or, l, r, booleanN)
   ELSE
      HALT  (* most likely op needs a clause as above.  *)
   END
END makeBinaryTok ;


(*
   makeUnaryTok - creates and returns a boolean type node with,
                  e, node.
*)

PROCEDURE makeUnaryTok (op: toktype; e: node) : node ;
BEGIN
   IF op=nottok
   THEN
      RETURN makeUnary (not, e, booleanN)
   ELSIF op=plustok
   THEN
      RETURN makeUnary (plus, e, NIL)
   ELSIF op=minustok
   THEN
      RETURN makeUnary (sub, e, NIL)
   ELSE
      HALT  (* most likely op needs a clause as above.  *)
   END
END makeUnaryTok ;


(*
   isOrdinal - returns TRUE if, n, is an ordinal type.
*)

PROCEDURE isOrdinal (n: node) : BOOLEAN ;
BEGIN
   CASE n^.kind OF

   address,
   byte,
   word,
   char,
   integer,
   longint,
   shortint,
   cardinal,
   longcard,
   shortcard,
   bitset   :  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END isOrdinal ;


(*
   getType - returns the type associated with node, n.
*)

PROCEDURE getType (n: node) : node ;
BEGIN
   WITH n^ DO
      CASE kind OF

      nil             :  RETURN addressN |
      true,
      false           :  RETURN booleanN |
      address         :  RETURN n |
      byte            :  RETURN n |
      word            :  RETURN n |
      (* base types.  *)
      boolean         :  RETURN n |
      proc            :  RETURN n |
      char            :  RETURN n |
      cardinal        :  RETURN n |
      longcard        :  RETURN n |
      shortcard       :  RETURN n |
      integer         :  RETURN n |
      longint         :  RETURN n |
      shortint        :  RETURN n |
      real            :  RETURN n |
      longreal        :  RETURN n |
      shortreal       :  RETURN n |
      bitset          :  RETURN n |
      ztype           :  RETURN n |
      rtype           :  RETURN n |
      (* language features and compound type attributes.  *)
      type            :  RETURN typeF.type |
      record          :  RETURN n |
      varient         :  RETURN n |
      var             :  RETURN varF.type |
      enumeration     :  RETURN n |
      subrange        :  RETURN subrangeF.type |
      array           :  RETURN arrayF.type |
      string          :  RETURN charN |
      const           :  RETURN constF.type |
      literal         :  RETURN literalF.type |
      varparam        :  RETURN varparamF.type |
      param           :  RETURN paramF.type |
      pointer         :  RETURN pointerF.type |
      recordfield     :  RETURN recordfieldF.type |
      varientfield    :  RETURN n |
      enumerationfield:  RETURN enumerationfieldF.type |
      set             :  RETURN setF.type |
      proctype        :  RETURN proctypeF.returnType |
      subscript       :  RETURN subscriptF.type |
      (* blocks.  *)
      procedure       :  RETURN procedureF.returnType |
      def,
      imp,
      module,
      (* statements.  *)
      loop,
      while,
      for,
      repeat,
      if,
      elsif,
      assignment      :  HALT |
      (* expressions.  *)
      componentref,
      cast,
      val,
      plus,
      sub,
      div,
      mod             :  RETURN binaryF.resultType |
      indirect,
      ord,
      neg,
      adr,
      size            :  RETURN unaryF.resultType |
      equal,
      notequal,
      less,
      greater,
      greequal,
      lessequal       :  RETURN booleanN

      END
   END ;
   HALT
END getType ;


(*
   skipType - skips over type aliases.
*)

PROCEDURE skipType (n: node) : node ;
BEGIN
   WHILE (n#NIL) AND isType (n) DO
      n := getType (n)
   END ;
   RETURN n
END skipType ;


(*
   getScope - returns the scope associated with node, n.
*)

PROCEDURE getScope (n: node) : node ;
BEGIN
   WITH n^ DO
      CASE kind OF

      nil,
      true,
      false           :  RETURN NIL |
      address,
      byte,
      word            :  RETURN systemN |
      (* base types.  *)
      boolean,
      proc,
      char,
      cardinal,
      longcard,
      shortcard,
      integer,
      longint,
      shortint,
      real,
      longreal,
      shortreal,
      bitset,
      ztype,
      rtype           :  RETURN NIL |
      (* language features and compound type attributes.  *)
      type            :  RETURN typeF.scope |
      record          :  RETURN recordF.scope |
      varient         :  RETURN varientF.scope |
      var             :  RETURN varF.scope |
      enumeration     :  RETURN enumerationF.scope |
      subrange        :  RETURN subrangeF.scope |
      array           :  RETURN arrayF.scope |
      string          :  RETURN NIL |
      const           :  RETURN constF.scope |
      literal         :  RETURN NIL |
      varparam        :  RETURN varparamF.scope |
      param           :  RETURN paramF.scope |
      pointer         :  RETURN pointerF.scope |
      recordfield     :  RETURN recordfieldF.scope |
      varientfield    :  RETURN varientfieldF.scope |
      enumerationfield:  RETURN enumerationfieldF.scope |
      set             :  RETURN setF.scope |
      proctype        :  RETURN proctypeF.scope |
      subscript       :  RETURN NIL |
      (* blocks.  *)
      procedure       :  RETURN procedureF.scope |
      def,
      imp,
      module,
      (* statements.  *)
      loop,
      while,
      for,
      repeat,
      if,
      elsif,
      assignment      :  RETURN NIL |
      (* expressions.  *)
      componentref,
      ord,
      cast,
      val,
      plus,
      sub,
      div,
      mod             :  RETURN NIL |
      neg,
      adr,
      size            :  RETURN NIL |
      indirect,
      equal,
      notequal,
      less,
      greater,
      greequal,
      lessequal       :  RETURN NIL

      END
   END
END getScope ;


(*
   foreachDefModuleDo - foreach definition node, n, in the module universe,
                        call p (n).
*)

PROCEDURE foreachDefModuleDo (p: performOperation) ;
BEGIN
   ForeachIndiceInIndexDo (defUniverseI, p)
END foreachDefModuleDo ;


(*
   openOutput -
*)

PROCEDURE openOutput ;
VAR
   s: String ;
BEGIN
   s := getOutputFile () ;
   IF EqualArray (s, '-')
   THEN
      outputFile := StdOut
   ELSE
      outputFile := OpenToWrite (s)
   END
END openOutput ;


(*
   closeOutput -
*)

PROCEDURE closeOutput ;
VAR
   s: String ;
BEGIN
   s := getOutputFile () ;
   FlushBuffer (outputFile) ;
   IF NOT EqualArray (s, '-')
   THEN
      Close (outputFile)
   END
END closeOutput ;


(*
   write - outputs a single char, ch.
*)

PROCEDURE write (ch: CHAR) ;
BEGIN
   WriteChar (outputFile, ch)
END write ;


(*
   writeln -
*)

PROCEDURE writeln ;
BEGIN
   WriteLine (outputFile)
END writeln ;


(*
   doIncludeC - include header file for definition module, n.
*)

PROCEDURE doIncludeC (n: node) ;
VAR
   s: String ;
BEGIN
   IF isDef (n)
   THEN
      s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
      print (doP, '#   include "G') ;
      prints (doP, s) ;
      print (doP, '.h"\n') ;
      s := KillString (s)
   ELSE
      HALT
   END
END doIncludeC ;


(*
   getSymScope - returns the scope where node, n, was declared.
*)

PROCEDURE getSymScope (n: node) : node ;
BEGIN
   WITH n^ DO
      CASE kind OF

      const    :  RETURN constF.scope |
      type     :  RETURN typeF.scope |
      var      :  RETURN varF.scope |
      procedure:  RETURN procedureF.scope

      END
   END ;
   HALT
END getSymScope ;


(*
   getFQstring -
*)

PROCEDURE getFQstring (n: node) : String ;
VAR
   i, s: String ;
BEGIN
   IF (getScope (n)=NIL) OR ignoreFQ
   THEN
      RETURN InitStringCharStar (keyToCharStar (getSymName (n)))
   ELSE
      i := InitStringCharStar (keyToCharStar (getSymName (n))) ;
      s := InitStringCharStar (keyToCharStar (getSymName (getScope (n)))) ;
      RETURN Sprintf2 (InitString ("%s_%s"), s, i)
   END
END getFQstring ;


(*
   getString - returns the name as a string.
*)

PROCEDURE getString (n: node) : String ;
BEGIN
   IF getSymName (n) = NulName
   THEN
      RETURN InitString ('')
   ELSE
      RETURN InitStringCharStar (keyToCharStar (getSymName (n)))
   END
END getString ;


(*
   getCardinal - returns the cardinal type node.
*)

PROCEDURE getCardinal () : node ;
BEGIN
   RETURN cardinalN
END getCardinal ;


(*
   doNone - call HALT.
*)

PROCEDURE doNone (n: node) ;
BEGIN
   HALT
END doNone ;

(*
   doConstC -
*)

PROCEDURE doConstC (n: node) ;
BEGIN
   print (doP, "#   define ") ;
   doFQNameC (doP, n) ;
   setNeedSpace (doP) ;
   doExprC (doP, n^.constF.value) ;
   print (doP, '\n')
END doConstC ;


(*
   doUnary -
*)

PROCEDURE doUnary (p: pretty; op: ARRAY OF CHAR; expr, type: node) ;
BEGIN
   print (p, op) ;
   setNeedSpace (p) ;
   IF StrEqual (op, 'ADR') OR StrEqual (op, 'SIZE') OR StrEqual (op, 'ORD')
   THEN
      outText (p, '(') ;
      doExprC (p, expr) ;
      outText (p, ')')
   ELSE
      outText (p, '(') ;
      doExprC (p, expr) ;
      outText (p, ')')
   END
END doUnary ;


(*
   doBinary -
*)

PROCEDURE doBinary (p: pretty; op: ARRAY OF CHAR; left, right: node) ;
BEGIN
   outText (p, '(') ;
   doExprC (p, left) ;
   outText (p, ')') ;
   outText (p, op) ;
   outText (p, '(') ;
   doExprC (p, right) ;
   outText (p, ')')
END doBinary ;


(*
   doPostUnary -
*)

PROCEDURE doPostUnary (p: pretty; op: ARRAY OF CHAR; expr: node) ;
BEGIN
   doExprC (p, expr) ;
   outText (p, op)
END doPostUnary ;


(*
   doPreBinary -
*)

PROCEDURE doPreBinary (p: pretty; op: ARRAY OF CHAR; left, right: node) ;
BEGIN
   outText (p, op) ;
   outText (p, '(') ;
   doExprC (p, left) ;
   outText (p, ',') ;
   doExprC (p, right) ;
   outText (p, ')')
END doPreBinary ;


(*
   doConstExpr -
*)

PROCEDURE doConstExpr (p: pretty; n: node) ;
BEGIN
   doFQNameC (p, n)
END doConstExpr ;


(*
   doExprC -
*)

PROCEDURE doExprC (p: pretty; n: node) ;
BEGIN
   assert (n#NIL) ;
   WITH n^ DO
      CASE kind OF

      nil         :  outText (p, 'NIL') |
      true        :  outText (p, 'TRUE') |
      false       :  outText (p, 'FALSE') |
      neg         :  doUnary (p, '-', unaryF.arg, unaryF.resultType) |
      not         :  doUnary (p, 'NOT', unaryF.arg, unaryF.resultType) |
      adr         :  doUnary (p, 'ADR', unaryF.arg, unaryF.resultType) |
      size        :  doUnary (p, 'SIZE', unaryF.arg, unaryF.resultType) |
      ord         :  doUnary (p, 'ORD', unaryF.arg, unaryF.resultType) |
      indirect    :  doPostUnary (p, '^', unaryF.arg) |
      equal       :  doBinary (p, '=', binaryF.left, binaryF.right) |
      notequal    :  doBinary (p, '#', binaryF.left, binaryF.right) |
      less        :  doBinary (p, '<', binaryF.left, binaryF.right) |
      greater     :  doBinary (p, '>', binaryF.left, binaryF.right) |
      greequal    :  doBinary (p, '>=', binaryF.left, binaryF.right) |
      lessequal   :  doBinary (p, '<=', binaryF.left, binaryF.right) |
      componentref:  doBinary (p, '.', binaryF.left, binaryF.right) |
      cast        :  doPreBinary (p, 'CAST', binaryF.left, binaryF.right) |
      val         :  doPreBinary (p, 'VAL', binaryF.left, binaryF.right) |
      plus        :  doBinary (p, '+', binaryF.left, binaryF.right) |
      sub         :  doBinary (p, '-', binaryF.left, binaryF.right) |
      div         :  doBinary (p, '/', binaryF.left, binaryF.right) |
      mod         :  doBinary (p, 'MOD', binaryF.left, binaryF.right) |
      literal     :  doLiteral (p, n) |
      const       :  doConstExpr (p, n)

      END
   END
END doExprC ;


(*
   doLiteral -
*)

PROCEDURE doLiteral (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   assert (isLiteral (n)) ;
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   IF n^.literalF.type=charN
   THEN
      IF DynamicStrings.char (s, -1)='C'
      THEN
         s := DynamicStrings.Slice (DynamicStrings.Mark (s), 0, -1) ;
	 IF DynamicStrings.char (s, 0)#'0'
         THEN
            s := DynamicStrings.ConCat (InitString('0'), DynamicStrings.Mark (s))
         END
      END ;
      outText (p, "(char)") ;
      setNeedSpace (p)
   END ;
   outTextS (p, s) ;
   s := KillString (s)
END doLiteral ;


(*
   isString - returns TRUE if node, n, is a string.
*)

PROCEDURE isString (n: node) : BOOLEAN ;
BEGIN
   assert (n#NIL) ;
   RETURN n^.kind=string
END isString ;


(*
   doString -
*)

PROCEDURE doString (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   assert (isString (n)) ;
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   IF DynamicStrings.Index (s, '"', 0)=-1
   THEN
      outText (p, '"') ;
      outTextS (p, s) ;
      outText (p, '"')
   ELSIF DynamicStrings.Index (s, "'", 0)=-1
   THEN
      outText (p, '"') ;
      outTextS (p, s) ;
      outText (p, '"')
   ELSE
      metaError1 ('illegal string {%1k}', n)
   END
END doString ;


(*
   isPunct -
*)

PROCEDURE isPunct (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch = '.') OR (ch = '(') OR (ch = ')') OR
          (ch = '^') OR (ch = ':') OR (ch = ';') OR
	  (ch = '{') OR (ch = '}') OR (ch = ',') OR
	  (ch = '*')
END isPunct ;


(*
   isWhite -
*)

PROCEDURE isWhite (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch = ' ') OR (ch = tab) OR (ch = lf)
END isWhite ;


(*
   outText -
*)

PROCEDURE outText (p: pretty; a: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   s := InitString (a) ;
   outTextS (p, s) ;
   s := KillString (s)
END outText ;


(*
   outTextS -
*)

PROCEDURE outTextS (p: pretty; s: String) ;
BEGIN
   IF (s # NIL) AND (NOT EqualArray (s, ''))
   THEN
      s := Sprintf0 (s) ;
      prints (p, s) ;
      (*
      IF isPunct (DynamicStrings.char (s, -1))
      THEN
         outputState := punct
      ELSIF isWhite (DynamicStrings.char (s, -1))
      THEN
         outputState := space
      ELSE
         outputState := text
      END
      *)
   END
END outTextS ;


(*
   doTypeAliasC -
*)

PROCEDURE doTypeAliasC (p: pretty; n: node; VAR m: node) ;
BEGIN
   print (p, "typedef") ; setNeedSpace (p) ;
   IF isTypeHidden (n) AND (isDef (getMainModule ()) OR (getScope (n) # getMainModule ()))
   THEN
      outText (p, "void *")
   ELSE
      doTypeC (p, getType (n), m)
   END ;
   IF m#NIL
   THEN
      doFQNameC (p, m)
   END ;
   print (p, ';\n\n')
END doTypeAliasC ;


(*
   doEnumerationC -
*)

PROCEDURE doEnumerationC (p: pretty; n: node) ;
VAR
   i, h: CARDINAL ;
   s   : node ;
   t   : String ;
BEGIN
   outText (p, "enum {") ;
   i := LowIndice (n^.enumerationF.listOfSons) ;
   h := HighIndice (n^.enumerationF.listOfSons) ;
   WHILE i <= h DO
      s := GetIndice (n^.enumerationF.listOfSons, i) ;
      doFQNameC (p, s) ;
      IF i < h
      THEN
         outText (p, ",") ; setNeedSpace (p)
      END ;
      INC (i)
   END ;
   outText (p, "}")
END doEnumerationC ;


(*
   doNamesC -
*)

PROCEDURE doNamesC (p: pretty; n: Name) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (keyToCharStar (n)) ;
   outTextS (p, s) ;
   s := KillString (s)
END doNamesC ;


(*
   doNameC -
*)

PROCEDURE doNameC (p: pretty; n: node) ;
BEGIN
   IF (n#NIL) AND (getSymName (n)#NulName)
   THEN
      doNamesC (p, getSymName (n))
   END
END doNameC ;


(*
   doFQNameC -
*)

PROCEDURE doFQNameC (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   s := getFQstring (n) ;
   outTextS (p, s) ;
   s := KillString (s)
END doFQNameC ;


(*
   doHighC -
*)

PROCEDURE doHighC (p: pretty; a: node; n: Name) ;
VAR
   s: String ;
BEGIN
   IF isArray (a) AND isUnbounded (a)
   THEN
      (* need to display high.  *)
      print (p, ",") ; setNeedSpace (p) ;
      doTypeNameC (p, cardinalN) ; setNeedSpace (p) ;
      s := InitStringCharStar (keyToCharStar (n)) ;
      print (p, "_") ; prints (p, s) ; print (p, "_high")
   END
END doHighC ;


(*
   doParamC -
*)

PROCEDURE doParamC (p: pretty; n: node) ;
VAR
   ptype: node ;
   i    : Name ;
   c, t : CARDINAL ;
   l    : wlist ;
BEGIN
   assert (isParam (n)) ;
   ptype := getType (n) ;
   IF n^.paramF.namelist = NIL
   THEN
      doTypeNameC (p, ptype)
   ELSE
      assert (isIdentList (n^.paramF.namelist)) ;
      l := n^.paramF.namelist^.identlistF.names ;
      IF l=NIL
      THEN
         doTypeNameC (p, ptype)
      ELSE
         t := wlists.noOfItemsInList (l) ;
         c := 1 ;
         WHILE c <= t DO
            doTypeNameC (p, ptype) ;
            i := wlists.getItemFromList (l, c) ;
	    setNeedSpace (p) ;
            doNamesC (p, i) ;
	    doHighC (p, ptype, i) ;
            IF c<t
            THEN
               outText (p, ',') ; setNeedSpace (p)
            END ;
            INC (c)
         END
      END
   END
END doParamC ;


(*
   doVarParamC -
*)

PROCEDURE doVarParamC (p: pretty; n: node) ;
VAR
   ptype: node ;
   i    : Name ;
   c, t : CARDINAL ;
   l    : wlist ;
BEGIN
   assert (isVarParam (n)) ;
   ptype := getType (n) ;
   IF n^.varparamF.namelist = NIL
   THEN
      doTypeC (p, ptype, n) ;
      IF NOT isArray (ptype)
      THEN
         setNeedSpace (p) ;
         outText (p, "*")
      END
   ELSE
      assert (isIdentList (n^.varparamF.namelist)) ;
      l := n^.varparamF.namelist^.identlistF.names ;
      IF l=NIL
      THEN
         doTypeNameC (p, ptype)
      ELSE
         t := wlists.noOfItemsInList (l) ;
         c := 1 ;
         WHILE c <= t DO
            doTypeNameC (p, ptype) ;
	    IF NOT isArray (ptype)
            THEN
               setNeedSpace (p) ;
               outText (p, "*")
            END ;
            i := wlists.getItemFromList (l, c) ;
            doNamesC (p, i) ;
            doHighC (p, ptype, i) ;
            IF c<t
            THEN
               outText (p, ',') ; setNeedSpace (p)
            END ;
            INC (c)
         END
      END
   END
END doVarParamC ;


(*
   doParameterC -
*)

PROCEDURE doParameterC (p: pretty; n: node) ;
BEGIN
   IF isParam (n)
   THEN
      doParamC (p, n)
   ELSIF isVarParam (n)
   THEN
      doVarParamC (p, n)
   ELSIF isVarargs (n)
   THEN
      print (p, "...")
   END
END doParameterC ;


(*
   doProcTypeC -
*)

PROCEDURE doProcTypeC (p: pretty; t, n: node) ;
BEGIN
   assert (isType (t)) ;
   outputPartial (t) ;
   doCompletePartialProcType (p, t, n)
END doProcTypeC ;


(*
   doTypesC -
*)

PROCEDURE doTypesC (n: node) ;
VAR
   m: node ;
BEGIN
   IF isType (n)
   THEN
      m := getType (n) ;
      IF isProcType (m)
      THEN
         doProcTypeC (doP, n, m)
      ELSE
         outText (doP, "typedef") ; setNeedSpace (doP) ;
         doTypeC (doP, m, m) ;
         setNeedSpace (doP) ;
         doTypeNameC (doP, n) ;
         outText (doP, ";\n\n")
      END
   END
END doTypesC ;


(*
   doCompletePartialC -
*)

PROCEDURE doCompletePartialC (n: node) ;
VAR
   m: node ;
BEGIN
   IF isType (n)
   THEN
      m := getType (n) ;
      IF isRecord (m)
      THEN
         doCompletePartialRecord (doP, n, m)
      ELSIF isArray (m)
      THEN
         doCompletePartialArray (doP, n, m)
      ELSIF isProcType (m)
      THEN
         doCompletePartialProcType (doP, n, m)
      END
   END
END doCompletePartialC ;


(*
   doCompletePartialRecord -
*)

PROCEDURE doCompletePartialRecord (p: pretty; t, r: node) ;
VAR
   i, h: CARDINAL ;
   f   : node ;
   q   : pretty ;
BEGIN
   assert (isRecord (r)) ;
   assert (isType (t)) ;
   outText (p, "struct") ; setNeedSpace (p) ;
   doFQNameC (p, t) ;
   outText (p, "_r {\n") ;
   q := dupPretty (p) ;
   i := LowIndice (r^.recordF.listOfSons) ;
   h := HighIndice (r^.recordF.listOfSons) ;
   setindent (q, getindent (q)+indentation) ;
   WHILE i<=h DO
      f := GetIndice (r^.recordF.listOfSons, i) ;
      IF isRecordField (f)
      THEN
         setNeedSpace (q) ;
         doRecordFieldC (q, f)
      END ;
      INC (i)
   END ;
   killPretty (q) ;
   outText (p, "};\n\n")
END doCompletePartialRecord ;


(*
   doCompletePartialArray -
*)

PROCEDURE doCompletePartialArray (p: pretty; t, r: node) ;
VAR
   type, s: node ;
BEGIN
   assert (isArray (r)) ;
   type := r^.arrayF.type ;
   s := NIL ;
   outText (p, "struct") ; setNeedSpace (p) ;
   doFQNameC (p, t) ;
   outText (p, "_a {") ;
   setNeedSpace (p) ;
   doTypeC (p, type, s) ;
   setNeedSpace (p) ;
   outText (p, "array[") ;
   doSubrC (p, r^.arrayF.subr) ;
   outText (p, "];") ;
   setNeedSpace (p) ;
   outText (p, "};\n")
END doCompletePartialArray ;


(*
   doSubrC -
*)

PROCEDURE doSubrC (p: pretty; s: node) ;
BEGIN
   doExprC (p, s^.subrangeF.high) ;
   outText (p, "-") ;
   doExprC (p, s^.subrangeF.low) ;
   outText (p, "+1")
END doSubrC ;


(*
   doCompletePartialProcType -
*)

PROCEDURE doCompletePartialProcType (p: pretty; t, n: node) ;
VAR
   i, h: CARDINAL ;
   v, u: node ;
BEGIN
   assert (isProcType (n)) ;
   u := NIL ;
   outText (p, "typedef") ; setNeedSpace (p) ;
   doTypeC (p, n^.proctypeF.returnType, u) ; setNeedSpace (p) ;
   outText (p, "(*") ;
   doFQNameC (p, t) ;
   outText (p, "_t) (") ;
   i := LowIndice (n^.proctypeF.parameters) ;
   h := HighIndice (n^.proctypeF.parameters) ;
   WHILE i <= h DO
      v := GetIndice (n^.proctypeF.parameters, i) ;
      doParameterC (p, v) ;
      noSpace (p) ;
      IF i < h
      THEN
         outText (p, ",") ; setNeedSpace (p)
      END ;
      INC (i)
   END ;
   IF h=0
   THEN
      outText (p, "void")
   END ;
   outText (p, ");\n") ;
   outText (p, "struct") ; setNeedSpace (p) ;
   doFQNameC (p, t) ;
   outText (p, "_p {") ; setNeedSpace (p) ;
   doFQNameC (p, t) ;
   outText (p, "_t proc; };\n\n")
END doCompletePartialProcType ;


(*
   isBase -
*)

PROCEDURE isBase (n: node) : BOOLEAN ;
BEGIN
   CASE n^.kind OF

   char,
   cardinal,
   longcard,
   shortcard,
   integer,
   longint,
   shortint,
   real,
   longreal,
   shortreal,
   bitset,
   boolean,
   proc     :  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END isBase ;


(*
   doBaseC -
*)

PROCEDURE doBaseC (p: pretty; n: node) ;
BEGIN
   CASE n^.kind OF

   char     :  outText (p, 'char') |
   cardinal :  outText (p, 'unsigned int') |
   longcard :  outText (p, 'long unsigned int') |
   shortcard:  outText (p, 'short unsigned int') |
   integer  :  outText (p, 'int') |
   longint  :  outText (p, 'long int') |
   shortint :  outText (p, 'short int') |
   real     :  outText (p, 'double') |
   longreal :  outText (p, 'long double') |
   shortreal:  outText (p, 'float') |
   bitset   :  outText (p, 'unsigned int') |
   boolean  :  outText (p, 'unsigned int') |
   proc     :  outText (p, 'PROC')

   END ;
   setNeedSpace (p)
END doBaseC ;


(*
   isSystem -
*)

PROCEDURE isSystem (n: node) : BOOLEAN ;
BEGIN
   CASE n^.kind OF

   address:  RETURN TRUE |
   byte   :  RETURN TRUE |
   word   :  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END isSystem ;


(*
   doSystemC -
*)

PROCEDURE doSystemC (p: pretty; n: node) ;
BEGIN
   CASE n^.kind OF

   address:  outText (p, 'void *') |
   byte   :  outText (p, 'char') ; setNeedSpace (p) |
   word   :  outText (p, 'unsigned int') ; setNeedSpace (p)

   END
END doSystemC ;


(*
   doArrayC -
*)

PROCEDURE doArrayC (p: pretty; n: node) ;
VAR
   t, s: node ;
BEGIN
   assert (isArray (n)) ;
   t := n^.arrayF.type ;
   s := NIL ;
   doTypeC (p, t, s) ;
   setNeedSpace (p) ;
   outText (p, "*")
END doArrayC ;


(*
   doPointerC -
*)

PROCEDURE doPointerC (p: pretty; n: node; VAR m: node) ;
VAR
   t, s: node ;
BEGIN
   t := n^.pointerF.type ;
   s := NIL ;
   doTypeC (p, t, s) ;
   setNeedSpace (p) ;
   outText (p, "*")
END doPointerC ;


(*
   doRecordFieldC -
*)

PROCEDURE doRecordFieldC (p: pretty; f: node) ;
VAR
   m: node ;
BEGIN
   m := NIL ;
   setNeedSpace (p) ;
   doTypeC (p, f^.recordfieldF.type, m) ;
   doNameC (p, f) ;
   outText (p, ";\n")
END doRecordFieldC ;


(*
   doRecordC -
*)

PROCEDURE doRecordC (p: pretty; n: node; VAR m: node) ;
VAR
   i, h: CARDINAL ;
   f   : node ;
   q   : pretty ;
BEGIN
   assert (isRecord (n)) ;
   outText (p, "struct {") ;
   q := dupPretty (p) ;
   i := LowIndice (n^.recordF.listOfSons) ;
   h := HighIndice (n^.recordF.listOfSons) ;
   setindent (q, getindent (q)+indentation) ;
   outText (p, "\n") ;
   WHILE i<=h DO
      f := GetIndice (n^.recordF.listOfSons, i) ;
      IF isRecordField (f)
      THEN
         doRecordFieldC (q, f)
      END ;
      INC (i)
   END ;
   killPretty (q) ;
   outText (p, "}") ; setNeedSpace (p)
END doRecordC ;


(*
   doTypeC -
*)

PROCEDURE doTypeC (p: pretty; n: node; VAR m: node) ;
BEGIN
   IF n=NIL
   THEN
      outText (p, "void")
   ELSIF isBase (n)
   THEN
      doBaseC (p, n)
   ELSIF isSystem (n)
   THEN
      doSystemC (p, n)
   ELSIF isEnumeration (n)
   THEN
      doEnumerationC (p, n)
   ELSIF isType (n)
   THEN
      doFQNameC (p, n)
      (* doTypeAliasC (p, n, n) *)  (* type, n, has a name, so we choose this over, m.  *)
(*
   ELSIF isProcType (n) OR isArray (n) OR isRecord (n)
   THEN
      HALT  (* n should have been simplified.  *)
*)
   ELSIF isProcType (n)
   THEN
      doProcTypeC (p, n, m)
   ELSIF isArray (n)
   THEN
      doArrayC (p, n)
   ELSIF isRecord (n)
   THEN
      doRecordC (p, n, m)
   ELSIF isPointer (n)
   THEN
      doPointerC (p, n, m)
   ELSE
      print (p, "to do ...  typedef etc etc ") ; doFQNameC (p, n) ; print (p, ";\n")
   END
END doTypeC ;


(*
   doArrayNameC - it displays the array declaration (it might be an unbounded).
*)

PROCEDURE doArrayNameC (p: pretty; n: node) ;
BEGIN
   doTypeNameC (p, getType (n)) ; setNeedSpace (p) ; outText (p, "*")
END doArrayNameC ;


(*
   doTypeNameC -
*)

PROCEDURE doTypeNameC (p: pretty; n: node) ;
VAR
   t: String ;
BEGIN
   IF n=NIL
   THEN
      outText (p, "void") ;
      setNeedSpace (p)
   ELSIF isBase (n)
   THEN
      doBaseC (p, n)
   ELSIF isSystem (n)
   THEN
      doSystemC (p, n)
   ELSIF isEnumeration (n)
   THEN
      print (p, "is enumeration type name required\n")
   ELSIF isType (n)
   THEN
      doFQNameC (p, n)
   ELSIF isProcType (n)
   THEN
      print (p, "is proc type name required\n")
   ELSIF isArray (n)
   THEN
      doArrayNameC (p, n)
   ELSE
      print (p, "some other kind of name required\n")
   END
END doTypeNameC ;


(*
   doVarC -
*)

PROCEDURE doVarC (n: node) ;
VAR
   s: node ;
BEGIN
   print (doP, "EXTERN") ; setNeedSpace (doP) ;
   s := NIL ;
   doTypeC (doP, getType (n), s) ;
   setNeedSpace (doP) ;
   doFQNameC (doP, n) ; print (doP, ";\n")
END doVarC ;


(*
   doPrototypeC -
*)

PROCEDURE doPrototypeC (n: node) ;
VAR
   i, h: CARDINAL ;
   p, q: node ;
BEGIN
   assert (isProcedure (n)) ;
   noSpace (doP) ; outText (doP, "EXTERN") ; setNeedSpace (doP) ;
   q := NIL ;
   doTypeC (doP, n^.procedureF.returnType, q) ; setNeedSpace (doP) ;
   doFQNameC (doP, n) ;
   setNeedSpace (doP) ;
   outText (doP, "(") ;
   i := LowIndice (n^.procedureF.parameters) ;
   h := HighIndice (n^.procedureF.parameters) ;
   WHILE i <= h DO
      p := GetIndice (n^.procedureF.parameters, i) ;
      doParameterC (doP, p) ;
      noSpace (doP) ;
      IF i < h
      THEN
         print (doP, ",") ; setNeedSpace (doP)
      END ;
      INC (i)
   END ;
   IF h=0
   THEN
      outText (doP, "void")
   END ;
   print (doP, ");\n")
END doPrototypeC ;


(*
   doPopulate - adds, n, to the todo list.
*)

PROCEDURE doPopulate (n: node) ;
BEGIN
   alists.includeItemIntoList (todoQ, n)
END doPopulate ;


(*
   tempName -
*)

PROCEDURE tempName () : Name ;
VAR
   s: String ;
   n: Name ;
BEGIN
   INC (tempCount) ;
   s := Sprintf1 (InitString ("_T%d"), tempCount) ;
   n := makekey (DynamicStrings.string (s)) ;
   s := KillString (s) ;
   RETURN n
END tempName ;


(*
   simplifyType -
*)

PROCEDURE simplifyType (VAR p: node) ;
VAR
   o: node ;
BEGIN
   IF (p#NIL) AND (isRecord (p) OR isArray (p) OR isProcType (p))
   THEN
      o := p ;
      enterScope (getScope (p)) ;
      p := makeType (tempName ()) ;
      putType (p, o) ;
      leaveScope ;
      simplified := FALSE
   END
END simplifyType ;


(*
   simplifyRecord -
*)

PROCEDURE simplifyRecord (n: node) ;
VAR
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   i := LowIndice (n^.recordF.listOfSons) ;
   t := HighIndice (n^.recordF.listOfSons) ;
   WHILE i<=t DO
      q := GetIndice (n^.recordF.listOfSons, i) ;
      simplifyNode (q) ;
      INC (i)
   END
END simplifyRecord ;


(*
   simplifyNode -
*)

PROCEDURE simplifyNode (n: node) ;
BEGIN
   IF n=NIL
   THEN
      (* nothing.  *)
   ELSIF isType (n)
   THEN
      simplifyNode (getType (n))
   ELSIF isVar (n)
   THEN
      simplifyType (n^.varF.type)
   ELSIF isRecord (n)
   THEN
      simplifyRecord (n)
   ELSIF isRecordField (n)
   THEN
      simplifyType (n^.recordfieldF.type)
   ELSIF isArray (n)
   THEN
      simplifyType (n^.arrayF.type)
   END
END simplifyNode ;


(*
   doSimplify -
*)

PROCEDURE doSimplify (n: node) ;
BEGIN
   simplifyNode (n)
END doSimplify ;


(*
   simplifyTypes -
*)

PROCEDURE simplifyTypes (s: scopeT) ;
BEGIN
   REPEAT
      simplified := TRUE ;
      ForeachIndiceInIndexDo (s.types, doSimplify) ;
      ForeachIndiceInIndexDo (s.variables, doSimplify)
   UNTIL simplified
END simplifyTypes ;


(*
   outDeclsDefC -
*)

PROCEDURE outDeclsDefC (p: pretty; s: scopeT) ;
BEGIN
   simplifyTypes (s) ;

   doP := p ;
   ForeachIndiceInIndexDo (s.constants, doPopulate) ;
   ForeachIndiceInIndexDo (s.variables, doPopulate) ;
   ForeachIndiceInIndexDo (s.types, doPopulate) ;

   topologicallyOutC ;

   ForeachIndiceInIndexDo (s.procedures, doPrototypeC)
END outDeclsDefC ;


(*
   outDeclsImpC -
*)

PROCEDURE outDeclsImpC (p: pretty; s: scopeT) ;
BEGIN
   simplifyTypes (s) ;

   doP := p ;
   ForeachIndiceInIndexDo (s.constants, doPopulate) ;
   ForeachIndiceInIndexDo (s.variables, doPopulate) ;
   ForeachIndiceInIndexDo (s.types, doPopulate) ;

   topologicallyOutC ;

   ForeachIndiceInIndexDo (s.procedures, doPrototypeC)
END outDeclsImpC ;


(*
   output -
*)

PROCEDURE output (n: node; c, t, v: nodeProcedure) ;
BEGIN
   IF isConst (n)
   THEN
      c (n)
   ELSIF isVar (n)
   THEN
      v (n)
   ELSE
      t (n)
   END
END output ;


(*
   allDependants -
*)

PROCEDURE allDependants (n: node) : dependentState ;
VAR
   l: alist ;
   s: dependentState ;
BEGIN
   l := alists.initList () ;
   s := walkDependants (l, n) ;
   alists.killList (l) ;
   RETURN s
END allDependants ;


(*
   walkDependants -
*)

PROCEDURE walkDependants (l: alist; n: node) : dependentState ;
BEGIN
   IF (n=NIL) OR alists.isItemInList (doneQ, n) OR alists.isItemInList (partialQ, n)
   THEN
      RETURN completed
   ELSIF alists.isItemInList (l, n)
   THEN
      RETURN recursive
   ELSE
      alists.includeItemIntoList (l, n) ;
      RETURN doDependants (l, n)
   END
END walkDependants ;


(*
   walkType -
*)

PROCEDURE walkType (l: alist; n: node) : dependentState ;
VAR
   t: node ;
BEGIN
   t := getType (n) ;
   IF alists.isItemInList (doneQ, t) OR alists.isItemInList (partialQ, t)
   THEN
      RETURN completed
   ELSE
      RETURN blocked
   END
END walkType ;


(*
   walkRecord -
*)

PROCEDURE walkRecord (l: alist; n: node) : dependentState ;
VAR
   s   : dependentState ;
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   i := LowIndice (n^.recordF.listOfSons) ;
   t := HighIndice (n^.recordF.listOfSons) ;
   WHILE i<=t DO
      q := GetIndice (n^.recordF.listOfSons, i) ;
      s := walkDependants (l, q) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      INC (i)
   END ;
   RETURN completed
END walkRecord ;


(*
   walkVarient -
*)

PROCEDURE walkVarient (l: alist; n: node) : dependentState ;
BEGIN
   HALT ;
   RETURN completed
END walkVarient ;


(*
   walkVar -
*)

PROCEDURE walkVar (l: alist; n: node) : dependentState ;
BEGIN
   IF alists.isItemInList (doneQ, getType (n))
   THEN
      RETURN completed
   ELSE
      RETURN blocked
   END
END walkVar ;


(*
   walkEnumeration -
*)

PROCEDURE walkEnumeration (l: alist; n: node) : dependentState ;
VAR
   s   : dependentState ;
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   i := LowIndice (n^.enumerationF.listOfSons) ;
   t := HighIndice (n^.enumerationF.listOfSons) ;
   s := completed ;
   WHILE i<=t DO
      q := GetIndice (n^.enumerationF.listOfSons, i) ;
      s := walkDependants (l, q) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      INC (i)
   END ;
   RETURN s
END walkEnumeration ;


(*
   walkSubrange -
*)

PROCEDURE walkSubrange (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   WITH n^.subrangeF DO
      s := walkDependants (l, low) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      s := walkDependants (l, high) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      s := walkDependants (l, type) ;
      IF s#completed
      THEN
         RETURN s
      END
   END
END walkSubrange ;


(*
   walkSubscript -
*)

PROCEDURE walkSubscript (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   WITH n^.subscriptF DO
      s := walkDependants (l, expr) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      s := walkDependants (l, type) ;
      IF s#completed
      THEN
         RETURN s
      END
   END ;
   RETURN completed
END walkSubscript ;


(*
   walkPointer -
*)

PROCEDURE walkPointer (l: alist; n: node) : dependentState ;
BEGIN
   RETURN walkDependants (l, getType (n))
END walkPointer ;


(*
   walkArray -
*)

PROCEDURE walkArray (l: alist; n: node) : dependentState ;
BEGIN
   RETURN walkDependants (l, getType (n))
END walkArray ;


(*
   walkConst -
*)

PROCEDURE walkConst (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   WITH n^.constF DO
      s := walkDependants (l, type) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      s := walkDependants (l, value) ;
      IF s#completed
      THEN
         RETURN s
      END
   END
END walkConst ;


(*
   walkVarParam -
*)

PROCEDURE walkVarParam (l: alist; n: node) : dependentState ;
BEGIN
   RETURN walkDependants (l, getType (n))
END walkVarParam ;


(*
   walkParam -
*)

PROCEDURE walkParam (l: alist; n: node) : dependentState ;
BEGIN
   RETURN walkDependants (l, getType (n))
END walkParam ;


(*
   walkRecordField -
*)

PROCEDURE walkRecordField (l: alist; n: node) : dependentState ;
BEGIN
   RETURN walkDependants (l, getType (n))
END walkRecordField ;


(*
   walkVarientField -
*)

PROCEDURE walkVarientField (l: alist; n: node) : dependentState ;
VAR
   s   : dependentState ;
   i, t: CARDINAL ;
   q   : node ;
BEGIN
   i := LowIndice (n^.varientfieldF.listOfSons) ;
   t := HighIndice (n^.varientfieldF.listOfSons) ;
   s := completed ;
   WHILE i<=t DO
      q := GetIndice (n^.varientfieldF.listOfSons, i) ;
      s := walkDependants (l, q) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      INC (i)
   END ;
   RETURN s
END walkVarientField ;


(*
   walkEnumerationField -
*)

PROCEDURE walkEnumerationField (l: alist; n: node) : dependentState ;
BEGIN
   RETURN completed
END walkEnumerationField ;


(*
   walkSet -
*)

PROCEDURE walkSet (l: alist; n: node) : dependentState ;
BEGIN
   RETURN walkDependants (l, getType (n))
END walkSet ;


(*
   walkProcType -
*)

PROCEDURE walkProcType (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   s := walkDependants (l, getType (n)) ;
   IF s#completed
   THEN
      RETURN s
   END ;
   RETURN walkParameters (l, n^.proctypeF.parameters)
END walkProcType ;


(*
   walkProcedure -
*)

PROCEDURE walkProcedure (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   s := walkDependants (l, getType (n)) ;
   IF s#completed
   THEN
      RETURN s
   END ;
   RETURN walkParameters (l, n^.procedureF.parameters)
END walkProcedure ;


(*
   walkParameters -
*)

PROCEDURE walkParameters (l: alist; p: Index) : dependentState ;
VAR
   s   : dependentState ;
   i, h: CARDINAL ;
   q   : node ;
BEGIN
   i := LowIndice (p) ;
   h := HighIndice (p) ;
   WHILE i<=h DO
      q := GetIndice (p, i) ;
      s := walkDependants (l, q) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      INC (i)
   END ;
   RETURN completed
END walkParameters ;


(*
   walkUnary -
*)

PROCEDURE walkUnary (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   WITH n^.unaryF DO
      s := walkDependants (l, arg) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      RETURN walkDependants (l, resultType)
   END
END walkUnary ;


(*
   walkBinary -
*)

PROCEDURE walkBinary (l: alist; n: node) : dependentState ;
VAR
   s: dependentState ;
BEGIN
   WITH n^.binaryF DO
      s := walkDependants (l, left) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      s := walkDependants (l, right) ;
      IF s#completed
      THEN
         RETURN s
      END ;
      RETURN walkDependants (l, resultType)
   END
END walkBinary ;


(*
   doDependants - return the dependentState depending upon whether
                  all dependants have been declared.
*)

PROCEDURE doDependants (l: alist; n: node) : dependentState ;
BEGIN
   WITH n^ DO
      CASE kind OF

      address,
      byte,
      word,
      (* base types.  *)
      boolean,
      char,
      cardinal,
      longcard,
      shortcard,
      integer,
      longint,
      shortint,
      real,
      longreal,
      shortreal,
      bitset,
      ztype,
      rtype,
      proc            :  RETURN completed |
      (* language features and compound type attributes.  *)
      type            :  RETURN walkType (l, n) |
      record          :  RETURN walkRecord (l, n) |
      varient         :  RETURN walkVarient (l, n) |
      var             :  RETURN walkVar (l, n) |
      enumeration     :  RETURN walkEnumeration (l, n) |
      subrange        :  RETURN walkSubrange (l, n) |
      pointer         :  RETURN walkPointer (l, n) |
      array           :  RETURN walkArray (l, n) |
      string          :  RETURN completed |
      const           :  RETURN walkConst (l, n) |
      literal         :  RETURN completed |
      varparam        :  RETURN walkVarParam (l, n) |
      param           :  RETURN walkParam (l, n) |
      recordfield     :  RETURN walkRecordField (l, n) |
      varientfield    :  RETURN walkVarientField (l, n) |
      enumerationfield:  RETURN walkEnumerationField (l, n) |
      set             :  RETURN walkSet (l, n) |
      proctype        :  RETURN walkProcType (l, n) |
      subscript       :  RETURN walkSubscript (l, n) |
      (* blocks.  *)
      procedure       :  RETURN walkProcedure (l, n) |
      def,
      imp,
      module,
      (* statements.  *)
      loop,
      while,
      for,
      repeat,
      if,
      elsif,
      assignment      :  HALT |
      (* expressions.  *)
      componentref    :  RETURN walkBinary (l, n) |
      ord             :  RETURN walkUnary (l, n) |
      cast,
      val,
      plus,
      sub,
      div,
      mod             :  RETURN walkBinary (l, n) |
      neg,
      adr,
      size,
      indirect        :  RETURN walkUnary (l, n) |
      equal,
      notequal,
      less,
      greater,
      greequal,
      lessequal       :  RETURN walkBinary (l, n)

      END
   END
END doDependants ;


(*
   tryComplete - returns TRUE if node, n, can be and was completed.
*)

PROCEDURE tryComplete (n: node; c, t, v: nodeProcedure) : BOOLEAN ;
BEGIN
   IF isEnumeration (n)
   THEN
      (* can always emit enumerated types.  *)
      output (n, c, t, v) ;
      RETURN TRUE
   ELSIF isType (n) AND isTypeHidden (n) AND (getType (n)=NIL)
   THEN
      (* can always emit hidden types.  *)
      outputHidden (n) ;
      RETURN TRUE
   ELSIF allDependants (n) = completed
   THEN
      output (n, c, t, v) ;
      RETURN TRUE
   END ;
   RETURN FALSE
END tryComplete ;


(*
   outputHidden -
*)

PROCEDURE outputHidden (n: node) ;
BEGIN
   outText (doP, "#if !defined (") ; doFQNameC (doP, n) ; outText (doP, "_D)\n") ;
   outText (doP, "#  defined ") ; doFQNameC (doP, n) ; outText (doP, "_D\n") ;
   outText (doP, "   typedef void *") ; doFQNameC (doP, n) ; outText (doP, ";\n") ;
   outText (doP, "#endif\n\n")
END outputHidden ;


(*
   tryPartial -
*)

PROCEDURE tryPartial (n: node) : BOOLEAN ;
VAR
   q: node ;
BEGIN
   IF (n#NIL) AND isType (n)
   THEN
      q := getType (n) ;
      IF (q#NIL) AND (isArray (q) OR isRecord (q) OR isProcType (q))
      THEN
         outputPartial (n) ;
	 RETURN TRUE
      END
   END ;
   RETURN FALSE
END tryPartial ;


(*
   outputPartial -
*)

PROCEDURE outputPartial (n: node) ;
VAR
   q: node ;
BEGIN
   q := getType (n) ;
   outText (doP, "typedef struct") ; setNeedSpace (doP) ;
   doFQNameC (doP, n) ;
   IF isRecord (q)
   THEN
      outText (doP, "_r")
   ELSIF isArray (q)
   THEN
      outText (doP, "_a")
   ELSIF isProcType (q)
   THEN
      outText (doP, "_p")
   END ;
   setNeedSpace (doP) ;
   doFQNameC (doP, n) ;
   outText (doP, ";\n\n")
END outputPartial ;


(*
   tryOutputTodo -
*)

PROCEDURE tryOutputTodo ;
VAR
   i, n: CARDINAL ;
   d   : node ;
BEGIN
   i := 1 ;
   n := alists.noOfItemsInList (todoQ) ;
   WHILE i<=n DO
      d := alists.getItemFromList (todoQ, i) ;
      IF tryComplete (d, doConstC, doTypesC, doVarC)
      THEN
         alists.removeItemFromList (todoQ, d) ;
	 alists.includeItemIntoList (doneQ, d) ;
         i := 1 ;
         n := alists.noOfItemsInList (todoQ)
      ELSIF tryPartial (d)
      THEN
         alists.removeItemFromList (todoQ, d) ;
         alists.includeItemIntoList (partialQ, d) ;
         i := 1 ;
         n := alists.noOfItemsInList (todoQ)
      ELSE
         INC (i)
      END
   END
END tryOutputTodo ;


(*
   tryOutputPartial -
*)

PROCEDURE tryOutputPartial ;
VAR
   i, n: CARDINAL ;
   d   : node ;
BEGIN
   i := 1 ;
   n := alists.noOfItemsInList (partialQ) ;
   WHILE i<=n DO
      d := alists.getItemFromList (partialQ, i) ;
      IF tryComplete (d, doNone, doCompletePartialC, doNone)
      THEN
         alists.removeItemFromList (partialQ, d) ;
         i := 1 ;
         DEC (n)
      ELSE
         INC (i)
      END
   END
END tryOutputPartial ;


(*
   topologicallyOutC -
*)

PROCEDURE topologicallyOutC ;
VAR
   tol, pal,
   to,  pa : CARDINAL ;
BEGIN
   tol := 0 ;
   pal := 0 ;
   to := alists.noOfItemsInList (todoQ) ;
   pa := alists.noOfItemsInList (partialQ) ;
   WHILE (tol#to) OR (pal#pa) DO
      tryOutputTodo ;
      tryOutputPartial ;
      tol := to ;
      pal := pa ;
      to := alists.noOfItemsInList (todoQ) ;
      pa := alists.noOfItemsInList (partialQ)
   END
END topologicallyOutC ;


(*
   outDefC -
*)

PROCEDURE outDefC (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (keyToCharStar (getSource (n))) ;
   print (p, "/* automatically created by mc from ") ; prints (p, s) ; print (p, ".  */\n\n") ;
   s := KillString (s) ;
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   print (p, "\n#if !defined (_") ; prints (p, s) ; print (p, "_H)\n") ;
   print (p, "#   define _") ; prints (p, s) ; print (p, "_H\n\n") ;

   doP := p ;
   ForeachIndiceInIndexDo (n^.defF.importedModules, doIncludeC) ;

   print (p, "\n") ;
   print (p, "#   if defined (_") ; prints (p, s) ; print (p, "_C)\n") ;
   print (p, "#      define EXTERN\n") ;
   print (p, "#   else\n") ;
   print (p, "#      if defined(__GNUG__)\n") ;
   print (p, '#         define EXTERN extern "C"\n') ;
   print (p, "#      else\n") ;
   print (p, '#         define EXTERN extern\n') ;
   print (p, "#      endif\n") ;
   print (p, "#   endif\n\n") ;

   IF procUsed
   THEN
      print (p, "#   if !defined (PROC_D)\n") ;
      print (p, "#      defined PROC_D\n") ;
      print (p, "#      typedef struct { void (*proc)(void); } PROC;\n") ;
      print (p, "#   endif\n\n")
   END ;

   outDeclsDefC (p, n^.defF.decls) ;

   print (p, "\n") ;
   print (p, "#   undef EXTERN\n") ;
   print (p, "#endif\n") ;
   s := KillString (s)
END outDefC ;


(*
   outImpC -
*)

PROCEDURE outImpC (p: pretty; n: node) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (keyToCharStar (getSource (n))) ;
   print (p, "/* automatically created by mc from ") ; prints (p, s) ; print (p, ".  */\n\n") ;
   s := KillString (s) ;
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   (* we don't want to include the .h file for this implementation module.  *)
   print (p, "#define _") ; prints (p, s) ; print (p, "_H\n") ;
   print (p, "#define _") ; prints (p, s) ; print (p, "_C\n\n") ;

   doP := p ;
   ForeachIndiceInIndexDo (n^.impF.importedModules, doIncludeC) ;

   print (p, "\n") ;

   IF procUsed
   THEN
      print (p, "#   if !defined (PROC_D)\n") ;
      print (p, "#      defined PROC_D\n") ;
      print (p, "#      typedef struct { void (*proc)(void); } PROC;\n") ;
      print (p, "#   endif\n\n")
   END ;

   outDeclsImpC (p, n^.impF.decls) ;
   s := KillString (s)
END outImpC ;


(*
   outModuleC -
*)

PROCEDURE outModuleC (p: pretty; n: node) ;
BEGIN

END outModuleC ;


(*
   outC -
*)

PROCEDURE outC (p: pretty; n: node) ;
BEGIN
   IF isDef (n)
   THEN
      outDefC (p, n)
   ELSIF isImp (n)
   THEN
      outImpC (p, n)
   ELSIF isModule (n)
   THEN
      outModuleC (p, n)
   ELSE
      HALT
   END
END outC ;


(*
   outCP -
*)

PROCEDURE outCP (p: pretty; n: node) ;
BEGIN

END outCP ;


(*
   outM2 -
*)

PROCEDURE outM2 (p: pretty; n: node) ;
BEGIN

END outM2 ;


(*
   out - walks the tree of node declarations for the main module
         and writes the output to the outputFile specified in
         mcOptions.  It outputs the declarations in the language
         specified above.
*)

PROCEDURE out ;
VAR
   p: pretty ;
BEGIN
   openOutput ;
   p := initPretty (write, writeln) ;
   CASE lang OF

   ansiC :  outC (p, getMainModule ()) |
   ansiCP:  outCP (p, getMainModule ()) |
   pim4  :  outM2 (p, getMainModule ())

   END ;
   closeOutput
END out ;


(*
   setLangC -
*)

PROCEDURE setLangC ;
BEGIN
   lang := ansiC
END setLangC ;


(*
   setLangCP -
*)

PROCEDURE setLangCP ;
BEGIN
   lang := ansiCP
END setLangCP ;


(*
   setLangM2 -
*)

PROCEDURE setLangM2 ;
BEGIN
   lang := pim4
END setLangM2 ;


(*
   addDone - adds node, n, to the doneQ.
*)

PROCEDURE addDone (n: node) ;
BEGIN
   alists.includeItemIntoList (doneQ, n)
END addDone ;


(*
   makeSystem -
*)

PROCEDURE makeSystem ;
BEGIN
   systemN := lookupDef (makeKey ('SYSTEM')) ;

   addressN := makeBase (address) ;
   byteN := makeBase (byte) ;
   wordN := makeBase (word) ;

   enterScope (systemN) ;
   addressN := addToScope (addressN) ;
   byteN := addToScope (byteN) ;
   wordN := addToScope (wordN) ;
   leaveScope ;

   addDone (addressN) ;
   addDone (byteN) ;
   addDone (wordN)
END makeSystem ;


(*
   makeBaseSymbols -
*)

PROCEDURE makeBaseSymbols ;
BEGIN
   baseSymbols := initTree () ;

   booleanN := makeBase (boolean) ;
   charN := makeBase (char) ;
   procN := makeBase (proc) ;
   cardinalN := makeBase (cardinal) ;
   longcardN := makeBase (longcard) ;
   shortcardN := makeBase (shortcard) ;
   integerN := makeBase (integer) ;
   longintN := makeBase (longint) ;
   shortintN := makeBase (shortint) ;
   bitsetN := makeBase (bitset) ;
   ztypeN := makeBase (ztype) ;
   rtypeN := makeBase (rtype) ;
   realN := makeBase (real) ;
   longrealN := makeBase (longreal) ;
   shortrealN := makeBase (shortreal) ;

   nilN := makeBase (nil) ;
   trueN := makeBase (true) ;
   falseN := makeBase (false) ;

   putSymKey (baseSymbols, makeKey ('BOOLEAN'), booleanN) ;
   putSymKey (baseSymbols, makeKey ('PROC'), procN) ;
   putSymKey (baseSymbols, makeKey ('CHAR'), charN) ;
   putSymKey (baseSymbols, makeKey ('CARDINAL'), cardinalN) ;
   putSymKey (baseSymbols, makeKey ('SHORTCARD'), shortcardN) ;
   putSymKey (baseSymbols, makeKey ('LONGCARD'), longcardN) ;
   putSymKey (baseSymbols, makeKey ('INTEGER'), integerN) ;
   putSymKey (baseSymbols, makeKey ('LONGINT'), longintN) ;
   putSymKey (baseSymbols, makeKey ('SHORTINT'), shortintN) ;
   putSymKey (baseSymbols, makeKey ('BITSET'), bitsetN) ;
   putSymKey (baseSymbols, makeKey ('REAL'), realN) ;
   putSymKey (baseSymbols, makeKey ('SHORTREAL'), shortrealN) ;
   putSymKey (baseSymbols, makeKey ('LONGREAL'), longrealN) ;

   putSymKey (baseSymbols, makeKey ('NIL'), nilN) ;
   putSymKey (baseSymbols, makeKey ('TRUE'), trueN) ;
   putSymKey (baseSymbols, makeKey ('FALSE'), falseN) ;

   addDone (booleanN) ;
   addDone (charN) ;
   addDone (cardinalN) ;
   addDone (longcardN) ;
   addDone (shortcardN) ;
   addDone (integerN) ;
   addDone (longintN) ;
   addDone (shortintN) ;
   addDone (bitsetN) ;
   addDone (ztypeN) ;
   addDone (rtypeN) ;
   addDone (realN) ;
   addDone (longrealN) ;
   addDone (shortrealN) ;
   addDone (procN) ;
   addDone (nilN) ;
   addDone (trueN) ;
   addDone (falseN)

END makeBaseSymbols ;


(*
   makeBuiltins -
*)

PROCEDURE makeBuiltins ;
BEGIN
   bitsperunitN := makeLiteralInt (makeKey ('8')) ;
   bitsperwordN := makeLiteralInt (makeKey ('32')) ;
   bitspercharN := makeLiteralInt (makeKey ('8')) ;
   unitsperwordN := makeLiteralInt (makeKey ('4')) ;

   addDone (bitsperunitN) ;
   addDone (bitsperwordN) ;
   addDone (bitspercharN) ;
   addDone (unitsperwordN)
END makeBuiltins ;


(*
   init -
*)

PROCEDURE init ;
BEGIN
   todoQ := alists.initList () ;
   partialQ := alists.initList () ;
   doneQ := alists.initList () ;
   modUniverse := initTree () ;
   defUniverse := initTree () ;
   modUniverseI := InitIndex (1) ;
   defUniverseI := InitIndex (1) ;
   scopeStack := InitIndex (1) ;
   makeBaseSymbols ;
   makeSystem ;
   makeBuiltins ;
   lang := ansiC ;
   outputState := punct ;
   tempCount := 0 ;
   procUsed := FALSE
END init ;


BEGIN
   init
END decl.
