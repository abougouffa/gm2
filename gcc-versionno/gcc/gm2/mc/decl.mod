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
FROM nameKey IMPORT NulName, makeKey, lengthKey, keyToCharStar ;
FROM SFIO IMPORT OpenToWrite, WriteS ;
FROM FIO IMPORT File, Close, FlushBuffer, StdOut ;
FROM DynamicStrings IMPORT String, InitString, EqualArray, InitStringCharStar, KillString ;
FROM mcOptions IMPORT getOutputFile ;
FROM mcPrintf IMPORT fprintf0, fprintf1, fprintf2 ;
FROM FormatStrings IMPORT Sprintf0, Sprintf2 ;
FROM libc IMPORT printf ;
FROM mcMetaError IMPORT metaError1, metaError2, metaErrors1, metaErrors2 ;
FROM StrLib IMPORT StrEqual ;

FROM Indexing IMPORT Index, InitIndex, ForeachIndiceInIndexDo,
                     IncludeIndiceIntoIndex, IsIndiceInIndex,
		     HighIndice, LowIndice, GetIndice, RemoveIndiceFromIndex ;

IMPORT DynamicStrings ;
IMPORT alists, wlists ;

FROM alists IMPORT alist ;
FROM wlists IMPORT wlist ;


TYPE
   language = (ansiC, ansiCP, pim4) ;

   nodeT = (
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
	    and, or, not, identlist) ;

    node = POINTER TO RECORD
                         CASE kind: nodeT OF

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

                         END ;
                         at:  where ;
                      END ;

       identlistT = RECORD
                       names:  wlist ;
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
   shortrealN    : node ;
   scopeStack,
   defUniverseI,
   modUniverseI  : Index ;
   modUniverse,
   defUniverse   : symbolTree ;
   baseSymbols   : symbolTree ;
   outputState   : outputStates ;
   needSpace     : BOOLEAN ;


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
      varF.scope := getDeclScope () ;
      varF.isInitialised := FALSE
   END ;
   RETURN addToScope (d)
END makeVar ;


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
   makeBase - create a base type.  It only supports the base type
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
   doIncludeC - include header file for definition module, n.
*)

PROCEDURE doIncludeC (n: node) ;
VAR
   s: String ;
BEGIN
   IF isDef (n)
   THEN
      s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
      fprintf1 (outputFile, '#   include "G%s.h"\n', s) ;
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
   IF getScope (n)=NIL
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
   doConstC -
*)

PROCEDURE doConstC (n: node) ;
VAR
   t: String ;
BEGIN
   t := getFQstring (n) ;
   fprintf1 (outputFile, "#   define %s ", t) ;
   doExprC (n^.constF.value) ;
   outText ('\n') ;
   t := KillString (t)
END doConstC ;


(*
   doUnary -
*)

PROCEDURE doUnary (op: ARRAY OF CHAR; expr, type: node) ;
BEGIN
   outText (op) ;
   IF StrEqual (op, 'ADR') OR StrEqual (op, 'SIZE') OR StrEqual (op, 'ORD')
   THEN
      outText (' (') ;
      doExprC (expr) ;
      outText (')')
   ELSE
      outText (' (') ;
      doExprC (expr) ;
      outText (')')
   END
END doUnary ;


(*
   doBinary -
*)

PROCEDURE doBinary (op: ARRAY OF CHAR; left, right: node) ;
BEGIN
   outText ('(') ;
   doExprC (left) ;
   outText (')') ;
   outText (op) ;
   outText ('(') ;
   doExprC (right) ;
   outText (')')
END doBinary ;


(*
   doPostUnary -
*)

PROCEDURE doPostUnary (op: ARRAY OF CHAR; expr: node) ;
BEGIN
   doExprC (expr) ;
   outText (op)
END doPostUnary ;


(*
   doPreBinary -
*)

PROCEDURE doPreBinary (op: ARRAY OF CHAR; left, right: node) ;
BEGIN
   outText (op) ;
   outText (' (') ;
   doExprC (left) ;
   outText (', ') ;
   doExprC (right) ;
   outText (')')
END doPreBinary ;


(*
   doConstExpr -
*)

PROCEDURE doConstExpr (n: node) ;
VAR
   s: String ;
BEGIN
   s := getFQstring (n) ;
   outTextS (s) ;
   s := KillString (s)
END doConstExpr ;


(*
   doExprC -
*)

PROCEDURE doExprC (n: node) ;
BEGIN
   assert (n#NIL) ;
   WITH n^ DO
      CASE kind OF

      neg         :  doUnary ('-', unaryF.arg, unaryF.resultType) |
      not         :  doUnary ('NOT', unaryF.arg, unaryF.resultType) |
      adr         :  doUnary ('ADR', unaryF.arg, unaryF.resultType) |
      size        :  doUnary ('SIZE', unaryF.arg, unaryF.resultType) |
      ord         :  doUnary ('ORD', unaryF.arg, unaryF.resultType) |
      indirect    :  doPostUnary ('^', unaryF.arg) |
      equal       :  doBinary ('=', binaryF.left, binaryF.right) |
      notequal    :  doBinary ('#', binaryF.left, binaryF.right) |
      less        :  doBinary ('<', binaryF.left, binaryF.right) |
      greater     :  doBinary ('>', binaryF.left, binaryF.right) |
      greequal    :  doBinary ('>=', binaryF.left, binaryF.right) |
      lessequal   :  doBinary ('<=', binaryF.left, binaryF.right) |
      componentref:  doBinary ('.', binaryF.left, binaryF.right) |
      cast        :  doPreBinary ('CAST', binaryF.left, binaryF.right) |
      val         :  doPreBinary ('VAL', binaryF.left, binaryF.right) |
      plus        :  doBinary ('+', binaryF.left, binaryF.right) |
      sub         :  doBinary ('-', binaryF.left, binaryF.right) |
      div         :  doBinary ('/', binaryF.left, binaryF.right) |
      mod         :  doBinary ('MOD', binaryF.left, binaryF.right) |
      literal     :  doLiteral (n) |
      const       :  doConstExpr (n)

      END
   END
END doExprC ;


(*
   doLiteral -
*)

PROCEDURE doLiteral (n: node) ;
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
      outText ("(char) ")
   END ;
   outTextS (s) ;
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

PROCEDURE doString (n: node) ;
VAR
   s: String ;
BEGIN
   assert (isString (n)) ;
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   IF DynamicStrings.Index (s, '"', 0)=-1
   THEN
      outText ('"') ;
      outTextS (s) ;
      outText ('"')
   ELSIF DynamicStrings.Index (s, "'", 0)=-1
   THEN
      outText ('"') ;
      outTextS (s) ;
      outText ('"')
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

PROCEDURE outText (a: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   s := InitString (a) ;
   outTextS (s) ;
   s := KillString (s)
END outText ;


(*
   outTextS -
*)

PROCEDURE outTextS (s: String) ;
BEGIN
   IF (s # NIL) AND (NOT EqualArray (s, ''))
   THEN
      s := Sprintf0 (s) ;
      IF needSpace
      THEN
         fprintf0 (outputFile, " ")
      ELSIF outputState = text
      THEN
         IF isWhite (DynamicStrings.char (s, 0))
         THEN
            (* do nothing.  *)
         ELSIF NOT isPunct (DynamicStrings.char (s, 0))
         THEN
            fprintf0 (outputFile, " ")
         END
      END ;
      s := WriteS (outputFile, s) ;
      IF isPunct (DynamicStrings.char (s, -1))
      THEN
         outputState := punct
      ELSIF isWhite (DynamicStrings.char (s, -1))
      THEN
         outputState := space
      ELSE
         outputState := text
      END ;
      needSpace := FALSE
   END
END outTextS ;


(*
   doTypeAliasC -
*)

PROCEDURE doTypeAliasC (n: node; VAR m: node) ;
VAR
   t: String ;
BEGIN
   outText ("typedef") ;
   IF isTypeHidden (n) AND (isDef (getMainModule ()) OR (getScope (n) # getMainModule ()))
   THEN
      outText ("void *")
   ELSE
      doTypeC (getType (n), m)
   END ;
   IF m#NIL
   THEN
      t := getFQstring (m) ;
      outTextS (t) ;
      t := KillString (t)
   END ;
   outText (';\n')
END doTypeAliasC ;


(*
   doEnumerationC -
*)

PROCEDURE doEnumerationC (n: node) ;
VAR
   i, h: CARDINAL ;
   s   : node ;
   t   : String ;
BEGIN
   outText ("enum {") ;
   i := LowIndice (n^.enumerationF.listOfSons) ;
   h := HighIndice (n^.enumerationF.listOfSons) ;
   WHILE i <= h DO
      s := GetIndice (n^.enumerationF.listOfSons, i) ;
      t := getFQstring (s) ;
      outTextS (t) ;
      IF i < h
      THEN
         outText (", ")
      END ;
      INC (i)
   END ;
   outText ("} ")
END doEnumerationC ;


(*
   doNamesC -
*)

PROCEDURE doNamesC (n: Name) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (keyToCharStar (n)) ;
   outTextS (s) ;
   s := KillString (s)
END doNamesC ;


(*
   doNameC -
*)

PROCEDURE doNameC (n: node) ;
BEGIN
   IF (n#NIL) AND (getSymName (n)#NulName)
   THEN
      doNamesC (getSymName (n))
   END
END doNameC ;


(*
   doHighC -
*)

PROCEDURE doHighC (a: node; n: Name) ;
VAR
   s: String ;
BEGIN
   IF isArray (a) AND isUnbounded (a)
   THEN
      (* need to display high.  *)
      outText (", ") ;
      doTypeNameC (cardinalN) ;
      outText (" ") ;
      s := InitStringCharStar (keyToCharStar (n)) ;
      fprintf1 (outputFile, "_%s_high", s)
   END
END doHighC ;


(*
   doParamC -
*)

PROCEDURE doParamC (n: node) ;
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
      doTypeNameC (ptype)
   ELSE
      assert (isIdentList (n^.paramF.namelist)) ;
      l := n^.paramF.namelist^.identlistF.names ;
      IF l=NIL
      THEN
         doTypeNameC (ptype)
      ELSE
         t := wlists.noOfItemsInList (l) ;
         c := 1 ;
         WHILE c <= t DO
            doTypeNameC (ptype) ;
            i := wlists.getItemFromList (l, c) ;
            doNamesC (i) ;
	    doHighC (ptype, i) ;
            IF c<t
            THEN
               outText (', ')
            END ;
            INC (c)
         END
      END
   END
END doParamC ;


(*
   doVarParamC -
*)

PROCEDURE doVarParamC (n: node) ;
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
      doTypeC (ptype, n) ;
      IF NOT isArray (ptype)
      THEN
         outText (" *")
      END
   ELSE
      assert (isIdentList (n^.varparamF.namelist)) ;
      l := n^.varparamF.namelist^.identlistF.names ;
      IF l=NIL
      THEN
         doTypeNameC (ptype)
      ELSE
         t := wlists.noOfItemsInList (l) ;
         c := 1 ;
         WHILE c <= t DO
            doTypeNameC (ptype) ;
	    IF NOT isArray (ptype)
            THEN
               outText (" *")
            END ;
            i := wlists.getItemFromList (l, c) ;
            doNamesC (i) ;
            doHighC (ptype, i) ;
            IF c<t
            THEN
               outText (', ')
            END ;
            INC (c)
         END
      END
   END
END doVarParamC ;


(*
   doParameterC -
*)

PROCEDURE doParameterC (n: node) ;
BEGIN
   IF isParam (n)
   THEN
      doParamC (n)
   ELSIF isVarParam (n)
   THEN
      doVarParamC (n)
   ELSIF isVarargs (n)
   THEN
      outText ("...")
   END
END doParameterC ;


(*
   doProcTypeC -
*)

PROCEDURE doProcTypeC (n: node; VAR m: node) ;
VAR
   i, h: CARDINAL ;
   p, q: node ;
   s   : String ;
BEGIN
   assert (isProcType (n)) ;
   q := NIL ;
   doTypeC (n^.proctypeF.returnType, q) ;
   IF m=NIL
   THEN
      (* make typedef and use it.  *)
   ELSE
      s := getFQstring (m) ;
      outText (" (*") ;
      outTextS (s) ;
      outText (") (") ;
      s := KillString (s) ;
      m := NIL
   END ;
   i := LowIndice (n^.proctypeF.parameters) ;
   h := HighIndice (n^.proctypeF.parameters) ;
   WHILE i <= h DO
      p := GetIndice (n^.proctypeF.parameters, i) ;
      doParameterC (p) ;
      IF i < h
      THEN
         outText (", ")
      END ;
      INC (i)
   END ;
   outText (")")
END doProcTypeC ;


(*
   doTypesC -
*)

PROCEDURE doTypesC (n: node) ;
BEGIN
   doTypeC (n, n)
END doTypesC ;


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

PROCEDURE doBaseC (n: node) ;
BEGIN
   CASE n^.kind OF

   char     :  outText ('char') |
   cardinal :  outText ('unsigned int') |
   longcard :  outText ('long unsigned int') |
   shortcard:  outText ('short unsigned int') |
   integer  :  outText ('int') |
   longint  :  outText ('long int') |
   shortint :  outText ('short int') |
   real     :  outText ('double') |
   longreal :  outText ('long double') |
   shortreal:  outText ('float') |
   bitset   :  outText ('unsigned int') |
   boolean  :  outText ('unsigned int') |
   proc     :  outText ('#########PROC##########')

   END
END doBaseC ;


(*
   doArrayC -
*)

PROCEDURE doArrayC (n: node) ;
VAR
   t, s: node ;
BEGIN
   assert (isArray (n)) ;
   t := n^.arrayF.type ;
   s := NIL ;
   doTypeC (t, s) ;
   outText (" *")
END doArrayC ;


(*
   doPointerC -
*)

PROCEDURE doPointerC (n: node; VAR m: node) ;
VAR
   t, s: node ;
BEGIN
   t := n^.pointerF.type ;
   s := NIL ;
   doTypeC (t, s) ;
   outText ("*")
END doPointerC ;


(*
   doRecordFieldC -
*)

PROCEDURE doRecordFieldC (f: node) ;
BEGIN
   doTypesC (f^.recordfieldF.type) ;
   outText (" ") ;
   doNameC (f) ;
   outText (" ;\n")
END doRecordFieldC ;


(*
   doRecordC -
*)

PROCEDURE doRecordC (n: node; VAR m: node) ;
VAR
   i, h: CARDINAL ;
   f: node ;
BEGIN
   assert (isRecord (n)) ;
   outText ("struct {\n") ;
   i := LowIndice (n^.recordF.listOfSons) ;
   h := HighIndice (n^.recordF.listOfSons) ;
   WHILE i<=h DO
      f := GetIndice (n^.recordF.listOfSons, i) ;
      IF isRecordField (f)
      THEN
         doRecordFieldC (f)
      END ;
      INC (i)
   END ;
   outText ("}\n")
END doRecordC ;


(*
   doTypeC -
*)

PROCEDURE doTypeC (n: node; VAR m: node) ;
VAR
   t: String ;
BEGIN
   IF n=NIL
   THEN
      outText ("void")
   ELSIF isBase (n)
   THEN
      doBaseC (n)
   ELSIF isEnumeration (n)
   THEN
      doEnumerationC (n)
   ELSIF isType (n)
   THEN
      doTypeAliasC (n, n)   (* type, n, has a name, so we choose this over, m.  *)
   ELSIF isProcType (n)
   THEN
      doProcTypeC (n, m)
   ELSIF isArray (n)
   THEN
      doArrayC (n)
   ELSIF isPointer (n)
   THEN
      doPointerC (n, m)
   ELSIF isRecord (n)
   THEN
      doRecordC (n, m)
   ELSE
      t := getFQstring (n) ;
      fprintf1 (outputFile, "to do ...  typedef etc etc %s;\n", t) ;
      t := KillString (t)
   END
END doTypeC ;


(*
   doArrayNameC - it displays the array declaration (it might be an unbounded).
*)

PROCEDURE doArrayNameC (n: node) ;
BEGIN
   doTypeNameC (getType (n)) ;
   outText (" *")
END doArrayNameC ;


(*
   doTypeNameC -
*)

PROCEDURE doTypeNameC (n: node) ;
VAR
   t: String ;
BEGIN
   IF n=NIL
   THEN
      outText ("void")
   ELSIF isBase (n)
   THEN
      doBaseC (n)
   ELSIF isEnumeration (n)
   THEN
      fprintf0 (outputFile, "is enumeration type name required\n")
   ELSIF isType (n)
   THEN
      t := getFQstring (n) ;
      outTextS (t) ;
      t := KillString (t)
   ELSIF isProcType (n)
   THEN
      fprintf0 (outputFile, "is proc type name required\n")
   ELSIF isArray (n)
   THEN
      doArrayNameC (n)
   ELSE
      fprintf0 (outputFile, "some other kind of name required\n") ;
   END
END doTypeNameC ;


(*
   doVarC -
*)

PROCEDURE doVarC (n: node) ;
VAR
   t: String ;
BEGIN
   t := getFQstring (n) ;
   fprintf1 (outputFile, "EXPORT typename %s;\n", t) ;
   t := KillString (t)
END doVarC ;


(*
   doPrototypeC -
*)

PROCEDURE doPrototypeC (n: node) ;
VAR
   i, h: CARDINAL ;
   p, q: node ;
   s   : String ;
BEGIN
   assert (isProcedure (n)) ;
   outText ("EXTERN");
   q := NIL ;
   doTypeC (n^.procedureF.returnType, q) ;
   s := getFQstring (n) ;
   outTextS (s) ;
   outText (" (") ;
   s := KillString (s) ;
   i := LowIndice (n^.procedureF.parameters) ;
   h := HighIndice (n^.procedureF.parameters) ;
   WHILE i <= h DO
      p := GetIndice (n^.procedureF.parameters, i) ;
      doParameterC (p) ;
      IF i < h
      THEN
         outText (", ")
      END ;
      INC (i)
   END ;
   outText (");\n")
END doPrototypeC ;


(*
   outDeclsDefC -
*)

PROCEDURE outDeclsDefC (s: scopeT) ;
BEGIN
   fprintf0 (outputFile, "\n") ;
   ForeachIndiceInIndexDo (s.constants, doConstC) ;

   fprintf0 (outputFile, "\n") ;
   ForeachIndiceInIndexDo (s.variables, doVarC) ;

   fprintf0 (outputFile, "\n") ;
   ForeachIndiceInIndexDo (s.types, doTypesC) ;

   fprintf0 (outputFile, "\n") ;
   ForeachIndiceInIndexDo (s.procedures, doPrototypeC)
END outDeclsDefC ;


(*
   outDefC -
*)

PROCEDURE outDefC (n: node) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar (keyToCharStar (getSource (n))) ;
   fprintf1 (outputFile, "/* automatically created by mc from %s.  */\n\n", s) ;
   s := KillString (s) ;
   s := InitStringCharStar (keyToCharStar (getSymName (n))) ;
   fprintf1 (outputFile, "\n#if !defined (_%s_H)\n", s) ;
   fprintf1 (outputFile, "#   define _%s_H\n\n", s) ;

   ForeachIndiceInIndexDo (n^.defF.importedModules, doIncludeC) ;

   fprintf0 (outputFile, "\n") ;
   fprintf1 (outputFile, "#   if defined (_%s_C)\n", s) ;
   fprintf0 (outputFile, "#      define EXTERN\n") ;
   fprintf0 (outputFile, "#   else\n") ;
   fprintf0 (outputFile, "#      if defined(__GNUG__)\n") ;
   fprintf0 (outputFile, '#         define EXTERN extern "C"\n') ;
   fprintf0 (outputFile, "#      else\n") ;
   fprintf0 (outputFile, '#         define EXTERN extern\n') ;
   fprintf0 (outputFile, "#      endif\n") ;
   fprintf0 (outputFile, "#   endif\n") ;

   outDeclsDefC (n^.defF.decls) ;

   fprintf0 (outputFile, "\n") ;
   fprintf0 (outputFile, "#   undef EXTERN\n") ;
   fprintf0 (outputFile, "#endif\n") ;
   s := KillString (s)
END outDefC ;


(*
   outImpC -
*)

PROCEDURE outImpC (n: node) ;
BEGIN

END outImpC ;


(*
   outModuleC -
*)

PROCEDURE outModuleC (n: node) ;
BEGIN

END outModuleC ;


(*
   outC -
*)

PROCEDURE outC (n: node) ;
BEGIN
   IF isDef (n)
   THEN
      outDefC (n)
   ELSIF isImp (n)
   THEN
      outImpC (n)
   ELSIF isModule (n)
   THEN
      outModuleC (n)
   ELSE
      HALT
   END
END outC ;


(*
   outCP -
*)

PROCEDURE outCP (n: node) ;
BEGIN

END outCP ;


(*
   outM2 -
*)

PROCEDURE outM2 (n: node) ;
BEGIN

END outM2 ;


(*
   out - walks the tree of node declarations for the main module
         and writes the output to the outputFile specified in
         mcOptions.  It outputs the declarations in the language
         specified above.
*)

PROCEDURE out ;
BEGIN
   openOutput ;
   CASE lang OF

   ansiC :  outC (getMainModule ()) |
   ansiCP:  outCP (getMainModule ()) |
   pim4  :  outM2 (getMainModule ())

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
   leaveScope
END makeSystem ;


(*
   makeBaseSymbols -
*)

PROCEDURE makeBaseSymbols ;
BEGIN
   baseSymbols := initTree () ;

   booleanN := makeBase (boolean) ;
   procN := makeBase (proc) ;
   charN := makeBase (char) ;
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
   putSymKey (baseSymbols, makeKey ('LONGREAL'), longrealN)
END makeBaseSymbols ;


(*
   makeBuiltins -
*)

PROCEDURE makeBuiltins ;
BEGIN
   bitsperunitN := makeLiteralInt (makeKey ('8')) ;
   bitsperwordN := makeLiteralInt (makeKey ('32')) ;
   bitspercharN := makeLiteralInt (makeKey ('8')) ;
   unitsperwordN := makeLiteralInt (makeKey ('4'))
END makeBuiltins ;


(*
   init -
*)

PROCEDURE init ;
BEGIN
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
   needSpace := FALSE
END init ;


BEGIN
   init
END decl.
