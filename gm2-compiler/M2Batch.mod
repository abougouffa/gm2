(* Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)
IMPLEMENTATION MODULE M2Batch ;


FROM M2Debug IMPORT Assert ;
FROM M2Error IMPORT InternalError ;
FROM SymbolTable IMPORT MakeModule, MakeDefImp, NulSym ;
FROM NameKey IMPORT GetKey, WriteKey ;
FROM M2Printf IMPORT printf2 ;
FROM M2Error IMPORT InternalError ;
FROM Indexing IMPORT Index, InitIndex, GetIndice, HighIndice, RemoveIndiceFromIndex, IncludeIndiceIntoIndex, InBounds ;
FROM Lists IMPORT List, InitList, IncludeItemIntoList, RemoveItemFromList,
                  GetItemFromList, NoOfItemsInList ;
FROM Storage IMPORT ALLOCATE ;
FROM DynamicStrings IMPORT String ;


TYPE
   Module = POINTER TO module ;
   module =            RECORD
                          SymNo  : CARDINAL ;
                          Key    : Name ;
                          DefFile,
                          ModFile: String ;
                       END ;

VAR
   DoneQueue   : Index ;
   PendingQueue: List ;


(* %%%FORWARD%%%
PROCEDURE DisplayModules ; FORWARD ;
PROCEDURE Get (n: Name) : CARDINAL ; FORWARD ;
PROCEDURE Pop () : CARDINAL ; FORWARD ;
PROCEDURE Push (Sym: CARDINAL) ; FORWARD ;
PROCEDURE Put (Sym: CARDINAL; n: Name) ; FORWARD ;
   %%%FORWARD%%% *)

(*
   MakeProgramSource - is given a Name, n, which is used to create a program module.
                       The program module will be placed onto the compilation
                       pending queue if it has not yet been compiled.
                       If the module has been compiled then no action is taken.
                       The Module Sym is returned.
*)

PROCEDURE MakeProgramSource (n: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := Get(n) ;
   IF Sym=NulSym
   THEN
      (* Neither been compiled or on the Pending Queue *)
      Sym := MakeModule(n) ;
      Put(Sym, n) ;
      Push(Sym)
   END ;
   RETURN( Sym )
END MakeProgramSource ;


(*
   MakeDefinitionSource - is given a Name, n, which is used to create a Definition
                          module.
                          The Definition Module will be placed onto the
                          compilation pending queue if it has not yet been
                          compiled.
                          If the module has been compiled then no action is
                          taken. The Module Sym is returned.
*)

PROCEDURE MakeDefinitionSource (n: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := Get(n) ;
   IF Sym=NulSym
   THEN
      (* Neither been compiled or on the Pending Queue *)
      Sym := MakeDefImp(n) ;
      Put(Sym, n) ;
      Push(Sym)
   END ;
   RETURN( Sym )
END MakeDefinitionSource ;


(*
   MakeImplementationSource - is given a Name, n, which is used to create an
                              implementation module.
                              The implementation Module will be placed onto
                              the compilation pending
                              queue if it has not yet been compiled.
                              If the module has been compiled then no
                              action is taken. The Module Sym is returned.
*)

PROCEDURE MakeImplementationSource (n: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := Get(n) ;
   IF Sym=NulSym
   THEN
      (* Neither been compiled or on the Pending Queue *)
      Sym := MakeDefImp(n) ;
      Put(Sym, n) ;
      Push(Sym)
   END ;
   RETURN( Sym )
END MakeImplementationSource ;


(*
   GetSource - returns with the symbol Sym of the next module to be compiled.
               If Sym returns with value 0 then no module should be compiled.
*)

PROCEDURE GetSource () : CARDINAL ;
BEGIN
   RETURN( Pop() )
END GetSource ;


(*
   GetModuleNo - returns with symbol number of the nth module read during Pass 1.
*)

PROCEDURE GetModuleNo (n: CARDINAL) : CARDINAL ;
VAR
   m: Module ;
BEGIN
   Assert(n#0) ;
   IF InBounds(DoneQueue, n)
   THEN
      m := GetIndice(DoneQueue, n) ;
      RETURN( m^.SymNo )
   ELSE
      RETURN( NulSym )
   END
END GetModuleNo ;


(*
   IsModuleKnown - returns TRUE if the Name, n, matches a module.
*)

PROCEDURE IsModuleKnown (n: Name) : BOOLEAN ;
BEGIN
   RETURN( Get(n)#NulSym )
END IsModuleKnown ;


(*
   Get - returns the module symbol matching name, n.
*)

PROCEDURE Get (n: Name) : CARDINAL ;
VAR
   i, no: CARDINAL ;
   m    : Module ;
BEGIN
   i := 1 ;
   no := HighIndice(DoneQueue) ;
   WHILE i<=no DO
      m := GetIndice(DoneQueue, i) ;
      WITH m^ DO
         IF Key=n
         THEN
            RETURN( SymNo )
         ELSE
            INC(i)
         END
      END
   END ;
   RETURN( NulSym )
END Get ;


PROCEDURE Put (Sym: CARDINAL; n: Name) ;
VAR
   m: Module ;
BEGIN
   NEW(m) ;
   IncludeIndiceIntoIndex(DoneQueue, m) ;
   WITH m^ DO
      SymNo   := Sym ;
      Key     := n ;
      DefFile := NIL ;
      ModFile := NIL
   END
END Put ;


PROCEDURE Push (Sym: CARDINAL) ;
BEGIN
   IncludeItemIntoList(PendingQueue, Sym)
END Push ;


PROCEDURE Pop () : CARDINAL ;
VAR
   n  : CARDINAL ;
   Sym: CARDINAL ;
BEGIN
   n := NoOfItemsInList(PendingQueue) ;
   IF n=0
   THEN
      RETURN( NulSym )
   ELSE
      Sym := GetItemFromList(PendingQueue, n) ;
      RemoveItemFromList(PendingQueue, Sym) ;
      RETURN( Sym )
   END
END Pop ;


PROCEDURE DisplayModules ;
VAR
   m   : Module ;
   n, i: CARDINAL ;
BEGIN
   i := 1 ;
   n := HighIndice(DoneQueue) ;
   WHILE i<=n DO
      m := GetIndice(DoneQueue, i) ;
      WITH m^ DO
         printf2('Module %a %d\n', Key, i)
      END ;
      INC(i)
   END
END DisplayModules ;


(*
   AssociateDefinition - associate the source file, filename, with the definition module,
                         Sym.
*)

PROCEDURE AssociateDefinition (filename: String; Sym: CARDINAL) : String ;
VAR
   no, i: CARDINAL ;
   m    : Module ;
BEGIN
   i := 1 ;
   no := HighIndice(DoneQueue) ;
   WHILE i<=no DO
      m := GetIndice(DoneQueue, i) ;
      WITH m^ DO
         IF SymNo=Sym
         THEN
            DefFile := filename ;
            RETURN( filename )
         ELSE
            INC(i)
         END
      END
   END ;
   InternalError('failed to find module sym', __FILE__, __LINE__)
END AssociateDefinition ;


(*
   GetDefinitionModuleFile - returns the filename associated with the definition module, Sym.
                             It may return a temporary preprocessed file.
*)

PROCEDURE GetDefinitionModuleFile (Sym: CARDINAL) : String ;
VAR
   no, i: CARDINAL ;
   m    : Module ;
BEGIN
   i := 1 ;
   no := HighIndice(DoneQueue) ;
   WHILE i<=no DO
      m := GetIndice(DoneQueue, i) ;
      WITH m^ DO
         IF SymNo=Sym
         THEN
            RETURN( DefFile )
         ELSE
            INC(i)
         END
      END
   END ;
   RETURN( NIL )
END GetDefinitionModuleFile ;


(*
   AssociateModule - associate the source file, filename, with the implementation/program
                     module, Sym.
*)

PROCEDURE AssociateModule (filename: String; Sym: CARDINAL) : String ;
VAR
   no, i: CARDINAL ;
   m    : Module ;
BEGIN
   i := 1 ;
   no := HighIndice(DoneQueue) ;
   WHILE i<=no DO
      m := GetIndice(DoneQueue, i) ;
      WITH m^ DO
         IF SymNo=Sym
         THEN
            ModFile := filename ;
            RETURN( filename )
         ELSE
            INC(i)
         END
      END
   END ;
   InternalError('failed to find module sym', __FILE__, __LINE__)
END AssociateModule ;


(*
   GetModuleFile - returns the filename associated with the implementation/program module, Sym.
                   It may return a temporary preprocessed file.
*)

PROCEDURE GetModuleFile (Sym: CARDINAL) : String ;
VAR
   no, i: CARDINAL ;
   m    : Module ;
BEGIN
   i := 1 ;
   no := HighIndice(DoneQueue) ;
   WHILE i<=no DO
      m := GetIndice(DoneQueue, i) ;
      WITH m^ DO
         IF SymNo=Sym
         THEN
            RETURN( ModFile )
         ELSE
            INC(i)
         END
      END
   END ;
   RETURN( NIL )
END GetModuleFile ;


BEGIN
   InitList(PendingQueue) ;
   DoneQueue := InitIndex(1)
END M2Batch.
