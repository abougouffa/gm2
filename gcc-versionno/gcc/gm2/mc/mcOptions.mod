(* Copyright (C) 2015 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE mcOptions ;

FROM SArgs IMPORT GetArg, Narg ;
FROM mcSearch IMPORT prependSearchPath ;
FROM libc IMPORT exit ;
FROM mcPrintf IMPORT printf0 ;
FROM Debug IMPORT Halt ;
FROM StrLib IMPORT StrLen ;
FROM decl IMPORT setLangC, setLangCP, setLangM2 ;

FROM DynamicStrings IMPORT String, Length, InitString, Mark, Slice, EqualArray,
                           InitStringCharStar, ConCatChar, ConCat, KillString,
                           Dup, string, char ;
CONST
   YEAR = '2016' ;

VAR
   debugTopological,
   extendedOpaque,
   internalDebugging,
   verbose,
   quiet            : BOOLEAN ;
   outputFile,
   cppArgs,
   cppProgram       : String ;


(*
   displayVersion - displays the version of the compiler.
*)

PROCEDURE displayVersion (mustExit: BOOLEAN) ;
BEGIN
   printf0 ('Copyright (C) ' + YEAR + ' Free Software Foundation, Inc.\n') ;
   printf0 ('License GPLv2: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>\n') ;
   printf0 ('This is free software: you are free to change and redistribute it.\n') ;
   printf0 ('There is NO WARRANTY, to the extent permitted by law.\n') ;
   IF mustExit
   THEN
      exit (0)
   END
END displayVersion ;


(*
   displayHelp - display the mc help summary.
*)

PROCEDURE displayHelp ;
BEGIN
   printf0 ("usage: mc [--cpp] [-g] [--quiet] [--extended-opaque] [-q] [-v] [--verbose] [--version] [--help] [-h] [-Ipath] [--olang=c] [--olang=c++] [--olang=m2] filename\n") ;
   printf0 ("  --cpp               preprocess through the C preprocessor\n") ;
   printf0 ("  -g                  emit debugging directives in the output language") ;
   printf0 ("                      so that the debugger will refer to the source\n") ;
   printf0 ("  -q --quiet          no output unless an error occurs\n") ;
   printf0 ("  -v --verbose        display preprocessor if invoked\n") ;
   printf0 ("  --version           display version and exit\n") ;
   printf0 ("  -h --help           display this help message\n") ;
   printf0 ("  -Ipath              set the module search path\n") ;
   printf0 ("  --olang=c           generate ansi C output\n") ;
   printf0 ("  --olang=c++         generate ansi C++ output\n") ;
   printf0 ("  --olang=m2          generate PIM4 output\n") ;
   printf0 ("  --extended-opaque   parse definition and implementation modules to\n") ;
   printf0 ("                      generate full type debugging of opaque types\n") ;
   printf0 ("  --debug-top         debug topological data structure resolving (internal)\n") ;
   exit (0)
END displayHelp ;


(*
   getCppCommandLine - returns the Cpp command line and all arguments.
*)

PROCEDURE getCppCommandLine () : String ;
VAR
   s: String ;
BEGIN
   IF EqualArray (cppProgram, '')
   THEN
      RETURN NIL
   ELSE
      s := Dup (cppProgram) ;
      s := ConCat (ConCatChar(s, ' '), cppArgs) ;
      IF getQuiet ()
      THEN
         s := ConCat (ConCatChar(s, ' '), Mark (InitString ('-quiet')))
      END ;
      RETURN s
   END
END getCppCommandLine ;


(*
   setOutputFile - sets the output filename to output.
*)

PROCEDURE setOutputFile (output: String) ;
BEGIN
   outputFile := output
END setOutputFile ;


(*
   getOutputFile - sets the output filename to output.
*)

PROCEDURE getOutputFile () : String ;
BEGIN
   RETURN outputFile
END getOutputFile ;


(*
   setQuiet - sets the quiet flag to, value.
*)

PROCEDURE setQuiet (value: BOOLEAN) ;
BEGIN
   quiet := value
END setQuiet ;


(*
   getQuiet - return the value of quiet.
*)

PROCEDURE getQuiet () : BOOLEAN ;
BEGIN
   RETURN quiet
END getQuiet ;


(*
   setVerbose - sets the verbose flag to, value.
*)

PROCEDURE setVerbose (value: BOOLEAN) ;
BEGIN
   verbose := value
END setVerbose ;


(*
   getVerbose - return the value of verbose.
*)

PROCEDURE getVerbose () : BOOLEAN ;
BEGIN
   RETURN verbose
END getVerbose ;


(*
   setExtendedOpaque - set extendedOpaque to value.
*)

PROCEDURE setExtendedOpaque (value: BOOLEAN) ;
BEGIN
   extendedOpaque := value
END setExtendedOpaque ;


(*
   getExtendedOpaque - return the extendedOpaque value.
*)

PROCEDURE getExtendedOpaque () : BOOLEAN ;
BEGIN
   RETURN extendedOpaque
END getExtendedOpaque ;


(*
   setSearchPath - set the search path for the module sources.
*)

PROCEDURE setSearchPath (arg: String) ;
BEGIN
   prependSearchPath (arg)
END setSearchPath ;


(*
   setInternalDebugging - turn on/off internal debugging.
*)

PROCEDURE setInternalDebugging (value: BOOLEAN) ;
BEGIN
   internalDebugging := value
END setInternalDebugging ;


(*
   getInternalDebugging - return the value of internalDebugging.
*)

PROCEDURE getInternalDebugging () : BOOLEAN ;
BEGIN
   RETURN internalDebugging
END getInternalDebugging ;


(*
   setDebugTopological - sets the flag debugTopological to value.
*)

PROCEDURE setDebugTopological (value: BOOLEAN) ;
BEGIN
   debugTopological := value
END setDebugTopological ;


(*
   getDebugTopological - returns the flag value of the command
                         line option --debug-top.
*)

PROCEDURE getDebugTopological () : BOOLEAN ;
BEGIN
   RETURN debugTopological
END getDebugTopological ;


(*
   optionIs - returns TRUE if the first len (right) characters
              match left.
*)

PROCEDURE optionIs (left: ARRAY OF CHAR; right: String) : BOOLEAN ;
VAR
   s: String ;
BEGIN
   IF Length (right) = StrLen (left)
   THEN
      RETURN EqualArray (right, left)
   ELSIF  Length (right) > StrLen (left)
   THEN
      s := Mark (Slice (right, 0, StrLen (left))) ;
      RETURN EqualArray (s, left)
   ELSE
      RETURN FALSE
   END
END optionIs ;


(*
   setLang - set the appropriate output language.
*)

PROCEDURE setLang (arg: String) ;
BEGIN
   IF optionIs ("c", arg)
   THEN
      setLangC
   ELSIF optionIs ("c++", arg)
   THEN
      setLangCP
   ELSIF optionIs ("m2", arg)
   THEN
      setLangM2
   ELSE
      displayHelp
   END
END setLang ;


(*
   handleOption -
*)

PROCEDURE handleOption (arg: String) ;
BEGIN
   IF optionIs ("--quiet", arg) OR optionIs ("-q", arg)
   THEN
      setQuiet (TRUE)
   ELSIF optionIs ("--verbose", arg) OR optionIs ("-v", arg)
   THEN
      setVerbose (TRUE)
   ELSIF optionIs ("--version", arg)
   THEN
      displayVersion (TRUE)
   ELSIF optionIs ("--olang=", arg)
   THEN
      setLang (Slice (arg, 10, 0))
   ELSIF optionIs ("-I", arg)
   THEN
      setSearchPath (Slice (arg, 2, 0))
   ELSIF optionIs ("--help", arg) OR optionIs ("-h", arg)
   THEN
      displayHelp
   ELSIF optionIs ("--cpp", arg)
   THEN
      cppProgram := InitString ('cpp')
   ELSIF optionIs ("-o=", arg)
   THEN
      setOutputFile (Slice (arg, 3, 0))
   ELSIF optionIs ("--extended-opaque", arg)
   THEN
      setExtendedOpaque (TRUE)
   ELSIF optionIs ("--debug-top", arg)
   THEN
      setDebugTopological (TRUE)
   END
END handleOption ;


(*
   handleOptions - iterates over all options setting appropriate
                   values and returns the single source file
                   if found at the end of the arguments.
*)

PROCEDURE handleOptions () : String ;
VAR
   i  : CARDINAL ;
   arg: String ;
BEGIN
   i := 1 ;
   WHILE GetArg (arg, i) DO
      IF Length (arg) > 0
      THEN
         IF char (arg, 0)='-'
         THEN
            handleOption (arg)
         ELSE
            RETURN arg
         END
      END ;
      INC (i)
   END ;
   RETURN NIL
END handleOptions ;


BEGIN
   internalDebugging := FALSE ;
   quiet := FALSE ;
   verbose := FALSE ;
   extendedOpaque := FALSE ;
   debugTopological := FALSE ;
   cppArgs := InitString ('') ;
   cppProgram := InitString ('') ;
   outputFile := InitString ('-')
END mcOptions.
