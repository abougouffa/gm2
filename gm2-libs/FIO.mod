(* Copyright (C) 2001 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA *)
IMPLEMENTATION MODULE FIO ;

(*
    Title      : FIO
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Thu Sep  2 22:07:21 1999
    Last edit  : Thu Sep  2 22:07:21 1999
    Description: a complete reimplememtation of FIO.mod
                 provides a simple buffered file input/output library.
*)

FROM SYSTEM IMPORT ADR, TSIZE, SIZE, WORD ;
FROM ASCII IMPORT nl, nul, tab ;
FROM StrLib IMPORT StrLen, StrConCat, StrCopy ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM NumberIO IMPORT CardToStr ;
FROM libc IMPORT exit, open, creat, read, write, close, lseek, strncpy, memcpy ;
FROM DynamicStrings IMPORT Length, string ;
FROM M2RTS IMPORT InstallTerminationProcedure ;

CONST
   SEEK_SET            =       0 ;   (* relative from beginning of the file *)
   UNIXREADONLY        =       0 ;
   CreatePermissions   =     666B;
   MaxNoOfFiles        =     100 ;
   MaxBufferLength     = 1024*16 ;
   MaxErrorString      = 1024* 8 ;

TYPE
   FileUsage         = (unused, openedforread, openedforwrite, openedforrandom) ;
   FileStatus        = (successful, outofmemory, toomanyfilesopen, failed, connectionfailure) ;

   NameInfo          = RECORD
                          address: ADDRESS ;
                          size   : CARDINAL ;
                       END ;

   Buffer            = POINTER TO buf ;
   buf               =            RECORD
                                     position: CARDINAL ;  (* where are we through this buffer *)
                                     address : ADDRESS ;   (* dynamic buffer address           *)
                                     filled  : CARDINAL ;  (* length of the buffer filled      *)
                                     size    : CARDINAL ;  (* maximum space in this buffer     *)
                                     left    : CARDINAL ;  (* number of bytes left to read     *)
                                     contents: POINTER TO ARRAY [0..MaxBufferLength] OF CHAR ;
                                  END ;

   FileDescriptors   = POINTER TO fds ;
   fds               =            RECORD
                                     unixfd: INTEGER ;
                                     name  : NameInfo ;
                                     state : FileStatus ;
                                     usage : FileUsage ;
                                     output: BOOLEAN ;     (* is this file going to write data *)
                                     buffer: Buffer ;
                                     abspos: CARDINAL ;    (* absolute position into file.     *)
                                  END ;

   PtrToChar         = POINTER TO CHAR ;

(* we only need forward directives for the p2c bootstrapping tool *)

(* %%%FORWARD%%%
PROCEDURE FormatError (a: ARRAY OF CHAR) ; FORWARD ;
PROCEDURE FormatError1 (a: ARRAY OF CHAR; w: ARRAY OF BYTE) ; FORWARD ;
PROCEDURE FlushBuffer (f: File) ; FORWARD ;
PROCEDURE CheckAccess (f: File; use: FileUsage; towrite: BOOLEAN) ; FORWARD ;
PROCEDURE BufferedRead (f: File; nBytes: CARDINAL; a: ADDRESS) : INTEGER ; FORWARD ;
PROCEDURE InitializeFile (f: File; fname: ADDRESS; flength: CARDINAL;
                          fstate: FileStatus; use: FileUsage; towrite: BOOLEAN; buflength: CARDINAL) : File ; FORWARD ;
PROCEDURE ConnectToUnix (f: File; towrite: BOOLEAN) ; FORWARD ;
   %%%FORWARD%%% *)

VAR
   FileInfo: ARRAY [0..MaxNoOfFiles] OF FileDescriptors ;


(*
   GetUnixFileDescriptor - returns the UNIX file descriptor of a file.
*)

PROCEDURE GetUnixFileDescriptor (f: File) : INTEGER ;
BEGIN
   IF (f<MaxNoOfFiles) AND (FileInfo[f]#NIL)
   THEN
      RETURN( FileInfo[f]^.unixfd )
   ELSE
      FormatError1('file %d has not been opened or is out of range\n', f) ;
      RETURN( -1 )
   END
END GetUnixFileDescriptor ;


(*
   WriteString - writes a string to file, f.
*)

PROCEDURE WriteString (f: File; a: ARRAY OF CHAR) ;
VAR
   l: CARDINAL ;
BEGIN
   l := StrLen(a) ;
   IF WriteNBytes(f, l, ADR(a))#l
   THEN
   END
END WriteString ;


(*
   Max - returns the maximum of two values.
*)

PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   Min - returns the minimum of two values.
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


(*
   GetNextFreeDescriptor - returns the index to the FileInfo array indicating
                           the next free slot. If we run out of slots then we
                           return MaxNoOfFiles.
*)

PROCEDURE GetNextFreeDescriptor () : File ;
VAR
   f: File ;
BEGIN
   f := 0 ;
   WHILE (f<MaxNoOfFiles) AND (FileInfo[f]#NIL) DO
      f := f+File(1)   (* --fixme-- compiler should allow INC(f) *)
   END ;
   RETURN( f )
END GetNextFreeDescriptor ;


(*
   IsNoError - returns a TRUE if no error has occured on file, f.
*)

PROCEDURE IsNoError (f: File) : BOOLEAN ;
BEGIN
   RETURN(
          (f<MaxNoOfFiles) AND (FileInfo[f]#NIL) AND (FileInfo[f]^.state=successful)
         )
END IsNoError ;


(*
   openToRead - attempts to open a file, fname, for reading and
                it returns this file.
                The success of this operation can be checked by
                calling IsNoError.
*)

PROCEDURE openToRead (fname: ADDRESS; flength: CARDINAL) : File ;
VAR
   f: File ;
BEGIN
   f := GetNextFreeDescriptor() ;
   IF f<MaxNoOfFiles
   THEN
      f := InitializeFile(f, fname, flength, successful, openedforread, FALSE, MaxBufferLength) ;
      ConnectToUnix(f, FALSE)
   ELSE
      FileInfo[f]^.state := toomanyfilesopen
   END ;
   RETURN( f )
END openToRead ;


(*
   openToWrite - attempts to open a file, fname, for write and
                 it returns this file.
                 The success of this operation can be checked by
                 calling IsNoError.
*)

PROCEDURE openToWrite (fname: ADDRESS; flength: CARDINAL) : File ;
VAR
   f: File ;
BEGIN
   f := GetNextFreeDescriptor() ;
   IF f<MaxNoOfFiles
   THEN
      f := InitializeFile(f, fname, flength, successful, openedforwrite, TRUE, MaxBufferLength) ;
      ConnectToUnix(f, TRUE)
   ELSE
      FileInfo[f]^.state := toomanyfilesopen
   END ;
   RETURN( f )
END openToWrite ;


(*
   openForRandom - attempts to open a file, fname, for random access
                   read or write and it returns this file.
                   The success of this operation can be checked by
                   calling IsNoError.
                   towrite, determines whether the file should be
                   opened for writing or reading.
*)

PROCEDURE openForRandom (fname: ADDRESS; flength: CARDINAL; towrite: BOOLEAN) : File ;
VAR
   f: File ;
BEGIN
   f := GetNextFreeDescriptor() ;
   IF f<MaxNoOfFiles
   THEN
      f := InitializeFile(f, fname, flength, successful, openedforrandom, towrite, MaxBufferLength) ;
      ConnectToUnix(f, towrite)
   ELSE
      FileInfo[f]^.state := toomanyfilesopen
   END ;
   RETURN( f )
END openForRandom ;


(*
   exists - returns TRUE if a file named, fname exists for reading.
*)

PROCEDURE exists (fname: ADDRESS; flength: CARDINAL) : BOOLEAN ;
VAR
   f: File ;
BEGIN
   f := openToRead(fname, flength) ;
   IF IsNoError(f)
   THEN
      Close(f) ;
      RETURN( TRUE )
   ELSE
      Close(f) ;
      RETURN( FALSE )
   END
END exists ;


(*
   InitializeFile - initialize a file descriptor
*)

PROCEDURE InitializeFile (f: File; fname: ADDRESS; flength: CARDINAL;
                          fstate: FileStatus; use: FileUsage; towrite: BOOLEAN; buflength: CARDINAL) : File ;
VAR
   p: PtrToChar ;
BEGIN
   NEW(FileInfo[f]) ;
   IF FileInfo[f]=NIL
   THEN
      FileInfo[MaxNoOfFiles]^.state := outofmemory ;
      RETURN( MaxNoOfFiles )
   ELSE
      WITH FileInfo[f]^ DO
         name.size := flength+1 ;  (* need to guarentee the nul for C *)
         usage     := use ;
         output    := towrite ;
         ALLOCATE(name.address, name.size) ;
         IF name.address=NIL
         THEN
            state := outofmemory ;
            RETURN( f )
         END ;
         name.address := strncpy(name.address, fname, flength) ;
         (* and assign nul to the last byte *)
         p := ADDRESS(name.address + flength) ;
         p^ := nul ;
         abspos := 0 ;
         (* now for the buffer *)
         NEW(buffer) ;
         IF buffer=NIL
         THEN
            FileInfo[MaxNoOfFiles]^.state := outofmemory ;
            RETURN( MaxNoOfFiles )
         ELSE
            WITH buffer^ DO
               size     := buflength ;
               position := 0 ;
               filled   := 0 ;
               IF size=0
               THEN
                  address := NIL
               ELSE
                  ALLOCATE(address, size) ;
                  IF address=NIL
                  THEN
                     state := outofmemory ;
                     RETURN( f )
                  END
               END ;
               IF towrite
               THEN
                  left := size
               ELSE
                  left := 0
               END ;
               contents := address ;  (* provides easy access for reading characters *)
            END ;
            state := fstate
         END
      END
   END ;
   RETURN( f )
END InitializeFile ;


(*
   ConnectToUnix - connects a FIO file to a UNIX file descriptor.
*)

PROCEDURE ConnectToUnix (f: File; towrite: BOOLEAN) ;
BEGIN
   IF (f<MaxNoOfFiles) AND (FileInfo[f]#NIL)
   THEN
      WITH FileInfo[f]^ DO
         IF towrite
         THEN
            unixfd := creat(name.address, CreatePermissions)
         ELSE
            unixfd := open(name.address, UNIXREADONLY, 0)
         END ;
         IF unixfd<0
         THEN
            state  := connectionfailure
         END
      END
   END
END ConnectToUnix ;


(*
   The following functions are wrappers for the above.
*)

PROCEDURE Exists (fname: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   RETURN( exists(ADR(fname), StrLen(fname)) )
END Exists ;


PROCEDURE OpenToRead (fname: ARRAY OF CHAR) : File ;
BEGIN
   RETURN( openToRead(ADR(fname), StrLen(fname)) )
END OpenToRead ;


PROCEDURE OpenToWrite (fname: ARRAY OF CHAR) : File ;
BEGIN
   RETURN( openToWrite(ADR(fname), StrLen(fname)) )
END OpenToWrite ;


PROCEDURE OpenForRandom (fname: ARRAY OF CHAR; towrite: BOOLEAN) : File ;
BEGIN
   RETURN( openForRandom(ADR(fname), StrLen(fname), towrite) )
END OpenForRandom ;


(*
   Close - close a file which has been previously opened using:
           OpenToRead, OpenToWrite, OpenForRandom.
           It is correct to close a file which has an error status.
*)

PROCEDURE Close (f: File) ;
BEGIN
   IF f<MaxNoOfFiles
   THEN
      (*
         although we allow users to close files which have an error status
         it is sensible to leave the MaxNoOfFiles file descriptor alone.
      *)
      IF FileInfo[f]#NIL
      THEN
         FlushBuffer(f) ;
         WITH FileInfo[f]^ DO
            IF unixfd>=0
            THEN
               IF close(unixfd)#0
               THEN
                  FormatError1('failed to close file (%s)\n', name.address) ;
                  state := failed   (* --fixme-- too late to notify user (unless we return a BOOLEAN) *)
               END
            END ;
            IF name.address#NIL
            THEN
               DEALLOCATE(name.address, name.size)
            END ;
            IF buffer#NIL
            THEN
               WITH buffer^ DO
                  IF address#NIL
                  THEN
                     DEALLOCATE(address, size)
                  END
               END ;
               DISPOSE(buffer)
            END
         END ;
         DISPOSE(FileInfo[f]) ;
         FileInfo[f] := NIL
      END
   END
END Close ;


(*
   ReadFromBuffer - attempts to read, nBytes, from file, f.
                    It firstly consumes the buffer and then performs
                    direct unbuffered reads. This should only be used
                    when wishing to read large files.

                    The actual number of bytes read is returned.
                    -1 is returned if EOF is reached.
*)

PROCEDURE ReadFromBuffer (f: File; a: ADDRESS; nBytes: CARDINAL) : INTEGER ;
VAR 
   result: INTEGER ;
   total,
   n     : CARDINAL ;
   p     : POINTER TO BYTE ;
BEGIN
   IF f<MaxNoOfFiles
   THEN
      total := 0 ;   (* how many bytes have we read *)
      WITH FileInfo[f]^ DO
         (* extract from the buffer first *)
         IF buffer#NIL
         THEN
            WITH buffer^ DO
               IF left>0
               THEN
                  IF nBytes=1
                  THEN
                     (* too expensive to call memcpy for 1 character *)
                     p := a ;
                     p^ := contents^[position] ;
                     DEC(left) ;         (* remove consumed bytes               *)
                     INC(position) ;     (* move onwards n bytes                *)
                     nBytes := 0 ;       (* reduce the amount for future direct *)
                                         (* read                                *)
                     INC(abspos) ;
                     RETURN( 1 )
                  ELSE
                     n := Min(left, nBytes) ;
                     p := memcpy(ADDRESS(address+position), a, n) ;
                     DEC(left, n) ;      (* remove consumed bytes               *)
                     INC(position, n) ;  (* move onwards n bytes                *)
                                         (* move onwards ready for direct reads *)
                     a := ADDRESS(a+n) ;
                     DEC(nBytes, n) ;    (* reduce the amount for future direct *)
                                         (* read                                *)
                     INC(total, n) ;
                     INC(abspos, n)
                  END
               END
            END
         END ;
         IF nBytes>0
         THEN
            (* still more to read *)
            result := read(unixfd, a, INTEGER(nBytes)) ;
            IF result>0
            THEN
               INC(total, result) ;
               INC(abspos, result)
            ELSE
               (* eof reached, set the buffer accordingly *)
               state := failed ;
               IF buffer#NIL
               THEN
                  WITH buffer^ DO
                     left     := 0 ;
                     position := 0 ;
                     IF address#NIL
                     THEN
                        contents^[position] := nul
                     END
                  END
               END ;
               RETURN( -1 )
            END
         END
      END ;
      RETURN( total )
   ELSE
      RETURN( -1 )
   END
END ReadFromBuffer ;


(*
   ReadNBytes - reads nBytes of a file into memory area, a, returning
                the number of bytes actually read.
                This function will consume from the buffer and then
                perform direct libc reads. It is ideal for large reads.
*)

PROCEDURE ReadNBytes (f: File; nBytes: CARDINAL; a: ADDRESS) : CARDINAL ;
VAR
   n: CARDINAL ;
BEGIN
   IF f<MaxNoOfFiles
   THEN
      CheckAccess(f, openedforread, FALSE) ;
      n := ReadFromBuffer(f, a, nBytes) ;
      IF n<0
      THEN
         RETURN( 0 )
      ELSE
         RETURN( n )
      END
   ELSE
      RETURN( 0 )
   END
END ReadNBytes ;


(*
   BufferedRead - will read, nBytes, through the buffer.
                  Similar to ReadFromBuffer, but this function will always
                  read into the buffer before copying into memory.

                  Useful when performing small reads.
*)

PROCEDURE BufferedRead (f: File; nBytes: CARDINAL; a: ADDRESS) : INTEGER ;
VAR 
   result: INTEGER ;
   total,
   n     : INTEGER ;
   p     : POINTER TO BYTE ;
BEGIN
   IF f<MaxNoOfFiles
   THEN
      total := 0 ;   (* how many bytes have we read *)
      WITH FileInfo[f]^ DO
         (* extract from the buffer first *)
         IF buffer#NIL
         THEN
            WITH buffer^ DO
               WHILE nBytes>0 DO
                  IF left>0
                  THEN
                     IF nBytes=1
                     THEN
                        (* too expensive to call memcpy for 1 character *)
                        p := a ;
                        p^ := contents^[position] ;
                        DEC(left) ;         (* remove consumed byte                *)
                        INC(position) ;     (* move onwards n byte                 *)
                        INC(total) ;
                        INC(abspos) ;
                        RETURN( total )
                     ELSE
                        n := Min(left, nBytes) ;
                        p := memcpy(ADDRESS(address+position), a, CARDINAL(n)) ;
                        DEC(left, n) ;      (* remove consumed bytes               *)
                        INC(position, n) ;  (* move onwards n bytes                *)
                                            (* move onwards ready for direct reads *)
                        a := ADDRESS(a+n) ;
                        DEC(nBytes, n) ;    (* reduce the amount for future direct *)
                                            (* read                                *)
                        INC(total, n) ;
                        INC(abspos, n)
                     END
                  ELSE
                     (* refill buffer *)
                     n := read(unixfd, address, size) ;
                     IF n>=0
                     THEN
                        position := 0 ;
                        left     := n ;
                        filled   := n ;
                        INC(abspos, n) ;
                        IF n=0
                        THEN
                           (* eof reached *)
                           state := failed ;
                           RETURN( -1 )
                        END
                     ELSE
                        position := 0 ;
                        left     := 0 ;
                        filled   := 0 ;
                        abspos   := 0 ;
                        state    := failed ;
                        RETURN( total )
                     END
                  END
               END
            END ;
            RETURN( total )
         ELSE
            RETURN( -1 )
         END
      END
   ELSE
      RETURN( -1 )
   END
END BufferedRead ;


(*
   HandleEscape - translates \n and \t into their respective ascii codes.
*)

PROCEDURE HandleEscape (VAR dest: ARRAY OF CHAR; src: ARRAY OF CHAR;
                        VAR i, j: CARDINAL; HighSrc, HighDest: CARDINAL) ;
BEGIN
   IF (i+1<HighSrc) AND (src[i]='\') AND (j<HighDest)
   THEN
      IF src[i+1]='n'
      THEN
         (* requires a newline *)
         dest[j] := nl ;
         INC(j) ;
         INC(i, 2)
      ELSIF src[i+1]='t'
      THEN
         (* requires a tab (yuck) tempted to fake this but I better not.. *)
         dest[j] := tab ;
         INC(j) ;
         INC(i, 2)
      ELSE
         (* copy escaped character *)
         INC(i) ;
         dest[j] := src[i] ;
         INC(j) ;
         INC(i)
      END
   END
END HandleEscape ;


(*
   Cast - casts a := b
*)

PROCEDURE Cast (VAR a: ARRAY OF BYTE; b: ARRAY OF BYTE) ;
VAR
   i: CARDINAL ;
BEGIN
   IF HIGH(a)=HIGH(b)
   THEN
      FOR i := 0 TO HIGH(a) DO
         a[i] := b[i]
      END
   ELSE
      FormatError('cast failed')
   END
END Cast ;


(*
   StringFormat1 - converts string, src, into, dest, together with encapsulated
                   entity, w. It only formats the first %s or %d with n.
*)

PROCEDURE StringFormat1 (VAR dest: ARRAY OF CHAR; src: ARRAY OF CHAR;
                         w: ARRAY OF BYTE) ;
VAR
   HighSrc,
   HighDest,
   c, i, j : CARDINAL ;
   str     : ARRAY [0..MaxErrorString] OF CHAR ;
   p       : POINTER TO CHAR ;
BEGIN
   HighSrc := StrLen(src) ;
   HighDest := HIGH(dest) ;
   i := 0 ;
   j := 0 ;
   WHILE (i<HighSrc) AND (src[i]#nul) AND (j<HighDest) AND (src[i]#'%') DO
      IF src[i]='\'
      THEN
         HandleEscape(dest, src, i, j, HighSrc, HighDest)
      ELSE
         dest[j] := src[i] ;
         INC(i) ;
         INC(j)
      END
   END ;

   IF (i+1<HighSrc) AND (src[i]='%') AND (j<HighDest)
   THEN
      IF src[i+1]='s'
      THEN
         Cast(p, w) ;
         WHILE (j<HighDest) AND (p^#nul) DO
            dest[j] := p^ ;
            INC(j) ;
            INC(p)
         END ;
         IF j<HighDest
         THEN
            dest[j] := nul
         END ;
         j := StrLen(dest) ;
         INC(i, 2)
      ELSIF src[i+1]='d'
      THEN
         dest[j] := nul ;
         Cast(c, w) ;
         CardToStr(c, 0, str) ;
         StrConCat(dest, str, dest) ;
         j := StrLen(dest) ;
         INC(i, 2)
      ELSE
         dest[j] := src[i] ;
         INC(i) ;
         INC(j)
      END
   END ;
   (* and finish off copying src into dest *)
   WHILE (i<HighSrc) AND (src[i]#nul) AND (j<HighDest) DO
      IF src[i]='\'
      THEN
         HandleEscape(dest, src, i, j, HighSrc, HighDest)
      ELSE
         dest[j] := src[i] ;
         INC(i) ;
         INC(j)
      END
   END ;
   IF j<HighDest
   THEN
      dest[j] := nul
   END ;
END StringFormat1 ;


(*
   FormatError - provides a orthoganal counterpart to the procedure below.
*)

PROCEDURE FormatError (a: ARRAY OF CHAR) ;
BEGIN
   WriteString(StdErr, a)
END FormatError ;


(*
   FormatError1 - fairly generic error procedure.
*)

PROCEDURE FormatError1 (a: ARRAY OF CHAR; w: ARRAY OF BYTE) ;
VAR
   s: ARRAY [0..MaxErrorString] OF CHAR ;
BEGIN
   StringFormat1(s, a, w) ;
   FormatError(s)
END FormatError1 ;


(*
   FormatError2 - fairly generic error procedure.
*)

PROCEDURE FormatError2 (a: ARRAY OF CHAR;
                        w1, w2: ARRAY OF BYTE) ;
VAR
   s: ARRAY [0..MaxErrorString] OF CHAR ;
BEGIN
   StringFormat1(s, a, w1) ;
   FormatError1(s, w2)
END FormatError2 ;


(*
   CheckAccess - checks to see whether a file, f, has been
                 opened for read/write.
*)

PROCEDURE CheckAccess (f: File; use: FileUsage; towrite: BOOLEAN) ;
BEGIN
   IF f<MaxNoOfFiles
   THEN
      IF FileInfo[f]=NIL
      THEN
         FormatError('this file has probably been closed and not reopened successfully or alternatively never opened\n') ;
         HALT
      ELSE
         WITH FileInfo[f]^ DO
            IF (use=openedforwrite) AND (usage=openedforread)
            THEN
               FormatError1('this file (%s) has been opened for reading but is now being written\n',
                            name.address) ;
               HALT
            ELSIF (use=openedforread) AND (usage=openedforwrite)
            THEN
               FormatError1('this file (%s) has been opened for writing but is now being read\n',
                            name.address) ;
               HALT
            ELSIF state=connectionfailure
            THEN
               FormatError1('this file (%s) was not successfully opened\n',
                            name.address) ;
               HALT
            ELSIF towrite#output
            THEN
               IF output
               THEN
                  FormatError1('this file (%s) was opened for writing but is now being read\n',
                               name.address) ;
                  HALT
               ELSE
                  FormatError1('this file (%s) was opened for reading but is now being written\n',
                               name.address) ;
                  HALT
               END
            END
         END
      END
   ELSE
      FormatError('this file has not been opened successfully\n') ;
      HALT
   END
END CheckAccess ;


(*
   ReadChar - returns a character read from file, f.
              Sensible to check with IsNoError or EOF after calling
              this function.
*)

PROCEDURE ReadChar (f: File) : CHAR ;
VAR
   ch: CHAR ;
BEGIN
   CheckAccess(f, openedforread, FALSE) ;
   IF BufferedRead(f, SIZE(ch), ADR(ch))=SIZE(ch)
   THEN
      RETURN( ch )
   ELSE
      RETURN( nul )
   END
END ReadChar ;


(*
   UnReadChar - replaces a character, ch, back into file, f.
                This character must have been read by ReadChar
                and it does not allow successive calls.
*)

PROCEDURE UnReadChar (f: File ; ch: CHAR) ;
BEGIN
   CheckAccess(f, openedforread, FALSE) ;
   IF (f<MaxNoOfFiles) AND (FileInfo[f]#NIL)
   THEN
      WITH FileInfo[f]^ DO
         IF buffer#NIL
         THEN
            WITH buffer^ DO
               (* we assume that a ReadChar has occurred, we will check just in case. *)
               IF (position>0) AND (filled>0)
               THEN
                  DEC(position) ;
                  INC(left) ;
                  contents^[position] := ch
               ELSE
                  FormatError1('performing too many UnReadChar calls on file (%d)\n', f)
               END
            END
         END
      END
   END
END UnReadChar ;


(*
   ReadAny - reads HIGH(a) bytes into, a. All input
             is fully buffered, unlike ReadNBytes and thus is more
             suited to small reads.
*)

PROCEDURE ReadAny (f: File; VAR a: ARRAY OF BYTE) ;
BEGIN
   CheckAccess(f, openedforread, FALSE) ;
   IF BufferedRead(f, HIGH(a), ADR(a))=HIGH(a)
   THEN
   END
END ReadAny ;


(*
   EOF - tests to see whether a file, f, has reached end of file.
*)

PROCEDURE EOF (f: File) : BOOLEAN ;
VAR
   ch: CHAR ;
   s : FileStatus ;
BEGIN
   CheckAccess(f, openedforread, FALSE) ;
   (*
      we will read a character and then push it back onto the input stream,
      having noted the file status, we also reset the status.
   *)
   IF (f<MaxNoOfFiles) AND (FileInfo[f]#NIL) AND (FileInfo[f]^.state=successful)
   THEN
      ch := ReadChar(f) ;
      s := FileInfo[f]^.state ;
      IF s=successful
      THEN
         UnReadChar(f, ch) ;
         FileInfo[f]^.state := s ;
         RETURN( FALSE )
      ELSE
         RETURN( TRUE )
      END
   ELSE
      RETURN( TRUE )
   END
END EOF ;


(*
   EOLN - tests to see whether a file, f, is upon a newline.
          It does NOT consume the newline.
*)

PROCEDURE EOLN (f: File) : BOOLEAN ;
VAR
   ch: CHAR ;
   s : FileStatus ;
BEGIN
   CheckAccess(f, openedforread, FALSE) ;
   (*
      we will read a character and then push it back onto the input stream,
      having noted the file status, we also reset the status.
   *)
   IF (f<MaxNoOfFiles) AND (FileInfo[f]#NIL)
   THEN
      ch := ReadChar(f) ;
      s := FileInfo[f]^.state ;
      UnReadChar(f, ch) ;
      FileInfo[f]^.state := s ;
      RETURN( (s=successful) AND (ch=nl) )
   ELSE
      RETURN( FALSE )
   END
END EOLN ;


(*
   WriteLine - writes out a linefeed to file, f.
*)

PROCEDURE WriteLine (f: File) ;
BEGIN
   WriteChar(f, nl)
END WriteLine ;


(*
   WriteNBytes - writes nBytes of a file into memory area, a, returning
                 the number of bytes actually written.
                 This function will flush the buffer and then
                 write the nBytes using a direct write from libc.
                 It is ideal for large writes.
*)

PROCEDURE WriteNBytes (f: File; nBytes: CARDINAL; a: ADDRESS) : CARDINAL ;
VAR
   total: INTEGER ;
BEGIN
   CheckAccess(f, openedforwrite, TRUE) ;
   FlushBuffer(f) ;
   IF (f<MaxNoOfFiles) AND (FileInfo[f]#NIL)
   THEN
      WITH FileInfo[f]^ DO
         total := write(unixfd, a, INTEGER(nBytes)) ;
         IF total<0
         THEN
            state := failed ;
            RETURN( 0 )
         ELSE
            RETURN( CARDINAL(total) )
         END
      END
   ELSE
      RETURN( 0 )
   END
END WriteNBytes ;


(*
   BufferedWrite - will write, nBytes, through the buffer.
                   Similar to WriteNBytes, but this function will always
                   write into the buffer before copying into memory.

                   Useful when performing small writes.
*)

PROCEDURE BufferedWrite (f: File; nBytes: CARDINAL; a: ADDRESS) : INTEGER ;
VAR 
   result: INTEGER ;
   total,
   n     : INTEGER ;
   p     : POINTER TO BYTE ;
BEGIN
   IF f<MaxNoOfFiles
   THEN
      total := 0 ;   (* how many bytes have we read *)
      WITH FileInfo[f]^ DO
         IF buffer#NIL
         THEN
            WITH buffer^ DO
               WHILE nBytes>0 DO
                  (* place into the buffer first *)
                  IF left>0
                  THEN
                     IF nBytes=1
                     THEN
                        (* too expensive to call memcpy for 1 character *)
                        p := a ;
                        contents^[position] := p^ ;
                        DEC(left) ;         (* reduce space                        *)
                        INC(position) ;     (* move onwards n byte                 *)
                        INC(total) ;
                        INC(abspos) ;
                        RETURN( total )
                     ELSE
                        n := Min(left, nBytes) ;
                        p := memcpy(a, ADDRESS(address+position), CARDINAL(n)) ;
                        DEC(left, n) ;      (* remove consumed bytes               *)
                        INC(position, n) ;  (* move onwards n bytes                *)
                                            (* move ready for further writes       *)
                        a := ADDRESS(a+n) ;
                        DEC(nBytes, n) ;    (* reduce the amount for future writes *)
                        INC(total, n) ;
                        INC(abspos, n)
                     END
                  ELSE
                     FlushBuffer(f) ;
                     IF state#successful
                     THEN
                        nBytes := 0
                     END
                  END
               END
            END ;
            RETURN( total )
         ELSE
            RETURN( -1 )
         END
      END
   ELSE
      RETURN( -1 )
   END
END BufferedWrite ;


(*
   FlushBuffer - flush contents of file, f.
*)

PROCEDURE FlushBuffer (f: File) ;
BEGIN
   IF (f<MaxNoOfFiles) AND (FileInfo[f]#NIL)
   THEN
      WITH FileInfo[f]^ DO
         IF output AND (buffer#NIL)
         THEN
            WITH buffer^ DO
               IF (position=0) OR (write(unixfd, address, position)=position)
               THEN
                  position := 0 ;
                  filled   := 0 ;
                  left     := size
               ELSE
                  state := failed
               END
            END
         END
      END
   END
END FlushBuffer ;


(*
   WriteAny - writes HIGH(a) bytes onto, file, f. All output
              is fully buffered, unlike WriteNBytes and thus is more
              suited to small writes.
*)

PROCEDURE WriteAny (f: File; VAR a: ARRAY OF BYTE) ;
BEGIN
   CheckAccess(f, openedforwrite, TRUE) ;
   IF BufferedWrite(f, HIGH(a), ADR(a))=HIGH(a)
   THEN
   END
END WriteAny ;


(*
   WriteChar - writes a single character to file, f.
*)

PROCEDURE WriteChar (f: File; ch: CHAR) ;
BEGIN
   CheckAccess(f, openedforwrite, TRUE) ;
   IF BufferedWrite(f, SIZE(ch), ADR(ch))=SIZE(ch)
   THEN
   END
END WriteChar ;


(*
   WriteCardinal - writes a CARDINAL to file, f.
                   (here for compatibility - suggest that WriteAny be used instead)
*)

PROCEDURE WriteCardinal (f: File; c: CARDINAL) ;
BEGIN
   WriteAny(f, c)
END WriteCardinal ;


(*
   ReadCardinal - reads a CARDINAL from file, f.
                  (here for compatibility - suggest that ReadAny be used instead)
*)

PROCEDURE ReadCardinal (f: File) : CARDINAL ;
VAR
   c: CARDINAL ;
BEGIN
   ReadAny(f, c) ;
   RETURN( c )
END ReadCardinal ;


(*
   ReadString - reads a string from file, f, into string, a.
                It terminates the string if HIGH is reached or
                if a newline is seen or an error occurs.
*)

PROCEDURE ReadString (f: File; VAR a: ARRAY OF CHAR) ;
VAR
   high,
   i   : CARDINAL ;
   ch  : CHAR ;
BEGIN
   CheckAccess(f, openedforread, FALSE) ;
   high := HIGH(a) ;
   i := 0 ;
   REPEAT
      ch := ReadChar(f) ;
      IF i<=high
      THEN
         IF (ch=nl) OR (NOT IsNoError(f))
         THEN
            a[i] := nul ;
            INC(i)
         ELSE
            a[i] := ch ;
            INC(i)
         END
      END
   UNTIL (ch=nl) OR (i>high) OR (NOT IsNoError(f))
END ReadString ;


(*
   SetPositionFromBeginning - sets the position from the beginning of the file.
*)

PROCEDURE SetPositionFromBeginning (f: File; pos: CARDINAL) ;
BEGIN
   IF f<MaxNoOfFiles
   THEN
      WITH FileInfo[f]^ DO
         (* always force the lseek, until we are confident that abspos is always correct,
            basically it needs some hard testing before we should remove the OR TRUE. *)
         IF (abspos#pos) OR TRUE
         THEN
            FlushBuffer(f) ;
            IF buffer#NIL
            THEN
               WITH buffer^ DO
                  IF output
                  THEN
                     left := size
                  ELSE
                     left := 0
                  END ;
                  position := 0 ;
                  filled   := 0
               END
            END ;
            abspos := pos ;
            IF lseek(unixfd, INTEGER(pos), SEEK_SET)#INTEGER(pos)
            THEN
               state  := failed ;
               abspos := 0
            END
         END
      END
   END
END SetPositionFromBeginning ;


(*
   FindPosition - returns the current absolute position in file, f.
*)

PROCEDURE FindPosition (f: File) : CARDINAL ;
BEGIN
   IF f<MaxNoOfFiles
   THEN
      WITH FileInfo[f]^ DO
         RETURN( abspos )
      END
   ELSE
      RETURN( 0 )
   END
END FindPosition ;


(*
   PreInitialize - preinitialize the file descriptor.
*)

PROCEDURE PreInitialize (f: File; fname: ARRAY OF CHAR;
                         state: FileStatus; use: FileUsage; towrite: BOOLEAN; bufsize: CARDINAL) ;
BEGIN
   IF InitializeFile(f, ADR(fname), StrLen(fname), state, use, towrite, bufsize)=f
   THEN
      IF f<MaxNoOfFiles
      THEN
         FileInfo[f]^.unixfd := INTEGER(f)
      ELSE
         FileInfo[f]^.unixfd := FileInfo[StdErr]^.unixfd    (* the error channel *)
      END
   ELSE
      HALT
   END
END PreInitialize ;


(*
   CloseOutErr - called when the application calls M2RTS.Terminate (automatically placed in
                 program modules by GM2.
*)

PROCEDURE CloseOutErr ;
BEGIN
   IF IsNoError(StdOut)
   THEN
      Close(StdOut)
   END ;
   IF IsNoError(StdErr)
   THEN
      Close(StdErr)
   END
END CloseOutErr ;


(*
   Init - initialize the modules, global variables.
*)

PROCEDURE Init ;
VAR
   f: File ;
BEGIN
   FOR f := 0 TO MaxNoOfFiles DO
      FileInfo[f] := NIL
   END ;
   StdIn := 0 ;
   PreInitialize(StdIn       , 'stdin' , successful      , openedforread , FALSE, MaxBufferLength) ;
   StdOut := 1 ;
   PreInitialize(StdOut      , 'stdout', successful      , openedforwrite,  TRUE, MaxBufferLength) ;
   StdErr := 2 ;
   PreInitialize(StdErr      , 'stderr', successful      , openedforwrite,  TRUE, MaxBufferLength) ;
   (* and now for the error file descriptor *)
   PreInitialize(MaxNoOfFiles, 'error' , toomanyfilesopen, unused        , FALSE, 0) ;
   InstallTerminationProcedure(CloseOutErr)
END Init ;


BEGIN
   Init
END FIO.
