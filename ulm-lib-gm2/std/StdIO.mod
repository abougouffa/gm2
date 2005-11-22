(* Ulm's Modula-2 Library
   Copyright (C) 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992,
   1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
   2004, 2005
   by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either version
   2 of the License, or (at your option) any later version.

   Ulm's Modula-2 Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
   ----------------------------------------------------------------------------
   E-mail contact: gm2@glam.ac.uk
   ----------------------------------------------------------------------------
   $Id: StdIO.mod,v 1.5 2005/11/22 15:13:21 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: StdIO.mod,v $
   Revision 1.5  2005/11/22 15:13:21  gaius
   fixed Copyright dates

   Revision 1.4  2005/11/21 12:09:59  gaius
   updated Copyright notices and dates

   Revision 1.3  2004/07/02 21:07:42  gaius
   fixed many IMPORT bugs in inner modules

   Revision 1.2  2004/06/29 08:51:42  gaius
   * made flex lexical analysers ignore carriage return
   * fixed bug in M2Quads.mod checking parameter of
     a const var before value was known.
   * fixed local MODULEs so that they can FROM mod IMPORT
   * tidied up some ulm implementation modules in ulm-lib-gm2/std

   Revision 1.1  2003/12/27 00:16:05  gaius
   added ulm libraries into the gm2 tree. Currently these
   are only used when regression testing, but later they
   will be accessible by users of gm2.

   Revision 0.3  1997/02/28  15:50:35  borchert
   header fixed

   Revision 0.2  1997/02/21  19:29:39  borchert
   removal of old copyright

   Revision 0.1  1997/02/21  19:18:35  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE StdIO; (* AFB 2/84 *)

   (* $T- *)

   FROM SYSTEM IMPORT ADDRESS, ADR;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM Errno IMPORT errno, EINVAL;
   FROM SysCreat IMPORT Creat;
   FROM SysOpen IMPORT Open;
   FROM SysRead IMPORT Read;
   FROM SysWrite IMPORT Write;
   FROM SysClose IMPORT Close;
   FROM SysLseek IMPORT Lseek, Tell;
   FROM SysTermIO IMPORT Isatty;
   FROM SysExit IMPORT EnterCleanup;
   FROM SystemTypes IMPORT OFF;

   CONST
      BufSiz = 1024;

   TYPE
      Flags = (reading, writing, error, eof);
      FlagsSet = SET OF Flags;
      FILE = POINTER TO FileStr;
      BufRange = OFF [0..BufSiz-1];
      Index = OFF [0..BufSiz];
      FileStr =
	 RECORD
	    fd: CARDINAL; (* file descriptor from UNIX *)
	    flags: FlagsSet;
            unget: BOOLEAN; (* if on: return ungetc on next call *)
            ungetc: CHAR;
	    CASE nobuf: BOOLEAN OF
	       FALSE:
		  cnt: OFF;
		  index: Index;
		  buf: ARRAY BufRange OF CHAR;
	    |  TRUE:
	    END;
	 END;

      Chain = POINTER TO ChainNode;
      ChainNode =
         RECORD
            f: FILE;
            link: Chain;
         END;

   (* see definition module

      MODE = (read, write, append);

   VAR
      stdin, stdout, stderr: FILE;

   *)

   VAR AllFiles: Chain;

   (* local procedures *)

   PROCEDURE Init;

      PROCEDURE Connect(VAR f: FILE; fd: CARDINAL; m: MODE);
      BEGIN
         (* stderr always unbuffered *)
         IF NOT Fdopen(f, fd, m, (fd <> 2) AND NOT Isatty(fd)) THEN
            f := NIL;
         END;
      END Connect;

   BEGIN
      AllFiles := NIL;
      Connect(stdin, 0, read);
      Connect(stdout, 1, write);
      Connect(stderr, 2, write);
   END Init;

   (* enter a file pointer into the chain *)

   PROCEDURE Enter(fp: FILE);
      VAR
         ptr: Chain;
   BEGIN
      NEW(ptr);
      WITH ptr^ DO
         f := fp;
         link := AllFiles;
      END;
      AllFiles := ptr;
   END Enter;

   PROCEDURE FillBuf(f: FILE) : BOOLEAN;
      VAR
	 bytecount: CARDINAL;
   BEGIN
      WITH f^ DO
	 bytecount := BufSiz;
	 IF NOT Read(fd, ADR(buf), bytecount) THEN
	    flags := flags + FlagsSet{error}; RETURN FALSE
	 ELSIF bytecount = 0 THEN
	    flags := flags + FlagsSet{eof}; RETURN FALSE
	 END;
         flags := flags - FlagsSet{eof, error};
	 cnt := bytecount;
         index := 0;
	 RETURN TRUE
      END;
   END FillBuf;

   (* exported procedures *)

   PROCEDURE Fflush(f: FILE) : BOOLEAN;
      VAR bytecount: CARDINAL;
   BEGIN
      WITH f^ DO
         IF NOT (writing IN flags) THEN RETURN FALSE END;
	 IF nobuf THEN RETURN TRUE END;
	 bytecount := cnt;
	 IF NOT Write(fd, ADR(buf), bytecount) THEN
	    flags := flags + FlagsSet{error};
	    RETURN FALSE
	 ELSE
	    cnt := 0;
            index := 0;
            flags := flags - FlagsSet{error};
	    RETURN TRUE
	 END;
      END;
   END Fflush;

   PROCEDURE Fdopen(VAR f: FILE; filedesc: CARDINAL; mode: MODE;
                    buffered: BOOLEAN) : BOOLEAN;
   BEGIN
      IF buffered THEN
         NEW(f)   (* NEW(f, FALSE); *)
      ELSE
         NEW(f)   (* NEW(f, TRUE); *)
      END;
      WITH f^ DO
         nobuf := NOT buffered;
         unget := FALSE;
         fd := filedesc;
	 CASE mode OF
	   read:
	       flags := FlagsSet{reading};
	 | write:
	       flags := FlagsSet{writing};
	 | append:
               IF NOT Lseek(fd, 0, 2) THEN
                  DISPOSE(f);
		  RETURN FALSE;
	       END;
	       flags := FlagsSet{writing};
	 ELSE
            errno := EINVAL; (* invalid argument *)
            DISPOSE(f);
	    RETURN FALSE;
	 END; (* CASE *)
         IF buffered THEN
            cnt := 0;
            index := 0;
         END;
      END; (* WITH *)
      Enter(f);
      RETURN TRUE;
   END Fdopen;

   PROCEDURE Fopen(VAR f: FILE; name: ARRAY OF CHAR; mode: MODE;
		   buffered: BOOLEAN) : BOOLEAN;
      CONST CreateMode = 666B; (* rw-rw-rw- *)
      VAR fd: CARDINAL;
   BEGIN
      CASE mode OF
	read:
	    IF NOT Open(fd, name, (* read = *) 0) THEN
               RETURN FALSE;
	    END;
      | write:
	    IF NOT Creat(fd, name, CreateMode) THEN
               RETURN FALSE;
	    END;
      | append:
	    IF NOT Open(fd, name, (* write = *) 1) AND
               NOT Creat(fd, name, CreateMode) THEN
               RETURN FALSE;
	    END;
            (* lseek will be done by Fdopen *)
      ELSE
            errno := EINVAL; (* invalid argument *)
            RETURN FALSE;
	 END; (* CASE *)
      RETURN Fdopen(f, fd, mode, buffered);
   END Fopen;

   PROCEDURE CloseAll() : BOOLEAN;
      VAR ok: BOOLEAN;
          ptr: Chain;
   BEGIN
      ok := TRUE;
      WHILE AllFiles <> NIL DO
         WITH AllFiles^ DO (* that's no endless loop, see Fclose !!! *)
            ok := Fclose(f) AND ok;
         END;
      END;
      RETURN ok;
   END CloseAll;

   PROCEDURE Cleanup;
   BEGIN
      IF NOT CloseAll() THEN (* ignore flush errors *) END;
   END Cleanup;

   PROCEDURE Fclose(f: FILE) : BOOLEAN;
      VAR ok: BOOLEAN;
          prev, ptr: Chain;
   BEGIN
      IF f = NIL THEN RETURN FALSE END;
      (* look for the file pointer in the chain *)
      ptr := AllFiles;
      prev := NIL;
      LOOP
	 IF ptr = NIL THEN
	    errno := EINVAL; (* invalid argument *)
	    RETURN FALSE; (* f isn't a result from Fopen !!! *)
	 END;
	 IF ptr^.f = f THEN
	    EXIT;
	 END;
	 prev := ptr;
	 ptr := ptr^.link;
      END;
      (* flush the buffer and dispose the structure *)
      WITH f^ DO
	 IF NOT nobuf
            AND NOT(error IN flags) AND (writing IN flags) AND (cnt > 0) THEN
	       ok := Fflush(f);
         ELSE
               ok := TRUE;
	 END;
         ok := Close(fd) AND ok;
         DISPOSE(f);
         IF prev = NIL THEN
            AllFiles := ptr^.link;
         ELSE
            prev^.link := ptr^.link;
         END;
         DISPOSE(ptr);
      END;
      RETURN ok;
   END Fclose;

   PROCEDURE Fread(ptr: ADDRESS; size: CARDINAL; VAR nitems: CARDINAL;
		   f: FILE) : BOOLEAN;
      TYPE TextPtr = POINTER TO ARRAY [0..BufSiz-1] OF CHAR;
      VAR text: TextPtr;
          bytecount: CARDINAL;
          ok: BOOLEAN;
          ch: CHAR;
   BEGIN
      WITH f^ DO
         IF nobuf THEN
            bytecount := size*nitems;
            IF unget AND (bytecount > 0) THEN
               text := TextPtr(ptr);
               text^[0] := ungetc;
               unget := FALSE;
               DEC(bytecount);
               INC(ptr);
            END;
            ok := Read(fd, ptr, bytecount);
            nitems := bytecount DIV size;
            RETURN ok;
         ELSE
            text := TextPtr(ptr);
            FOR bytecount := 0 TO size*nitems-1 DO
               IF NOT Fgetc(ch, f) THEN
                  nitems := bytecount DIV size;
                  RETURN FALSE;
               END;
               text^[bytecount] := ch;
            END;
            RETURN TRUE;
         END;
      END;
   END Fread;

   PROCEDURE Fwrite(ptr: ADDRESS; size: CARDINAL; VAR nitems: CARDINAL;
		    f: FILE) : BOOLEAN;
      TYPE TextPtr = POINTER TO ARRAY [0..BufSiz-1] OF CHAR;
      VAR text: TextPtr;
          bytecount: CARDINAL;
          ok: BOOLEAN;
   BEGIN
      WITH f^ DO
         IF nobuf THEN
            bytecount := size*nitems;
            ok := Write(fd, ptr, bytecount);
            IF NOT ok THEN
               nitems := bytecount DIV size;
            END;
            RETURN ok;
         ELSE
            text := TextPtr(ptr);
            FOR bytecount := 0 TO size*nitems-1 DO
               IF NOT Fputc(text^[bytecount], f) THEN
                  nitems := bytecount DIV size;
                  RETURN FALSE;
               END;
            END;
            RETURN TRUE;
         END;
      END;
   END Fwrite;

   PROCEDURE Fseek(f: FILE; offset: OFF; whence: CARDINAL) : BOOLEAN;
      VAR pos: OFF;
   BEGIN
      WITH f^ DO
         flags := flags - FlagsSet{eof};
         IF NOT nobuf THEN (* seek in buffer ??? *)
            IF writing IN flags THEN
               IF NOT Fflush(f) THEN RETURN FALSE END;
            ELSIF whence = 0 THEN (* absolute seek *)
               IF NOT Ftell(f, pos) THEN RETURN FALSE END;
               IF pos - offset = 0 THEN
                  RETURN TRUE
               ELSIF (offset - pos > 0) AND (offset - pos < cnt) THEN
                  DEC(cnt, offset-pos); INC(index, offset-pos); RETURN TRUE
               ELSIF (offset - pos < 0) AND (pos - offset <= index) THEN
                  INC(cnt, pos-offset); DEC(index, pos-offset); RETURN TRUE
               END;
            ELSIF whence = 1 THEN (* relative seek *)
               IF offset = 0 THEN
                  RETURN TRUE
               ELSIF (offset > 0) AND (offset < cnt) THEN
                  DEC(cnt, offset); INC(index, offset); RETURN TRUE
               ELSIF (offset < 0) AND (ABS(offset) <= index) THEN
                  INC(cnt, ABS(offset)); DEC(index, ABS(offset)); RETURN TRUE;
               ELSE (* calculate correct offset for Lseek *)
                  DEC(offset, cnt);
               END;
            END;
            index := 0;
            cnt := 0;
         END;
         RETURN Lseek(fd, offset, whence)
      END;
   END Fseek;

   PROCEDURE Ftell(f: FILE; VAR pos: OFF) : BOOLEAN;
   BEGIN
      WITH f^ DO
         IF NOT Tell(fd, pos) THEN RETURN FALSE END;
         IF NOT nobuf THEN
            IF reading IN flags THEN
	       DEC(pos, cnt);
            ELSE
	       INC(pos, index);
            END;
         END;
      END;
      RETURN TRUE
   END Ftell;

   PROCEDURE Ferror(f: FILE) : BOOLEAN;
   BEGIN
      RETURN error IN f^.flags
   END Ferror;

   PROCEDURE Feof(f: FILE) : BOOLEAN;
   BEGIN
      RETURN eof IN f^.flags
   END Feof;

   PROCEDURE FileNo(f: FILE) : CARDINAL;
   BEGIN
      RETURN f^.fd
   END FileNo;

   PROCEDURE Fgetc(VAR ch: CHAR; f: FILE) : BOOLEAN;
      VAR ok: BOOLEAN; bytecount: CARDINAL;
   BEGIN
      WITH f^ DO
         IF unget THEN
            ch := ungetc;
            unget := FALSE;
            RETURN TRUE
         END;
	 IF nobuf THEN
            bytecount := 1;
	    ok := Read(fd, ADR(ch), bytecount);
            IF bytecount = 0 THEN   (* eof ?? *)
               flags := flags + FlagsSet{eof};
               ok := FALSE;
               ch := 0C;
            END;
	    RETURN ok
	 ELSE
	    IF cnt = 0 THEN
	       ok := FillBuf(f);
	    ELSE
	       ok := TRUE;
	    END;
            IF ok THEN
               DEC(cnt);
               ch := buf[index];
               INC(index);
	       RETURN TRUE
	    ELSE
	       ch := 0C;
	       RETURN FALSE
            END;
	 END;
      END;
   END Fgetc;

   PROCEDURE Fputc(ch: CHAR; f: FILE) : BOOLEAN;
      VAR bytecount: CARDINAL;
   BEGIN
      WITH f^ DO
	 IF nobuf THEN
            bytecount := 1;
	    RETURN Write(fd, ADR(ch), bytecount);
	 ELSE
	    IF cnt = BufSiz THEN
	       IF NOT Fflush(f) THEN RETURN FALSE END;
	    END;
	    INC(cnt);
	    buf[index] := ch;
	    INC(index);
	    RETURN TRUE;
	 END;
      END;
   END Fputc;

   PROCEDURE Fungetc(ch: CHAR; f: FILE) : BOOLEAN;
   BEGIN
      WITH f^ DO
         IF nobuf THEN
            IF unget THEN RETURN FALSE END;
            ungetc := ch;
            unget := TRUE;
         ELSE
            IF cnt <> 0 THEN  (* if buffer isn't empty *)
               IF index > 0 THEN
                  DEC(index);
                  INC(cnt);
                  buf[index] := ch;
               ELSE
                  IF unget THEN RETURN FALSE END;
                  ungetc := ch;
                  unget := TRUE;
               END;
            ELSE
               buf[0] := ch;
               INC(cnt);
               index := 0;
            END;
         END;
      END;
      RETURN TRUE;
   END Fungetc;

BEGIN
   Init;
   EnterCleanup(Cleanup);
END StdIO.
