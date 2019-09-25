(* GetPass.mod.

Copyright (C) 2004-2019 Free Software Foundation, Inc.
Contributed by University of Ulm.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)


(* Ulm's Modula-2 Library
   Copyright (C) 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991,
   1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003, 2004, 2005
   by University of Ulm, SAI, D-89069 Ulm, Germany
*)

IMPLEMENTATION MODULE GetPass;

   FROM SysTermIO IMPORT SetTermIO, GetTermIO, TermIO, ControlChar, Flag, Modes ;
   FROM SysOpen IMPORT Open;
   FROM FtdIO IMPORT FreadChar, FwriteChar, FwriteString, FwriteLn;
   FROM ASCII IMPORT nl, nak, bs, cr, bell;
   FROM RandomGenerator IMPORT Random;
   FROM StdIO IMPORT FILE, stdin, stdout, Fdopen, Fclose, read, write;

   PROCEDURE GetPass(prompt: ARRAY OF CHAR; VAR passwd: ARRAY OF CHAR);
      CONST
         MaxPasswdLen = 14;
         Terminal = "/dev/tty";
         Read = 0;
         Write = 1;
      VAR
         index: CARDINAL; (* in passwd *)
         oldterm, term: TermIO;
         ch: CHAR; (* last character read *)
         bslen: ARRAY[0..MaxPasswdLen-1] OF CARDINAL;
         i: CARDINAL;
         low, high: CARDINAL;
         termin, termout: FILE;
         termfdin: CARDINAL; (* file descriptor of terminal input *)
         termfdout: CARDINAL;
         terminal: BOOLEAN;

      PROCEDURE Isatty(fd: CARDINAL) : BOOLEAN;
	 VAR
	    termio: TermIO;
      BEGIN
	 RETURN GetTermIO(fd, termio)
      END Isatty;

   BEGIN (* GetPass *)
      passwd[0] := 0C;
      terminal := Isatty(0) AND Isatty(1);
      IF terminal THEN
         termin := stdin; termout := stdout;
         termfdin := 0; termfdout := 1;
      ELSIF NOT Open(termfdin, Terminal, Read) OR
            NOT Open(termfdout, Terminal, Write) OR
            NOT Fdopen(termin, termfdin, read, (* buffered = *) FALSE) OR
            NOT Fdopen(termout, termfdout, write, (* buffered = *) FALSE) THEN
         RETURN
      END;
      FwriteString(termout, prompt);
      IF ~GetTermIO(termfdin, term) THEN RETURN END;
      oldterm := term;
      WITH term DO
	 cc[vmin] := 1C; cc[vtime] := 1C;
         modes := modes + Modes{ignpar, istrip} ;
         modes := modes - Modes{lecho, licanon, iparmrk, inpck, ignpar, istrip, icrnl, opost}
      END;
      IF ~SetTermIO(termfdin, term) THEN RETURN END;
      index := 0;
      REPEAT
         FreadChar(termin, ch);
         CASE ch OF
         | cr, nl: FwriteChar(termout, cr); FwriteChar(termout, nl);
         | bs:
             IF index > 0 THEN
                DEC(index);
                FOR i := 1 TO bslen[index] DO
                   FwriteChar(termout, bs);
                   FwriteChar(termout, " ");
                   FwriteChar(termout, bs);
                END;
             END;
         | nak: (* ^U *)
             WHILE index > 0 DO
                DEC(index);
                FOR i := 1 TO bslen[index] DO
                   FwriteChar(termout, bs);
                   FwriteChar(termout, " ");
                   FwriteChar(termout, bs);
                END;
             END;
         ELSE
	     IF (index <= HIGH(passwd)) & (index <= HIGH(bslen)) &
	           (index <= MaxPasswdLen) THEN
		passwd[index] := ch;
		bslen[index] := Random(1, 3);
		low := ORD('!'); high := ORD('~');
		FOR i := 1 TO bslen[index] DO
		   FwriteChar(termout, CHR(Random(low, high)));
		END;
		INC(index);
	     ELSE
	        FwriteChar(termout, bell);
	     END;
         END;
      UNTIL (ch = nl) OR (ch = cr);
      IF index <= HIGH(passwd) THEN
         passwd[index] := 0C;
      END;
      IF ~SetTermIO(termfdin, oldterm) THEN END;
      IF NOT terminal AND Fclose(termin) AND Fclose(termout) THEN END;
   END GetPass;

END GetPass.
