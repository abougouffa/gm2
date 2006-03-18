.\"
.\" Copyright (C) 2006 Free Software Foundation, Inc.
.\" 
.\" Permission is granted to copy, distribute and/or modify this document
.\" under the terms of the GNU Free Documentation License, Version 1.2 or
.\" any later version published by the Free Software Foundation; with no
.\" Invariant Sections, with the Front-Cover texts being `A GNU Manual,''
.\" and with the Back-Cover Texts as in (a) below.  A copy of the
.\" license is included in the section entitled `GNU Free Documentation
.\" License.''
.\" 
.\" (a) The FSF's Back-Cover Text is: `You have freedom to copy and modify
.\" this GNU Manual, like GNU software.  Copies published by the Free
.\" Software Foundation raise funds for GNU development.''
.\" 
.ds GCCVERSION 3.3.6
.ds GM2HARNESS 0.7
.ds LATESTGM2 gcc-\*[GCCVERSION]+gm2-20051118.tar.gz
.ds CURRENTDATE "Mon Feb 13 11:41:33 GMT 2006
.ds CURRENTGM2 0.50
.HEAD "<link rel=""stylesheet"" type=""text/css"" href=""gm2.css"" />"
.HEAD "<link rel=""icon"" type=""image/png"" href=""gm2-logo.png"" />"
.de OPT
.   nop \fC\\$1\fP\\$2
..
.de CDS
.   ft C
.   nf
..
.de CDE
.   fi
.   ft P
..
.de GM2URL
.   URL "http://floppsie.comp.glam.ac.uk/download/c/\\$1" \\$2 \\$3
..
.LP
.NHR
.defcolor darkred rgb 0.65f 0.1f 0.2f
.defcolor darkgreen rgb 0.1f 0.4f 0.2f
.defcolor darkblue rgb 0.1f 0.2f 0.6f
.if '\*(.T'html' .nr LL 8i
.ds GNU \m[darkred]G\mP\m[darkgreen]N\mP\m[darkblue]U\mP
.nr LL 8i
.HTL
GNU Modula-2
.LP
.HTML-IMAGE-LEFT
.PSPIC -L gnubw.eps 1.5i 1.5i
\v'-0.75i'\h'1.5i'
\s[60]\*[GNU] Modula-2\s0
.HTML-IMAGE-END
.nr LL 6i
.\" .SH
.\" GNU Modula-2 status
.LNS
The home of the GNU Modula-2 compiler.
This compiler is still in development.
.LP
The compiler has undergone quite a number of changes recently
and the current release is gm2-\*[CURRENTGM2].
It can build all of the University of Ulm libraries.
It has the following characteristics:
.sp
\m[darkblue]
.sp
.ULS
.LI
gm2-harness 0.7 has been released which will download, patch and build
\fCgcc\fP, \fCgdb\fP, \fCgm2\fP with the SET and TYPE enhancements below.
It also honours the \fC--prefix=\fP option.
.LI
much better \fCdwarf-2\fP debugging support and when used with
a patched \fCgdb-6.3\fP the programmer can display \f[C]RECORD\f[P]s,
\f[C]ARRAY\f[P]s, \f[C]SET\f[P]s, subranges and constant char literals
in Modula-2 syntax.
.LI
it currently complies with Programming in Modula-2 Edition 2, 3 and 4.
Users can switch on specific mutually exclusive features by
.OPT -Wpim
or
.OPT -Wpim2 .
.LI
supports sets of any ordinal size (memory permitting).
.LI
implements ISO Modula-2 SYSTEM which can be switched on via:
.OPT -Wiso .
Several ISO features are implemented and it is the goal that eventually
GNU Modula-2 will be fully ISO complient (see the release map for details in the
documentation).
.LI
easy interface to C, and varargs can be passed to C routines.
.LI
University of Ulm libraries are installed together with the compiler
and can be accessed via:
.OPT -Wlibs=ulm
.LI
some Logitech libraries have been implemented and can be accessed via:
.OPT -Wlibs=logitech
.LI
coroutines have been implemented in the PIM style and these are accessible
from SYSTEM. A number of supporting libraries (executive and file descriptor
mapping to interrupt vector libraries are available through the
.OPT -Wlibs=pim-coroutines
switch).
.ULE
\mP
.LP
.SH
Licence
.LP
GNU Modula-2 is free software, the compiler is held under the
.URL http://www.gnu.org/licenses/gpl.txt GPL ,
its libraries (pim, iso, Logitech compatible) are under the
.URL http://www.gnu.org/licenses/lgpl.txt LGPL
whereas the Ulm libraries are under the GPL.
For more information on how these licences work
.URL http://www.gnu.org/licenses/licenses.html "please see" .
.SH
Documentation
.LP
Currently some GNU Modula-2
.URL gm2.html documentation
documentation exists, albeit incomplete,
but it does contain a fully indexed list of all library procedures,
a release map, and full building and downloading instructions.
.SH
Successful \*[CURRENTGM2] builds
.LP
I have deliberately started a new table here to verify \*[CURRENTGM2].
The test used to determine a successful build is the \fCmake gm2.paranoid\fP
which should be issued inside the \fCbuild/gcc\fP directory. So for example
suppose you had used the gm2-harness-\*[GM2HARNESS].tar.gz package, then you could:
.sp
.CDS
cd gm2-harness-\*[GM2HARNESS]/build/gcc/
make gm2.paranoid
.CDE
.sp
and then wait and see whether it completes successfully.
.sp
.CDS
Operating       | Architecture   | gcc and gm2   (or)
System          | Host           | gcc + latest cvs
================+================+=====================
Debian Pure 64  | Opteron 64bit  | gcc-3.3.6 + gm2-0.50
----------------+----------------+---------------------
Debian Sarge    | 686            | gcc-3.3.6 + gm2-0.50
----------------+----------------+---------------------
Apple Darwin    | ppc G5         | gcc-3.3.6 + gm2-0.50
----------------+----------------+---------------------
Solaris 10      | Sparc 32 bit   | gcc-3.3.6 + gm2-0.50
----------------+----------------+---------------------
Solaris 10      | Sparc 64 bit   | gcc-3.3.6 + gm2-0.50
----------------+----------------+---------------------
Slackware 10.2  | AMD-K6         | gcc-3.3.6 + gm2-0.50
----------------+----------------+---------------------
Source Mage     | AMD Athlon XP  | gcc-3.3.6 + gm2-0.50
GNU/Linux       | 2800+          |
----------------+----------------+---------------------
MacOSX-10.3.9   | G4 eMac        | gcc-3.3.6 + gm2-0.50
Panther         |                | 3 regressions fail
----------------+----------------+---------------------\fP
.CDE
.sp
Other combinations almost certainly work, if you successfully
build gm2 on a different platform please email
.MTO "gaius@gnu.org" "me"
or the
.MTO "gm2@nongnu.org" "GNU Modula-2"
mailing list. You will need to subscribe to send email to
the mailing list.
.SH
Older builds
.LP
Prior to the release of GNU Modula-2 0.50 this table
was constructed.
.CDS
Operating       | Architecture   | gcc and gm2   (or)
System          | Host           | gcc + latest cvs
================+================+=====================
Debian Pure 64  | Opteron 64bit  | gcc-3.3.6 + gm2-0.49
----------------+----------------+---------------------
Debian Unstable | Athlon 32bit   | gcc-3.3.6 + gm2-0.49
----------------+----------------+---------------------
FreeBSD 5.3     | Pentium III    | gcc-3.3.6 + cvs
----------------+----------------+---------------------
Slackware 10    | 32bit x86      | gcc-3.3.6 + gm2-0.49
----------------+----------------+---------------------
Solaris 10      | Sparc          | gcc-3.3.6 + gm2-0.49
                | i32lp64        | [nb. ilp32 fails]
----------------+----------------+---------------------
Suse 9.2        | 64bit Athlon 64| gcc-3.3.6 + gm2-0.49
                |                | 22Jul05
----------------+----------------+---------------------
Suse 9.3        | 32bit          | gcc-3.3.6 + gm2-0.49
                | 64bit Athlon 64| 22Jul05
----------------+----------------+---------------------
FEDORA Core 3   | 32 bit         | gcc-3.3.6 + gm2-0.49
                |                | 22Jul05 build and
                |                | paranoid pass but
                |                | regressions fail
----------------+----------------+---------------------
FEDORA Core 4   | 32 and 64 bit  | build fails
----------------+----------------+---------------------
Tru64 v5.1      | Alpha (64 bit) | gcc-3.3.6 + gm2
                |                | 16Jan06
----------------+----------------+---------------------
FreeBSD 4.11    | i386           | gcc-3.3.6 + gm2
                |                | 11Jan06
----------------+----------------+---------------------
GNU/Linux stable| Itanium 64 bit | gcc-3.3.6 + gm2
                |                | 16Jan06
----------------+----------------+---------------------
Apple Darwin    | ppc G4 and G5  | gcc-3.3.6 + gm2
                |                | 18Jan06
----------------+----------------+---------------------\fP
.CDE
.SH
Limitations
.LP
.ULS
.LI
ISO Modula-2 is not implemented in full yet.
.LI
ISO library implementation has started but is not useable yet.
The module SYSTEM has been completely implemented.
.LI
Full PIM Modula-2 has been implemented with the exception that
(currently) variables cannot be declared at particular addresses.
.LI
Logitech compatibility library is incomplete (FileSystem.mod has not
yet been implemented, nevertheless a number of common libraries have
been implemented: Terminal, Display, InOut, RealInOut and others.
.ULE
.SH
Building GNU Modula-2
.LP
Currently GNU Modula-2 is grafted onto the GCC \*[GCCVERSION] source tree.
.ULS
.LI
The easiest way to build gm2 is to download the build harness
.GM2URL gm2-harness-\*[GM2HARNESS].tar.gz gm2-harness-\*[GM2HARNESS].tar.gz .
.LI
.nf
\fC
tar zxf gm2-harness-\*[GM2HARNESS].tar.gz
cd gm2-harness-\*[GM2HARNESS]
\&./configure --with-gdb --prefix=$HOME/opt
make
make install
\fP
.fi
.LI
This simple package, downloads the latest gm2 from CVS, the
latest supported gcc, gdb and the testsuite. It puts all source
components in the correct place and proceeds to build
gdb, gm2 and run the regression tests.
.ULE
.sp
you can do manually graft the gm2 source tree onto
gcc by following these instructions:
.ULS
.LI
firstly you need to download gcc-\*[GCCVERSION].tar.gz
.LI
then the gm2 directory (as obtained via the CVS)
needs to be grafted onto the gcc source tree, configured and built.
.sp
.nf
\fC
tar zxf gcc-\*[GCCVERSION].tar.gz
mv gm2 gcc-\*[GCCVERSION]/gcc
cd gcc-\*[GCCVERSION]
for i in gcc/gm2/patches/gcc/3.3.6/* ; do
   patch -p1 < $i
done
mkdir host-build
cd host-build
\&../gcc-\*[GCCVERSION]/configure --enable-languages=c,gm2 --prefix=$HOME/opt
make
make install
\fP
.fi
.ULE
.SH
GM2 development download via CVS
.LP
The sources of GNU Modula-2 can be obtained via cvs using the commands:
.sp
.nf
\fCcvs -z3 -d:pserver:anoncvs@cvs.sv.gnu.org:/sources/gm2 co gm2 testsuite\fP
.fi
.LP
The source code in the CVS can be
.URL http://cvs.savannah.nongnu.org/viewcvs/?root=gm2 "browsed here" .
.SH
GNU Modula-2 binaries
.LP
Can be found
.URL http://freepages.modula2.org/downl.html here .
There are also some useful building tips
.URL http://freepages.modula2.org/gnum2.html described .
Many thanks to Christoph for making these available.
.SH
GM2 and GCC tarball download
.LP
At the time of writing: \*[CURRENTDATE]
gm2-\*[CURRENTGM2] is the latest release and this works
when grafted onto gcc-\*[GCCVERSION].
You can download a combined gcc+gm2 and patched tarball from
.GM2URL "\*[LATESTGM2]" \*[LATESTGM2] .
.SH
Contributing code
.LP
Please do. But also please read the GNU Emacs info under
.sp
.CDS
* Standards: (standards).  GNU coding standards.
* Intellectual Property::  Keeping Free Software Free
* Reading Non-Free Code::  Referring to Proprietary Programs
* Contributions::          Accepting Contributions
.CDE
.sp
before you start coding.
.SH
Objectives
.LP
Here follows the objectives of GNU Modula-2 note that some of these
have already been achieved and they are listed in no particular order.
.ULS
.LI
The intention of GNU Modula-2 is to provide a production modula-2
front end to GCC. Initially GNU Modula-2 will comply with the
PIM-[234] dialect of M2, eventually it will fully compliant with ISO
Modula-2.
.LI
It is an aim to provide PIM library compatibility.
.LI
ISO Modula-2 library support is likely to occur before the compiler is fully
ISO compliant.
.LI
Compatibility libraries. Topspeed, Logitech, Ulm are also required.
.LI
There should be an easy interface to C.
.LI
To fold the gm2 source code into the gcc tree at a convenient time in
the future.
.LI
Exploit the features of GCC.
.LI
Listen to the requests of the users.
.ULE
.SH
Mailing list
.LP
You can subscribe to the GNU Modula-2 mailing by sending an
email to:
.MTO "gm2-subscribe@nongnu.org"
or by 
.URL "http://lists.nongnu.org/mailman/listinfo/gm2" "clicking here" .
The mailing list contents can be
.URL "http://lists.gnu.org/archive/html/gm2" "viewed here" .
.SH
Other languages for GCC
.LP
These exist and can be found on the
.URL "http://gcc.gnu.org/frontends.html" "gcc web site" .
.LNE
\fI\s-2This document was produced using
.     URL http://groff.ffii.org/ groff-\n[.x].\n[.y] .\s0\fP
.HR
