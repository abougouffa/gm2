.\"
.\" Copyright (C) 2006, 2007, 2008, 2009, 2010
.\" Free Software Foundation, Inc.
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
.ds GCCVERSION 4.1.2
.ds LATESTCVSGM2 gcc-\*[GCCVERSION]+gm2-cvs-latest.tar.gz
.ds CURRENTDATE "Thu Jul  8 14:06:20 BST 2010
.ds CURRENTGM2 0.93
.ds DEBIANGM2 0.9.2
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
.   ft R
..
.de GM2URL
.   URL "http://floppsie.comp.glam.ac.uk/download/c/\\$1" \\$2 \\$3
..
.de PRG
.   nop \f[C]\\$1\f[P]\\$2
..
.de CMD
.   nop \f[C]$ \f[CB]\\$*\f[C]
..
.de RCMD
.   nop \f[C]# \f[CB]\\$*\f[C]
..
.de KEY
.   nop \f[CB]\\$*\f[R]
..
.de Q
.   sp
.   nop \m[darkblue]\c
..
.de A
.   br
.   nop \m[default]\c
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
The compiler has undergone quite a number of changes recently and the
current version in the CVS being worked upon is gm2-\*[CURRENTGM2].
The native x86_64 version is the most stable.
.PP
The GNU Modula-2 compiler is one of a number of
.URL http://gcc.gnu.org/frontends.html "front end languages"
to GCC (\c
.URL http://gcc.gnu.org "GNU Compiler Collection" ).
As such it has been designed to coexist with other GCC languages for
example it can be used in mixed language projects and it can catch C++
exceptions and throw exceptions which can be caught by C++.  Users can
also exploit conditional compilation and full gcc backend optimization
and architecture coverage.
.PP
GNU Modula-2 can produce position independant code and can easily
produce shared libraries from modules.  The compiler provides a swig
interface file generator option which allows scripting languages such
as Python to import modules written in Modula-2 and also catch
exceptions thrown by Modula-2.
.sp
Recent news:
\m[darkblue]
.sp
.ULS
.LI
gm2 0.92 was released on July 8 2010.
.LI
all regression tests pass on the LP64 platform.
.LI
the ISO Modula-2 language is now complete.
.LI
all of the ISO libraries have been completed.
.LI
many bug fixes have been applied to the ISO libraries and also the
compiler.
.LI
builds under OSX leopard and snow leopard using \f[I]macports\f[P]
(although there are regression test failures on these platforms).
.LI
now builds under Cygwin (however at present -O3 does not work on this platform
and also swig and Modula-2 have not been seen to work together yet).
.ULE
.
.SH
GNU Modula-2 Features
.LP
.ULS
.LI
the option
.OPT -fswig
will automatically create a swig interface file which corresponds to
the definition module of the file being compiled.
.LI
exception handling is compatible with C++ and swig.
Modula-2 code can be used with C or C++ code.
.LI
Python can call GNU Modula-2 modules via swig.
.LI
shared libraries can be built.
.LI
fixed sized types are now available from \f[C]SYSTEM\f[P].
.LI
support for dynamic \f[C]ARRAY\f[P]s has been added into \f[C]gdb\f[P].
.LI
variables can be declared at addresses.
.LI
much better \fCdwarf-2\fP debugging support and when used with
\fCgdb\fP the programmer can display \f[C]RECORD\f[P]s,
\f[C]ARRAY\f[P]s, \f[C]SET\f[P]s, subranges and constant char literals
in Modula-2 syntax.
.LI
it currently complies with Programming in Modula-2 Edition 2, 3, 4
and ISO Modula-2.
Users can switch on specific mutually exclusive features by using:
.OPT -fpim ,
.OPT -fpim2 ,
.OPT -fpim3 ,
.OPT -fpim4
or
.OPT -fiso .
.LI
supports sets of any ordinal size (memory permitting).
.LI
easy interface to C, and varargs can be passed to C routines.
.LI
University of Ulm libraries are installed together with the compiler
and can be accessed via:
.OPT -flibs=ulm
.LI
many Logitech libraries have been implemented and can be accessed via:
.OPT -flibs=logitech .
.LI
coroutines have been implemented in the PIM style and these are
accessible from SYSTEM. A number of supporting libraries (executive
and file descriptor mapping to interrupt vector libraries are
available through the
.OPT -flibs=pim-coroutines
switch).
.LI
can be built as a cross compiler (for embedded microprocessors
such as the AVR and the ARM).
.ULE
\mP
.LP
.
.SH
Licence
.LP
GNU Modula-2 is free software, the compiler is held under the
.URL http://www.gnu.org/licenses/gpl.txt GPL ,
its libraries (pim, iso, Logitech compatible) are under the
.URL http://www.gnu.org/licenses/lgpl.txt LGPL
whereas the Ulm libraries are under the GPL.  For more information on
how these licences work
.URL http://www.gnu.org/licenses/licenses.html "please see" .
.
.SH
Documentation
.LP
Currently some GNU Modula-2
.URL gm2.html documentation
documentation exists, albeit incomplete, but it does contain a fully
indexed list of all library procedures, a release map, and full
building and downloading instructions.
A 
.URL http://floppsie.comp.glam.ac.uk/download/m2/gm2.pdf (pdf)
version can be found
.URL http://floppsie.comp.glam.ac.uk/download/m2/gm2.pdf here.
The pdf version is built nightly.
.PP
A paper was presented at the GCC 2006 conference on the state of GNU Modula-2,
if you are interested a copy can be found
.URL http://floppsie.comp.glam.ac.uk/Papers/paper15/mulley-proc.pdf "here (pdf)" .
.SH
Regression tests for gm2-\*[CURRENTGM2] in the CVS
.LP
The result of the GNU Modula-2 regression tests for the current
CVS snapshot can be found
.URL http://floppsie.comp.glam.ac.uk/gm2-results/regression-tests.html here .
.sp
Other combinations almost certainly work, if you successfully
build gm2 on a different platform please email
.MTO "gaius@gnu.org" "me"
or the
.MTO "gm2@nongnu.org" "GNU Modula-2"
mailing list. You will need to subscribe to send email to
the mailing list.
.
.SH
Limitations
.LP
.ULS
.LI
The compiler is complete but it is still in an alpha state.  The most
stable build for GNU Modula-2 is x86_64 Debian.  At the time of
writing all regressions tests pass.  There are 1439 individual tests
and each test is compiled with varying options resulting in a total of
8634 test cases).
.LI
ISO library implementation is now very near completion, all but four
libraries are finished.  Only Processes, Semaphores, LowReal, LowLong
are missing.
.LI
Logitech compatibility library is incomplete.  For a comprehensive
list of completed modules please check the
.URL gm2.html documentation .
.ULE
.sp
.
.SH
Building GNU Modula-2
.LP
Rather than duplicate building instructions here, please see the
\f[B]Obtaining GNU Modula-2\f[P] section in the
.URL http://www.nongnu.org/gm2/gm2.html "gm2 texinfo documentation"
which matches the CVS.
.sp
.
.SH
GM2 development download via CVS
.LP
The latest sources of GNU Modula-2 can be obtained via cvs using the commands:
.sp
.nf
\fCcvs -z3 -d:pserver:anoncvs@cvs.sv.gnu.org:/sources/gm2 co gm2 testsuite\fP
.fi
.sp
The source code in the CVS can be
.URL http://cvs.savannah.nongnu.org/viewcvs/?root=gm2 "browsed here" .
.
.SH
GNU Modula-2 Debian binaries
.LP
GNU Modula-2 (\*[DEBIANGM2]) binaries are now available for i386 and
AMD64 architecture debian Lenny based distributions.  Add these lines
to your
.PRG /etc/apt/sources.list :
.sp
.CDS
#
#  GNU Modula-2 repo
#
deb http://floppsie.comp.glam.ac.uk/debian/ lenny main
deb-src http://floppsie.comp.glam.ac.uk/debian/ lenny main
.CDE
.sp
Then as root type:
.sp
.CDS
.RCMD apt-get update
.RCMD apt-get install gm2 gm2-doc
.CDE
.sp
You can obtain source to these binaries as a normal user by the following command:
.sp
.CDS
.CMD apt-get source gm2
.CDE
.
.SH
Creating a deb package for GNU Modula-2
.LP
If you are interested in building your own package for GNU Modula-2 then
you might consider this information useful.   There are probably other ways to
build Debian packages but this is the way the packages in the previous section
were built.
.ULS
.LI
firstly you need to create a pbuilder environment.  I used the excellent
.URL http://people.connexer.com/~roberto/howtos/debcustomize "tutorial found here" .
.LI
before building GNU Modula-2 you should update your pbuilder environment by:
.sp
.CDS
.CMD sudo pbuilder create
.CDE
.sp
.LI
now you can obtain the GNU Modula-2 source code.
.sp
.CDS
.CMD apt-get source gm2
.CDE
.sp
.LI
before creating the package it is advisable to revise the version number.
This is should be done semi automatically via the following commands:
.sp
.CDS
.CMD cd gm2-\\*[DEBIANGM2]
.CMD myrevdeb
.CDE
.sp
the script
.PRG myrevdeb
can be found
.URL http://floppsie.comp.glam.ac.uk/download/scripts/myrevdeb here .
.LI
if you just want to rebuild the rebuild the package with no
alterations you should skip this bullet point.  However if you wish to
introduce a later gcc/gm2 source tree into the package you need to
download the desired version from
.GM2URL "" "here"
and extact and move it into gm2-\*[DEBIANGM2].
Now you should remove the old source tree inside gm2-\*[DEBIANGM2]
and then you will have to edit
.PRG gm2-\\*[DEBIANGM2]/Makefile.in
and
.PRG gm2-\\*[DEBIANGM2]/debian/rules
and change the references to the previous source tree to your source new tree.
Once this is completed you must rebuild the
.PRG Makefile
by:
.sp
.CDS
.CMD cd gm2-\\*[DEBIANGM2]
.CMD ./configure
.CDE
.LI
finally the package can be built using the following script:
.sp
.CDS
.CMD cd gm2-\\*[DEBIANGM2]
.CMD mypdebbuild
.CDE
.sp
and this script can be found
.URL http://floppsie.comp.glam.ac.uk/download/scripts/mypdebbuild here .
Once this completes you should have a binary package and a
corresponding documentation package available in the directory
.PRG /usr/local/src/results .
.ULE
.
.SH
GM2 and GCC tarball download
.LP
At the time of writing: \*[CURRENTDATE]
gm2-\*[CURRENTGM2] is the latest release being worked on in the CVS
and this works when grafted onto gcc-\*[GCCVERSION].
You can download the latest CVS and patched gcc combination from
.GM2URL "\*[LATESTCVSGM2]" \*[LATESTCVSGM2] .
You can also browse for older releases
.GM2URL "" "here" .
.
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
.
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
.
.SH
FAQ
.LP
.Q
Why do you need to specify \f[C]--enable-languages=c,c++,gm2\f[P]?
.A
GNU Modula-2 uses the C++ compiler to compile the underlying main
function which consists of a try catch clause.  The try clause
calls each module in order and the catch clause calls the default
exception handler.  It needs the C compiler during the initial
bootstrap.
.Q
Why use the C++ exception mechanism in GCC, rather than a bespoke
Modula-2 mechanism?
.A
The C++ mechanism is tried and tested, it also provides GNU Modula-2
with the ability to link with C++ modules and via swig it can raise
Python exceptions.
.Q
Why not copy the C++ exception handing code into the Modula-2 runtime
thus saving the necessity to build the C++ compiler?
.A
The C++ compiler is needed to compile the underlying \f[C]main()\f[P]
function.
.ULS
.LI
there appear to be differing versions of the exception
handling code (we are using v3) and it would be expensive to maintain
the gm2 version and keep it in sync with the particular version of
gcc which is being used.
.LI
it would also mean a duplication of code and this would be frowned upon
when GNU Modula-2 is grafted into the main GCC tree.
.LI
there is talk about the gcc \f[C]libbackend.a\f[P] being reimplemented
in C++ and therefore in future releases it will be necessary to build
\f[C]-lstdc++\f[P] anyway.
.ULE
.
.Q
When will release 1.0 be available?
.A
Very soon :-) all of the ISO language is implemented and all of the
libraries are complete.  Many bugs have been fixed and all the
regression tests pass on the LP64.  Porting to the i386 platform and
MacOS platform is being used to hunt down bugs.
.
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
