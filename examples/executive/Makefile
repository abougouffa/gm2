# Copyright (C) 2005 Free Software Foundation, Inc.
# This file is part of GNU Modula-2.
#
# GNU Modula-2 is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 2, or (at your option) any later
# version.
# 
# GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License along
# with gm2; see the file COPYING.  If not, write to the Free Software
# Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

# DEBUGGING=-g -gstabs
DEBUGGING=-g

all: testexecutive testtime winexec

testexecutive: force
	gm2 $(DEBUGGING) -Wmakeall -Wlibs=pim-coroutine testexecutive.mod -lpth
	mv a.out $@

testtime: force
	gm2 $(DEBUGGING) -Wmakeall -Wlibs=pim-coroutine testtime.mod -lpth
	mv a.out $@

winexec: force
	( cd ../ncurses ; make clean ; make all )
	gm2 -I.:../ncurses $(DEBUGGING) -Wmakeall -Wlibs=pim-coroutine winexec.mod -lncurses -lpth
	mv a.out $@

clean: force
	$(RM) *.o a.out testtime testexecutive winexec

force:
