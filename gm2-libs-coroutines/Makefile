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

all: testtransfer testiotransfer 

testtransfer: force
	gm2 -g -Wmakeall -I.:../gm2-libs $(DEBUGGING) testtransfer.mod -lpth
	@if ./a.out ; then \
            echo "transfer ok" ; \
         else \
            echo "transfer failed" ; \
            exit 1 ; \
         fi

testiotransfer: force
	gm2 -g -Wmakeall -I.:../gm2-libs $(DEBUGGING) testiotransfer.mod -lpth
	@if ./a.out ; then \
            echo "iotransfer ok" ; \
         else \
            echo "io transfer failed" ; \
            exit 1 ; \
         fi

clean: force
	$(RM) *.o a.out testiotransfer testtransfer

force:
