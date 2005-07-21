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
