# DEBUGGING=-g -gstabs
DEBUGGING=-g

all: testexecutive testtime winexec

testexecutive: force KeyBoardLEDs.o
	cd ../../gm2-libs-coroutines ; make clean ; make
	gm2 -I.:../../gm2-libs-coroutines $(DEBUGGING) -Wmakeall testexecutive.mod -lpth
	mv a.out $@

testtime: force KeyBoardLEDs.o
	cd ../../gm2-libs-coroutines ; make clean ; make
	gm2 -I.:../../gm2-libs-coroutines $(DEBUGGING) -Wmakeall testtime.mod KeyBoardLEDs.o -lpth
	mv a.out $@

winexec: force
	( cd ../../gm2-libs-coroutines ; make clean ; make )
	( cd ../ncurses ; make clean ; make all )
	gm2 -I.:../../gm2-libs-coroutines:../ncurses $(DEBUGGING) -Wmakeall winexec.mod -lncurses -lpth
	mv a.out $@

KeyBoardLEDs.o: force
	gcc -c -g KeyBoardLEDs.c -o $@

clean: force
	$(RM) *.o a.out testtime testexecutive winexec

force:
