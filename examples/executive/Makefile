# DEBUGGING=-g -gstabs
DEBUGGING=-g

all: testexecutive testtime

testexecutive: force KeyBoardLEDs.o
	cd ../../gm2-libs-coroutines ; make clean ; make
	gm2 -v -I.:../../gm2-libs-coroutines $(DEBUGGING) -Wmakeall testexecutive.mod -lpth
	mv a.out $@

testtime: force KeyBoardLEDs.o
	cd ../../gm2-libs-coroutines ; make clean ; make
	gm2 -I.:../../gm2-libs-coroutines $(DEBUGGING) -Wmakeall testtime.mod KeyBoardLEDs.o -lpth
	mv a.out $@

testwin: force
	make clean
	( cd ../../gm2-libs-coroutines ; make clean ; make )
	( cd ../ncurses ; make clean ; make testwin )
	gm2 $(DEBUGGING) -Wmakeall -I.:../../gm2-libs-coroutines:../ncurses testwin.mod -lncurses -lpth

KeyBoardLEDs.o: force
	gcc -c -g KeyBoardLEDs.c -o $@

clean: force
	$(RM) *.o a.out testtime testexecutive

force:
