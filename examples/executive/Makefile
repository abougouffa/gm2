# DEBUGGING=-g -gstabs
DEBUGGING=-g

all: testexecutive testtime

testexecutive: force KeyBoardLEDs.o
	gm2 -I.:../../gm2-libs-coroutines $(DEBUGGING) -Wmakeall testexecutive.mod -lpth
	mv a.out $@

testtime: force KeyBoardLEDs.o
	gm2 -I.:../../gm2-libs-coroutines $(DEBUGGING) -Wmakeall testtime.mod KeyBoardLEDs.o -lpth
	mv a.out $@

KeyBoardLEDs.o: force
	gcc -c -g KeyBoardLEDs.c -o $@

clean: force
	$(RM) *.o a.out testtime testexecutive

force:
