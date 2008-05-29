
all: force
	swig -python MyStrLib.i
	gcc -c -fPIC MyStrLib_wrap.c -I/usr/include/python2.4
	gm2 -fshared -c -fPIC -g MyStrLib.mod
	gm2 -fmakeall -I. -fPIC -g -shared -fshared MyStrLib.mod MyStrLib_wrap.o -o _MyStrLib.so
	python testlen.py

clean: force
	$(RM) -f *.o *.so a.out *.pyc *~ *_wrap* MyStrLib.py *.lst

force:
