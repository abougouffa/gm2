
all: force
	swig -python StrLib.i
	gcc -c -fPIC StrLib_wrap.c -I/usr/include/python2.4
	gm2 -fsources -I. -c -fPIC -g StrLib.mod
	gcc -shared StrLib.o StrLib_wrap.o -o _StrLib.so
	python testlen.py

clean: force
	$(RM) -f *.o *.so a.out *.pyc *~ *_wrap* StrLib.py

force:
