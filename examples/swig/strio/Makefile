
all: strio

strio: force
	gm2 -fshared -I. -c -fPIC -g -fswig -I../../../gm2-libs ../../../gm2-libs/StrIO.mod
	swig -c++ -python StrIO.i
	gcc -c -fPIC StrIO_wrap.cxx -I/usr/include/python2.4
	gm2 -shared -fshared -fPIC ../../../gm2-libs/StrIO.mod StrIO_wrap.o -o _StrIO.so
	python teststrio.py

clean: force
	$(RM) -f *.o *.so a.out *.pyc *~ *_wrap* StrIO.py *.i

force:
