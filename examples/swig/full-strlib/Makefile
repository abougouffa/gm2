PYTHON_VERSION=2.5

all: strlib numberio

strlib: force
	gm2 -fshared -I. -c -fPIC -g -fswig -I../../../gm2-libs ../../../gm2-libs/StrLib.mod
	swig -c++ -python StrLib.i
	gcc -c -fPIC StrLib_wrap.cxx -I/usr/include/python$(PYTHON_VERSION)
	gm2 -fPIC -fshared -shared ../../../gm2-libs/StrLib.mod StrLib_wrap.o -o _StrLib.so
	python testlen.py
	python testequ.py
	python testcopy.py

numberio: force
	gm2 -fshared -I. -c -fPIC -g -fswig -I../../../gm2-libs ../../../gm2-libs/NumberIO.mod
	swig -c++ -python NumberIO.i
	gcc -c -fPIC NumberIO_wrap.cxx -I/usr/include/python$(PYTHON_VERSION)
	gm2 -shared -fshared -fPIC ../../../gm2-libs/NumberIO.mod NumberIO_wrap.o -o _NumberIO.so
	python testnum.py

clean: force
	$(RM) -f *.o *.so a.out *.pyc *~ *_wrap* StrLib.py NumberIO.py *.i *_m2.cpp

force:
