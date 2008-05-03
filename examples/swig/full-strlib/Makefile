
all: strlib numberio

strlib: force
	gm2 -fsources -I. -c -fPIC -g -fswig -I../../../gm2-libs \
            ../../../gm2-libs/StrLib.mod
	swig -python StrLib.i
	gcc -c -fPIC StrLib_wrap.c -I/usr/include/python2.4
	gcc -shared StrLib.o StrLib_wrap.o -o _StrLib.so
	python testlen.py
	python testequ.py
	python testcopy.py

numberio: force
	gm2 -fsources -I. -c -fPIC -g -I../../../gm2-libs \
            ../../../gm2-libs/M2RTS.mod
	gm2 -fsources -I. -c -fPIC -g -I../../../gm2-libs \
            ../../../gm2-libs/RTExceptions.mod
	gm2 -fsources -I. -c -fPIC -g -I../../../gm2-libs \
            ../../../gm2-libs/StrIO.mod
	gm2 -fsources -I. -c -fPIC -g -I../../../gm2-libs \
            ../../../gm2-libs/StdIO.mod
	gm2 -fsources -I. -c -fPIC -g -I../../../gm2-libs \
            ../../../gm2-libs/IO.mod
	gm2 -fsources -I. -c -fPIC -g -I../../../gm2-libs \
            ../../../gm2-libs/FIO.mod
	gm2 -fsources -I. -c -fPIC -g -I../../../gm2-libs \
            ../../../gm2-libs/Storage.mod
	gm2 -fsources -I. -c -fPIC -g -I../../../gm2-libs \
            ../../../gm2-libs/SysStorage.mod
	gm2 -fsources -I. -c -fPIC -g -I../../../gm2-libs \
            ../../../gm2-libs/Debug.mod
	gm2 -fsources -I. -c -fPIC -g -fswig -I../../../gm2-libs \
            ../../../gm2-libs/NumberIO.mod
	swig -python NumberIO.i
	gcc -c -fPIC NumberIO_wrap.c -I/usr/include/python2.4
	gcc -shared NumberIO.o NumberIO_wrap.o StrLib.o FIO.o IO.o Debug.o StrIO.o SysStorage.o Storage.o StdIO.o RTExceptions.o M2RTS.o -o _NumberIO.so
	python testnum.py

clean: force
	$(RM) -f *.o *.so a.out *.pyc *~ *_wrap* StrLib.py NumberIO.py *.i

force:
