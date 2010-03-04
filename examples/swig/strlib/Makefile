PYTHON_VERSION=2.5

all: force
	gm2 -fshared -fsources -I. -c -fPIC -g -fswig MyStrLib.mod
	swig -c++ -python MyStrLib.i
	gcc -c -fPIC MyStrLib_wrap.cxx -I/usr/include/python$(PYTHON_VERSION)
	gm2 -fshared -c -fPIC -g -fmakelist MyStrLib.mod
	gm2 -fuselist -fobject-path=. -fPIC -g -shared -fshared MyStrLib.mod MyStrLib_wrap.o -o _MyStrLib.so
	python testlen.py

clean: force
	$(RM) -f *.o *.so a.out *.pyc *~ *_wrap* MyStrLib.py *.i *.lst

force:
