PYTHON_VERSION=2.5

all: tiny

tiny: force
	gm2 -fshared -I. -c -fPIC -fswig -g -I../../../gm2-libs tiny.mod
	swig -c++ -python tiny.i
	gcc -c -fPIC tiny_wrap.cxx -I/usr/include/python$(PYTHON_VERSION)
	gm2 -fPIC -fshared -shared tiny.mod tiny_wrap.o -o _tiny.so
	python testtiny.py
	python stresstiny.py

clean: force
	$(RM) -f *.o *.so a.out *.pyc *~ *_wrap* tiny.py tiny.i *_m2.cpp

force:
