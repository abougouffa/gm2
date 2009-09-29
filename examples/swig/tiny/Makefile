PYTHON_VERSION=2.5

all: force
	gm2 -fshared -fswig -I. -c -fPIC -g -fswig tiny.mod
	swig -c++ -python tiny.i
	gcc -c -fPIC tiny_wrap.cxx -I/usr/include/python$(PYTHON_VERSION)
	gm2 -fshared -c -fPIC -g -fmakelist tiny.mod
	gm2 -fuselist -fobject-path=. -fPIC -g -shared -fshared tiny.mod tiny_wrap.o -o _tiny.so
	python testtiny.py

clean: force
	$(RM) -f *.o *.so a.out *.pyc *~ *_wrap* tiny.py *.i *.lst *_m2.cpp

force:
