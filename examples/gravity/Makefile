GM2FLAGS=-g -fPIC
PYTHON_VERSION=2.5

all: force
	gm2 $(GM2FLAGS) -c -I.:../../gm2-compiler ../../gm2-compiler/Indexing.mod
	gm2 $(GM2FLAGS) -fiso -c deviceGnuPic.mod
	gm2 $(GM2FLAGS) -fiso -c -fswig -I.:../../gm2-compiler twoDsim.mod
	swig -c++ -python twoDsim.i
	gcc -c -fPIC twoDsim_wrap.cxx -I/usr/include/python$(PYTHON_VERSION)
	gm2 -fiso -I. -fPIC -fshared -shared twoDsim.mod twoDsim_wrap.o -o _twoDsim.so
	python testsim.py

static: clean
	gm2 -g -c -I.:../../gm2-compiler ../../gm2-compiler/Indexing.mod
	gm2 -fiso -g -fmakeall -I.:../../gm2-compiler test.mod
	
clean: force
	$(RM) *.o a.out *.i *.pyc *_wrap.cxx *.so

force:
