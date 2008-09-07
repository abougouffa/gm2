
all: force
	gm2 -g -c m2.mod
	g++ -g -c cpp.cpp
	gm2 -g m2.mod cpp.o
	./a.out

clean: force
	$(RM) a.out *.o

force:
