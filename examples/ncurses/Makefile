

all: force
	gm2 -g -Wlibs=pim-coroutine -Wmakeall testwin.mod -I../../gm2-libs-coroutines -lncurses -lpth

test: force
	gm2 -g -Wmakeall testmove.mod -lncurses

clean: force
	$(RM) *.o a.out

force:
