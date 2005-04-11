# DEBUGGING=-g -gstabs
DEBUGGING=-g

all: testtransfer testiotransfer 

testtransfer: force
	gm2 -g -Wmakeall -I. $(DEBUGGING) testtransfer.mod -lpth
	@if ./a.out ; then \
            echo "transfer ok" ; \
         else \
            echo "transfer failed" ; \
            exit 1 ; \
         fi

testiotransfer: force
	gm2 -g -Wmakeall -I. $(DEBUGGING) testiotransfer.mod -lpth
	@if ./a.out ; then \
            echo "iotransfer ok" ; \
         else \
            echo "io transfer failed" ; \
            exit 1 ; \
         fi

clean: force
	$(RM) *.o a.out testiotransfer testtransfer

force:
