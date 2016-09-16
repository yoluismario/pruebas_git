CC=mpiicc
CFLAGS=-Wall -Wextra -O2 -mmic -openmp -mkl=parallel

TARGETS=proctest

proctest: proctest.o
	$(CC) $(CFLAGS) -o $@ $<

.PHONY: clean

clean:
	rm -f *.o $(TARGETS)

