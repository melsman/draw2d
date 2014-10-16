all:
	make -C test

simple:
	make -C test simple

.PHONY: clean
clean:
	make -C test clean
	rm -rf *~ MLB
