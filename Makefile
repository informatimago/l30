PROGRAMS=\
	l30_1_missing_number

all:$(PROGRAMS)

LINE=----------------------------------------
run-all:all
	@echo "$(LINE)"
	@for p in $(PROGRAMS) ; do echo "$$p" ; ./$$p ; echo "$(LINE)" ; done

clean:
	-rm -rf $(PROGRAMS) *.o *.dSYM

.PHONY: run-all
