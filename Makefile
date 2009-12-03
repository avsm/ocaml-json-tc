.PHONY: all test clean

all:
	@cd lib && $(MAKE)

test: all
	@cd lib_test && $(MAKE)

clean:
	@cd lib && $(MAKE) clean
	@cd lib_test && $(MAKE) clean
