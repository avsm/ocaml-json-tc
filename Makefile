.PHONY: all test clean

SUDO ?= sudo

all:
	@cd lib && $(MAKE)

test: all
	@cd lib_test && $(MAKE)

clean:
	@cd lib && $(MAKE) clean
	@cd lib_test && $(MAKE) clean

install:
	@cd lib && $(SUDO) $(MAKE) install

uninstall:
	@cd lib && $(SUDO) $(MAKE) uninstall

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install
