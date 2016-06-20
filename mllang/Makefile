PREFIX ?= /usr/local
BIN_DIR = $(PREFIX)/bin
LIB_DIR = $(PREFIX)/lib/python2.7/site-packages
# $PREFIX ?= /usr/local
# $BIN_DIR = $(PREFIX)/bin
# $LIB_DIR = /Library/Python/2.7/site-packages

INSTALL_TARGETS = install-dir install-package clean

install: $(INSTALL_TARGETS)

install-dir:
	sudo install -d $(PREFIX) $(BIN_DIR) $(LIB_DIR)

install-package: install-dir clean
	#PYTHONPATH=$(LIB_DIR) python setup.py install --prefix=$(PREFIX)
	@sudo python setup.py install
	@echo 'MLScheme successfully installed'

clean:
	@find . -name \*.pyc -delete
	@sudo rm -rf build dist mlscheme.egg-info
