EMACS ?= emacs
CASK ?= cask

all: compile

test: clean
	${MAKE} all
	${MAKE} unit
unit:
	${CASK} exec ert-runner

clean:
	$(RM) *.elc

compile:
	${CASK} exec  ${EMACS} -Q -batch -f batch-byte-compile ox-remark.el

.PHONY: all test unit compile
