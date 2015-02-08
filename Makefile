EMACS ?= emacs
CASK ?= cask
WGET ?= wget

all: compile

README.md: make-readme-markdown.el
	${EMACS} --script $< <ox-remark.el>$@ 2>/dev/null
make-readme-markdown.el:
	wget -q -O $@ https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el
.INTERMEDIATE: make-readme-markdown.el

test: clean
	${MAKE} all
	${MAKE} unit
unit:
	${CASK} exec ert-runner

clean:
	$(RM) *.elc

compile:
	${CASK} exec  ${EMACS} -Q -batch -f batch-byte-compile ox-remark.el

update-js:
	mkdir -p js
	wget http://gnab.github.io/remark/downloads/remark-latest.min.js -O js/remark.min.js

.PHONY: all test unit compile clean README.md update-js
