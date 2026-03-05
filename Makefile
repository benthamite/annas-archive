EMACS ?= emacs

.PHONY: test test-batch compile clean

test: test-batch

test-batch:
	$(EMACS) -Q --batch \
	  -L . \
	  -l annas-archive.el \
	  -l test/annas-archive-test.el \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch -L . --eval '(byte-compile-file "annas-archive.el")'

clean:
	rm -f *.elc
