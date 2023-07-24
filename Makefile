.PHONEY: all build test clean

emacs ?= emacs

all: test

build: clean
	"$(emacs)" -Q --batch -L . -f batch-byte-compile elsewhere.el

test: build
	"$(emacs)" -Q --batch -L . -l ert -l elsewhere-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f elsewhere.elc
