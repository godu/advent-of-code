.PHONY: default run build test clean

default: build

run:
	dune exec bin/main.exe

build:
	dune build bin/main.exe

test:
	dune runtest

test-watch:
	dune runtest -w

clean:
	dune clean
