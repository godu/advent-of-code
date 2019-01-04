.PHONY: default run build test clean

default: install build

run:
	dune exec bin/main.exe

install:
	opam install .

build:
	dune build bin/main.exe

test:
	dune runtest

test-watch:
	dune runtest -w

clean:
	dune clean

lint:
	dune promote
