.PHONY: all test clean


build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

zip:
	rm -f studentcenter.zip
	zip -r studentcenter.zip . -x@exclude.lst

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

serve:
	OCAMLRUNPARAM=b dune exec server/server.exe

doc:
	dune build @doc
	open _build/default/_doc/_html/ClassDB/Final_project_3110/index.html


opendoc: doc
	@bash opendoc.sh

	