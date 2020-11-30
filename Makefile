project_name = ReqCheck

opam_file = $(project_name).opam

.PHONY: install run

install:
	opam install --deps-only .

run:
	dune exec bin/main.exe
