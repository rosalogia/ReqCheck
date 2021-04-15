project_name = ReqCheck

opam_file = $(project_name).opam

.PHONY: install run

install:
	opam install --deps-only .
	# For installing the latest version of opium
	opam pin add rock.~dev https://github.com/rgrinberg/opium.git
	opam pin add opium.~dev https://github.com/rgrinberg/opium.git

run:
	dune exec bin/main.exe
