# ReqCheck

A flexible command-line program to determine which university courses are or will be available to you based on which courses you've already taken

## Building

ReqCheck is built using `dune`, the OCaml build tool. Make sure you have OCaml as well as dune installed.

```
$ opam install --deps-only .
$ dune build
$ ./_build/default/bin/main.exe
```
