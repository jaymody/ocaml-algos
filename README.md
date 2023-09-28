# Data Structures and Algorithms in OCaml
Various data structure and algorithm implementations in OCaml, with a focus on using data structures that lend themselves well to a functional language.

### Development
Dependencies:
```shell
opam switch create . -w --with-test
```

Build project:
```shell
dune build
```

Run tests:
```shell
dune runtest
```

For testing/debugging, it's useful to look at outputs directly using `utop`:
```shell
dune utop
```

Make sure you're running `eval $(opam env)` before running `dune` to make sure you're using the local switches dune binary and not some other dune binary.
