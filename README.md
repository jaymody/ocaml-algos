# Data Structures and Algorithms in OCaml
Various data structure and algorithm implementations in OCaml, with a focus on using data structures that lend themselves well to a functional language.

I try to keep the implementations as simple as possible for educational purposes. As such, most of these implementations are basic/naive and do not take advantage of all possible optimizations. NOTE: I cannot guarantee the correctness of the implementations outside of the unit tests. Use at your own risk.

### Development
Dependencies:
```shell
opam switch create .
```

Build project:
```shell
dune build
```

Run tests:
```shell
dune runtest
```

Make sure you're running `eval $(opam env)` before running `dune` commands to make sure dune is pointing to the correct local version at `./_opam/bin/dune`.
