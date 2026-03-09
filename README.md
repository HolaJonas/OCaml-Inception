# OCaml-Inception

A simple OCaml interpreter... written entirely in OCaml.

Seems completely useless... but provides a framework to implement any functional programming language in OCaml.

![Thanos Meme](img/ThanosMeme.png)

Work in progress. The goal of this project is reaching:

$$\lim_{n \to \infty} \text{OCaml}^n$$

In other words: **OCaml Inception**

## Features

- Lexing, parsing, type inference, and evaluation of a subset of OCaml
- Supports integers, booleans, tuples, lambda expressions, let bindings, recursive functions, and basic operators
- Hindley-Milner style type checking

## Build

Requires OCaml and [opam](https://opam.ocaml.org/).

```console
$ cd ocaml_inception
$ opam install . --deps-only
$ dune build
```

## Usage

```console
$ cd ocaml_inception
$ opam exec -- dune utop
# Ocaml_inception.Interpreter.interpret "<your code>";;
```

The `interpret` function in `lib/interpreter.ml` takes a source string and returns a `(type, value)` pair.

## License

MIT. See more in [LICENSE](LICENSE).
