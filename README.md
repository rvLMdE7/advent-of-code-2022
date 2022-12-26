# Advent Of Code 2022

Doing the Advent of Code 2022 challenges in Haskell.

## Cabal Structure

We expose all the code as libraries, and then we specify in the `.cabal` file
how to build the executables/test suites using them. I settled on this
approach as:

- it works well with IDE integration (in particular, I have found that
  [HLS][hls] doesn't like being used in executable or test code, so we dodge
  that problem by exposing everything as library code).
- it has the added benefit that you can load up every bit of code (including
  test suites) in a REPL to play around with, which can be nice during
  development.
- using `mixins` in the cabal file we can reduce the boilerplate that might
  otherwise be needed to structure things this way.

## Usage

### Cabal

```shell
> cabal build
> cabal run day-01
> cabal run test-01
```

```shell
> cabal repl adventofcode2022
...
>>> :load Day01
```

### Stack

Stack is not fully supported, but efforts have been made to do so. It doesn't
support the use of backpack features such as mixins, but at time of writing
it compiles with them nonetheless. There is a `stack.yaml` set up, and the
version requirements in the `.cabal` file are compatible with the selected
stack resolver.

```shell
> stack build
> stack run day-01
> stack test :test-01
```

```shell
> stack repl adventofcode2022:lib
...
>>> :load Day01
```

### Nix

We have a `default.nix` that should be usable for building the whole project
(via `nix-build`), creating executables named `day-01`, `day-02` etc and
running all the test suites at the same time. It also has all tools etc.
configured, so that running `nix-shell` should spawn a shell where stack or
cabal can be used like normal. Also, opening an editor from that shell will
spawn it with `$PATH` etc. configured to provide matching versions of HLS
needed for developing or browsing the code.

#### Building

```shell
> nix-build
...
> ./result/bin/day-01
```

#### Everything Else

```shell
> nix-shell
...
> [nix-shell]$ stack build
> [nix-shell]$ cabal build
> [nix-shell]$ preferred-editor .
```

[hls]: https://github.com/haskell/haskell-language-server
