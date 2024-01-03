# CION: Concurrent Trace Reductions

## Dependencies

0. Basic packages:
```
sudo apt-get install opam graphviz
```
1. Install OCaml, CIL, etc:

```
opam switch create 4.05.0 
opam install cil ocamlfind ocamlbuild oasis camlp4
```

2. Install Ultimate Automizer

After unziping [the Ultimate release](https://github.com/ultimate-pa/ultimate/releases/download/v0.2.1/UltimateAutomizer-linux.zip), 
the zip file creates a folder called "UAutomizer-...". 
```
wget https://github.com/ultimate-pa/ultimate/releases/download/v0.2.1/UltimateAutomizer-linux.zip
unzip UltimateAutomizer-linux.zip 
```
(Below you will set the `ULTIMATE_HOME` environment variable to point Cion at Ultimate.)

## Installation

Go into the Cion repository and compile it:

```
cd cion-master/
oasis setup
make
```

## Running

To run a single file:
```
export ULTIMATE_HOME=~/UAutomizer-linux/
./main.native bench/counter.c
dot -Tsvg bench/counter.c.dot -o bench/counter.c.svg # Optional
```

To run all the benchmarks:
```
export ULTIMATE_HOME=~/UAutomizer-linux/
./runall.sh
```

## Input assumptions

 1. A single C file implementation (see `bench/counter.c`) where the object's state is defined as global variables and each method has an implicit outer while loop, ie:
```
void incr() {
    // implicit while(true) {
    ...
    // } implicit end while
}
```
2. Atomic read-writes (ARW) are implemented as:
```
    if (_beginARW_ | (condition)) {
        // all writes here will be treated atomically
        _endARWsucc_ = 1;
    } else {
        // code executed if 
    }
```

3. The C file must also define the automaton states.
This is configured in `get_state_exprs_tmp` in `src/aut.ml`
and can be done in one of three ways:

  * Option 1: As atomic propositions (from which Cion will construct states based on all possible combinations of truth variables):
```
void __states() {
  int _ap1; int _ap2;
  _ap1 = Q.tail == Q.head;
  _ap2 = Q.tail->next == (void *)0;
}
```
   
  * Option 2: As full descriptions of the states (you can use LAND2, LAND3 etc for {2,3}-ary logical AND)
```
void __fullstates() {
    int _st1, _st2, _st3, _st4;
    _st1 = LAND3(gy->key == gk, !gx->del, gx->next == gy); 
    _st2 = LAND3(gy->key < gk, !gx->del, gx->next == gy); 
```

  * Option 3: As a series of choices:
```
void __choice() { ... } // see bench/listset3.c
```

4. The C file must also define the initial state because,
currently, Ultimate assumes that uninitialized fields are 0.
Typically we want them to instead be nondeterministic. 
Therefore you must also update `src/aut.ml` function `get_init_stmts`
to specify which variables should be nondeterministically assigned. 
(In the future this should be replaced with an Ultimate configuration flag.)
