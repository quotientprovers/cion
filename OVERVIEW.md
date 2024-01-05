# Overview of the Artifact

This is the artifact for the tool **Cion**, as part of the paper **Scenario-based Proofs for Concurrent Objects** (OOPSLA 2024).

## Introduction

This artifact is a tool called **Cion**, which supports Section 7 of the paper ("Generating Candidate Quotient Expressions"), which is Claim 5 of the Introduction. Specifically, Cion is a tool that analyzes a concurrent object implementation and generates a candidate quotient (in the form of a layer automaton) for that object.

Cion takes C-like programs as input and generates layer automata output (as a DOT/Tex/PDF).

This Cion artifact is available as a docker image. 



## Hardware Dependencies

This has been tested on **X86_64 Ubuntu 20.04.6 LTS**.

It may also work on other systems, but it depends on an older version of OCaml (4.05.0). To our knowledge that older version is not available for ARM.

Apart from the X86_64 architecture, there are no particular hardware requirements beyond a reasonable amount of RAM, CPU, etc. 

## Getting Started Guide

1. Using the Docker image and a fresh Ubuntu 20.04:

```
sudo apt-get update
sudo apt-get install docker.io
wget http://www.erickoskinen.com/cion-docker.tar.gz
gunzip cion-docker.tar.gz
sudo docker load --input cion-docker.tar
sudo docker run --name testcion -it cion:latest
```

2. Running the program (from inside the docker):

The last step above will place you inside the docker. Now you can run Cion on a single benchmark (the `counter`) as follows:

```
eval $(opam env)
cd /cion/cion
export ULTIMATE_HOME=/opt/UAutomizer-linux
mv cache /tmp/cache.saved1; touch cache # (optional)
dune exec cion counter
```

## Step by Step Instructions

### Running Cion: Input and Expected Output

Please begin by following the Getting Started Guide above first. When you are inside the docker, Cion can be executed as:

```
eval $(opam env)
cd /cion/cion
export ULTIMATE_HOME=/opt/UAutomizer-linux
mv cache /tmp/cache.saved2; touch cache # (optional)
dune exec cion counter
```

The final argument to `dune exec cion <benchmark>` is the name of the benchmark. The other benchmarks (`counter`, `descriptor`, `treiber`, `msq`, etc.) are defined in `cion/bin/benchmarks.ml`.

For convenience, as seen above, this artifact comes with a `cache` file that maps Ultimate solver queries (as an md5 of the feasibility query) to True/False. With the cache in place, you can quickly re-run the experiments. However, to compare execution times, you should of course first delete the cache.

The **output** will include a line of LaTex as used in the table of the paper such as:
```
                (filename)          states RdPaths WrPaths #Trans #layers Time Queries
RESULT: \texttt{../bench/treiber.c} &    2 &    3 &    2 &    6 &    5 &   0.1 &   37\\
```
Notably, this indicates the number of Transitions and Layers discovered (as well as read/write paths, execution time and number of queries to Ultimate).

The **output** also includes a generated PDF of the layer automaton found, for example, in
`../bench/counter.c.pdf` (and a graphviz `dot` source). 


### How to reproduce the experiments (from within the docker)

If you haven't already, enter the docker as explained in Getting Started.

```
eval $(opam env)
export ULTIMATE_HOME=/opt/UAutomizer-linux
cd /cion
mv cion/cache /tmp/cache.saved3; touch cion/cache
./runall.sh
```

The output now lists all of the rows generated for the table, and all generated layer automata can be found in the `/cion/bench/` folder.

You can then compare the LaTeX output with Table 1 on Page 22 of the paper.

It took us about 20 minutes to run all the benchmarks in Parallels on a MacBook Pro M1. If you wish to run just one benchmark of reasonable size, try `msq`, which should take about 10 minutes.


## Reusability Guide

Cion can be re-used on other inputs. There are two main steps to do this:

1. Create a source C file (see the Input assumptions below)

2. Create a configuration for the benchmark in `cion/bin/benchmarks.ml`

3. Re-compile Cion with `cd /cion && dune build`

### Input assumptions

Since this is a prototype tool, rather than focusing on parsing details, some input is simply encoded into the input program. Please follow these input assumptions:

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
This is configured in the benchmark's `make_state_exprs` function, defined in `cion/bin/benchmarks.ml` and can be done in one of three ways:

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
Therefore you must also update `cion/bin/benchmarks.ml` and set the 
benchmark's `init_stmts` configuration to specify which variables
should be nondeterministically assigned. 
(In the future this should be replaced with an Ultimate configuration flag.)
