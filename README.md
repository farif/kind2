Kind 2
======

A multi-engine, parallel, SMT-based automatic model checker for safety properties of Lustre programs. 

Kind 2 takes as input a Lustre file annotated with properties to prove invariant (see [Lustre syntax](doc/Lustre.md)), and outputs which of the properties are true for all inputs, as well as an input sequence for those properties that are falsified. To ease processing by front-end tools, Kind 2 can output its results in [XML format](doc/XML.md).

Kind 2 runs a process for bounded model checking (BMC), a process for k-induction, and a proces for IC3 in parallel on all properties simultaneously. It incrementally outputs counterexamples to properties as well as properties proved invariant.

The following command-line options control its operation (run ```kind2 --help``` for a full list).

```--enable {BMC|IND|PDR}``` Select model checking engines
   
By default, all three model checking engines are run in parallel. Give any combination of ```--enable BMC```, ```--enable IND``` and ```--enable PDR``` to select which engines to run. The option ``--enable BMC`` alone will not be able to prove properties valid, choosing ``--enable IND`` only will not produce any results. Any other combination is sound and complete.

```--timeout_wall SECS``` Run for SECS seconds of wallclock time

```--timeout_virtual SECS``` Run for SECS of CPU time
 
```--smtsolver {Z3|CVC4|mathsat5} ``` Select SMT solver

The default is ```Z3```, but see options of the ```./build.sh``` script to override at compile-time
  
```--z3_bin PROGRAM``` Executable for Z3

```--cvc4_bin PROGRAM``` Executable for CVC4

```--mathsat5_bin PROGRAM``` Executable for MathSat5

```--bmc_max K``` Run bounded model checking for up to ```K``` steps

```-v``` Output informational messages

```-xml``` Output in XML format


Requirements
============

- Linux or Mac OS X
- OCaml 4.01 or later
- A supported SMT solver
 - [Z3](http://z3.codeplex.com) (recommended), 
 - [CVC4](http://cvc4.cs.nyu.edu), (must use ```--pdr_tighten_unsat_core false```) or
 - [MathSat5](http://mathsat.fbk.eu/)

Building and installing
=======================

If you got the sources from the Github repository, you need to run first

    ./autogen.sh

In any case, the commands

    ./build.sh
    make install

will configure and build ZeroMQ, CZMQ, the OCaml bindings and Kind 2 and install the binary `kind2` into `/usr/local/bin`. Call `./build.sh --prefix=PATH` to install the Kind 2 binary into `PATH/bin` instead. If you need to pass options to the configure scripts of any of ZeroMQ, CZMQ, the OCaml bindings or Kind 2, add these to the `build.sh` call. Use `./configure --help` after `autogen.sh` to see all available options.

You need a supported SMT solver, at the momemt either Z3, CVC4 or MathSat5 on your path when running `kind2`.
