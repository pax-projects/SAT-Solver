# SAT&Lite
===========
A Light and Slow SAT solver

By: Maxence ABRILE & Natale RICHELLE

---

## Overview

SAT&Lite is a lightweight **DPLL-based SAT solver** implemented in OCaml.  
It uses a **recursive approach** with:

- Unit propagation
- Pure literal elimination
- Backtracking

---

## How to run the project

The project is fully managed via a Makefile. All commands are executed from the terminal.

### Compile the project

make 

#### Solve a CNF file

make solve FILE=examples/SAT/sudoku-4x4.cnf

### Run unit test

make test

### Help 
Pour connaitre les commandes 

make help

