# DPLL-Ada
Implementation of a DLPP-based SAT solver in Ada.

 - [X] Conflict analysis and backjumping
 - [X] Two-watched literals scheme
 - [X] Built-in support for At-Most-One constraints

# Custom theories

The solver can solve arbitrary theories via a lazy approach. For that, the DPLL solver can be instantiated with a custom `Theory` package
that has a very simple interface consisting in a single `Check` function:
```
with function Check (M : Model; SAT : out Boolean) return Formula;
```
This function is invoked whenever the solver needs the theory to validate a model that satisfies the SAT formula.
In case the model does not satisfy the theory, the function must return a new formula which contradicts the model.
This formula is appended to the initial formula and the solver tries to find a new solution.

# Build
The library can be built with `gprbuild -P dpll.gpr -p -XBUILD_MODE=prod`.

To use in your projects, simply add `with "dpll";` in your project's dependencies.

# Test
First build the tests with `gprbuild -P dpll_test.gpr -p -XBUILD_MODE=prod`
You can now run them:
```
➜  ./bin/simple                                 
Solved
FALSE TRUE TRUE TRUE FALSE TRUE TRUE 
SAT

➜ ./bin/sudoku tests/sudoku_grids/sudoku_15.txt         
Solved
 7 9 2 5 6 8 1 4 3
 4 5 3 2 1 9 8 6 7
 8 6 1 3 7 4 9 5 2
 6 2 5 8 9 3 7 1 4
 3 7 9 1 4 2 6 8 5
 1 4 8 7 5 6 2 3 9
 2 8 4 9 3 1 5 7 6
 9 3 7 6 8 5 4 2 1
 5 1 6 4 2 7 3 9 8

SAT
```
