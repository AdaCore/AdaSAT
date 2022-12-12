# AdaSAT
Implementation of a DPLL-based SAT solver in Ada. Main features:

 - [X] Conflict analysis and backjumping
 - [X] Two-watched literals scheme
 - [X] Built-in support for At-Most-One constraints
 - [X] Custom theories

# Custom theories

The solver can solve arbitrary theories via a lazy approach. For that, the DPLL
solver can be instantiated with a custom `Theory` package that has a very
simple interface consisting in a single `Check` function and context type:

```ada
generic
   type User_Context;
   with function Check
     (Ctx : in out User_Context;
      M   : Model;
      F   : in out Formula) return Boolean;
package Theory is
end Theory;
```
The `Check` function is invoked whenever the solver needs the theory to
validate a model that satisfies the SAT formula. In case the model does not
satisfy the theory, the function must return a new formula which contradicts
the model via the `F` out parameter. The provided contradiction is appended to
the initial formula and the solver tries to find a new solution.

*Note* that the context `Ctx` is propagated from the initial call to `Solve` to
each call to `Check` and its sole use is to simplify user-facing code, in
particular to offer another way of accessing data required for solving the
theory than having the `Check` subprogram be a nested subprogam of wherever
your are invoking the solver.

# Build

The library can be built with `gprbuild -P adasat.gpr -p
-XBUILD_MODE=[dev|prof|prod]`.

To use in your projects, simply add `with "adasat";` in your project's
dependencies.

# Test

- Make sure you have a Python3 available

- Install the `e3-testsuite` package.

- Put AdaSAT's root directory in your `GPR_PROJECT_PATH` (so that `gprbuild`
  can find `adasat.gpr`)

- Finally, run `python3 testsuite/testsuite.py`.
