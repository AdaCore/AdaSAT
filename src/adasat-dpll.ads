--
--  Copyright (C) 2019-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with AdaSAT.Decisions;
with AdaSAT.Formulas; use AdaSAT.Formulas;
with AdaSAT.Theory;

--  This is the main package of this library. Users should instantiate their
--  package with their custom theory (or use the SAT-only solver instantiated
--  in package `AdaSAT.Helpers`) and call `Solve` with their formula.
--  A custom decision procedures can be given, else the default one will be
--  used (see `AdaSAT.Decisions` for more information).

generic
   with package T is new Theory (<>);
   --  The theory against which SAT models will be checked
   with function Next_Decision
     (M : Model; First_Unset : in out Variable) return Variable_Or_Null
   is AdaSAT.Decisions.First_Unassigned;
package AdaSAT.DPLL is
   function Solve
     (F        : Formula;
      Ctx      : in out T.User_Context;
      M        : in out Model;
      Min_Vars : Variable_Or_Null := 0) return Boolean;
   --  Determine whether the formula is satisfiable or not.
   --  In case it is satisfiable, fill the given model with a solution.
   --  if ``Min_Vars`` is not 0, the solver does not decide variables
   --  that after the given ``Min_Vars`` index.
   --  Note that the given formula becomes owned by the solver, and therefore
   --  its memory and that of its inner clauses will be deallocated when the
   --  solving terminates.
end AdaSAT.DPLL;
