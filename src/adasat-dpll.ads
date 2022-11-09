--
--  Copyright (C) 2019-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with AdaSAT.Formulas; use AdaSAT.Formulas;
with AdaSAT.Theory;

generic
   with package T is new Theory (<>);
   --  The theory to use
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
end AdaSAT.DPLL;
