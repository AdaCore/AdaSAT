with Solver.Theory;

generic
   with package T is new Theory (<>);
   --  The theory to use
package Solver.DPLL is
   function Solve (F : Formula; M : in out Model) return Boolean;
   --  Determine whether the formula is satisfiable or not.
   --  In case it is satisfiable, fill the given model with a solution.

   function Solve_Partial
     (F        : Formula;
      M        : in out Model;
      Min_Vars : Variable_Or_Null) return Boolean;
   --  Like the above ``Solve`` function, but does not try to decide variables
   --  that after the given ``Min_Vars`` index.
end Solver.DPLL;
