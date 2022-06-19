with Solver.Theory;

generic
   with package T is new Theory (<>);
   --  The theory to use
package Solver.DPLL is
   function Solve (F : Formula; M : in out Model) return Boolean;
   --  Determine whether the formula is satisfiable or not.
   --  In case it is satisfiable, fill the given model with a solution.
end Solver.DPLL;
