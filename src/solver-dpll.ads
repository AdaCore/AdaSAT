with Solver.Theory;

generic
   with package T is new Theory (<>);
package Solver.DPLL is
   function Solve (F : Formula; M : in out Model) return Boolean;
end Solver.DPLL;
