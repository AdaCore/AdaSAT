generic
   pragma Warnings (Off, "referenced");
   with function Check
     (F : Formula; M : Model; SAT : out Boolean) return Formula;
   --  Given a formula and a model which satisfies it, indicate whether the
   --  model also satisfies the theory through the SAT out parameter.
   --  If it does not satisfy it, the function should either return a
   --  formula that constrains the SAT problem further, or return an empty
   --  formula to indicate that the problem cannot be satisfies in the theory.
   pragma Warnings (On, "referenced");
package Solver.Theory is
end Solver.Theory;
