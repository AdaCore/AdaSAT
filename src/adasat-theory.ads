with AdaSAT.Formulas; use AdaSAT.Formulas;

generic
   type User_Context is private;
   pragma Warnings (Off, "referenced");
   with function Check
     (Ctx : in out User_Context;
      M   : Model;
      F   : in out Formula) return Boolean;
   --  Callback invoked by the SAT solver once a model has been found.
   --  This function should indicate whether the model also satisfies the
   --  theory using its return value.
   --  If it does not satisfy it, the function should fill the `F` formula
   --  with a contradiction in order to constrain the SAT problem further,
   --  or let it empty to indicate that the problem cannot be satisfied in
   --  the theory.
   pragma Warnings (On, "referenced");
package AdaSAT.Theory is
end AdaSAT.Theory;
