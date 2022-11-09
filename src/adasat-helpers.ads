with AdaSAT.Formulas;

--  Provide various helpers for using the AdaSAT library

package AdaSAT.Helpers is
   use AdaSAT.Formulas;

   function DPLL_Solve
     (F        : Formula;
      M        : in out Model;
      Min_Vars : Variable_Or_Null := 0) return Boolean;
   --  Solve the given Formula and provide a model for its variables.
   --  It is assumed that this is a pure SAT problem. This simply invokes
   --  the `Solve` function on an instantiation of the `AdaSAT.DPLL` package
   --  with a theory that always returns True.

end AdaSAT.Helpers;
