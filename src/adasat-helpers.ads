with AdaSAT.Formulas;

--  Provide various helpers for using the AdaSAT library

package AdaSAT.Helpers is
   use AdaSAT.Formulas;

   function DPLL_Solve
     (F        : Formula;
      M        : in out Model;
      Min_Vars : Variable_Or_Null := 0) return Boolean;

end AdaSAT.Helpers;
