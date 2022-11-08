with AdaSAT.Vectors;

package AdaSAT.Formulas is
   type Clause_Array is array (Positive range <>) of Clause;

   package Clause_Vectors is new AdaSAT.Vectors
     (Clause, Clause_Array);

   subtype Formula is Clause_Vectors.Vector;
   --  A CNF formula as a vector of clause

   function Image (F : Formula) return String;
   --  Returns a string representation of the formula

   type SAT_Result is (SAT, UNSAT, UNKNOWN);
   --  The result of solving a formula

   function Satisfies (F : Formula; M : Model) return SAT_Result;
   --  Given a formula and a model, evaluates whether the model
   --  satisfies the formula or not.
end AdaSAT.Formulas;
