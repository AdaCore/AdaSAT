package Solver is
   type Variable is new Positive;
   type Literal is private;

   function "+" (V : Variable) return Literal;
   function "-" (V : Variable) return Literal;
   function "abs" (L : Literal) return Variable;

   type Literal_Array is array (Positive range <>) of Literal;
   type Literal_Options is access Literal_Array;
   type Formula is array (Positive range <>) of Literal_Options;

   type Literal_Value is (True, False, Unset);
   type Model is array (Variable range <>) of Literal_Value;

   type SAT_Result is (SAT, UNSAT, UNKNOWN);

   function Satisfies (F : Formula; M : Model) return SAT_Result;

private
   type Literal is new Integer;
end Solver;
