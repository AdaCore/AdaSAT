package Solver is
   type Literal_Array is array (Positive range <>) of Integer;
   type Literal_Options is access Literal_Array;
   type Formula is array (Positive range <>) of Literal_Options;

   type Literal_Value is (True, False, Unset);
   type Model is array (Positive range <>) of Literal_Value;

   type SAT_Result is (SAT, UNSAT, UNKNOWN);

   function Satisfies (F : Formula; M : Model) return SAT_Result;
end Solver;
