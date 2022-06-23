package Solver is
   type Variable is new Positive;

   subtype Variable_Or_Null is Variable'Base range 0 .. Variable'Last;

   type Literal is private;
   --  A positive or negative occurrence of a variable

   function "+" (V : Variable) return Literal
      with Inline;
   --  Create a literal with a positive polarity occurrence of a variable

   function "-" (V : Variable) return Literal
      with Inline;
   --  Create a literal with a negative polarity occurrence of a variable

   function "abs" (L : Literal) return Variable
      with Inline;
   --  Get the variable that occurs in this literal

   type Literal_Array is array (Positive range <>) of Literal;
   type Literal_Array_Access is access Literal_Array;

   subtype Clause is Literal_Array_Access;
   --  A clause represents a disjunction of literals

   function Image (C : Clause) return String;
   --  Returns a string representation of the clause

   type Formula is array (Positive range <>) of Clause;
   --  A CNF formula as a list of clause

   function Image (F : Formula) return String;
   --  Returns a string representation of the formula

   type Variable_Value is (True, False, Unset);
   --  A variable can either be set to True or False, or Unset

   type Model is array (Variable range <>) of Variable_Value;
   --  A mapping from variable to its value

   type SAT_Result is (SAT, UNSAT, UNKNOWN);
   --  The result of solving a formula

   function Satisfies (F : Formula; M : Model) return SAT_Result;
   --  Given a formula and a model, evaluates whether the model
   --  satisfies the formula or not.

private
   type Literal is new Integer;
end Solver;
