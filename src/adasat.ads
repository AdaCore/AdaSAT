--
--  Copyright (C) 2019-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;

--  The root package of the AdaSAT library. Defines the base data structures
--  that are used everywhere else.
--  You should instantiate the `AdaSAT.DPLL` package with your own theory or
--  use the `AdaSAT.Helpers.DPLL_Solve` function to start solving!

package AdaSAT is
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
   --  Note that we don't use a generalized access type to avoid users from
   --  giving stack-allocated clauses to the solver.

   subtype Clause is Literal_Array_Access;
   --  A clause represents a disjunction of literals

   function Image (C : Clause) return String;
   --  Returns a string representation of the clause

   type Variable_Value is (True, False, Unset);
   --  A variable can either be set to True or False, or Unset

   type Model is array (Variable range <>) of Variable_Value;
   --  A mapping from variable to its value

   function Image (M : Model) return String;
   --  Returns a string representation of the model

private

   type Literal is new Integer;

   procedure Free is new Ada.Unchecked_Deallocation
     (Literal_Array, Clause);
end AdaSAT;
