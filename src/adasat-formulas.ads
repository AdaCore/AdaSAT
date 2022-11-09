--
--  Copyright (C) 2019-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with AdaSAT.Vectors;

--  Defines the structure used to represent a formula in CNF (Conjunctive
--  Normal Form). The content cannot directly lie in the root `AdaSAT` package
--  because it would create a circular dependency on the `AdaSAT.Vectors`
--  package.

package AdaSAT.Formulas is
   type Clause_Array is array (Positive range <>) of Clause;

   package Clause_Vectors is new AdaSAT.Vectors
     (Clause, Clause_Array);

   subtype Formula is Clause_Vectors.Vector;
   --  A CNF formula is a vector of clause.
   --  TODO: Ideally this type would be private and users should only create
   --  formulas using routines in the `AdaSAT.Builders` package, however I
   --  could not come up with a satisfying way to do this kind of encapsulation
   --  in Ada yet.

   function Image (F : Formula) return String;
   --  Returns a string representation of the formula

   type SAT_Result is (SAT, UNSAT, UNKNOWN);
   --  The result of solving a formula

   function Satisfies (F : Formula; M : Model) return SAT_Result;
   --  Given a formula and a model, evaluates whether the model
   --  satisfies the formula or not.
end AdaSAT.Formulas;
