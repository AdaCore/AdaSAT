--
--  Copyright (C) 2019-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with AdaSAT.Formulas; use AdaSAT.Formulas;
private with AdaSAT.Internals;

package AdaSAT.Builders is
   type Clause_Builder is tagged private;

   Empty_Clause_Builder : constant Clause_Builder;

   procedure Reserve (C : in out Clause_Builder; Size : Natural);
   procedure Add (C : in out Clause_Builder; L : Literal);
   procedure Add_Simplify (C : in out Clause_Builder; L : Literal);
   function Copy (C : Clause_Builder) return Clause_Builder;
   procedure Destroy (C : in out Clause_Builder);
   function Build (C : in out Clause_Builder) return Clause;

   type Formula_Builder is tagged private;

   Empty_Formula_Builder : constant Formula_Builder;

   procedure Add (F : in out Formula_Builder; C : Clause);
   procedure Add_Simplify (F : in out Formula_Builder; C : Clause);
   function Is_Feasible (F : Formula_Builder; L : Literal) return Boolean;
   procedure Destroy (F : in out Formula_Builder);
   function Build (F : in out Formula_Builder) return Formula;

private
   use AdaSAT.Internals;

   type Clause_Builder is tagged record
      V : Literal_Vectors.Vector;
   end record;

   type Formula_Builder is tagged record
      V : Clause_Vectors.Vector;
   end record;

   Empty_Clause_Builder  : constant Clause_Builder :=
     (V => Literal_Vectors.Empty_Vector);

   Empty_Formula_Builder : constant Formula_Builder :=
     (V => Clause_Vectors.Empty_Vector);
end AdaSAT.Builders;
