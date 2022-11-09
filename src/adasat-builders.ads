--
--  Copyright (C) 2019-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with AdaSAT.Formulas; use AdaSAT.Formulas;
private with AdaSAT.Internals;

--  This package provides some useful data types to build formulas ready to be
--  fed to the solver.

package AdaSAT.Builders is
   type Clause_Builder is tagged private;
   --  Type that provide some useful operations to construct a Clause object.
   --  Note that copying the clause builder must be done using the `Copy`
   --  primitive defined below.

   Empty_Clause_Builder : constant Clause_Builder;
   --  An initially empty clause builder

   procedure Reserve (C : in out Clause_Builder; Size : Natural);
   --  Reserve enough space in the internal vector to hold `Size` literals

   procedure Add (C : in out Clause_Builder; L : Literal);
   --  Add the given literal to the clause, without checking if it is already
   --  present.

   procedure Add_Simplify (C : in out Clause_Builder; L : Literal);
   --  Add the given literal to the clause, unless it is already present

   function Copy (C : Clause_Builder) return Clause_Builder;
   --  Copy this clause builder and its internal clause in a new independent
   --  builder.

   procedure Destroy (C : in out Clause_Builder);
   --  Free the memory associated to this builder. Note that you don't need
   --  to call it if you already build the clause.

   function Build (C : in out Clause_Builder) return Clause;
   --  Build the final clause. You do not need to explicitly destroy the
   --  clause builder afterwards.

   type Formula_Builder is tagged private;
   --  Type that provide some useful operations to construct a Formula object

   Empty_Formula_Builder : constant Formula_Builder;
   --  An initially empty formula builder

   procedure Add (F : in out Formula_Builder; C : Clause);
   --  Add the given clause to the formula builder without checking if it can
   --  simplify or be simplified by the clauses already present in the formula.
   --  Note that the given clause is now owned by the builder, along with its
   --  associated memory.

   procedure Add_Simplify (F : in out Formula_Builder; C : Clause);
   --  Add the given clause to the formula builder but also check if it can
   --  simplify or be simplified by the clauses already present in the formula.
   --  Note that the given clause is now owned by the builder, along with its
   --  associated memory. In particular, it could be freed immediatly if is
   --  deemed redundant.

   function Is_Feasible (F : Formula_Builder; L : Literal) return Boolean;
   --  Run a simple analysis to determine the value of the given literal.
   --  In particular, this will return False if we can know for sure that
   --  the literal cannot be true, and will consevatively return True for
   --  all other cases.

   procedure Destroy (F : in out Formula_Builder);
   --  Free the memory associatged to this builder. This will also free
   --  all the clauses that it was managing.

   function Build (F : in out Formula_Builder) return Formula;
   --  Build the final formula. You do not need to explicitly destroy the
   --  clause builder afterwards. Note that the clauses are not managed by
   --  the builder anymore, so they should be freed if you don't plan on
   --  feeding them back to the solver.

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
