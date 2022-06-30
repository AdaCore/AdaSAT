with Support.Vectors;

package Solver.Builders is
   type Clause_Builder is tagged private;

   procedure Add (C : in out Clause_Builder; L : Literal);
   function Build (C : Clause_Builder) return Clause;
   procedure Destroy (C : in out Clause_Builder);
   function Build_And_Destroy (C : in out Clause_Builder) return Clause;

   type Formula_Builder is tagged private;

   procedure Add (F : in out Formula_Builder; C : Clause);
   function Build (F : Formula_Builder) return Formula;
   procedure Destroy (F : in out Formula_Builder);
   function Build_And_Destroy (F : in out Formula_Builder) return Formula;

private

   package Literal_Vectors is new Support.Vectors
     (Literal, Literal_Array);

   package Clause_Vectors is new Support.Vectors
     (Clause, Formula);

   type Clause_Builder is tagged record
      V : Literal_Vectors.Vector;
   end record;

   type Formula_Builder is tagged record
      V : Clause_Vectors.Vector;
   end record;
end Solver.Builders;
