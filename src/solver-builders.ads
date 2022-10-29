with Support.Vectors;

package Solver.Builders is
   type Clause_Builder is tagged private;

   Empty_Clause_Builder : constant Clause_Builder;

   procedure Add (C : in out Clause_Builder; L : Literal);
   function Copy (C : Clause_Builder) return Clause_Builder;
   function Build (C : Clause_Builder) return Clause;
   procedure Destroy (C : in out Clause_Builder);
   function Build_And_Destroy (C : in out Clause_Builder) return Clause;

   type Formula_Builder is tagged private;

   Empty_Formula_Builder : constant Formula_Builder;

   procedure Add (F : in out Formula_Builder; C : Clause);
   procedure Add_Simplify (F : in out Formula_Builder; C : Clause);
   function Is_Feasible (F : Formula_Builder; V : Variable) return Boolean;
   procedure Destroy (F : in out Formula_Builder);
   function Build_And_Destroy (F : in out Formula_Builder) return Formula;

private

   package Literal_Vectors is new Support.Vectors
     (Literal, Literal_Array);

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
end Solver.Builders;
