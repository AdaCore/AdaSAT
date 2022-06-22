package Solver.Propositions is
   type Proposition is private;

   Empty_Proposition : constant Proposition;

   function "+"   (V    : Variable)    return Proposition;
   function "not" (P    : Proposition) return Proposition;
   function "and" (L, R : Proposition) return Proposition;
   function "or"  (L, R : Proposition) return Proposition;
   function "xor" (L, R : Proposition) return Proposition;

   type CNF_Technique is (Naive, Quadra, Tseitin);

   function To_CNF
     (P         : Proposition;
      Var_Count : in out Variable;
      Technique : CNF_Technique := Naive) return Formula;
   --  Transform the given proposition in an equisat CNF formula.
   --  Var_Count should denote the number of variables appearing inside P.
   --  It will be updated if the transformation needs to add new variables.
   --  Method indicates which method should be used to perform the
   --  transformation.
   --  Note:
   --   - Naive never introduces new variables.
   --   - Quadra may introduce new variables on disjunctions
   --   - Tseitin introduces a new variable for each sub-formula

   function Image (P : Proposition) return String;
   procedure Destroy (P : in out Proposition);

private
   type Proposition_Record;

   type Proposition is access all Proposition_Record;

   type Proposition_Kind is (Kind_Var, Kind_And, Kind_Or, Kind_Xor, Kind_Not);

   type Proposition_Record (Kind : Proposition_Kind) is record
      case Kind is
         when Kind_Var =>
            Var : Variable;
         when Kind_Not =>
            Inner : Proposition;
         when others =>
            Left, Right : Proposition;
      end case;
   end record;

   Empty_Proposition : constant Proposition := null;
end Solver.Propositions;
