package Solver.Propositions is
   type Proposition is private;

   function "+"   (V    : Variable)    return Proposition;
   function "not" (P    : Proposition) return Proposition;
   function "and" (L, R : Proposition) return Proposition;
   function "or"  (L, R : Proposition) return Proposition;
   function "xor" (L, R : Proposition) return Proposition;

   function To_CNF (P : Proposition) return Formula;

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
end Solver.Propositions;
