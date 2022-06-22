with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;

with Support.Vectors;

package body Solver.Propositions is
   procedure Free is new Ada.Unchecked_Deallocation
     (Proposition_Record, Proposition);

   function "+"   (V : Variable)    return Proposition is
     (new Proposition_Record'(Kind_Var, V));

   function "not" (P : Proposition) return Proposition is
     (new Proposition_Record'(Kind_Not, P));

   function "and" (L, R : Proposition) return Proposition is
     (new Proposition_Record'(Kind_And, L, R));

   function "or"  (L, R : Proposition) return Proposition is
     (new Proposition_Record'(Kind_Or, L, R));

   function "xor" (L, R : Proposition) return Proposition is
     (new Proposition_Record'(Kind_Xor, L, R));

   package Clause_Vectors is new Support.Vectors (Clause, Formula);

   type Formula_Access is access Formula;

   function Get_Array is new Clause_Vectors.Internal_Array (Formula_Access);

   function To_CNF (P : Proposition) return Formula is
      subtype Any is Literal_Array;
   begin
      case P.Kind is
         when Kind_Var =>
            return (1 => new Any'(1 => +P.Var));
         when Kind_Not =>
            declare
               Q : constant Proposition := P.Inner;
            begin
               case Q.Kind is
                  when Kind_Var =>
                     return (1 => new Any'(1 => -Q.Var));
                  when Kind_Not =>
                     return To_CNF (Q.Inner);
                  when Kind_And =>
                     return To_CNF (not Q.Left or not Q.Right);
                  when Kind_Or =>
                     return To_CNF (not Q.Left and not Q.Right);
                  when Kind_Xor =>
                     raise Program_Error;
               end case;
            end;
         when Kind_And =>
            return To_CNF (P.Left) & To_CNF (P.Right);
         when Kind_Or =>
            declare
               L : constant Formula := To_CNF (P.Left);
               R : constant Formula := To_CNF (P.Right);
               F : Clause_Vectors.Vector;
            begin
               for I of L loop
                  for J of R loop
                     F.Append (new Any'(I.all & J.all));
                  end loop;
               end loop;
               return E : constant Formula := Get_Array (F).all do
                  F.Destroy;
               end return;
            end;
         when Kind_Xor =>
            return To_CNF
              ((P.Left and not P.Right) or (not P.Left and P.Right));
      end case;
   end To_CNF;

   function Image (P : Proposition) return String is
      use Ada.Strings.Unbounded;

      R : Unbounded_String;

      procedure Process (P : Proposition);

      procedure Process (P : Proposition) is
      begin
         case P.Kind is
            when Kind_Var =>
               Append (R, P.Var'Image);
            when Kind_Not =>
               Append (R, "¬(");
               Process (P.Inner);
               Append (R, ")");
            when others =>
               Append (R, "(");
               Process (P.Left);
               Append
                 (R,
                  (case P.Kind is
                     when Kind_And => " | ",
                     when Kind_Or  => " & ",
                     when Kind_Xor => " ^ ",
                     when others   => raise Program_Error));
               Process (P.Right);
               Append (R, ")");
         end case;
      end Process;
   begin
      Process (P);
      return To_String (R);
   end Image;

   procedure Destroy (P : in out Proposition) is
   begin
      case P.Kind is
         when Kind_Var =>
            null;
         when Kind_Not =>
            Destroy (P.Inner);
         when others =>
            Destroy (P.Left);
            Destroy (P.Right);
      end case;
      Free (P);
   end Destroy;

end Solver.Propositions;
