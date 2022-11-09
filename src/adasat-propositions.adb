--
--  Copyright (C) 2019-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;

package body AdaSAT.Propositions is

   procedure Free is new Ada.Unchecked_Deallocation
     (Proposition_Record, Proposition);

   function Is_Simple (P : Proposition) return Boolean;

   function "+"   (V : Variable)    return Proposition is
     (new Proposition_Record'(Kind_Var, V));

   function "not" (P : Proposition) return Proposition is
     (new Proposition_Record'(Kind_Not, P));

   function "and" (L, R : Proposition) return Proposition is
     (if L = null then R
      elsif R = null then L
      else new Proposition_Record'(Kind_And, L, R));

   function "or"  (L, R : Proposition) return Proposition is
     (if L = null then R
      elsif R = null then L
      else new Proposition_Record'(Kind_Or, L, R));

   function "xor" (L, R : Proposition) return Proposition is
     (if L = null then R
      elsif R = null then L
      else new Proposition_Record'(Kind_Xor, L, R));

   function Implies (L, R : Proposition) return Proposition is
     (not L or R);

   function Exactly_One (P : Proposition_Array) return Proposition is
      F : Proposition := Empty_Proposition;
   begin
      for Q of P loop
         F := F or Q;
      end loop;
      for I in P'Range loop
         for J in I + 1 .. P'Last loop
            F := F and (not P (I) or not P (J));
         end loop;
      end loop;
      return F;
   end Exactly_One;

   function Is_Simple (P : Proposition) return Boolean is
   begin
      case P.Kind is
         when Kind_Var =>
            return True;
         when Kind_Not =>
            return Is_Simple (P.Inner);
         when others =>
            return False;
      end case;
   end Is_Simple;

   function "&" (A, B : Formula) return Formula;
   function "&" (A, B : Formula) return Formula is
      R : Formula := A.Copy;
   begin
      for X of B loop
         R.Append (X);
      end loop;
      return R;
   end "&";

   function Singleton (C : Clause) return Formula;
   function Singleton (C : Clause) return Formula is
      R : Formula;
   begin
      R.Append (C);
      return R;
   end Singleton;

   function To_CNF
     (P         : Proposition;
      Var_Count : in out Variable_Or_Null;
      Technique : CNF_Technique := Naive) return Formula
   is
      subtype Any is Literal_Array;

      function Fresh_Var return Variable;
      function Transform (P : Proposition) return Formula;
      function Transform_Or_Naive  (P : Proposition) return Formula;
      function Transform_Or_Quadra (P : Proposition) return Formula;

      function Fresh_Var return Variable is
      begin
         Var_Count := Var_Count + 1;
         return Var_Count;
      end Fresh_Var;

      function Transform_Or_Naive (P : Proposition) return Formula is
         L : constant Formula := Transform (P.Left);
         R : constant Formula := Transform (P.Right);
         F : Clause_Vectors.Vector;
      begin
         for I of L loop
            for J of R loop
               F.Append (new Any'(I.all & J.all));
            end loop;
         end loop;
         return F;
      end Transform_Or_Naive;

      function Transform_Or_Quadra (P : Proposition) return Formula is
      begin
         if Is_Simple (P.Left) or else Is_Simple (P.Right) then
            return Transform_Or_Naive (P);
         end if;
         declare
            Z : constant Proposition := +Fresh_Var;
            L : constant Formula := Transform_Or_Naive
              (not Z or P.Left);
            R : constant Formula := Transform_Or_Naive
              (Z or P.Right);
         begin
            return L & R;
         end;
      end Transform_Or_Quadra;

      function Transform (P : Proposition) return Formula is
      begin
         case P.Kind is
            when Kind_Var =>
               return Singleton (new Any'(1 => +P.Var));
            when Kind_Not =>
               declare
                  Q : constant Proposition := P.Inner;
               begin
                  case Q.Kind is
                     when Kind_Var =>
                        return Singleton (new Any'(1 => -Q.Var));
                     when Kind_Not =>
                        return Transform (Q.Inner);
                     when Kind_And =>
                        return Transform (not Q.Left or not Q.Right);
                     when Kind_Or =>
                        return Transform (not Q.Left and not Q.Right);
                     when Kind_Xor =>
                        return Transform
                          ((not Q.Left or Q.Right) and
                           (Q.Left or not Q.Right));
                  end case;
               end;
            when Kind_And =>
               return Transform (P.Left) & Transform (P.Right);
            when Kind_Or =>
               if Technique in Naive then
                  return Transform_Or_Naive (P);
               else
                  return Transform_Or_Quadra (P);
               end if;
            when Kind_Xor =>
               return Transform
                 ((P.Left and not P.Right) or (not P.Left and P.Right));
         end case;
      end Transform;
   begin
      if P = null then
         return Clause_Vectors.Empty_Vector;
      else
         return Transform (P);
      end if;
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
      if P = null then
         return;
      end if;
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

end AdaSAT.Propositions;
