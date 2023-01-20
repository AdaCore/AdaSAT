with Ada.Text_IO; use Ada.Text_IO;

with AdaSAT.Builders;
with AdaSAT.Formulas;
with AdaSAT.DPLL;
with AdaSAT.Theory;

--  This is to test instantiating a DPLL solver with user-provided theory.
--  The theory below doesn't do anything smart and its only purpose is to check
--  that everything works as expected.

procedure Test is
   use AdaSAT;
   use AdaSAT.Formulas;

   type Context is record
      Solved : Natural := 0;
   end record;

   function Check
     (Ctx : in out Context;
      M   : Model;
      F   : in out Formula) return Boolean;

   function Check
     (Ctx : in out Context;
      M   : Model;
      F   : in out Formula) return Boolean
   is
      Contradiction : Builders.Formula_Builder;
   begin
      --  Dump theory: for all pairs of set variables, the product of their
      --  indices should not exceed 30.
      for U in M'Range loop
         if M (U) in True then
            for V in M'Range loop
               if M (V) in True then
                  if Natural (U) * Natural (V) > 30 then
                     Contradiction.Add_Simplify
                       (new Literal_Array'(-U, -V));
                  end if;
               end if;
            end loop;
         end if;
      end loop;

      F := Contradiction.Build;

      if F.Is_Empty then
         Ctx.Solved := Ctx.Solved + 1;
         Put ("Found model : ");
         for E of M loop
            Put (E'Image);
            Put (" ");
         end loop;
         New_Line;

         --  We found one solution but we want to keep looking for others,
         --  so return a contradiction that cancels this specific model.
         declare
            Not_Model : Builders.Clause_Builder;
         begin
            for V in M'Range loop
               Not_Model.Add (if M (V) in True then -V else +V);
            end loop;
            F.Append (Not_Model.Build);
         end;
      end if;
      return False;
   end Check;

   package My_Theory is new Theory (Context, Check);
   package My_Solver is new DPLL (My_Theory);

   F   : Builders.Formula_Builder;
   M   : Model := (1 .. 10 => Unset);
   Ctx : Context;
begin
   if not My_Solver.Solve (F.Build, Ctx, M) then
      Put_Line ("Found" & Ctx.Solved'Image & " solutions.");
   end if;
   New_Line;
end Test;
