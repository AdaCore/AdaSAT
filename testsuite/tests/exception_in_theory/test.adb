with Ada.Text_IO; use Ada.Text_IO;

with AdaSAT.Builders;
with AdaSAT.Formulas;
with AdaSAT.DPLL;
with AdaSAT.Theory;

--  This is to check that exception raised during a theory check are correctly
--  propagated back to the user. It is also here to make sure that memory is
--  properly freed along the way.

procedure Test is
   use AdaSAT;
   use AdaSAT.Formulas;

   type Context is null record;

   User_Error : exception;

   function Check
     (Ctx : in out Context;
      M   : Model;
      F   : in out Formula) return Boolean;

   function Check
     (Ctx : in out Context;
      M   : Model;
      F   : in out Formula) return Boolean
   is
   begin
      if M (1) = True then
         Put_Line ("First iteration in theory check");
         F.Append (new Literal_Array'(1 => -1));
      else
         Put_Line ("Second iteration in theory check");
         F.Append (new Literal_Array'(1 => -2));
         raise User_Error;
      end if;
      return False;
   end Check;

   package My_Theory is new Theory (Context, Check);
   package My_Solver is new DPLL (My_Theory);

   F   : Builders.Formula_Builder;
   M   : Model := (1 .. 2 => Unset);
   Ctx : Context;
begin
   F.Add (new Literal_Array'(+1, +2));
   if My_Solver.Solve (F.Build, Ctx, M) then
      Put_Line ("Solved!");
   end if;
exception
   when others =>
      Put_Line ("An exception occurred while solving!");
end Test;
