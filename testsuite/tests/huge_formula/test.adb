--  Test that the following program does not crash due to a stack overflow.
--  It used to be the case because internal data structures used inside the
--  DPLL implementation were allocated on the stack, and the size is directly
--  related to the number of variables to solve for.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with AdaSAT.Formulas;
with AdaSAT.Helpers;

procedure Test is
   use AdaSAT;
   use AdaSAT.Formulas;

   type Model_Access is access Model;

   procedure Free is new Ada.Unchecked_Deallocation
     (Model, Model_Access);

   Var_Count : constant := 1_000_000;

   F : Formula;
   M : Model_Access := new Model'(1 .. Var_Count => Unset);
begin
   F.Append (new Literal_Array'((+1, +Var_Count)));
   for I in Variable (2) .. Var_Count loop
      F.Append (new Literal_Array'((-1, -I)));
   end loop;
   if Helpers.DPLL_Solve (F, M.all) then
      Put_Line ("Solved");
   else
      Put_Line ("Failed solving");
   end if;
   Free (M);
end Test;
