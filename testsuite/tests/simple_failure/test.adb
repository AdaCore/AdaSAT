with Ada.Text_IO; use Ada.Text_IO;

with AdaSAT.Formulas;
with AdaSAT.Helpers;

procedure Test is
   use AdaSAT;
   use AdaSAT.Formulas;

   F : Formula;
   M : Model := (1 .. 2 => Unset);
begin
   F.Append (new Literal_Array'(-1, -2));
   F.Append (new Literal_Array'(+1, +2));
   F.Append (new Literal_Array'(-1, +2));
   F.Append (new Literal_Array'(+1, -2));
   if Helpers.DPLL_Solve (F, M) then
      Put_Line ("Solved");
   else
      Put_Line ("Failed solving");
   end if;
   for E of M loop
      Put (E'Image);
      Put (" ");
   end loop;
   New_Line;
end Test;
