with Ada.Text_IO; use Ada.Text_IO;

with AdaSAT.Formulas;
with AdaSAT.Propositions;
with AdaSAT.Helpers;

procedure Propositions is
   use AdaSAT;
   use AdaSAT.Formulas;
   use AdaSAT.Propositions;

   P : Proposition := (+1 and +2) xor (+1 and +3);
   V : Variable := 3;
   F : constant Formula := To_CNF (P, V, Quadra);
   M : Model := [1 .. V => Unset];
begin
   Put_Line (Image (P));
   Destroy (P);
   Put_Line (Image (F));
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
end Propositions;
