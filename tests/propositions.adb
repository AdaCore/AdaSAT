with Ada.Text_IO; use Ada.Text_IO;

with Solver.DPLL;
with Solver.Propositions;
with Solver.Theory;

procedure Propositions is
   use Solver;
   use Solver.Propositions;

   function Check
     (F : Formula; M : Model; SAT : out Boolean) return Formula;

   function Check
     (F : Formula; M : Model; SAT : out Boolean) return Formula
   is
      pragma Unreferenced (F, M);
   begin
      SAT := True;
      return [];
   end Check;

   package Empty_Theory is new Theory (Check);
   package DPLLT is new DPLL (Empty_Theory);

   P : Proposition := (+1 and +2) xor (+1 and +1);
   F : constant Formula := To_CNF (P);
   M : Model := [1 .. 2 => Unset];
begin
   Put_Line (Image (P));
   Destroy (P);
   if DPLLT.Solve (F, M) then
      Put_Line ("Solved");
   else
      Put_Line ("Failed solving");
   end if;
   for E of M loop
      Put (E'Image);
      Put (" ");
   end loop;
   New_Line;
   Put_Line (Satisfies (F, M)'Image);
end Propositions;
