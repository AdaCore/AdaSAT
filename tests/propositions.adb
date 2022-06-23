with Ada.Text_IO; use Ada.Text_IO;

with Solver.DPLL;
with Solver.Propositions;
with Solver.Theory;

procedure Propositions is
   use Solver;
   use Solver.Propositions;

   function Check (M : Model; SAT : out Boolean) return Formula;

   function Check (M : Model; SAT : out Boolean) return Formula is
      pragma Unreferenced (M);
   begin
      SAT := True;
      return [];
   end Check;

   package Empty_Theory is new Theory (Check);
   package DPLLT is new DPLL (Empty_Theory);

   P : Proposition := (+1 and +2) xor (+1 and +3);
   V : Variable := 3;
   F : constant Formula := To_CNF (P, V, Quadra);
   M : Model := [1 .. V => Unset];
begin
   Put_Line (Image (P));
   Destroy (P);
   Put_Line (Image (F));
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
