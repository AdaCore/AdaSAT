with Ada.Text_IO; use Ada.Text_IO;

with Solver.DPLL;
with Solver.Theory;

procedure Simple is
   use Solver;

   function Check
     (F : Formula; M : Model; SAT : out Boolean) return Formula;

   function Check
     (F : Formula; M : Model; SAT : out Boolean) return Formula
   is
      pragma Unreferenced (F, M);
   begin
      SAT := True;
      return (1 .. 0 => <>);
   end Check;

   package Empty_Theory is new Theory (Check);
   package DPLLT is new DPLL (Empty_Theory);

   F : constant Formula :=
     (new Literal_Array'(-1, +2),
      new Literal_Array'(-3, +4),
      new Literal_Array'(-6, -5, -2),
      new Literal_Array'(-5, +6),
      new Literal_Array'(+5, +7),
      new Literal_Array'(-1, +5, -7));
   M : Model := (1 .. 7 => Unset);
begin
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
end Simple;
