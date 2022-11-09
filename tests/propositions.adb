with Ada.Text_IO; use Ada.Text_IO;

with AdaSAT.DPLL;
with AdaSAT.Formulas;
with AdaSAT.Propositions;
with AdaSAT.Theory;

procedure Propositions is
   use AdaSAT;
   use AdaSAT.Formulas;
   use AdaSAT.Propositions;

   type Empty_Context is null record;
   function Check
     (Ctx : in out Empty_Context;
      M   : Model;
      F   : in out Formula) return Boolean;

   function Check
     (Ctx : in out Empty_Context;
      M   : Model;
      F   : in out Formula) return Boolean
   is (True);

   package Empty_Theory is new Theory (Empty_Context, Check);
   package DPLLT is new DPLL (Empty_Theory);

   P : Proposition := (+1 and +2) xor (+1 and +3);
   V : Variable := 3;
   F : constant Formula := To_CNF (P, V, Quadra);
   M : Model := [1 .. V => Unset];
   C : Empty_Context;
begin
   Put_Line (Image (P));
   Destroy (P);
   Put_Line (Image (F));
   if DPLLT.Solve (F, C, M) then
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
