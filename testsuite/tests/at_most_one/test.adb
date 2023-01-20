with Ada.Text_IO; use Ada.Text_IO;

with AdaSAT.Builders;
with AdaSAT.Formulas;
with AdaSAT.Helpers;

--  Test the built-in handling of At-Most-One constraints

procedure Test is
   use AdaSAT;
   use AdaSAT.Builders;
   use AdaSAT.Formulas;

   procedure Solve (B : in out Formula_Builder; Name : String);
   --  Simple helper to reduce the boilerplate when solving multiple formulas
   --  through formula builders

   -----------
   -- Solve --
   -----------

   procedure Solve (B : in out Formula_Builder; Name : String) is
      M : Model := (1 .. 7 => Unset);
   begin
      if Helpers.DPLL_Solve (B.Build, M) then
         Put_Line ("Solved " & Name);
      else
         Put_Line ("Failed solving " & Name);
      end if;
      for E of M loop
         Put (E'Image);
         Put (" ");
      end loop;
      New_Line;
   end Solve;

   B_1, B_2, B_3, B_4 : Formula_Builder;
begin
   B_1.Add_At_Most_One (1, 5);
   B_1.Add (new Literal_Array'(1 => -1));
   B_1.Add (new Literal_Array'(1 => -6));
   B_2 := B_1.Copy;
   B_4 := B_1.Copy;
   Solve (B_1, "B_1");

   B_2.Add (new Literal_Array'(1 => +4));
   B_3 := B_2.Copy;
   Solve (B_2, "B_2");

   B_3.Add (new Literal_Array'(1 => +5));
   Solve (B_3, "B_3");

   B_4.Add (new Literal_Array'(1 => -2));
   B_4.Add (new Literal_Array'(1 => -3));
   B_4.Add (new Literal_Array'(1 => -4));
   B_4.Add (new Literal_Array'(1 => -5));
   Solve (B_4, "B_4");
end Test;
