--  Translated from https://nickp.svbtle.com/sudoku-satsolver

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

with Solver.DPLL;
with Solver.Theory;

procedure Sudoku is
   use Solver;

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

   N : constant := 9;
   M : constant := 3;

   function Transform (I, J, K : Natural) return Variable is
     (Variable ((I - 1) * N * N + (J - 1) * N + K));

   package Clause_Vectors is new Ada.Containers.Vectors
     (Positive, Clause);

   Formula_Builder : Clause_Vectors.Vector;

   type Var_Array is array (Positive range <>) of Variable;

   procedure Exactly_One (Vars : Var_Array);
   procedure Exactly_One (Vars : Var_Array) is
   begin
      Formula_Builder.Append (new Literal_Array'(for V of Vars => +V));

      for I in Vars'First .. Vars'Last loop
         for J in I + 1 .. Vars'Last loop
            Formula_Builder.Append
              (new Literal_Array'(-Vars (I), -Vars (J)));
         end loop;
      end loop;
   end Exactly_One;

   procedure Read_Sudoku;
   procedure Read_Sudoku is
      Input_File : File_Type;
      Char       : Character;
      Value      : Natural;
   begin
      Open
         (File => Input_File,
          Mode => In_File,
          Name => Ada.Command_Line.Argument (1));
      for Y in 1 .. N loop
         for X in 1 .. N loop
            Get (File => Input_File, Item => Char);
            Value := Natural'Value ("" & Char);
            if Value /= 0 then
               Formula_Builder.Append
                 (new Literal_Array'[+Transform (Y, X, Value)]);
            end if;
         end loop;
      end loop;
      Close (File => Input_File);
   end Read_Sudoku;

   type Formula_Access is access Formula;

   F   : Formula_Access;
   Sol : Model := [1 .. N * N * N => Unset];
begin
   for I in 1 .. N loop
      for S in 1 .. N loop
         Exactly_One ([for J in 1 .. N => Transform (I, J, S)]);
         Exactly_One ([for J in 1 .. N => Transform (J, I, S)]);
      end loop;
      for J in 1 .. N loop
         Exactly_One ([for K in 1 .. N => Transform (I, J, K)]);
      end loop;
   end loop;

   for K in 1 .. N loop
      for X in 1 .. M loop
         for Y in 1 .. M loop
            declare
               Vars : Var_Array (1 .. 9);
               Iter : Natural := 1;
            begin
               for I in 1 .. M loop
                  for J in 1 .. M loop
                     Vars (Iter) := Transform
                       ((Y - 1) * M + I, (X - 1) * M + J, K);
                     Iter := Iter + 1;
                  end loop;
               end loop;
               Exactly_One (Vars);
            end;
         end loop;
      end loop;
   end loop;

   Read_Sudoku;

   F := new Formula'
     (for I in 1 .. Natural (Formula_Builder.Length) => Formula_Builder (I));

   if DPLLT.Solve (F.all, Sol) then
      Put_Line ("Solved");
   else
      Put_Line ("Failed solving");
   end if;
   for I in 1 .. N loop
      for J in 1 .. N loop
         for K in 1 .. N loop
            if Sol (Transform (I, J, K)) in True then
               Put (K'Image);
            end if;
         end loop;
      end loop;
      New_Line;
   end loop;
   New_Line;
   Put_Line (Satisfies (F.all, Sol)'Image);
end Sudoku;
