with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Solver.DPLL is
   type Decision_Array is array (Positive range <>) of Natural;
   type Literal_Mask is array (Integer range <>) of Boolean;

   type Formula_Access is access Formula;

   procedure Free is new Ada.Unchecked_Deallocation
     (Formula, Formula_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Literal_Array, Literal_Options);

   function Unassigned_Count (M : Model) return Natural;

   function First_Unassigned (M : Model) return Natural;

   function Solve_No_Theory
     (F : in out Formula_Access; M : in out Model) return Boolean;

   function Unassigned_Count (M : Model) return Natural
   is
      Count : Natural := 0;
   begin
      for I in M'Range loop
         if M (I) = Unset then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Unassigned_Count;

   function First_Unassigned (M : Model) return Natural is
   begin
      for I in M'Range loop
         if M (I) = Unset then
            return I;
         end if;
      end loop;
      return 0;
   end First_Unassigned;

   function Solve_No_Theory
     (F : in out Formula_Access; M : in out Model) return Boolean
   is
      Unassigned_Left    : Natural := Unassigned_Count (M);
      Lit_Decisions      : Decision_Array := (1 .. Unassigned_Left => 0);
      Lit_Antecedants    : Decision_Array := (1 .. Unassigned_Left => 0);
      Decision_Level     : Natural := 0;
      Conflicting_Clause : Natural := 0;

      procedure Assign
        (Lit_Index : Natural; Value : Boolean; Antecedant : Natural);

      procedure Unassign (Lit_Index : Natural);

      function Unit_Propagate return Boolean;

      function Backtrack return Boolean;
      function Backjump return Boolean;

      function Resolve
        (Left, Right : Literal_Options;
         Pivot_Lit   : Natural) return Literal_Options;

      procedure Decide;

      procedure Assign
        (Lit_Index : Natural; Value : Boolean; Antecedant : Natural)
      is
      begin
         M (Lit_Index) := (if Value then True else False);
         Lit_Decisions (Lit_Index) := Decision_Level;
         Lit_Antecedants (Lit_Index) := Antecedant;
         Unassigned_Left := Unassigned_Left - 1;
      end Assign;

      procedure Unassign (Lit_Index : Natural) is
      begin
         M (Lit_Index) := Unset;
         Lit_Decisions (Lit_Index) := 0;
         Unassigned_Left := Unassigned_Left + 1;
      end Unassign;

      function Unit_Propagate return Boolean is
         Unit_Clause_Found : Boolean := True;
      begin
         while Unit_Clause_Found loop
            Unit_Clause_Found := False;

            for C in F.all'Range loop
               declare
                  Dis         : constant Literal_Options := F.all (C);
                  Unset_Count : Natural := 0;
                  Last_Unset  : Integer;
                  Is_Sat      : Boolean := False;
               begin
                  for L of Dis.all loop
                     case M (abs L) is
                        when True =>
                           if L > 0 then
                              Is_Sat := True;
                              exit;
                           end if;
                        when False =>
                           if L < 0 then
                              Is_Sat := True;
                              exit;
                           end if;
                        when Unset =>
                           Unset_Count := Unset_Count + 1;
                           Last_Unset := L;
                     end case;
                  end loop;

                  if Is_Sat then
                     null;  -- continue
                  elsif Unset_Count = 1 then
                     Put_Line ("Propagate");
                     Assign (abs Last_Unset, Last_Unset > 0, C);
                     Unit_Clause_Found := True;
                  elsif Unset_Count = 0 then
                     Conflicting_Clause := C;
                     return False;
                  end if;
               end;
            end loop;
         end loop;
         return True;
      end Unit_Propagate;

      function Backtrack return Boolean is
         First : Positive := M'Last;
         Value : Literal_Value;
      begin
         if Decision_Level <= 0 then
            return False;
         end if;

         Put_Line ("Backtrack");
         Decision_Level := Decision_Level - 1;
         for Index in M'Range loop
            if Lit_Decisions (Index) > Decision_Level then
               if Index < First then
                  First := Index;
                  Value := M (Index);
               end if;
               Unassign (Index);
            end if;
         end loop;
         Put_Line ("Redecide " & First'Image);
         Assign (First, (if Value in True then False else True), 0);
         return True;
      end Backtrack;

      function Backjump return Boolean is
         Found             : Natural := 0;
         Pivot             : Natural := 0;
         Learnt_Clause     : Literal_Options :=
            new Literal_Array'(F (Conflicting_Clause).all);
      begin
         if Decision_Level <= 0 then
            return False;
         end if;

         Put_Line ("Backjump");
         Put_Line ("Conflicting clause :" & Conflicting_Clause'Image);
         while True loop
            Found := 0;

            for Lit of Learnt_Clause.all loop
               Put_Line
                 ("Antecedant of " & Lit'Image & " is "
                  & Lit_Antecedants (abs Lit)'Image);
               if Lit_Decisions (abs Lit) = Decision_Level then
                  Found := Found + 1;
                  if Lit_Antecedants (abs Lit) /= 0 then
                     Pivot := abs Lit;
                  end if;
               end if;
            end loop;

            if Found = 1 then
               exit;
            end if;

            declare
               Old_Learnt_Clause : Literal_Options := Learnt_Clause;
            begin
               Learnt_Clause := Resolve
                 (Learnt_Clause, F (Lit_Antecedants (Pivot)), Pivot);
               Free (Old_Learnt_Clause);
            end;
         end loop;
         Put ("Learnt clause : ");
         for Lit of Learnt_Clause.all loop
            Put (Lit'Image);
            Put (" ");
         end loop;
         New_Line;

         declare
            Backjump_Decision_Level : Natural := 0;
            Lit_Decision_Level      : Natural := 0;
         begin
            for Lit of Learnt_Clause.all loop
               Lit_Decision_Level := Lit_Decisions (abs Lit);
               if Lit_Decision_Level /= Decision_Level and then
                  Lit_Decision_Level > Backjump_Decision_Level
               then
                  Backjump_Decision_Level := Lit_Decision_Level;
               end if;
            end loop;
            Decision_Level := Backjump_Decision_Level;
         end;

         for Lit in Lit_Decisions'Range loop
            if Lit_Decisions (Lit) > Decision_Level then
               Unassign (Lit);
            end if;
         end loop;

         declare
            Old_Formula : Formula_Access := F;
         begin
            F := new Formula'(F.all & Learnt_Clause);
            Free (Old_Formula);
         end;

         Put_Line ("Backjumping to level :" & Decision_Level'Image);
         return True;
      end Backjump;

      function Resolve
        (Left, Right : Literal_Options;
         Pivot_Lit   : Natural) return Literal_Options
      is
         New_Clause : Literal_Array := Left.all & Right.all;
         Index : Natural := 1;
         Last  : Natural := New_Clause'Last;
         Seen  : Literal_Mask (-M'Last .. M'Last) := (others => False);
      begin
         while Index <= Last loop
            if abs New_Clause (Index) = Pivot_Lit then
               New_Clause (Index) := New_Clause (Last);
               Last := Last - 1;
            elsif Seen (New_Clause (Index)) then
               New_Clause (Index) := New_Clause (Last);
               Last := Last - 1;
            else
               Seen (New_Clause (Index)) := True;
               Index := Index + 1;
            end if;
         end loop;
         return new Literal_Array'(New_Clause (1 .. Last));
      end Resolve;

      procedure Decide is
         Index : constant Natural := First_Unassigned (M);
      begin
         Put_Line ("Decide " & Index'Image);
         Decision_Level := Decision_Level + 1;
         Assign (Index, True, 0);
      end Decide;
   begin
      if not Unit_Propagate then
         return False;
      end if;
      while Unassigned_Left > 0 loop
         Decide;
         while True loop
            if Unit_Propagate then
               exit;
            elsif not Backjump then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Solve_No_Theory;

   function Solve (F : Formula; M : in out Model) return Boolean is
      Orig_Model : constant Model := M;
      Updated    : Formula_Access := new Formula'(F);
   begin
      while Solve_No_Theory (Updated, M) loop
         declare
            OK          : Boolean;
            Old_Formula : constant Formula := Updated.all;
            New_Clause  : constant Formula := T.Check (Old_Formula, M, OK);
         begin
            Free (Updated);
            if OK then
               return True;
            elsif New_Clause'Length = 0 then
               return False;
            end if;
            Put_Line ("T-Restart");
            M := Orig_Model;
            Updated := new Formula'(Old_Formula & New_Clause);
         end;
      end loop;
      Free (Updated);
      return False;
   end Solve;
end Solver.DPLL;
