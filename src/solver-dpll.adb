with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Solver.DPLL is
   type Decision_Array is array (Positive range <>) of Natural;

   function Unassigned_Count (M : Model) return Natural;

   function First_Unassigned (M : Model) return Natural;

   function Solve_No_Theory (F : Formula; M : in out Model) return Boolean;

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

   function Solve_No_Theory (F : Formula; M : in out Model) return Boolean is
      Unassigned_Left : Natural := Unassigned_Count (M);
      Lit_Decisions   : Decision_Array := (1 .. Unassigned_Left => 0);
      Decision_Level  : Natural := 0;

      procedure Assign (Lit_Index : Natural; Value : Boolean);
      procedure Unassign (Lit_Index : Natural);
      function Unit_Propagate return Boolean;
      function Backtrack return Boolean;
      procedure Decide;

      procedure Assign (Lit_Index : Natural; Value : Boolean) is
      begin
         M (Lit_Index) := (if Value then True else False);
         Lit_Decisions (Lit_Index) := Decision_Level;
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

            for Dis of F loop
               declare
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
                     Assign (abs Last_Unset, Last_Unset > 0);
                     Unit_Clause_Found := True;
                  elsif Unset_Count = 0 then
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
         Assign (First, (if Value in True then False else True));
         return True;
      end Backtrack;

      procedure Decide is
         Index : constant Natural := First_Unassigned (M);
      begin
         Put_Line ("Decide");
         Decision_Level := Decision_Level + 1;
         Assign (Index, True);
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
            elsif not Backtrack then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Solve_No_Theory;

   type Formula_Access is access Formula;

   procedure Free is new Ada.Unchecked_Deallocation (Formula, Formula_Access);

   function Solve (F : Formula; M : in out Model) return Boolean is
      Orig_Model : constant Model := M;
      Updated    : Formula_Access := new Formula'(F);
   begin
      while Solve_No_Theory (Updated.all, M) loop
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
