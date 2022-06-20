with Ada.Unchecked_Deallocation;

package body Solver.DPLL is
   subtype Variable_Or_Null is Variable'Base range 0 .. Variable'Last;

   type Decision_Array is array (Variable range <>) of Natural;
   type Antecedant_Array is array (Variable range <>) of Clause;
   type Literal_Mask is array (Literal range <>) of Boolean;

   type Formula_Access is access Formula;

   procedure Free is new Ada.Unchecked_Deallocation
     (Formula, Formula_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Literal_Array, Literal_Array_Access);

   function Unassigned_Count (M : Model) return Natural;
   --  Return the number of Unset variables in the given model

   function First_Unassigned (M : Model) return Variable_Or_Null;
   --  Return the first unassigned variable in the given model

   function Solve_No_Theory
     (F : in out Formula_Access; M : in out Model) return Boolean;
   --  Solve the given formula using the given mode, without invoking the
   --  theory at all. This is where the DPLL/CDCL algoritm is implemented.

   ----------------------
   -- Unassigned_Count --
   ----------------------

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

   ----------------------
   -- First_Unassigned --
   ----------------------

   function First_Unassigned (M : Model) return Variable_Or_Null is
   begin
      for I in M'Range loop
         if M (I) = Unset then
            return I;
         end if;
      end loop;
      return 0;
   end First_Unassigned;

   ---------------------
   -- Solve_No_Theory --
   ---------------------

   function Solve_No_Theory
     (F : in out Formula_Access; M : in out Model) return Boolean
   is
      Unassigned_Left    : Natural := Unassigned_Count (M);
      --  Track the number of variables that are not yet set

      Decision_Level     : Natural := 0;
      --  The current decision level

      Conflicting_Clause : Clause  := null;
      --  The clause that triggered the conflict

      Lit_Decisions      : Decision_Array :=
        (1 .. Variable (Unassigned_Left) => 0);
      --  Maps each variable to the decision level in which a value was
      --  set for it.

      Lit_Antecedants    : Antecedant_Array :=
        (1 .. Variable (Unassigned_Left) => null);
      --  Maps each variable to its antecedant clause: if the variable was
      --  assigned a value through unit propagation, this is the clause that
      --  was unit. If the variable was assigned a value through a decision,
      --  its antecedant is null.

      procedure Assign
        (Var : Variable; Value : Boolean; Antecedant : Clause);
      --  Assigns a value to the given variable, updating the appropriate
      --  data structures.

      procedure Unassign (Var : Variable);
      --  Unassigns a variable, updating the appropriate data structures

      function Unit_Propagate return Boolean;
      --  Implements the BCP routine.

      function Backtrack return Boolean with Unreferenced;
      --  Implements chronological backtracking. This is not used ATM but
      --  is kept here to allow experiments.

      function Backjump return Boolean;
      --  Implements non-chronological backtracking through conflict analysis.

      function Resolve
        (Left, Right : Clause;
         Pivot       : Variable) return Clause;
      --  Implements the resolution rule: Assuming that Left and Right both
      --  contain an occurrence of Pivot, return a new clause (the resolvent)
      --  which is the concatenation of the Left and Right clauses without
      --  occurrences of the pivot.

      procedure Decide;
      --  Performs the decision to set a yet unset variable

      ------------
      -- Assign --
      ------------

      procedure Assign
        (Var : Variable; Value : Boolean; Antecedant : Clause)
      is
      begin
         M (Var) := (if Value then True else False);
         Lit_Decisions (Var) := Decision_Level;
         Lit_Antecedants (Var) := Antecedant;
         Unassigned_Left := Unassigned_Left - 1;
      end Assign;

      --------------
      -- Unassign --
      --------------

      procedure Unassign (Var : Variable) is
      begin
         M (Var) := Unset;
         Lit_Decisions (Var) := 0;
         Unassigned_Left := Unassigned_Left + 1;
      end Unassign;

      --------------------
      -- Unit_Propagate --
      --------------------

      function Unit_Propagate return Boolean is
         Unit_Clause_Found : Boolean := True;
      begin
         while Unit_Clause_Found loop
            Unit_Clause_Found := False;

            for C of F.all loop
               declare
                  Unset_Count : Natural := 0;
                  Last_Unset  : Literal;
                  Is_Sat      : Boolean := False;
               begin
                  for L of C.all loop
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
                     --  This clause is SAT, continue
                     null;
                  elsif Unset_Count = 1 then
                     --  We found a unit clause, choose the right value to
                     --  satisfy it.
                     Assign (abs Last_Unset, Last_Unset > 0, C);
                     Unit_Clause_Found := True;
                  elsif Unset_Count = 0 then
                     --  If all variables were set but the formula is not SAT,
                     --  it is necessarily UNSAT.
                     Conflicting_Clause := C;
                     return False;
                  end if;
               end;
            end loop;
         end loop;
         return True;
      end Unit_Propagate;

      ---------------
      -- Backtrack --
      ---------------

      function Backtrack return Boolean is
         First : Variable := M'Last;
         Value : Variable_Value;
      begin
         if Decision_Level <= 0 then
            return False;
         end if;

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
         Assign (First, (if Value in True then False else True), null);
         return True;
      end Backtrack;

      --------------
      -- Backjump --
      --------------

      function Backjump return Boolean is
         Found         : Natural := 0;
         Pivot         : Variable_Or_Null := 0;
         Learnt_Clause : Clause :=
            new Literal_Array'(Conflicting_Clause.all);
      begin
         if Decision_Level <= 0 then
            return False;
         end if;

         while True loop
            Found := 0;

            --  Find all the variables that were set at this decision level
            --  and choose a pivot among those.
            for Lit of Learnt_Clause.all loop
               if Lit_Decisions (abs Lit) = Decision_Level then
                  Found := Found + 1;
                  if Lit_Antecedants (abs Lit) /= null then
                     Pivot := abs Lit;
                  end if;
               end if;
            end loop;

            if Found = 1 then
               exit;
            end if;

            --  Update the learnt clause by resolving it with the antecendant
            --  of the pivot.
            declare
               Old_Learnt_Clause : Clause := Learnt_Clause;
            begin
               Learnt_Clause := Resolve
                 (Learnt_Clause, Lit_Antecedants (Pivot), Pivot);
               Free (Old_Learnt_Clause);
            end;
         end loop;

         --  Find the decision level to which we should backjump by taking
         --  the maximum decision level among the literals of the learnt
         --  clause.
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

         --  Unset all the variables that were set at a decision level higher
         --  than the one we are backjumping to.
         for Var in Lit_Decisions'Range loop
            if Lit_Decisions (Var) > Decision_Level then
               Unassign (Var);
            end if;
         end loop;

         --  Add the learnt clause to the formula
         declare
            Old_Formula : Formula_Access := F;
         begin
            F := new Formula'(F.all & Learnt_Clause);
            Free (Old_Formula);
         end;

         return True;
      end Backjump;

      -------------
      -- Resolve --
      -------------

      function Resolve
        (Left, Right : Clause;
         Pivot       : Variable) return Clause
      is
         New_Clause : Literal_Array := Left.all & Right.all;
         Index : Natural := 1;
         Last  : Natural := New_Clause'Last;
         Seen  : Literal_Mask (-M'Last .. +M'Last) := (others => False);
      begin
         while Index <= Last loop
            if abs New_Clause (Index) = Pivot then
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

      ------------
      -- Decide --
      ------------

      procedure Decide is
         Var : constant Variable := First_Unassigned (M);
      begin
         Decision_Level := Decision_Level + 1;
         Assign (Var, True, null);
      end Decide;
   begin
      --  Perform initial BCP: the formula might be resolvable without
      --  making any decision.
      if not Unit_Propagate then
         return False;
      end if;

      --  While there are still variables that have not been set,
      --  make a decision and propagate as much as possible, or backjump if
      --  necessary.
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

   ------------
   --  Solve --
   ------------

   function Solve (F : Formula; M : in out Model) return Boolean is
      Orig_Model : constant Model := M;
      Updated    : Formula_Access := new Formula'(F);
   begin
      --  Solve the pure SAT problem first. Then, check if the theory accepts
      --  the model found. If not, restart but update the formula with the
      --  theory-provided formula.
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
            M := Orig_Model;
            Updated := new Formula'(Old_Formula & New_Clause);
         end;
      end loop;
      Free (Updated);
      return False;
   end Solve;
end Solver.DPLL;
