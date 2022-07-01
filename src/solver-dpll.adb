with Support.Vectors;
with Ada.Unchecked_Deallocation;

package body Solver.DPLL is
   type Decision_Array is array (Variable_Or_Null range <>) of Natural;
   type Antecedant_Array is array (Variable_Or_Null range <>) of Clause;
   type Literal_Mask is array (Literal range <>) of Boolean;

   procedure Free is new Ada.Unchecked_Deallocation
     (Literal_Array, Literal_Array_Access)
      with Unreferenced;

   package Clause_Vectors is new Support.Vectors
     (Clause, Formula);

   package Literal_Vectors is new Support.Vectors
     (Literal, Literal_Array);

   function Get_Literal_Vector_Array is new Literal_Vectors.Internal_Array
     (Literal_Array_Access);

   type Literal_To_Clause_Map is array (Literal range <>) of
      aliased Clause_Vectors.Vector;

   type Internal_Formula (First, Last : Literal) is record
      Clauses     : aliased Clause_Vectors.Vector;
      Occurs_List : Literal_To_Clause_Map (First .. Last);
   end record;
   --  An internal formula is the internal representation of a formula,
   --  which uses a vector for clauses so as to easily append new ones.
   --  It also maintaints some data structures around the clauses such as
   --  the occurrence list, to optimize the different routines.

   procedure Destroy (F : in out Internal_Formula);
   --  Free the memory allocated for the formula's internal structures.
   --  Does not free the inner clauses.

   procedure Append_Clause
     (F : in out Internal_Formula; C : Clause);
   --  Append the given clause to the internal formula, updating its
   --  data structures.

   procedure Append_Formula
     (F : in out Internal_Formula; Other : Formula);
   --  Append the given formula to the internal formula, updating its
   --  data structures.

   function Unassigned_Count (M : Model) return Natural;
   --  Return the number of Unset variables in the given model

   function First_Unassigned
     (M : Model; From : Variable) return Variable_Or_Null;
   --  Return the first unassigned variable in the given model
   --  Start looking after `From`.

   function Solve_Internal
     (F : in out Internal_Formula; M : in out Model) return Boolean;
   --  Solve the given formula with the given partial model.
   --  This is where the DPLL/CDCL algorithm is implemented.

   procedure Destroy (F : in out Internal_Formula) is
   begin
      F.Clauses.Destroy;
      for V of F.Occurs_List loop
         V.Destroy;
      end loop;
   end Destroy;

   -------------------
   -- Append_Clause --
   -------------------

   procedure Append_Clause
     (F : in out Internal_Formula; C : Clause)
   is
   begin
      F.Clauses.Append (C);
      for Lit of C.all loop
         F.Occurs_List (Lit).Append (C);
      end loop;
   end Append_Clause;

   --------------------
   -- Append_Formula --
   --------------------

   procedure Append_Formula
     (F : in out Internal_Formula; Other : Formula)
   is
   begin
      for C of Other loop
         Append_Clause (F, C);
      end loop;
   end Append_Formula;

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

   function First_Unassigned
     (M : Model; From : Variable) return Variable_Or_Null
   is
   begin
      for I in From .. M'Last loop
         if M (I) in Unset then
            return I;
         end if;
      end loop;
      return 0;
   end First_Unassigned;

   --------------------
   -- Solve_Internal --
   --------------------

   function Solve_Internal
     (F : in out Internal_Formula; M : in out Model) return Boolean
   is
      Unassigned_Left    : Natural := Unassigned_Count (M);
      --  Track the number of variables that are not yet set

      First_Unset        : Variable := M'First;
      --  Track the closest variable which can be decided next

      Decision_Level     : Natural := 0;
      --  The current decision level

      Conflicting_Clause : Clause  := null;
      --  The clause that triggered the conflict

      Lit_Decisions      : Decision_Array :=
        (1 .. Variable_Or_Null (Unassigned_Left) => 0);
      --  Maps each variable to the decision level in which a value was
      --  set for it.

      Lit_Antecedants    : Antecedant_Array :=
        (1 .. Variable_Or_Null (Unassigned_Left) => null);
      --  Maps each variable to its antecedant clause: if the variable was
      --  assigned a value through unit propagation, this is the clause that
      --  was unit. If the variable was assigned a value through a decision,
      --  its antecedant is null.

      To_Propagate : Literal_Vectors.Vector;
      --  The list of literals that need to be propagated during the next
      --  call to Unit_Propagate.

      procedure Assign
        (Var : Variable; Value : Boolean; Antecedant : Clause);
      --  Assigns a value to the given variable, updating the appropriate
      --  data structures.

      procedure Unassign (Var : Variable);
      --  Unassigns a variable, updating the appropriate data structures

      procedure Unassign_All (Level : Natural);
      --  Unassigns all variables that have a decision level higher than
      --  the given level.

      procedure Add_To_Propagate (L : Literal);
      --  Include the given variable in the list of variables that must be
      --  propagated during the next round of Unit_Propagate.

      function Unit_Propagate return Boolean;
      --  Implements the BCP routine.

      function Backtrack return Boolean with Unreferenced;
      --  Implements chronological backtracking. This is not used ATM but
      --  is kept here to allow experiments.

      function Backjump return Boolean;
      --  Implements non-chronological backtracking through conflict analysis

      procedure Resolve
        (Left        : in out Literal_Vectors.Vector;
         Right       : Clause;
         Mask        : in out Literal_Mask;
         Pivot_Index : Natural);
      --  Implements the resolution rule: Assuming that Left and Right both
      --  contain an occurrence of Pivot, update the Left clause to be the
      --  resolvent, that is the concatenation of the Left and Right clauses
      --  without occurrences of the pivot.
      --  Mask should map literals that appear in the Left clause to True and
      --  the rest to False.

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
         Add_To_Propagate ((if Value then -Var else +Var));
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

      ------------------
      -- Unassign_All --
      ------------------

      procedure Unassign_All (Level : Natural) is
         Found_First : Boolean := False;
      begin
         for Var in Lit_Decisions'Range loop
            if Lit_Decisions (Var) > Level then
               Unassign (Var);
               if not Found_First then
                  Found_First := True;
                  First_Unset := Var;
               end if;
            end if;
         end loop;
      end Unassign_All;

      ----------------------
      -- Add_To_Propagate --
      ----------------------

      procedure Add_To_Propagate (L : Literal) is
      begin
         for I in 1 .. To_Propagate.Length loop
            if L = To_Propagate.Get (I) then
               return;
            end if;
         end loop;
         To_Propagate.Append (L);
      end Add_To_Propagate;

      --------------------
      -- Unit_Propagate --
      --------------------

      function Unit_Propagate return Boolean is
         type Clause_Vector_Access is access all Clause_Vectors.Vector;

         Clauses_Access    : Clause_Vector_Access := null;
         Being_Propagated  : Literal := 0;
      begin
         pragma Assert (To_Propagate.Length >= 1);
         while not To_Propagate.Is_Empty loop
            Being_Propagated := To_Propagate.Pop;

            if Being_Propagated = 0 then
               Clauses_Access := F.Clauses'Access;
            else
               Clauses_Access := F.Occurs_List (Being_Propagated)'Access;
            end if;

            for J in 1 .. Clauses_Access.Length loop
               declare
                  C           : constant Clause := Clauses_Access.Get (J);
                  Unset_Count : Natural := 0;
                  Last_Unset  : Literal := 0;
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
                           if L /= Last_Unset then
                              Unset_Count := Unset_Count + 1;
                              Last_Unset := L;
                           end if;
                     end case;
                  end loop;

                  if Is_Sat then
                     --  This clause is SAT, continue
                     null;
                  elsif Unset_Count = 1 then
                     --  We found a unit clause, choose the right value to
                     --  satisfy it.
                     Assign (abs Last_Unset, Last_Unset > 0, C);
                  elsif Unset_Count = 0 then
                     --  If all variables were set but the formula is not SAT,
                     --  it is necessarily UNSAT.
                     Conflicting_Clause := C;
                     To_Propagate.Clear;
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
         Assign (First, Value in False, null);
         First_Unset := First + 1;
         return True;
      end Backtrack;

      --------------
      -- Backjump --
      --------------

      function Backjump return Boolean is
         Found         : Natural := 0;
         Pivot         : Variable_Or_Null := 0;
         Pivot_Index   : Natural;
         Learnt_Clause : Literal_Vectors.Vector;
         Mask          : Literal_Mask (-M'Last .. +M'Last) :=
            (others => False);
      begin
         if Decision_Level <= 0 then
            return False;
         end if;

         Learnt_Clause.Reserve (Conflicting_Clause.all'Length);
         for Lit of Conflicting_Clause.all loop
            if not Mask (Lit) then
               Learnt_Clause.Append (Lit);
               Mask (Lit) := True;
            end if;
         end loop;

         while True loop
            Found := 0;
            Pivot := 0;

            --  Find all the variables that were set at this decision level
            --  and choose a pivot among those.
            for Lit_Index in 1 .. Learnt_Clause.Length loop
               declare
                  Var : constant Variable := abs Learnt_Clause.Get (Lit_Index);
               begin
                  if Lit_Decisions (Var) = Decision_Level then
                     Found := Found + 1;
                     if Pivot = 0 and then
                        Lit_Antecedants (Var) /= null
                     then
                        Pivot       := Var;
                        Pivot_Index := Lit_Index;
                     end if;
                  end if;
               end;
            end loop;

            if Found = 1 then
               exit;
            end if;

            --  Update the learnt clause by resolving it with the antecendant
            --  of the pivot.
            Resolve
              (Learnt_Clause, Lit_Antecedants (Pivot), Mask, Pivot_Index);
         end loop;

         --  Find the decision level to which we should backjump by taking
         --  the maximum decision level among the literals of the learnt
         --  clause.
         declare
            Backjump_Decision_Level : Natural := 0;
            Lit_Decision_Level      : Natural := 0;

            Literal_Count : constant Natural := Learnt_Clause.Length;
         begin
            for I in 1 .. Literal_Count loop
               Lit_Decision_Level := Lit_Decisions (abs Learnt_Clause.Get (I));

               if Lit_Decision_Level /= Decision_Level and then
                  Lit_Decision_Level > Backjump_Decision_Level
               then
                  Backjump_Decision_Level := Lit_Decision_Level;
               end if;
            end loop;

            Decision_Level := Backjump_Decision_Level;
         end;

         Add_To_Propagate (Learnt_Clause.Get (1));

         --  Unset all the variables that were set at a decision level higher
         --  than the one we are backjumping to.
         Unassign_All (Decision_Level);

         --  Add the learnt clause to the formula
         Append_Clause (F, Get_Literal_Vector_Array (Learnt_Clause));

         return True;
      end Backjump;

      -------------
      -- Resolve --
      -------------

      procedure Resolve
        (Left        : in out Literal_Vectors.Vector;
         Right       : Clause;
         Mask        : in out Literal_Mask;
         Pivot_Index : Natural)
      is
         Pivot : constant Variable := abs Left.Get (Pivot_Index);
      begin
         Left.Swap_And_Remove (Pivot_Index);

         --  Assume pivot is seen so that the following loop
         --  never considers adding the pivot back into Left.
         Mask (+Pivot) := True;
         Mask (-Pivot) := True;

         for Lit of Right.all loop
            if not Mask (Lit) then
               Mask (Lit) := True;
               Left.Append (Lit);
            end if;
         end loop;

         --  Now we can mark the pivot as not seen.
         Mask (+Pivot) := False;
         Mask (-Pivot) := False;
      end Resolve;

      ------------
      -- Decide --
      ------------

      procedure Decide is
         Var : constant Variable := First_Unassigned (M, First_Unset);
      begin
         First_Unset := Var + 1;
         Decision_Level := Decision_Level + 1;
         Assign (Var, True, null);
      end Decide;

   begin
      --  Perform initial BCP: the formula might be resolvable without
      --  making any decision.
      Add_To_Propagate (0);
      if not Unit_Propagate then
         return False;
      end if;

      while True loop
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

         --  The pure SAT problem was solved. Now, check if the theory accepts
         --  the model found. If not, restart but update the formula with the
         --  theory-provided reason for conflict.
         declare
            OK          : Boolean;
            Explanation : constant Formula := T.Check (M, OK);
         begin
            if OK then
               return True;
            elsif Explanation'Length = 0 then
               return False;
            end if;

            declare
               Backjump_Decision_Level : Natural := 0;
               Lit_Decision_Level      : Natural := 0;
            begin
               for C of Explanation loop
                  for I in 1 .. C.all'Length loop
                     Lit_Decision_Level := Lit_Decisions (abs C (I));

                     if Lit_Decision_Level /= Decision_Level and then
                        Lit_Decision_Level > Backjump_Decision_Level
                     then
                        Backjump_Decision_Level := Lit_Decision_Level;
                     end if;
                  end loop;
                  Add_To_Propagate (C (1));
               end loop;

               Decision_Level := Backjump_Decision_Level;
            end;

            Unassign_All (Decision_Level);

            Append_Formula (F, Explanation);

            while True loop
               if Unit_Propagate then
                  exit;
               elsif not Backjump then
                  return False;
               end if;
            end loop;
         end;
      end loop;
      return True;
   end Solve_Internal;

   ------------
   --  Solve --
   ------------

   function Solve (F : Formula; M : in out Model) return Boolean is
      Is_Empty   : constant Boolean := M'Length = 0;
      First      : constant Literal := (if Is_Empty then 1 else -M'Last);
      Last       : constant Literal := (if Is_Empty then 0 else +M'Last);
      Internal   : Internal_Formula (First, Last);
      Result     : Boolean;
   begin
      Internal.Clauses.Reserve (F'Length);
      Append_Formula (Internal, F);
      Result := Solve_Internal (Internal, M);
      Destroy (Internal);
      return Result;
   end Solve;
end Solver.DPLL;
