with Support.Vectors;
with Ada.Unchecked_Deallocation;

package body Solver.DPLL is
   subtype Variable_Or_Null is Variable'Base range 0 .. Variable'Last;
   type Variable_Array is array (Positive range <>) of Variable_Or_Null;

   type Decision_Array is array (Variable range <>) of Natural;
   type Antecedant_Array is array (Variable range <>) of Clause;
   type Literal_Mask is array (Literal range <>) of Boolean;

   procedure Free is new Ada.Unchecked_Deallocation
     (Literal_Array, Literal_Array_Access)
      with Unreferenced;

   package Clause_Vectors is new Support.Vectors
     (Clause, Formula);

   package Variable_Vectors is new Support.Vectors
     (Variable_Or_Null, Variable_Array);

   package Literal_Vectors is new Support.Vectors
     (Literal, Literal_Array);

   function Get_Literal_Vector_Array is new Literal_Vectors.Internal_Array
     (Literal_Array_Access);

   type Variable_To_Clause_Map is array (Variable range <>) of
      aliased Clause_Vectors.Vector;

   type Internal_Formula (Var_Count : Variable) is record
      Clauses     : aliased Clause_Vectors.Vector;
      Occurs_List : Variable_To_Clause_Map (1 .. Var_Count);
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

   function First_Unassigned (M : Model) return Variable_Or_Null;
   --  Return the first unassigned variable in the given model

   function Solve_No_Theory
     (F : in out Internal_Formula; M : in out Model) return Boolean;
   --  Solve the given formula using the given mode, without invoking the
   --  theory at all. This is where the DPLL/CDCL algoritm is implemented.

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
         F.Occurs_List (abs Lit).Append (C);
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
     (F : in out Internal_Formula; M : in out Model) return Boolean
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

      To_Propagate : Variable_Vectors.Vector;
      --  The list of variables that need to be propagated during the next
      --  call to Unit_Propagate.

      procedure Assign
        (Var : Variable; Value : Boolean; Antecedant : Clause);
      --  Assigns a value to the given variable, updating the appropriate
      --  data structures.

      procedure Unassign (Var : Variable);
      --  Unassigns a variable, updating the appropriate data structures

      procedure Add_To_Propagate (V : Variable_Or_Null);
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
         Add_To_Propagate (Var);
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

      ----------------------
      -- Add_To_Propagate --
      ----------------------

      procedure Add_To_Propagate (V : Variable_Or_Null) is
      begin
         for E of To_Propagate loop
            if E = V then
               return;
            end if;
         end loop;
         To_Propagate.Append (V);
      end Add_To_Propagate;

      --------------------
      -- Unit_Propagate --
      --------------------

      function Unit_Propagate return Boolean is
         type Clause_Vector_Access is access all Clause_Vectors.Vector;

         Clauses_Access    : Clause_Vector_Access := null;
         Being_Propagated  : Variable_Or_Null := 0;
      begin
         pragma Assert (To_Propagate.Length = 1);
         while not To_Propagate.Is_Empty loop
            Being_Propagated := To_Propagate.Pop;

            if Being_Propagated = 0 then
               Clauses_Access := F.Clauses'Access;
            else
               Clauses_Access := F.Occurs_List (Being_Propagated)'Access;
            end if;

            for C of Clauses_Access.all loop
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
         Assign (First, (if Value in True then False else True), null);
         return True;
      end Backtrack;

      --------------
      -- Backjump --
      --------------

      function Backjump return Boolean is
         Found         : Natural := 0;
         Lit_Index     : Positive;
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
            Lit_Index := 1;

            --  Find all the variables that were set at this decision level
            --  and choose a pivot among those.
            for Lit of Learnt_Clause loop
               if Lit_Decisions (abs Lit) = Decision_Level then
                  Found := Found + 1;
                  if Pivot = 0 and then
                     Lit_Antecedants (abs Lit) /= null
                  then
                     Pivot       := abs Lit;
                     Pivot_Index := Lit_Index;
                  end if;
               end if;
               Lit_Index := Lit_Index + 1;
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

         --  Unset all the variables that were set at a decision level higher
         --  than the one we are backjumping to.
         for Var in Lit_Decisions'Range loop
            if Lit_Decisions (Var) > Decision_Level then
               Unassign (Var);
            end if;
         end loop;

         --  Add the learnt clause to the formula
         Append_Clause (F, Get_Literal_Vector_Array (Learnt_Clause));

         Add_To_Propagate (0);
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
         Var : constant Variable := First_Unassigned (M);
      begin
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
      Internal   : Internal_Formula (M'Length);

      function Cleanup (Result : Boolean) return Boolean;
      --  Return the given value, but first cleanup the data structures
      --  allocated during this subprogram's run-time.

      function Cleanup (Result : Boolean) return Boolean is
      begin
         Destroy (Internal);
         return Result;
      end Cleanup;
   begin
      Internal.Clauses.Reserve (F'Length);
      Append_Formula (Internal, F);

      --  Solve the pure SAT problem first. Then, check if the theory accepts
      --  the model found. If not, restart but update the formula with the
      --  theory-provided formula.
      while Solve_No_Theory (Internal, M) loop
         declare
            OK           : Boolean;
            New_Formula  : constant Formula := T.Check (F, M, OK);
         begin
            if OK then
               return Cleanup (True);
            elsif New_Formula'Length = 0 then
               return Cleanup (False);
            end if;
            M := Orig_Model;
            Append_Formula (Internal, New_Formula);
         end;
      end loop;
      Destroy (Internal);
      return Cleanup (False);
   end Solve;
end Solver.DPLL;
