package body Solver.DPLL is
   type Decision_Array is array (Variable_Or_Null range <>) of Natural;
   type Antecedant_Array is array (Variable_Or_Null range <>) of Clause;
   type Literal_Mask is array (Literal range <>) of Boolean;

   type Watcher is record
      Blit     : Literal;
      Other    : Literal;
      Literals : Clause;
   end record;

   type Watcher_Array is array (Positive range <>) of Watcher;

   package Watcher_Vectors is new Support.Vectors (Watcher, Watcher_Array);

   type Literal_To_Watcher_Map is array (Literal range <>) of
      aliased Watcher_Vectors.Vector;

   type Internal_Formula (First, Last : Literal) is record
      Clauses     : aliased Watcher_Vectors.Vector;
      Occurs_List : Literal_To_Watcher_Map (First .. Last);
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
     (F        : in out Internal_Formula;
      Ctx      : in out T.User_Context;
      M        : in out Model;
      Min_Vars : Variable_Or_Null) return Boolean;
   --  Solve the given formula with the given partial model.
   --  This is where the DPLL/CDCL algorithm is implemented.

   procedure Destroy (F : in out Internal_Formula) is
      Mutable_C : Clause;
   begin
      for C of F.Clauses loop
         Mutable_C := C.Literals;
         Free (Mutable_C);
      end loop;
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
      W : Watcher := (0, 0, null);
   begin
      if C'Length > 0 then
         W.Blit := C (C'First);
         if C'Length = 2 then
            W.Other := C (C'Last);
         end if;
         W.Literals := C;
         for Lit of C.all loop
            F.Occurs_List (Lit).Append (W);
         end loop;
      end if;
      F.Clauses.Append (W);
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
     (F        : in out Internal_Formula;
      Ctx      : in out T.User_Context;
      M        : in out Model;
      Min_Vars : Variable_Or_Null) return Boolean
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

      Propagate_Mask : Literal_Mask (-M'Last .. +M'Last) := (others => False);
      --  A literal is set to True in this mask if it is present in the
      --  `To_Propagate` stack.

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

      procedure Clear_Propagation;
      --  Clears the propagation related data structure, in particular the
      --  propagation stack as well as the propagation mask.

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

      function Cleanup (Result : Boolean) return Boolean;
      --  Cleanup allocated resources and return the given boolean result

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
         if not Propagate_Mask (L) then
            Propagate_Mask (L) := True;
            To_Propagate.Append (L);
         end if;
      end Add_To_Propagate;

      -----------------------
      -- Clear_Propagation --
      -----------------------

      procedure Clear_Propagation is
      begin
         To_Propagate.Clear;
         Propagate_Mask := (others => False);
      end Clear_Propagation;

      function Val (X : Literal) return Variable_Value with Inline_Always;
      function Val (X : Literal) return Variable_Value is
      begin
         if X > 0 then
            return M (abs X);
         else
            return (case M (abs X) is
               when True => False,
               when False => True,
               when Unset => Unset);
         end if;
      end Val;

      --------------------
      -- Unit_Propagate --
      --------------------

      function Unit_Propagate return Boolean is
         type Watcher_Vector_Access is access all Watcher_Vectors.Vector;

         Watchers_Access  : Watcher_Vector_Access := null;
         Being_Propagated : Literal := 0;
      begin
         pragma Assert (To_Propagate.Length >= 1);
         while not To_Propagate.Is_Empty loop
            Being_Propagated := To_Propagate.Pop;
            Propagate_Mask (Being_Propagated) := False;

            if Being_Propagated = 0 then
               Watchers_Access := F.Clauses'Access;
            else
               Watchers_Access := F.Occurs_List (Being_Propagated)'Access;
            end if;

            for J in 1 .. Watchers_Access.Length loop
               declare
                  W : constant Watcher_Vectors.Element_Access :=
                     Watchers_Access.Get_Access (J);

                  Unset_Count : Natural  := 0;
                  Last_Unset  : Literal  := 0;
                  Is_Sat      : Boolean  := False;
                  Index       : Positive := 1;
               begin
                  if W.Other /= 0 then
                     case Val (W.Blit) is
                        when True =>
                           null;
                        when False =>
                           case Val (W.Other) is
                              when True =>
                                 Last_Unset := W.Blit;
                                 W.Blit := W.Other;
                                 W.Other := Last_Unset;
                              when False =>
                                 Conflicting_Clause := W.Literals;
                                 Clear_Propagation;
                                 return False;
                              when Unset =>
                                 Assign (abs W.Other, W.Other > 0, W.Literals);
                           end case;
                        when Unset =>
                           case Val (W.Other) is
                              when True =>
                                 Last_Unset := W.Blit;
                                 W.Blit := W.Other;
                                 W.Other := Last_Unset;
                              when False =>
                                 Assign (abs W.Blit, W.Blit > 0, W.Literals);
                              when Unset =>
                                 null;
                           end case;
                     end case;
                  elsif Val (W.Blit) not in True then
                     for L of W.Literals.all loop
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
                                 if Unset_Count >= 2 then
                                    exit;
                                 else
                                    Last_Unset := L;
                                 end if;
                              end if;
                        end case;
                        Index := Index + 1;
                     end loop;

                     if Is_Sat then
                        --  This clause is SAT, continue
                        W.Blit := W.Literals (Index);
                        if Index > 1 then
                           Last_Unset := W.Literals (Index);
                           W.Literals (Index) := W.Literals (W.Literals'First);
                           W.Literals (W.Literals'First) := Last_Unset;
                        end if;
                     elsif Unset_Count = 1 then
                        --  We found a unit clause, choose the right value to
                        --  satisfy it.
                        Assign (abs Last_Unset, Last_Unset > 0, W.Literals);
                        W.Blit := Last_Unset;
                     elsif Unset_Count = 0 then
                        --  If all variables were set but the formula is not
                        --  SAT, it is necessarily UNSAT.
                        Conflicting_Clause := W.Literals;
                        Clear_Propagation;
                        return False;
                     end if;
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

      -------------
      -- Cleanup --
      -------------

      function Cleanup (Result : Boolean) return Boolean is
      begin
         To_Propagate.Destroy;
         Destroy (F);
         return Result;
      end Cleanup;

   begin
      --  Perform initial BCP: the formula might be resolvable without
      --  making any decision.
      Add_To_Propagate (0);
      if not Unit_Propagate then
         return Cleanup (False);
      end if;

      while True loop
         --  While there are still variables that have not been set,
         --  make a decision and propagate as much as possible, or backjump if
         --  necessary.
         while Unassigned_Left > 0 and then First_Unset <= Min_Vars loop
            Decide;
            while True loop
               if Unit_Propagate then
                  exit;
               elsif not Backjump then
                  return Cleanup (False);
               end if;
            end loop;
         end loop;

         --  The pure SAT problem was solved. Now, check if the theory accepts
         --  the model found. If not, restart but update the formula with the
         --  theory-provided reason for conflict.
         declare
            OK          : Boolean;
            Explanation : constant Formula := T.Check (Ctx, M, OK);
         begin
            if OK then
               return Cleanup (True);
            elsif Explanation'Length = 0 then
               return Cleanup (False);
            end if;

            for C of Explanation loop
               if C'Length = 0 then
                  return Cleanup (False);
               end if;

               declare
                  Backjump_Decision_Level : Natural := 0;
                  Max_Decision_Level      : Natural := 0;
                  Lit_Decision_Level      : Natural := 0;
               begin
                  for Lit of C.all loop
                     Lit_Decision_Level := Lit_Decisions (abs Lit);

                     if Lit_Decision_Level > Max_Decision_Level then
                        Backjump_Decision_Level := Max_Decision_Level;
                        Max_Decision_Level := Lit_Decision_Level;
                     elsif Lit_Decision_Level > Backjump_Decision_Level then
                        Backjump_Decision_Level := Lit_Decision_Level;
                     end if;
                  end loop;
                  Add_To_Propagate (C (1));
                  Decision_Level := Natural'Min
                    (Decision_Level, Backjump_Decision_Level);
               end;
            end loop;

            Unassign_All (Decision_Level);

            Append_Formula (F, Explanation);

            while True loop
               if Unit_Propagate then
                  exit;
               elsif not Backjump then
                  return Cleanup (False);
               end if;
            end loop;
         end;
      end loop;
      return Cleanup (True);
   end Solve_Internal;

   ------------
   --  Solve --
   ------------

   function Solve
     (F   : Formula;
      Ctx : in out T.User_Context;
      M   : in out Model) return Boolean
   is
      Is_Empty : constant Boolean := M'Length = 0;
      First    : constant Literal := (if Is_Empty then 1 else -M'Last);
      Last     : constant Literal := (if Is_Empty then 0 else +M'Last);
      Internal : Internal_Formula (First, Last);
   begin
      Internal.Clauses.Reserve (F'Length);
      Append_Formula (Internal, F);
      return Solve_Internal (Internal, Ctx, M, M'Last);
   end Solve;

   --------------------
   --  Solve_Partial --
   --------------------

   function Solve_Partial
     (F        : Formula;
      Ctx      : in out T.User_Context;
      M        : in out Model;
      Min_Vars : Variable_Or_Null) return Boolean
   is
      Is_Empty : constant Boolean := M'Length = 0;
      First    : constant Literal := (if Is_Empty then 1 else -M'Last);
      Last     : constant Literal := (if Is_Empty then 0 else +M'Last);
      Internal : Internal_Formula (First, Last);
   begin
      Internal.Clauses.Reserve (F'Length);
      Append_Formula (Internal, F);
      return Solve_Internal (Internal, Ctx, M, Min_Vars);
   end Solve_Partial;
end Solver.DPLL;
