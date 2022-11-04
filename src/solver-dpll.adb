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
      Clauses     : Clause_Vectors.Vector;
      Occurs_List : Literal_To_Watcher_Map (First .. Last);
   end record;
   --  An internal formula is the internal representation of a formula,
   --  which uses a vector for clauses so as to easily append new ones.
   --  It also maintaints some data structures around the clauses such as
   --  the occurrence list, to optimize the different routines.

   procedure Destroy (F : in out Internal_Formula);
   --  Free the memory allocated for the formula's internal structures.
   --  Does not free the inner clauses.

   procedure Append_Watcher
     (F : in out Internal_Formula; C : Clause);
   --  Create a watcher for the given non-empty clause and install it
   --  in the internal formula.

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

   -------------
   -- Destroy --
   -------------

   procedure Destroy (F : in out Internal_Formula) is
      Mutable_C : Clause;
   begin
      for C of F.Clauses loop
         Mutable_C := C;
         Free (Mutable_C);
      end loop;
      F.Clauses.Destroy;
      for V of F.Occurs_List loop
         V.Destroy;
      end loop;
   end Destroy;

   --------------------
   -- Create_Watcher --
   --------------------

   procedure Append_Watcher
     (F : in out Internal_Formula; C : Clause)
   is
      K : constant Natural := C'First;
   begin
      if C (K) = 0 then
         --  This is a special AMO constraint
         declare
            W : constant Watcher := (0, 0, C);
         begin
            for Lit in C (K + 1) .. C (C'Last) loop
               F.Occurs_List (-Lit).Append (W);
            end loop;
         end;
      elsif C'Length = 1 then
         F.Occurs_List (C (K)).Append ((C (K), 0, C));
      elsif C'Length = 2 then
         F.Occurs_List (C (K)).Append ((C (K + 1), C (K), C));
         F.Occurs_List (C (K + 1)).Append ((C (K), C (K + 1), C));
      else
         F.Occurs_List (C (K)).Append ((C (K + 1), 0, C));
         F.Occurs_List (C (K + 1)).Append ((C (K), 0, C));
      end if;
   end Append_Watcher;

   -------------------
   -- Append_Clause --
   -------------------

   procedure Append_Clause
     (F : in out Internal_Formula; C : Clause)
   is
   begin
      if C'Length > 0 then
         Append_Watcher (F, C);
      end if;
      F.Clauses.Append (C);
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

      function Val (X : Literal) return Variable_Value with Inline_Always;
      --  Return evaluation of the given literal

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

      function Reorder_Clause (C : Clause) return Natural;
      --  Reorder the given clause to place apprioriate watched literals first

      ------------
      -- Assign --
      ------------

      procedure Assign
        (Var : Variable; Value : Boolean; Antecedant : Clause)
      is
      begin
         pragma Assert (M (Var) in Unset);
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
         pragma Assert (Val (L) in False);
         To_Propagate.Append (L);
      end Add_To_Propagate;

      -----------------------
      -- Clear_Propagation --
      -----------------------

      procedure Clear_Propagation is
      begin
         To_Propagate.Clear;
      end Clear_Propagation;

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
         Watchers         : Watcher_Vectors.Vector;
         Watch_Count      : Natural;
         Being_Propagated : Literal;
         J                : Natural;
      begin
         pragma Assert (To_Propagate.Length >= 1);
         while not To_Propagate.Is_Empty loop
            Being_Propagated := To_Propagate.Pop;
            Watchers := F.Occurs_List (Being_Propagated);
            Watch_Count := Watchers.Length;
            J := 1;

            while J <= Watch_Count loop
               declare
                  W : constant Watcher := Watchers.Get (J);

                  Last_Unset  : Literal  := 0;
                  Is_Sat      : Boolean  := False;
                  Index       : Positive;
               begin
                  if W.Blit = 0 then
                     --  This is an AMO constraint
                     if M (abs Being_Propagated) in True then
                        declare
                           From : constant Variable :=
                              abs W.Literals (W.Literals'First + 1);
                           To   : constant Variable :=
                              abs W.Literals (W.Literals'Last);
                        begin
                           for Var in From .. To loop
                              case M (Var) is
                                 when True =>
                                    if Var /= abs Being_Propagated then
                                       raise Program_Error;
                                    end if;
                                 when False =>
                                    null;
                                 when Unset =>
                                    Assign (Var, False, W.Literals);
                              end case;
                           end loop;
                        end;
                     end if;
                  elsif W.Other /= 0 then
                     pragma Assert (W.Other = Being_Propagated);
                     case Val (W.Blit) is
                        when True =>
                           null;
                        when False =>
                           Conflicting_Clause := W.Literals;
                           Clear_Propagation;
                           return False;
                        when Unset =>
                           Assign (abs W.Blit, W.Blit > 0, W.Literals);
                     end case;
                  elsif Val (W.Blit) not in True then
                     Index := W.Literals'First;

                     --  Make sure the false literal is in second position
                     --  and Index points on the other one.
                     if Being_Propagated = W.Literals (Index) and then
                        W.Literals'Length > 1
                     then
                        W.Literals (Index) := W.Literals (Index + 1);
                        W.Literals (Index + 1) := Being_Propagated;
                     end if;

                     pragma Assert
                       (W.Literals'Length = 1 or else
                        W.Literals (Index + 1) = Being_Propagated);
                     Last_Unset := W.Literals (Index);

                     if Val (Last_Unset) in True then
                        --  The other watched literal is true meaning the
                        --  clause is satisfied, simply update the watch's
                        --  blocking literal and we're done.
                        F.Occurs_List
                          (Being_Propagated).Get_Access (J).Blit := Last_Unset;
                     else
                        --  The other watched literal is not true, so let's
                        --  see if we can find a true literal among the other
                        --  literals of the clause.
                        for K in Index + 2 .. W.Literals'Last loop
                           if Val (W.Literals (K)) not in False then
                              --  Found a non-false literal! swap its place
                              --  inside the clause and update their watchers.
                              W.Literals (Index + 1) := W.Literals (K);
                              W.Literals (K) := Being_Propagated;
                              F.Occurs_List
                                (Being_Propagated).Swap_And_Remove (J);
                              F.Occurs_List
                                (W.Literals (Index + 1)).Append (W);
                              Watch_Count := Watch_Count - 1;
                              J := J - 1;
                              Is_Sat := True;
                              exit;
                           end if;
                        end loop;

                        if not Is_Sat then
                           --  We couldn't find another non-False literal, so
                           --  we have either a unit clause or a conflict.
                           if Val (Last_Unset) in False then
                              Conflicting_Clause := W.Literals;
                              Clear_Propagation;
                              return False;
                           else
                              Assign
                                (abs Last_Unset, Last_Unset > 0, W.Literals);
                           end if;
                        end if;
                     end if;
                  end if;
               end;
               J := J + 1;
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
                     --  TODO: exit if Found > 1?
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
            Asserting_Lit           : Literal;

            Learnt : constant Clause :=
               Get_Literal_Vector_Array (Learnt_Clause);
         begin
            for I in Learnt'Range loop
               Lit_Decision_Level := Lit_Decisions (abs Learnt (I));

               if Lit_Decision_Level = Decision_Level then
                  --  since we are building asserting clauses, only one literal
                  --  should be unset right now, and all the others should be
                  --  False.
                  Asserting_Lit := Learnt (I);
                  Learnt (I) := Learnt (1);
                  Learnt (1) := Asserting_Lit;
               elsif Lit_Decision_Level > Backjump_Decision_Level then
                  Backjump_Decision_Level := Lit_Decision_Level;
               end if;
            end loop;

            Decision_Level := Backjump_Decision_Level;

            --  Unset all the variables that were set at a decision level
            --  higher than the one we are backjumping to.
            Unassign_All (Decision_Level);

            --  Add the learnt clause to the formula
            Append_Clause (F, Learnt);
            Assign (abs Learnt (1), Learnt (1) > 0, Learnt);
            return True;
         end;
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

         if Right (Right'First) = 0 then
            --  AMO constraint
            for Var in
               Variable (Right (Right'First + 1))
               .. Variable (Right (Right'Last))
            loop
               if M (Var) in True then
                  if not Mask (-Var) then
                     Mask (-Var) := True;
                     Left.Append (-Var);
                  end if;
                  exit;
               end if;
            end loop;
         else
            for Lit of Right.all loop
               if not Mask (Lit) then
                  Mask (Lit) := True;
                  Left.Append (Lit);
               end if;
            end loop;
         end if;

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

      --------------------
      -- Reorder_Clause --
      --------------------

      function Reorder_Clause (C : Clause) return Natural is
         I : Natural := 1;
         T : Literal := 0;
      begin
         --  Find first literal to watch
         while I <= C'Length loop
            if Val (C (I)) not in False then
               T := C (I);
               C (I) := C (C'First);
               C (C'First) := T;
               I := I + 1;
               exit;
            end if;
            I := I + 1;
         end loop;

         if T = 0 then
            --  We could not even find on non-False literal in the clause,
            --  signal it by returning False
            return 0;
         end if;

         --  Find second literal to watch
         while I <= C'Length loop
            if Val (C (I)) not in False then
               T := C (I);
               C (I) := C (C'First + 1);
               C (C'First + 1) := T;
               return 2;
            end if;
            I := I + 1;
         end loop;

         return 1;
      end Reorder_Clause;

   begin
      --  Perform initial BCP: the formula might be resolvable without
      --  making any decision.
      for C of F.Clauses loop
         --  Check that it has not already been set by a duplicate clause
         if C'Length = 1 and then M (abs C (C'First)) in Unset then
            Assign (abs C (C'First), C (C'First) > 0, C);
         end if;
      end loop;
      if not To_Propagate.Is_Empty and then not Unit_Propagate then
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
            Explanation : Formula := T.Check (Ctx, M, OK);
         begin
            if OK then
               return Cleanup (True);
            elsif Explanation.Length = 0 then
               return Cleanup (False);
            end if;

            Decision_Level := 0;
            Unassign_All (Decision_Level);

            --  Process the explanations so that two unset literals appear
            --  first in the clauses to setup the two watched literals.
            for C of Explanation loop
               declare
                  Non_False : constant Natural := Reorder_Clause (C);
               begin
                  if Non_False = 0 then
                     Explanation.Destroy;
                     return Cleanup (False);
                  elsif Non_False = 1 and then Val (C (C'First)) in Unset then
                     Assign (abs C (C'First), C (C'First) > 0, C);
                  end if;
               end;
            end loop;

            Append_Formula (F, Explanation);

            Explanation.Destroy;

            if not To_Propagate.Is_Empty and then not Unit_Propagate then
               return Cleanup (False);
            end if;
         end;
      end loop;
      return Cleanup (True);
   end Solve_Internal;

   ------------
   --  Solve --
   ------------

   function Solve
     (F        : Formula;
      Ctx      : in out T.User_Context;
      M        : in out Model;
      Min_Vars : Variable_Or_Null := 0) return Boolean
   is
      Is_Empty : constant Boolean := M'Length = 0;
      First    : constant Literal := (if Is_Empty then 1 else -M'Last);
      Last     : constant Literal := (if Is_Empty then 0 else +M'Last);
      Internal : Internal_Formula (First, Last);
   begin
      Internal.Clauses := F;
      for C of F loop
         if C'Length > 0 then
            Append_Watcher (Internal, C);
         end if;
      end loop;
      return Solve_Internal
        (Internal, Ctx, M, (if Min_Vars = 0 then M'Last else Min_Vars));
   end Solve;
end Solver.DPLL;
