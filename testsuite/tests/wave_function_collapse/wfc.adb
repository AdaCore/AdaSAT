with Ada.Calendar;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

with AdaSAT.Builders;
with AdaSAT.DPLL;
with AdaSAT.Formulas;
with AdaSAT.Theory;

procedure WFC is
   use Ada.Calendar;
   use AdaSAT;
   use AdaSAT.Formulas;

   Print_Steps : constant Boolean := False;
   --  Set to True to print the generated map at each decision routine
   --  invocation. Useful for debug and visualization of the algorithm.

   N : constant := 9;
   --  size of the resulting image

   T : constant := 5;
   --  Number of tile kinds

   type Tile_Kind is ('0', 'x', '^', '_', ' ');

   type Direction_Kind is (Top, Down, Left, Right);

   type Tile_Kind_Array is array (Positive range <>) of Tile_Kind;

   type Adjacency_Matrix is array
     (Tile_Kind, Direction_Kind, Tile_Kind) of Boolean;
   --  encodes which tile kind is allowed or forbidden as the neighbor
   --  of the given tile kind in the given which direction.

   package Natural_Randoms is new Ada.Numerics.Discrete_Random (Natural);
   Rnd : Natural_Randoms.Generator;

   function Transform (I, J : Natural; K : Tile_Kind) return Variable is
     (Variable ((I - 1) * N * T + (J - 1) * T + Tile_Kind'Pos (K) + 1));
   --  The the boolean variable representing 'cell at coords (I, J) is of
   --  tile kind K).

   function Get_Neighbour
     (I, J : Natural;
      D    : Direction_Kind;
      X, Y : out Natural) return Boolean;

   function Get_Neighbour
     (I, J : Natural;
      D    : Direction_Kind;
      X, Y : out Natural) return Boolean
   is
   begin
      case D is
         when Top =>
            if J = 1 then
               return False;
            else
               X := I;
               Y := J - 1;
            end if;
         when Down =>
            if J = N then
               return False;
            else
               X := I;
               Y := J + 1;
            end if;
         when Left =>
            if I = 1 then
               return False;
            else
               X := I - 1;
               Y := J;
            end if;
         when Right =>
            if I = N then
               return False;
            else
               X := I + 1;
               Y := J;
            end if;
      end case;
      return True;
   end Get_Neighbour;

   function Image (K : Tile_Kind) return String;

   function Image (K : Tile_Kind) return String is
      Img : String := K'Image;
   begin
      return Img (2 .. 2);
   end Image;

   type Empty_Context is null record;

   C : Empty_Context;

   function Empty_Theory_Check
     (Ctx : in out Empty_Context;
      M   : Model;
      F   : in out Formula) return Boolean;

   function Empty_Theory_Check
     (Ctx : in out Empty_Context;
      M   : Model;
      F   : in out Formula) return Boolean
   is
      pragma Unreferenced (Ctx, M, F);
   begin
      return True;
   end Empty_Theory_Check;

   package Empty_Theory is new Theory (Empty_Context, Empty_Theory_Check);

   function Least_Entropy_Decision
     (M : Model; First_Unset : in out Variable) return Variable_Or_Null;

   procedure Print_Map (M : Model);
   procedure Print_Map (M : Model) is
      Unset : Boolean;
   begin
      for J in 1 .. N loop
         for I in 1 .. N loop
            Unset := True;
            for K in Tile_Kind loop
               if M (Transform (I, J, K)) in True then
                  Put (Image (K));
                  Unset := False;
                  exit;
               end if;
            end loop;
            if Unset then
               Put ('?');
            end if;
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Map;

   function Least_Entropy_Decision
     (M : Model; First_Unset : in out Variable) return Variable_Or_Null
   is
      Best_Cell_X, Best_Cell_Y : Natural := 1;
      Best_Cell_Unset_Count    : Natural := T + 1;
      Cur_Cell_Unset_Count     : Natural;
      Chosen_Tile              : Natural;
   begin
      if Print_Steps then
         Print_Map (M);
      end if;
      for J in 1 .. N loop
         for I in 1 .. N loop
            Cur_Cell_Unset_Count := 0;
            for K in Tile_Kind loop
               if M (Transform (I, J, K)) in Unset then
                  Cur_Cell_Unset_Count := Cur_Cell_Unset_Count + 1;
               end if;
            end loop;

            if Cur_Cell_Unset_Count > 0 then
               --  todo change <= to < and handle = specially
               if Cur_Cell_Unset_Count <= Best_Cell_Unset_Count then
                  Best_Cell_X := I;
                  Best_Cell_Y := J;
                  Best_Cell_Unset_Count := Cur_Cell_Unset_Count;
               end if;
            end if;

            if Print_Steps then
               Put (Cur_Cell_Unset_Count'Image);
            end if;
         end loop;
         if Print_Steps then
            New_Line;
         end if;
      end loop;
      if Print_Steps then
         New_Line;
         New_Line;
         New_Line;
      end if;
      Chosen_Tile := Natural_Randoms.Random (Rnd) mod Best_Cell_Unset_Count;
      for K in Tile_Kind loop
         if M (Transform (Best_Cell_X, Best_Cell_Y, K)) in Unset then
            if Chosen_Tile = 0 then
               return Transform (Best_Cell_X, Best_Cell_Y, K);
            else
               Chosen_Tile := Chosen_Tile - 1;
            end if;
         end if;
      end loop;
      raise Program_Error;
   end Least_Entropy_Decision;

   package SAT is new DPLL (Empty_Theory, Least_Entropy_Decision);

   F : Builders.Formula_Builder;

   Adjacency_Rules : Adjacency_Matrix :=
     (others => (others => (others => True)));

   Sol : Model := [1 .. N * N * T => Unset];
begin
   --  Setup problem specific adjacency rules
   Adjacency_Rules ('0', Top, '0') := False;
   Adjacency_Rules ('0', Top, 'x') := False;
   Adjacency_Rules ('0', Left, '^') := False;
   Adjacency_Rules ('0', Right, '^') := False;
   Adjacency_Rules ('0', Left, ' ') := False;
   Adjacency_Rules ('0', Right, ' ') := False;
   Adjacency_Rules ('x', Top, '0') := False;
   Adjacency_Rules ('^', Top, '0') := False;
   Adjacency_Rules ('^', Top, 'x') := False;
   Adjacency_Rules ('^', Left, 'x') := False;
   Adjacency_Rules ('^', Right, 'x') := False;
   Adjacency_Rules (' ', Down, '0') := False;
   Adjacency_Rules (' ', Left, 'x') := False;
   Adjacency_Rules (' ', Right, 'x') := False;
   Adjacency_Rules (' ', Top, '0') := False;
   Adjacency_Rules (' ', Top, '^') := False;
   Adjacency_Rules (' ', Top, 'x') := False;
   for K in Tile_Kind loop
      Adjacency_Rules ('_', Down, K) := False;
      Adjacency_Rules ('_', Left, K) := False;
      Adjacency_Rules ('_', Right, K) := False;
   end loop;
   Adjacency_Rules ('_', Left, '_') := True;
   Adjacency_Rules ('_', Right, '_') := True;

   --  Uncomment the following line to get a random output.
   --  Natural_Randoms.Reset (Rnd, Integer (Seconds (Clock)));

   --  Add the constraint that each cell must be only one tile
   for I in 1 .. N loop
      for J in 1 .. N loop
         declare
            C : Builders.Clause_Builder;
         begin
            for K in Tile_Kind loop
               C.Add (+Transform (I, J, K));
            end loop;
            F.Add (C.Build);
            F.Add_At_Most_One
              (Transform (I, J, Tile_Kind'First),
               Transform (I, J, Tile_Kind'Last));
         end;
      end loop;
   end loop;

   --  Setup the adjacency constraints
   for I in 1 .. N loop
      for J in 1 .. N loop
         for D in Direction_Kind loop
            declare
               X, Y : Natural;
               C    : Builders.Clause_Builder;
            begin
               if Get_Neighbour (I, J, D, X, Y) then
                  for K_1 in Tile_Kind loop
                     C.Add (-Transform (I, J, K_1));
                     for K_2 in Tile_Kind loop
                        if Adjacency_Rules (K_1, D, K_2) then
                           C.Add (+Transform (X, Y, K_2));
                        else
                           F.Add (new Literal_Array'
                             (-Transform (I, J, K_1),
                              -Transform (X, Y, K_2)));
                        end if;
                     end loop;
                     F.Add (C.Build);
                  end loop;
               end if;
            end;
         end loop;
      end loop;
   end loop;

   if SAT.Solve (F.Build, C, Sol) then
      Put_Line ("Solved");
   else
      Put_Line ("Failed solving");
   end if;
   Print_Map (Sol);
end WFC;
