with Ada.Strings.Unbounded;

package body Solver is
   ---------
   -- "+" --
   ---------

   function "+" (V : Variable) return Literal is
     (Literal (V));

   ---------
   -- "-" --
   ---------

   function "-" (V : Variable) return Literal is
     (-Literal (V));

   -----------
   -- "abs" --
   -----------

   function "abs" (L : Literal) return Variable is
      pragma Suppress (Range_Check);
   begin
      return Variable (Literal'(abs L));
   end "abs";

   -----------
   -- Image --
   -----------

   function Image (C : Clause) return String is
      use Ada.Strings.Unbounded;

      Res : Unbounded_String;
   begin
      Append (Res, "(");
      for I in C.all'Range loop
         declare
            Raw : constant String := C (I)'Image;
            Suf : constant String := Raw (Raw'First + 1 .. Raw'Last);
         begin
            if C (I) < 0 then
               Append (Res, "¬");
            end if;
            Append (Res, Suf);
            if I < C.all'Last then
               Append (Res, " | ");
            end if;
         end;
      end loop;
      Append (Res, ")");
      return To_String (Res);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (F : Formula) return String is
      use Ada.Strings.Unbounded;

      Res : Unbounded_String;
   begin
      for I in F'Range loop
         Append (Res, Image (F (I)));
         if I < F'Last then
            Append (Res, " & ");
         end if;
      end loop;
      return To_String (Res);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (M : Model) return String is
      use Ada.Strings.Unbounded;

      Res   : Unbounded_String;
      First : Boolean := True;
   begin
      for I in M'Range loop
         if M (I) in True then
            if First then
               First := False;
            else
               Append (Res, " & ");
            end if;
            Append (Res, I'Image);
         end if;
      end loop;
      return To_String (Res);
   end Image;

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies (F : Formula; M : Model) return SAT_Result is
      OK : Variable_Value := False;
   begin
      for Dis of F loop
         for L of Dis.all loop
            declare
               Model_Value : constant Variable_Value := M (abs L);
               Required    : constant Variable_Value :=
                 (if L < 0 then False else True);
            begin
               if Model_Value in Unset then
                  OK := Unset;
               elsif Model_Value = Required then
                  OK := True;
                  exit;
               end if;
            end;
         end loop;

         case OK is
            when True =>
               OK := False;
            when False =>
               return UNSAT;
            when Unset =>
               return UNKNOWN;
         end case;
      end loop;
      return SAT;
   end Satisfies;
end Solver;
