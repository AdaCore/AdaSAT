with Ada.Strings.Unbounded;

package body AdaSAT is
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

   ---------
   -- AMO --
   ---------

   function At_Most_One (From, To : Variable) return Clause is
   begin
      if To - From = 1 then
         return new Literal_Array'(-From, -To);
      else
         return new Literal_Array'(0, +From, +To);
      end if;
   end At_Most_One;

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
end AdaSAT;
