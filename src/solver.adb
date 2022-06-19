package body Solver is
   function "+" (V : Variable) return Literal is
     (Literal (V));

   function "-" (V : Variable) return Literal is
     (-Literal (V));

   function "abs" (L : Literal) return Variable is
      Res : constant Literal := abs L;
   begin
      return Variable (Res);
   end "abs";

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
