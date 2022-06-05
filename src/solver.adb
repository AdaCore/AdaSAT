package body Solver is
   function Satisfies (F : Formula; M : Model) return SAT_Result is
      OK : Literal_Value := False;
   begin
      for Dis of F loop
         for L of Dis.all loop
            declare
               Model_Value : constant Literal_Value := M (abs L);
               Required    : constant Literal_Value :=
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
