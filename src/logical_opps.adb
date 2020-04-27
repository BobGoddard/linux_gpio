package body Logical_Opps is
   function L_And (left, right : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
      left_temp   : Interfaces.Unsigned_32 := left;
      right_temp  : Interfaces.Unsigned_32 := right;
      result      : Interfaces.Unsigned_32 := 0;
   begin
      for t in 1 .. Interfaces.Unsigned_32'Size loop
         if left_temp rem 2 = 1 and then right_temp rem 2 = 1 then
            result := result + 1;
         end if;

         left_temp  := Interfaces.Rotate_Right (left_temp,  1);
         right_temp := Interfaces.Rotate_Right (right_temp, 1);
         result     := Interfaces.Rotate_Right (result,     1);
      end loop;
      return result;
   end L_And;

   function L_Or (left, right : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
      left_temp  : Interfaces.Unsigned_32 := left;
      right_temp : Interfaces.Unsigned_32 := right;
      result     : Interfaces.Unsigned_32 := 0;
   begin
      for t in 1 .. Interfaces.Unsigned_32'Size loop
         if left_temp rem 2 = 1 or else right_temp rem 2 = 1 then
            result := result + 1;
         end if;

         left_temp := Interfaces.Rotate_Right (left_temp, 1);
         right_temp := Interfaces.Rotate_Right (right_temp, 1);
         result := Interfaces.Rotate_Right (result, 1);
      end loop;

      return result;
   end L_Or;
end Logical_Opps;
