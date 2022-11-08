with AdaSAT.Vectors;

private package AdaSAT.Internals is
   package Literal_Vectors is new AdaSAT.Vectors
     (Literal, Literal_Array);

   function Get_Literal_Vector_Array is new Literal_Vectors.Internal_Array
     (Literal_Array_Access);
end AdaSAT.Internals;
