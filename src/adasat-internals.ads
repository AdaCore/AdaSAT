--
--  Copyright (C) 2019-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with AdaSAT.Vectors;

--  This factors out some data structures and routines that are used by all
--  instantiations of the `AdaSAT.DPLL` generic package but that are common
--  to each of them and therefore which do not need to be inside the instances.
--  This could theoretically be inside the root `AdaSAT` package but cannot
--  because of a circular dependency on the `AdaSAT.Vectors` package.

private package AdaSAT.Internals is
   package Literal_Vectors is new AdaSAT.Vectors
     (Literal, Literal_Array);

   function Get_Literal_Vector_Array is new Literal_Vectors.Internal_Array
     (Literal_Array_Access);
end AdaSAT.Internals;
