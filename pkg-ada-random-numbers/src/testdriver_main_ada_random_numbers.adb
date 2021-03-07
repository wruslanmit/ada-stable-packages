-- File   : testdriver_main_ada_random_numbers.adb
-- Date   : Sun 07 Mar 2021 06:00:42 PM +08
-- Author : wruslandr@gmail.com
-- Version: 1.0 Sun 07 Mar 2021 06:00:42 PM +08
-- ========================================================
-- IMPORT STANDARD ADA PACKAGES
with Ada.Text_IO;
use  Ada.Text_IO;

-- IMPORT USER-DEFINED ADA PACKAGES
with pkg_ada_random_numbers;

-- ========================================================
procedure testdriver_main_ada_random_numbers
-- ========================================================
--	with SPARK_Mode => on
is 
   -- RENAME STANDARD ADA PACKAGES FOR CONVENIENCE
   package ATIO   renames Ada.Text_IO;
       
   -- RENAME USER-DEFINED ADA PACKAGES FOR CONVENIENCE
   package PARN   renames pkg_ada_random_numbers;  
   
   -- PACKAGE-WIDE VARIABLES
   -- ====================================================
   int_random : Integer  := 0;
   flt_random : Float    := 0.0;
   
-- ========================================================   
begin  -- FOR procedure main_xxx
   
   ATIO.Put_Line ("STARTED: main Bismillah 3 times WRY");
   ATIO.New_Line;
   
   for idx in 1 .. 5 loop
      int_random := PARN.get_random_integer (-100, +100);
      ATIO.Put_Line ("int_random (-100, +100) = " & Integer'Image (int_random));     
   end loop;
   ATIO.New_Line;
   
   for idx in 1 .. 5 loop   
      flt_random := PARN.get_random_float (-10.0, +10.0); 
      ATIO.Put_Line ("flt_random (-10.0, +10.0) = " & Float'Image (flt_random));   
   end loop;   
   ATIO.New_Line;
   
   PARN.generate_10_random_integers; 
   ATIO.New_Line;
   
   PARN.generate_10_random_floats; 
   ATIO.New_Line; 
 
   ATIO.Put_Line ("ENDED: main Alhamdulillah 3 times WRY");
-- ========================================================   
end testdriver_main_ada_random_numbers;
-- ========================================================
