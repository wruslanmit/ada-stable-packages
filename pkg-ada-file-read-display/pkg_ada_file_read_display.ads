-- File	: pkg_ada_file_read_display.ads
-- Date	: Tue 23 Feb 2021 04:39:09 PM +08
-- Env	: Linux HPEliteBook8470p-Ub2004-rt38 5.4.66-rt38 
-- #1 SMP PREEMPT_RT Sat Sep 26 16:51:59 +08 2020 x86_64 x86_64 x86_64 GNU/Linux
-- Author: WRY wruslandr@gmail.com
-- ========================================================
with Ada.Text_IO;

-- ========================================================
package pkg_ada_file_read_display 
-- ========================================================
--   with SPARK_Mode => on
is
   -- LIST OF PACKAGES RENAMED -- S for specification (.ads)
   package SATIO renames Ada.Text_IO; 
   
   -- LIST OF PROCEDURES
   procedure exec_file_read_display (inp_fmode : in SATIO.File_Mode; inp_fname : in String);
   
   
   
   procedure about_package;
-- ========================================================
end pkg_ada_file_read_display;
-- ========================================================    
  
