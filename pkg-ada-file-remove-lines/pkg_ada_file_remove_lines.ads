-- File   : pkg_ada_file_remove_lines.ads
-- Date   : Thu 25 Feb 2021 01:51:47 PM +08
-- Author : WRY wruslandr@gmail.com
-- ========================================================


-- ========================================================
package pkg_ada_file_remove_lines
-- ========================================================
--   with SPARK_Mode => on
is
   -- LIST OF PACKAGES RENAMED -- S for specification (.ads)
   
   
   -- LIST OF PROCEDURES
   procedure exec_remove_blank_lines (inp_fname : in String; out_fname : in String);
   procedure exec_remove_lines_oncondition (inp_fname : in String; out_fname : in String; substr_condition : in String);
     
   -- LIST OF FUNCTIONS
      
-- ========================================================
end pkg_ada_file_remove_lines;
-- ========================================================    
    
