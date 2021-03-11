-- File   : pkg_ada_file_trim_lines.ads
-- Date   : Thu 25 Feb 2021 01:51:47 PM +08
-- Author : WRY wruslandr@gmail.com
-- ========================================================
-- LIST OF IMPORTED PACKAGES

-- ========================================================
package pkg_ada_file_trim_lines
-- ========================================================
--   with SPARK_Mode => on
is
   -- LIST OF PACKAGES RENAMED -- S for specification (.ads)
   
   -- LIST OF PROCEDURES
   procedure exec_file_trim_lines_left  (inp_fname : String; out_fname : String);
                                         
   procedure exec_file_trim_lines_right (inp_fname : String; out_fname : String);
                                         
   procedure exec_file_trim_lines_both  (inp_fname : String; out_fname : String);
     
   -- LIST OF FUNCTIONS
      
-- ========================================================
end pkg_ada_file_trim_lines;
-- ========================================================    
    
