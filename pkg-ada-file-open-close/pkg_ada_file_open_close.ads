-- File	 : pkg_ada_file_open_close.ads
-- Date  : Tue 09 Mar 2021 09:18:35 AM +08
-- Author: WRY wruslandr@gmail.com
-- ========================================================
with Ada.Text_IO;

-- ========================================================
package pkg_ada_file_open_close 
-- ========================================================
--   with SPARK_Mode => on
is
   -- LIST OF PACKAGES RENAMED -- S for specification (.ads)
   package SATIO renames Ada.Text_IO; 
   
   -- LIST OF PROCEDURES
   procedure exec_file_open (inp_fhandle  : in out SATIO.File_Type; 
                             inp_fmode    : in SATIO.File_Mode; 
                             inp_fname    : in String; 
                             inp_fform    : in String;
                             inp_fOwnID   : in String
                            );
   
   procedure exec_file_close (inp_fhandle : in out SATIO.File_Type;
                              inp_fform   : in String;
                              inp_fOwnID  : in String 
                             );
   
   -- LIST OF FUNCTIONS
      
-- ========================================================
end pkg_ada_file_open_close;
-- ========================================================    
     
