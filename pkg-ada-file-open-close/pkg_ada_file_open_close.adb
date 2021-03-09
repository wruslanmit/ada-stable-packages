-- File	 : pkg_ada_file_open_close.adb
-- Date	 : Tue 09 Mar 2021 09:18:35 AM +08
-- Author: WRY wruslandr@gmail.com
-- ========================================================

-- ADA STANDARD PACKAGES
with Ada.Text_IO;

-- USER CREATED PACKAGES 

-- ========================================================
package body pkg_ada_file_open_close 
-- ========================================================
--   with SPARK_Mode => on
is
   package ATIO renames Ada.Text_IO;
      
   -- =======================================================
   -- TEMPLATE FOR GENERIC FILE VARIABLES
   -- =======================================================
         
   -- =====================================================
   procedure exec_file_open (inp_fhandle : in out ATIO.File_Type; -- must be in/out
                             inp_fmode   : in ATIO.File_Mode;
                             inp_fname   : in String; 
                             inp_fform   : in String;
                             inp_fOwnID  : in String
                            )
   -- =====================================================
   -- with SPARK_Mode => on
   is
      
   begin 
      
      -- Attempt to open file  (Does not use inp_fOwnID) 
      ATIO.Open (inp_fhandle, inp_fmode, inp_fname, inp_fform); 
      
      if  not ATIO.Is_Open (inp_fhandle) then
         ATIO.Put_Line ("FAILED. Procedure exec_open_file (inp_fhandle, inp_fmode, inp_fname, inp_fform)");
         -- Never get to this point (intervention by Ada language engine on open failure) 
         
      else  -- COMMENT WHEN NOT REQUIRED LIKE DEBUGGING
            -- ATIO.Put_Line ("SUCCESS. Procedure exec_open_file (inp_fhandle, inp_fmode, inp_fname, " & inp_fform & ")");
            -- ATIO.Put_Line ("inp_fmode   = " & ATIO.File_Mode'Image (inp_fmode));
            -- ATIO.Put_Line ("inp_fname   = " & inp_fname);
            -- ATIO.Put_Line ("inp_fform   = " & inp_fform);
            -- ATIO.Put_Line ("inp_fOwnID  = " & inp_fOwnID);
         null;
      end if;
      -- ATIO.New_Line;
      
   end exec_file_open;

   -- =====================================================
   procedure exec_file_close (inp_fhandle : in out ATIO.File_Type; -- Must be in/out 
                              inp_fform   : in String;
                              inp_fOwnID  : in String
                             )
   -- =====================================================
   -- with SPARK_Mode => on
   is
      
   begin 
      
      -- Attempt to close file    
      ATIO.Close (inp_fhandle);   -- fhandle should be closed now.
      
      if  ATIO.Is_Open (inp_fhandle) then  -- fhandle still open
          ATIO.Put_Line ("FAILED. Procedure exec_close_file (inp_fhandle, " & inp_fform & ", " & inp_fOwnID & ")");
          ATIO.New_Line;
      else  -- COMMENT WHEN NOT REQUIRED LIKE DEBUGGING
         -- ATIO.Put_Line ("SUCCESS. Procedure exec_close_file (inp_fhandle, " & inp_fform & ", " & inp_fOwnID & ")");
         -- ATIO.New_Line;
         null;
      end if;
    
   end exec_file_close;
   
-- =======================================================   
begin  -- FOR PACKAGE BODY
  null;
-- ========================================================
end pkg_ada_file_open_close;
-- ========================================================    
