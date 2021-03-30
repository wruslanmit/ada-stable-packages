-- File   : pkg_ada_linestring_split.adb
-- Date   : Sun 07 Mar 2021 06:00:42 PM +08
-- Author : wruslandr@gmail.com
-- Version: 1.0 Sun 07 Mar 2021 06:00:42 PM +08
-- ========================================================
with Ada.Text_IO;
use  Ada.Text_IO;
with Ada.Real_Time; 
use  Ada.Real_Time;  
with Ada.Strings.Unbounded;

-- ========================================================
package body pkg_ada_linestring_split 
-- ========================================================
--   with SPARK_Mode => on
is
   -- RENAMING STANDARD PACKAGES FOR CONVENIENCE
   package ATIO   renames Ada.Text_IO;
   package ART    renames Ada.Real_Time;
   package ASU    renames Ada.Strings.Unbounded;
   
    -- ALL REQUIRED FOR INITIALIZATION ONLY
  
   -- =====================================================
   procedure display_help_file is 
   -- =====================================================
      inp_fhandle : ATIO.File_Type; 
      inp_fmode   : ATIO.File_Mode := ATIO.In_File;
      inp_fname   : String := "src/pkg-ada-linestring-split/pkg_ada_linestring_split.hlp";
      inp_UBlineStr : ASU.Unbounded_String;
   
   begin
      ATIO.Open (inp_fhandle, inp_fmode, inp_fname); 
      
      -- Traverse file line by line and display line to screen
      while not ATIO.End_Of_File (inp_fhandle) loop
         inp_UBlineStr := ASU.To_Unbounded_String(ATIO.Get_Line (inp_fhandle));
         ATIO.Put_Line (ATIO.Standard_Output, ASU.To_String (inp_UBlineStr)); 
      end loop;   
      
      ATIO.Close(inp_fhandle);
   end display_help_file;
     
   -- =====================================================
   procedure about_package is 
   -- =====================================================  
   begin
      -- Read from external text file and display      
      display_help_file;
      
   end about_package; 
  
-- ========================================================
begin -- PACKAGE BEGIN
   null;
-- ========================================================
end pkg_ada_linestring_split;
-- ========================================================
