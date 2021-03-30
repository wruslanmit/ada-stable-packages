-- File	: pkg_ada_file_read_display.adb
-- Date	: Tue 23 Feb 2021 04:39:09 PM +08
-- Env	: Linux HPEliteBook8470p-Ub2004-rt38 5.4.66-rt38 
-- #1 SMP PREEMPT_RT Sat Sep 26 16:51:59 +08 2020 x86_64 x86_64 x86_64 GNU/Linux
-- Author: WRY wruslandr@gmail.com
-- ========================================================

-- ADA STANDARD PACKAGES
with Ada.Text_IO;
with Ada.Strings.Unbounded;

-- USER-CREATED ADA PACKAGES 


-- ========================================================
package body pkg_ada_file_read_display 
-- ========================================================
--   with SPARK_Mode => on
is

   package ATIO renames Ada.Text_IO;
   package ASU  renames Ada.Strings.Unbounded;
   
   -- =======================================================
   -- TEMPLATE FOR GENERIC FILE VARIABLES
   -- =======================================================
   inp_fhandle : ATIO.File_type;
   -- out_fhandle : ATIO.File_type;
   
   inp_UBlineStr : ASU.Unbounded_String;
   len_UBlineStr : Natural := 999;
   inp_lineCount : Integer := 999; 
   inp_lineStr   : String  := ASU.To_String(inp_UBlineStr);
   
   -- ==================================================== 
   procedure exec_file_read_display (inp_fmode : in ATIO.File_Mode; inp_fname : in String)
   -- =====================================================   
   -- with SPARK_Mode => on 
   is
      
   begin
      
      -- UNCOMMENT BELOW FOR DEBUGGING
      -- ATIO.Put_Line("Run exec_read_write_display (inp_fmode, inp_fname)");
      -- ATIO.Put_Line("inp_fmode = ATIO.In_File ");
      -- ATIO.Put_Line("inp_fname = " & (inp_fname));
           
      ATIO.Open (inp_fhandle, inp_fmode, inp_fname); 
           
      ATIO.New_Line;
      inp_lineCount := 0;
      
      -- Traverse file line by line 
      while not ATIO.End_Of_File (inp_fhandle) loop
         
         -- From file read (get) single line
         inp_UBlineStr := ASU.To_Unbounded_String(ATIO.Get_Line (inp_fhandle));
                  
         len_UBlineStr := ASU.Length (inp_UBlineStr);
         inp_lineCount := inp_lineCount + 1;
         
         --DISPLAY read line to terminal
         ATIO.Put_Line (ATIO.Standard_Output, ASU.To_String (inp_UBlineStr)); 
               
      end loop;   
            
      ATIO.Close(inp_fhandle);
            
   end exec_file_read_display;
   
   -- =====================================================
   procedure display_help_file is 
   -- =====================================================
      inp_fhandle : ATIO.File_Type; 
      inp_fmode   : ATIO.File_Mode := ATIO.In_File;
      inp_fname   : String := "src/pkg-ada-file-read-display/pkg_ada_file_read_display.hlp";
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
      -- Internal procedure call. Read and display help file
      display_help_file;
      
      ATIO.New_Line;
   end about_package;
   -- =======================================================   
   
-- =======================================================   
begin
  null;
-- ========================================================
end pkg_ada_file_read_display;
-- ========================================================    
     
