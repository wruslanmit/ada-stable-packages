-- File   : pkg_ada_file_trim_lines.adb
-- Date   : Thu 25 Feb 2021 01:51:47 PM +08
-- Author : WRY wruslandr@gmail.com
-- ========================================================

-- ADA STANDARD PACKAGES
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Real_Time;

-- WRY CREATED PACKAGES 
with pkg_ada_datetime_stamp;

-- ========================================================
package body pkg_ada_file_trim_lines
-- ========================================================
--   with SPARK_Mode => on
is

   package ATIO   renames Ada.Text_IO;
   package ASU    renames Ada.Strings.Unbounded;
   package ASF    renames Ada.Strings.Fixed;
   package AS     renames Ada.Strings;
   package ACL1   renames Ada.Characters.Latin_1;
   package ART    renames Ada.Real_Time;
   package PADTS  renames pkg_ada_datetime_stamp;
   
   -- FILE RELATED PACKAGE-WIDE GLOBAL VARIABLES
   -- =====================================================
   inp_fhandle : ATIO.File_Type; 
   inp_fmode   : ATIO.File_Mode := ATIO.In_File;
-- inp_fname   : String := already defined in procedure call
   
   out_fhandle : ATIO.File_Type;
   out_fmode   : ATIO.File_Mode := ATIO.Out_File;
-- out_fname   : String := already defined in procedure call   
   
   -- FOR REPORT FILE
   rep_fhandle  : ATIO.File_Type;
   rep_fmode    : ATIO.File_Mode := ATIO.Out_File; 
   rep_UBSfname : ASU.Unbounded_String; -- Initialization not needed
   
   inp_UBSlineStr : ASU.Unbounded_String; -- Initialization not needed
   
   -- NON-FILE PACKAGE-WIDE GLOBAL VARIABLEs
   -- =====================================================
   lineCount         : Integer := 999;
   cnt_lineRemoved   : Integer := 999;
   cnt_lineRemaining : Integer := 999;
   cnt_lineTotal     : Integer := 999;
   
   cur_UBSlineLength : Integer := 999;
   int_FirstIndexInString : Integer := 999; -- First index for non-blank in string
   cnt_lineBlank_BUT_WhiteSpace   : Integer := 999;  -- With white spaces
   cnt_lineBlank_NO_WhiteSpace    : Integer := 999;  -- Truly "Null" line
   cnt_lineNonBlank_NonWhiteSpace : Integer := 999;
           
   UBSTimeString   : ASU.Unbounded_String;
   
   -- SEARCH substring ASU.Index (Unbounded_String, substring) return Natural;
   substr_found    : Boolean := False;
   int_substrIndex : Natural := 999; -- First location of substring in Unbounded_String
      
   -- =====================================================
   procedure exec_file_trim_lines_left  (inp_fname : String; out_fname : String)
   -- =====================================================
   -- with SPARK_Mode => on
   is
      line_LengthBefore : Integer := 999;
      line_LengthAfter  : Integer := 999;  
      
   begin
      
      ATIO.Put_Line ("REPORT ON FILE TRIM LEFT END OF WHITESPACES");
      ATIO.Put_Line ("Running ... exec_file_trim_lines_left");
    
      -- CHECK INPUT AND OUTPUT FILES
      ATIO.Put_Line ("   inp_fname: " & inp_fname);
      ATIO.Put_Line ("   out_fname: " & out_fname);
      
      -- CHECK TIME STAMPED REPORT FILE NAME
      UBSTimeString := PADTS.get_time_stamp (ART.Clock); -- return AASU.Unbounded_String;
      rep_UBSfname := ASU.To_Unbounded_String(out_fname) & ("_report_") 
                    & ASU.To_String (UBSTimeString) & (".txt");  
      ATIO.Put_Line ("   rep_fname: " & ASU.To_String (rep_UBSfname));
      
      -- OPEN INPUT, OUTPUT AND REPORT FILES
      ATIO.Open   (inp_fhandle, ATIO.In_File,  inp_fname);
      ATIO.Create (out_fhandle, ATIO.Out_File, out_fname);
      ATIO.Create (rep_fhandle, ATIO.Out_File, ASU.To_String (rep_UBSfname)); 
      -- ==================================================
      
       -- WRITE HEADER TO REPORT FILE
      ATIO.Set_Output (rep_fhandle);
      PADTS.dtstamp; ATIO.New_Line; 
      ATIO.Put_Line ("REPORT ON FILE TRIM LINES");
      ATIO.Put_Line ("TRIM LEFT (LEADING WHITESPACES)");
      ATIO.Put_Line ("===========================================================");
      ATIO.Set_Output (ATIO.Standard_Output);
      
      
      lineCount := 0; 
      -- =================================================
      -- RUN READ AND WRITE LOOP FOR BOTH INPUT/OUTPUT FILES
      -- ==================================================
      while not ATIO.End_Of_File (inp_fhandle) loop
         inp_UBSlineStr := ASU.To_Unbounded_String(ATIO.Get_Line (inp_fhandle));
         lineCount := lineCount + 1;
         
         line_lengthBefore := ASU.Length (inp_UBSlineStr);
         
         -- OPTION 1 IN-SITU REPLACEMENT PROCEDURE          
         -- ASU.Trim (inp_UBSlineStr, AS.Left);     
         
         -- OPTION 2 RETURN FROM A FUNCTION        
         inp_UBSlineStr := ASU.Trim (inp_UBSlineStr, AS.Left);  
         line_lengthAfter := ASU.Length (inp_UBSlineStr);
                         
          -- REPORT CHANGES IN LINE LENGTH
         if (line_LengthBefore > line_LengthAfter) then
            -- WRITE TRIMMED LINE TO REPORT FILE, OUTPUT FILE AND SCREEN TERMINAL
            ATIO.Put (rep_fhandle, "LINENO: " & Integer'Image(lineCount) & " trimmed left end.");
            ATIO.Put (rep_fhandle, " line_lengthBefore: " & Integer'Image (line_LengthBefore));
            ATIO.Put (rep_fhandle, " line_lengthAfter: " & Integer'Image (line_LengthAfter));
            ATIO.Put (rep_fhandle, " DELTA = " & Integer'Image (line_lengthBefore - line_LengthAfter));
            ATIO.Put_Line (rep_fhandle, " left whitespaces removed.");
         end if; 
         
         
         -- WRITE TO OUTPUT FILE AND SCREEN TERMINAL
         ATIO.Put_Line (out_fhandle, ASU.To_String (inp_UBSlineStr));
         -- ATIO.Put_Line (ATIO.Standard_Output, ASU.To_String (inp_UBSlineStr));
            
      end loop;
      -- ==================================================
      -- CLOSE INPUT, OUTPUT AND REPORT FILES
      ATIO.Close (inp_fhandle);
      ATIO.Close (out_fhandle);
      ATIO.Close (rep_fhandle);
      -- Ensure finish file closing.
     
     
   end exec_file_trim_lines_left;
   
   -- =====================================================
   procedure exec_file_trim_lines_right (inp_fname : String; out_fname : String)
   -- =====================================================
   -- with SPARK_Mode => on
   is
      line_LengthBefore : Integer := 999;
      line_LengthAfter  : Integer := 999;  
      
   begin
      ATIO.Put_Line ("REPORT ON FILE TRIM RIGHT END OF WHITESPACES");
      ATIO.Put_Line ("Running ... exec_file_trim_lines_right");
      
     -- CHECK INPUT AND OUTPUT FILES
     ATIO.Put_Line ("   inp_fname: " & inp_fname);
     ATIO.Put_Line ("   out_fname: " & out_fname);
      
      -- CHECK TIME STAMPED REPORT FILE NAME
      UBSTimeString := PADTS.get_time_stamp (ART.Clock); -- return AASU.Unbounded_String;
      rep_UBSfname := ASU.To_Unbounded_String(out_fname) & ("_report_") 
                    & ASU.To_String (UBSTimeString) & (".txt");  
      ATIO.Put_Line ("   rep_fname: " & ASU.To_String (rep_UBSfname));
      
      -- OPEN INPUT, OUTPUT AND REPORT FILES
      ATIO.Open   (inp_fhandle, ATIO.In_File,  inp_fname);
      ATIO.Create (out_fhandle, ATIO.Out_File, out_fname);
      ATIO.Create (rep_fhandle, ATIO.Out_File, ASU.To_String (rep_UBSfname)); 
      -- ==================================================
      
        -- WRITE HEADER TO REPORT FILE
      ATIO.Set_Output (rep_fhandle);
      PADTS.dtstamp; ATIO.New_Line; 
      ATIO.Put_Line ("REPORT ON FILE TRIM LINES");
      ATIO.Put_Line ("TRIM RIGHT (TRAILING WHITESPACES)");
      ATIO.Put_Line ("===========================================================");
      ATIO.Set_Output (ATIO.Standard_Output);
      
      lineCount := 0; 
      -- =================================================
      -- RUN READ AND WRITE LOOP FOR BOTH INPUT/OUTPUT FILES
      -- ==================================================
      while not ATIO.End_Of_File (inp_fhandle) loop
         inp_UBSlineStr := ASU.To_Unbounded_String(ATIO.Get_Line (inp_fhandle));
         lineCount := lineCount + 1;
         
         line_lengthBefore := ASU.Length (inp_UBSlineStr);
         -- OPTION 1 IN-SITU REPLACEMENT PROCEDURE           
         -- ASU.Trim (inp_UBSlineStr, AS.Right);  
                 
          -- OPTION 2 RETURN FROM A FUNCTION   
         inp_UBSlineStr := ASU.Trim (inp_UBSlineStr, AS.Right);  
         
         line_lengthAfter := ASU.Length (inp_UBSlineStr);
         
          -- REPORT CHANGES IN LINE LENGTH
         if (line_LengthBefore > line_LengthAfter) then
            -- WRITE TRIMMED LINE TO REPORT FILE, OUTPUT FILE AND SCREEN TERMINAL
            ATIO.Put (rep_fhandle, "LINENO: " & Integer'Image(lineCount) & " trimmed right end.");
            ATIO.Put (rep_fhandle, " line_lengthBefore: " & Integer'Image (line_LengthBefore));
            ATIO.Put (rep_fhandle, " line_lengthAfter: " & Integer'Image (line_LengthAfter));
            ATIO.Put (rep_fhandle, " DELTA = " & Integer'Image (line_lengthBefore - line_LengthAfter));
            ATIO.Put_Line (rep_fhandle, " right whitespaces removed.");
         end if; 
         
         -- WRITE TO OUTPUT FILE AND SCREEN TERMINAL                  
         ATIO.Put_Line (out_fhandle, ASU.To_String (inp_UBSlineStr));
         -- ATIO.Put_Line (ATIO.Standard_Output, ASU.To_String (inp_UBSlineStr));
            
      end loop;
      -- ==================================================
      -- CLOSE INPUT, OUTPUT AND REPORT FILES
      ATIO.Close (inp_fhandle);
      ATIO.Close (out_fhandle);
      ATIO.Close (rep_fhandle);
      -- Ensure finish file closing.
 
   end exec_file_trim_lines_right; 
   
   -- =====================================================
   procedure exec_file_trim_lines_both  (inp_fname : String; out_fname : String)
   -- =====================================================
   -- with SPARK_Mode => on
   is
      line_LengthBefore : Integer := 999;
      line_LengthAfter  : Integer := 999;  
   begin
      ATIO.Put_Line ("REPORT ON FILE TRIM BOTH ENDS OF WHITESPACES");
      ATIO.Put_Line ("Running ... exec_file_trim_lines_both");
     
      -- CHECK INPUT AND OUTPUT FILES
      ATIO.Put_Line ("   inp_fname: " & inp_fname);
      ATIO.Put_Line ("   out_fname: " & out_fname);
      
      -- CHECK TIME STAMPED REPORT FILE NAME
      UBSTimeString := PADTS.get_time_stamp (ART.Clock); -- return AASU.Unbounded_String;
      rep_UBSfname := ASU.To_Unbounded_String(out_fname) & ("_report_") 
                    & ASU.To_String (UBSTimeString) & (".txt");  
      ATIO.Put_Line ("   rep_fname: " & ASU.To_String (rep_UBSfname));
      
      -- OPEN INPUT, OUTPUT AND REPORT FILES
      ATIO.Open   (inp_fhandle, ATIO.In_File,  inp_fname);
      ATIO.Create (out_fhandle, ATIO.Out_File, out_fname);
      ATIO.Create (rep_fhandle, ATIO.Out_File, ASU.To_String (rep_UBSfname)); 
      -- ==================================================
      
        -- WRITE HEADER TO REPORT FILE
      ATIO.Set_Output (rep_fhandle);
      PADTS.dtstamp; ATIO.New_Line; 
      ATIO.Put_Line ("REPORT ON FILE TRIM LINES");
      ATIO.Put_Line ("TRIM BOTH (LEADING AND TRAILING WHITESPACES)");
      ATIO.Put_Line ("===========================================================");
      ATIO.Set_Output (ATIO.Standard_Output);
      
      lineCount := 0; 
      -- =================================================
      -- RUN READ AND WRITE LOOP FOR BOTH INPUT/OUTPUT FILES
      -- ==================================================
      while not ATIO.End_Of_File (inp_fhandle) loop
         inp_UBSlineStr := ASU.To_Unbounded_String(ATIO.Get_Line (inp_fhandle));
         lineCount := lineCount + 1;
         
          line_lengthBefore := ASU.Length (inp_UBSlineStr);
         
         -- TRIM BOTH ENDS OF inp_UBSlineStr STRING IN-SITU
         -- USING ARM SPECS RM-A-4.5: 
         -- REF: https://www2.seas.gwu.edu/~adagroup/adalib_html/ada-html/a-string.html#trim_end
         
         -- (1) Option using procedure for In-Situ Trimming (in out Unbounded_String)
         -- procedure Trim (Source : in out Unbounded_String; Side : in Trim_End);
         -- Note: AS.Both is required, otherwise Both is not visible
         
         --       ASU.Trim (inp_UBSlineStr, AS.Both);  -- OPTION 1     
         
         -- (2) Option using function for Trimming (clearer way)
         -- function Trim (Source : in Unbounded_String; Side   : in Trim_End) return Unbounded_String;
         -- Note: Using function becomes
                
         inp_UBSlineStr := ASU.Trim (inp_UBSlineStr, AS.Both);  -- OPTION 2 
         line_lengthAfter := ASU.Length (inp_UBSlineStr);
         
         -- REPORT CHANGES IN LINE LENGTH
         if (line_LengthBefore > line_LengthAfter) then
            -- WRITE TRIMMED LINE TO REPORT FILE, OUTPUT FILE AND SCREEN TERMINAL
            ATIO.Put (rep_fhandle, "LINENO: " & Integer'Image(lineCount) & " trimmed both ends");
            ATIO.Put (rep_fhandle, " line_lengthBefore: " & Integer'Image (line_LengthBefore));
            ATIO.Put (rep_fhandle, " line_lengthAfter: " & Integer'Image (line_LengthAfter));
            ATIO.Put (rep_fhandle, " DELTA = " & Integer'Image (line_lengthBefore - line_LengthAfter));
            ATIO.Put_Line (rep_fhandle, " whitespaces removed.");
         end if; 
         
         -- WRITE TO OUTPUT FILE AND SCREEN TERMINAL
         ATIO.Put_Line (out_fhandle, ASU.To_String (inp_UBSlineStr));
         -- ATIO.Put_Line (ATIO.Standard_Output, ASU.To_String (inp_UBSlineStr));
            
      end loop;
      -- ==================================================
      -- CLOSE INPUT, OUTPUT AND REPORT FILES
      ATIO.Close (inp_fhandle);
      ATIO.Close (out_fhandle);
      ATIO.Close (rep_fhandle);
      -- Ensure finish file closing.

   end exec_file_trim_lines_both; 

-- =======================================================
begin
       null;
-- ========================================================
end pkg_ada_file_trim_lines;
-- ========================================================    
