-- File   : testdriver_main_ada_datetime_stamp.adb
-- Date   : Sun 07 Mar 2021 06:00:42 PM +08
-- Author : wruslandr@gmail.com
-- Version: 1.0 Sun 07 Mar 2021 06:00:42 PM +08
-- ========================================================
-- IMPORT STANDARD ADA PACKAGES
with Ada.Text_IO;
use  Ada.Text_IO;
with Ada.Real_Time; 
use  Ada.Real_Time;
with Ada.Strings.Unbounded;  
use  Ada.Strings.Unbounded;  

-- IMPORT USER-DEFINED ADA PACKAGES
with pkg_ada_datetime_stamp;

-- ========================================================
procedure testdriver_main_ada_datetime_stamp
-- ========================================================
--	with SPARK_Mode => on
is 
   -- RENAME STANDARD ADA PACKAGES FOR CONVENIENCE
   package ATIO   renames Ada.Text_IO;
   package ART    renames Ada.Real_Time;
   package ASU    renames Ada.Strings.Unbounded;  
      
   -- RENAME USER-DEFINED ADA PACKAGES FOR CONVENIENCE
   package PADTS  renames pkg_ada_datetime_stamp;
      
   -- PACKAGE-WIDE VARIABLES
   -- ====================================================
   currentTime      : ART.Time;
   UB_date_stamp    : ASU.Unbounded_String;
   UB_time_stamp    : ASU.Unbounded_String;
  
-- ========================================================   
begin  -- FOR procedure main_xxx
   
   ATIO.Put_Line ("STARTED: main Bismillah 3 times WRY");
   ATIO.New_Line;
   
   ATIO.Put_Line ("(1) Executing procedure : PADTS.dtstamp; "); 
   PADTS.dtstamp;
   ATIO.New_Line(2);
   
   currentTime := ART.Clock;    -- Get current date and time from hardware clock
   ATIO.Put_Line ("(2) Executing function  : PADTS.get_date_stamp (currentTime); "); 
   UB_date_stamp := PADTS.get_date_stamp (currentTime);
   ATIO.Put_Line (ASU.To_String (UB_date_stamp));
   ATIO.New_Line;
   
   currentTime := ART.Clock;    -- Get current date and time from hardware clock
   ATIO.Put_Line ("(3) Executing function  : PADTS.get_time_stamp (currentTime); "); 
   UB_time_stamp := PADTS.get_time_stamp (currentTime);
   ATIO.Put_Line (ASU.To_String (UB_time_stamp));
   ATIO.New_Line;
     
   ATIO.Put_Line ("ENDED: main Alhamdulillah 3 times WRY");
-- ========================================================   
end testdriver_main_ada_datetime_stamp;
-- ========================================================
