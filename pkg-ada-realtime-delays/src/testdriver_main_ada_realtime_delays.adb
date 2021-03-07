-- File   : testdriver_main_ada_realtime_delays.adb
-- Date   : Sun 07 Mar 2021 06:00:42 PM +08
-- Author : wruslandr@gmail.com
-- Version: 1.0 Sun 07 Mar 2021 06:00:42 PM +08
-- ========================================================
-- IMPORT STANDARD ADA PACKAGES
with Ada.Text_IO;
use  Ada.Text_IO;
with Ada.Real_Time; 
use  Ada.Real_Time;

-- IMPORT USER-DEFINED ADA PACKAGES
with pkg_ada_datetime_stamp;
with pkg_ada_realtime_delays;

-- ========================================================
procedure testdriver_main_ada_realtime_delays
-- ========================================================
--	with SPARK_Mode => on
is 
   -- RENAME STANDARD ADA PACKAGES FOR CONVENIENCE
   package ATIO   renames Ada.Text_IO;
   package ART    renames Ada.Real_Time;
      
   -- RENAME USER-DEFINED ADA PACKAGES FOR CONVENIENCE
   package PADTS  renames pkg_ada_datetime_stamp;
   package PARTD  renames pkg_ada_realtime_delays;  
   
   -- PACKAGE-WIDE VARIABLES
   -- ====================================================
   intervalTime     : ART.Time_Span;  -- interval in seconds with decimal fractions
   startClock       : ART.Time;
   finishClock      : ART.Time;
   executionTime    : ART.Time_Span;
   deadlineDuration : ART.Time_Span;

-- ========================================================   
begin  -- FOR procedure main_xxx
   
   ATIO.Put_Line ("STARTED: main Bismillah 3 times WRY");
   
   -- TEST VARIOUS REALTIME DELAY PROCEDURES
   -- =====================================================
   ATIO.New_Line;
   ATIO.Put_Line ("Test 2 sec delay ...");
   for idx in 1 .. 3 loop
      PADTS.dtstamp; 
      ATIO.Put_Line ("Value of idx = " & Integer'Image(idx));
      PARTD.exec_delay_sec (2); 
   end loop;
   ATIO.New_Line;
   -- =====================================================
   ATIO.Put_Line ("Test 1_500 msec delay ...  equals 1.5 sec");
   for idx in 1 .. 3 loop
      PADTS.dtstamp; 
      ATIO.Put_Line ("Value of idx = " & Integer'Image(idx));
      PARTD.exec_delay_msec (1_500); 
   end loop;
   ATIO.New_Line;
   -- =====================================================
   ATIO.Put_Line ("Test 300_000 usec delay ...  equals 0.3 sec");
   for idx in 1 .. 3 loop
      PADTS.dtstamp; 
      ATIO.Put_Line ("Value of idx = " & Integer'Image(idx));
      PARTD.exec_delay_usec (300_000); 
   end loop;
   ATIO.New_Line;
   -- =====================================================
   ATIO.Put_Line ("Test 2.5 sec delay ...  ");
   for idx in 1 .. 3 loop
      PADTS.dtstamp; 
      ATIO.Put_Line ("Value of idx = " & Integer'Image(idx));
      PARTD.exec_delay_time (ART.To_Time_Span(2.5)); 
   end loop;
   ATIO.New_Line;
   -- =====================================================
   ATIO.Put_Line ("Test 1.558432 sec delay ...  ");
   intervalTime  :=  ART.To_Time_Span(1.558432);
   for idx in 1 .. 3 loop
      PADTS.dtstamp; 
      ATIO.Put_Line ("Value of idx = " & Integer'Image(idx));
      PARTD.exec_delay_time (intervalTime); 
   end loop;
   ATIO.New_Line;
      
   -- DISPLAY PROCESS EXECUTION TIME (Start to finish)
   -- =====================================================
   startClock   := ART.Clock;           -- Get current time from realtime clock
        PARTD.exec_delay_msec (4_000);  -- Assume execution of some process
     -- Next process ....
   finishClock  := ART.Clock;           -- Get current time from realtime clock
   PARTD.exec_display_execution_time (startClock, finishClock);
   ATIO.New_Line;
   
   -- CAPTURE PROCESS EXECUTION TIME
   -- =====================================================
   startClock   := ART.Clock;               
        PARTD.exec_delay_usec (2_500_000);  
     -- Next process ....
   finishClock  := ART.Clock;           
   executionTime := PARTD.get_execution_time_duration (startClock, finishClock);
   
   -- Display captured value
   ATIO.Put ("Captured execution time: ");
   ATIO.Put_Line (Duration'Image (To_Duration (executionTime)) & " seconds");
   ATIO.New_Line;
    
   -- TEST EXECUTION TIME AGAINST DEADLINE
   -- =====================================================
   deadlineDuration :=  ART.To_Time_Span(2.500); -- Set the deadline in seconds
   startClock   := ART.Clock;               
        PARTD.exec_delay_usec (3_500_000);  -- Assumes a running process
     -- Next process execution ....
   finishClock  := ART.Clock;           
   
   if PARTD.is_timing_overrun (startClock, finishClock, deadlineDuration) then
      ATIO.Put_Line ("Execution overrun: True ");
   else 
      ATIO.Put_Line ("Execution overrun: False ");
   end if;
   ATIO.New_Line;
   
    -- TEST OVERRUN (Execution time greater than deadline)
   -- =====================================================
   deadlineDuration :=  ART.To_Time_Span(2.500); -- Set the deadline in seconds
   startClock   := ART.Clock;               
        PARTD.exec_delay_msec (2_754);  -- Assumes a running process
     -- Next process execution ....
   finishClock  := ART.Clock;           
   PARTD.exec_checktiming_overrun_underrun (startClock, finishClock, deadlineDuration); 
   ATIO.New_Line;
   
    -- TEST UNDERRUN (Execution time lesser than deadline)
   -- =====================================================
   deadlineDuration :=  ART.To_Time_Span(2.500); -- Set the deadline in seconds
   startClock   := ART.Clock;               
        PARTD.exec_delay_msec (2_366);  -- Assumes a running process
     -- Next process execution ....
   finishClock  := ART.Clock;           
   PARTD.exec_checktiming_overrun_underrun (startClock, finishClock, deadlineDuration); 
   ATIO.New_Line;
   
   ATIO.Put_Line ("ENDED: main Alhamdulillah 3 times WRY");
-- ========================================================   
end testdriver_main_ada_realtime_delays;
-- ========================================================
