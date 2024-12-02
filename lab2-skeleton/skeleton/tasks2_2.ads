with Ada.Real_Time;       use Ada.Real_Time;
-- Add required sensor and actuator package --

package tasks2_2 is
  procedure Background;
private
  --  Define periods and times  --
  Period_Display10m : Time_Span := Milliseconds(10); 
  Time_Zero      : Time := Clock;
      
  --  Other specifications  --
  type Dir is (Up, Down, Undefined);
  type EventID is (Idle, UpButtonPressed, 
    UpButtonReleased, DownButtonPressed, 
    DownButtonReleased, LeftButtonPressed, 
    LeftButtonReleased, RightButtonPressed, 
    RightButtonReleased, On_Blackline, Off_Blackline);
end tasks2_2;
