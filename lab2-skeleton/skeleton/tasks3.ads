with Ada.Real_Time;
use Ada.Real_Time;
-- Add required sensor and actuator package --

package tasks3 is
   procedure Background;
private

   --  Define periods and times  --
   TIME_DELTA  : Time_Span := Milliseconds (10);
   Time_Zero   : Time := Clock;

   --  Other specifications  --
   type Dir is (Up, Down, Undefined);
   type EventID is
     (Idle,
      UpButtonPressed,
      UpButtonReleased,
      DownButtonPressed,
      DownButtonReleased,
      LeftButtonPressed,
      LeftButtonReleased,
      RightButtonPressed,
      RightButtonReleased,
      On_Blackline,
      Off_Blackline);

end tasks3;
