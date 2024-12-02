with Ada.Real_Time;
use Ada.Real_Time;
-- Add required sensor and actuator package --

package tasks3 is
   procedure Background;
private
   --  Define periods and times  --
   TIME_DELTA  : Time_Span := Milliseconds (2); -- used for measuring the light sensors as fast as possible to not jitter on the black line
   TIME_DELTA2  : Time_Span := Microseconds (500); -- used to measure the distance as fast as possible
   Time_Zero   : Time := Clock;

   --  Other specifications  --
end tasks3;
