with Ada.Real_Time;
use Ada.Real_Time;
-- Add required sensor and actuator package --

package tasks3 is
   procedure Background;
private
   --  Define periods and times  --
   TIME_DELTA   : Time_Span := Milliseconds (2); -- used for measuring the light sensors as fast as possible to not jitter on the black line
   TIME_DELTA2  : Time_Span := Microseconds (500); -- used to measure the distance as fast as possible
   Time_Zero    : Time := Clock;


   --  Other specifications  --
  BLACKLINE_THRESHOLD  : constant Integer := 600; -- For white area, light is around 830
  MOTORSPEED           : constant Integer := 400; -- constant motor speed value
  THRESHOLD            : constant Integer := 80;  -- threshold value
end tasks3;
