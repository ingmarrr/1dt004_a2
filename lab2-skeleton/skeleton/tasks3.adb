with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Real_Time;  use Ada.Real_Time;
with System;

with Webots_API;   use Webots_API;

package body Tasks3 is

   BLACKLINE_THRESHOLD  : constant Integer := 600; -- For white area, light is around 830
   MOTORSPEED           : constant Integer := 400;

   protected MotorData is
      procedure SetLeft(Left: Integer);
      procedure SetRight(Right: Integer);
      function GetLeft  return Integer;
      function GetRight return Integer;
   private
      pragma Priority(4);
      left_speed : Integer := 0;
      right_speed: Integer := 0;
   end MotorData;

   task MotorControlTask is
     pragma Priority(2);
   end MotorControlTask;

   task LineFollowingTask is
     pragma Priority(3);
   end LineFollowingTask;

   task DistanceTask is
     pragma Priority(4);
   end DistanceTask;
   
   task DisplayTask is
     pragma Priority(5);
   end DisplayTask;

   protected body MotorData is
      procedure SetLeft(Left: Integer) is
      begin
         left_speed := Left;
      end SetLeft;

      procedure SetRight(Right: Integer) is
      begin
         right_speed := Right;
      end SetRight;

      function GetLeft return Integer is
      begin
         return left_speed;
      end GetLeft;

      function GetRight return Integer is
      begin
         return right_speed;
      end GetRight;
   end MotorData;

  task body MotorControlTask is
    next_time : Time := Time_Zero;
  begin
    loop
      delay until next_time;
      next_time := next_time + TIME_DELTA;

      set_motor_speed(LeftMotor, MotorData.GetLeft);
      set_motor_speed(RightMotor, MotorData.GetRight);
      exit when simulation_stopped;
    end loop;
  end MotorControlTask;

  task body LineFollowingTask is
    ls1        : Integer;
    ls2        : Integer;
    ls3        : Integer;
    next_time  : Time := Time_Zero;
    blackline_detected: Boolean := False;
  begin
    loop
      delay until next_time;
      next_time := next_time + TIME_DELTA;

      ls1 := read_light_sensor(LS1);
      ls2 := read_light_sensor(LS2);
      ls3 := read_light_sensor(LS3);

      if ls1 < BLACKLINE_THRESHOLD and ls2 < BLACKLINE_THRESHOLD and ls3 < BLACKLINE_THRESHOLD 
         or not ls1 < BLACKLINE_THRESHOLD and ls2 < BLACKLINE_THRESHOLD and not ls3 < BLACKLINE_THRESHOLD
         or ls1 < BLACKLINE_THRESHOLD and not ls2 < BLACKLINE_THRESHOLD and ls3 < BLACKLINE_THRESHOLD 
      then
         MotorData.SetLeft(MOTORSPEED);
         MotorData.SetRight(MOTORSPEED);
      elsif ls1 < BLACKLINE_THRESHOLD then
         MotorData.SetLeft(0);
         MotorData.SetRight(MOTORSPEED);
      elsif ls3 < BLACKLINE_THRESHOLD then
         MotorData.SetLeft(MOTORSPEED);
         MotorData.SetRight(0);
      else
         MotorData.SetLeft(0);
         MotorData.SetRight(0);
      end if;
    end loop;
  end LineFollowingTask;

  task body DistanceTask is 
    next_time : Time := Time_Zero;
    distance  : Intger;
  begin
    loop
      delay until next_time;
      next_time := next_time + TIME_DELTA;

      distance := read_distance_sensor;
      if distance < 60 then
        MotorData.SetLeft(MOTORSPEED * 0.5);
        MotorData.SetRight(MOTORSPEED * 0.5);
      elsif distance < 40 then
        MotorData.SetLeft(MOTORSPEED * 0.3);
        MotorData.SetRight(MOTORSPEED * 0.3);
      elsif distance < 20 then
        MotorData.SetLeft(MOTORSPEED * 0.1);
        MotorData.SetRight(MOTORSPEED * 0.1);
      else
        MotorData.SetLeft(0);
        MotorData.SetRight(0);
      end if;

    end loop;
  end DistanceTask;

  task body DisplayTask is
    next_time : Time := Time_Zero;
  begin
    loop
      delay until next_time;
      next_time := next_time + TIME_DELTA;

      Ada.Text_IO.Put_Line("LS1      : " & read_light_sensor(LS1)'Image);
      Ada.Text_IO.Put_Line("LS2      : " & read_light_sensor(LS2)'Image);
      Ada.Text_IO.Put_Line("LS3      : " & read_light_sensor(LS3)'Image);
      Ada.Text_IO.Put_Line("UP       : " & button_pressed(UpButton)'Image);
      Ada.Text_IO.Put_Line("DOWN     : " & button_pressed(DownButton)'Image);
      Ada.Text_IO.Put_Line("LEFT     : " & button_pressed(LeftButton)'Image);
      Ada.Text_IO.Put_Line("RIGHT    : " & button_pressed(RightButton)'Image);
      Ada.Text_IO.Put_Line("DISTANCE : " & read_distance_sensor'Image);
      Ada.Text_IO.Put_Line("-----------------------------------");
    end loop;
  end DisplayTask;

  ----------------
  -- Background Procedure --
  ----------------
  procedure Background is
  begin
     while not simulation_stopped loop
        delay 0.25; -- Prevents busy waiting
     end loop;
  end Background;

end Tasks3;
