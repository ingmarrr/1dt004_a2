with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Real_Time;  use Ada.Real_Time;
with System;

with Webots_API;   use Webots_API;

package body Tasks3 is
  protected MotorData is
    procedure SetLeft (Left: Integer);
    procedure SetRight (Right: Integer);
    procedure SetMoving (value: Integer);
    function GetLeft  return Integer;
    function GetRight return Integer;
    function GetMoving return Boolean;
  private
    left_speed  : Integer := 0;
    right_speed : Integer := 0;
    moving      : Boolean;
  end MotorData;

  task MotorControlTask is
    pragma Priority (2);
  end MotorControlTask;

  task LineFollowingTask is
    pragma Priority (3);
  end LineFollowingTask;

  task DistanceTask is
    pragma Priority (4);
  end DistanceTask;

  task DisplayTask is
    pragma Priority (5);
  end DisplayTask;

  protected body MotorData is
    procedure SetLeft (Left: Integer) is
    begin
      left_speed := Left;
    end SetLeft;

    procedure SetRight (Right: Integer) is
    begin
      right_speed := Right;
    end SetRight;

    procedure SetMoving (value: Integer) is
    begin
      if value = 1 then
        moving := True;
      else
        moving := False;
      end if;
    end SetMoving;

    function GetMoving return Boolean is
    begin
      return moving;
    end GetMoving;

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

      set_motor_speed (LeftMotor, MotorData.GetLeft);
      set_motor_speed (RightMotor, MotorData.GetRight);
      exit when simulation_stopped;
    end loop;
  end MotorControlTask;

  task body LineFollowingTask is
    ls_1      : Integer;
    ls_2      : Integer;
    ls_3      : Integer;
    next_time : Time := Time_Zero;
    blackline_detected: Boolean := False;
  begin
    loop
      delay until next_time;
      next_time := next_time + TIME_DELTA;

      if not MotorData.GetMoving then  -- the robot should not move
        MotorData.SetLeft (0);
        MotorData.SetRight (0);
      else -- when it is moving
        ls_1 := read_light_sensor(LS1);
        ls_2 := read_light_sensor(LS2);
        ls_3 := read_light_sensor(LS3);

        if (ls_1 < BLACKLINE_THRESHOLD and ls_2 < BLACKLINE_THRESHOLD and ls_3 < BLACKLINE_THRESHOLD)
          or (ls_1 > BLACKLINE_THRESHOLD and ls_2 < BLACKLINE_THRESHOLD and ls_3 > BLACKLINE_THRESHOLD)
          or (ls_1 < BLACKLINE_THRESHOLD and ls_2 > BLACKLINE_THRESHOLD and ls_3 < BLACKLINE_THRESHOLD)
        then
          MotorData.SetLeft (MOTORSPEED);
          MotorData.SetRight (MOTORSPEED);
        elsif ls_1 < BLACKLINE_THRESHOLD then
          MotorData.SetLeft (0);
          MotorData.SetRight (MOTORSPEED);
        elsif ls_3 < BLACKLINE_THRESHOLD then
          MotorData.SetLeft (MOTORSPEED);
          MotorData.SetRight (0);
        else
          MotorData.SetLeft (0);
          MotorData.SetRight (0);
        end if;
      end if;
    end loop;
  end LineFollowingTask;

  task body DistanceTask is 
    next_time : Time := Time_Zero;
    distance  : Integer;
  begin
    loop
      delay until next_time;
      next_time := next_time + TIME_DELTA2;

      distance := read_distance_sensor;

      if distance > THRESHOLD then --distance greater than a threshold - should stop the motors
        MotorData.SetMoving (2);
      else -- above the threshold - resumes moving
        MotorData.SetMoving (1);
      end if;
    end loop;
  end DistanceTask;

  task body DisplayTask is -- display some stats
    next_time : Time := Time_Zero;
  begin
    loop
      delay until next_time;
      next_time := next_time + TIME_DELTA;

      Put_Line ("LS1      : " & read_light_sensor (LS1)'Image);
      Put_Line ("LS2      : " & read_light_sensor (LS2)'Image);
      Put_Line ("LS3      : " & read_light_sensor (LS3)'Image);
      Put_Line ("DISTANCE : " & read_distance_sensor'Image);
      Put_Line ("-----------------------------------");
    end loop;
  end DisplayTask;

  ----------------
  -- Background Procedure --
  ----------------
  procedure Background is
  begin
    while not simulation_stopped 
      loop
        delay 0.25; -- Prevents busy waiting
      end loop;
  end Background;
end Tasks3;
