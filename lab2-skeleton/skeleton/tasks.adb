with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Real_Time;  use Ada.Real_Time;
with Ada.Command_Line; use Ada.Command_Line;
with System;

with Webots_API;   use Webots_API;

package body Tasks is
  -------------
  --  Tasks  --
  -------------   
  task HelloworldTask is
    -- define its priority higher than the main procedure --
    pragma Priority(2);
    -- main procedure priority is declared at main.adb:9
  end HelloworldTask;

  task body HelloworldTask is
    Next_Time : Time := Time_Zero;
    MOTORSPEED : constant Integer := 200; -- Could be adjusted between [-999, +999]

  begin      
    -- task body starts here ---
    loop
      -- read sensors and print ----
      Put_Line("LS1      : " & read_light_sensor(LS1)'Image);
      Put_Line("LS2      : " & read_light_sensor(LS2)'Image);
      Put_Line("LS3      : " & read_light_sensor(LS3)'Image);
      Put_Line("UP       : " & button_pressed(UpButton)'Image);
      Put_Line("DOWN     : " & button_pressed(DownButton)'Image);
      Put_Line("LEFT     : " & button_pressed(LeftButton)'Image);
      Put_Line("RIGHT    : " & button_pressed(RightButton)'Image);
      Put_Line("DISTANCE : " & read_distance_sensor'Image);
      Put_Line("-----------------------------------");

      set_motor_speed(LeftMotor, MOTORSPEED);
      set_motor_speed(RightMotor, MOTORSPEED);

      Next_Time := Next_Time + Period_Display;
      delay until Next_Time;

      exit when simulation_stopped;
    end loop;
  end HelloworldTask;

  -- Background procedure required for package
  procedure Background is begin
    while not simulation_stopped 
      loop
        delay 0.25;
      end loop;
  end Background;

end Tasks;
