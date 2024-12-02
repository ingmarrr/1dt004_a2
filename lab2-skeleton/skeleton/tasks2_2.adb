with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Real_Time;  use Ada.Real_Time;
with System;

with Webots_API;   use Webots_API;

package body Tasks2_2 is

  BLACKLINE_THRESHOLD : constant Integer := 600; -- For white area, light is around 830
  MOTORSPEED : constant Integer := 400;

  protected Event is
    entry Wait (id : out EventID);
    procedure Signal (id : in EventID);
  private
    pragma Priority (4);
    current_id : EventID := Idle; 
    signalled  : Boolean := false;
  end Event;

  task EventDispatcherTask is
    pragma Priority (2);
  end EventDispatcherTask;

  task MotorControlTask is
    pragma Priority (3);
  end MotorControlTask;

  protected body Event is
    entry Wait (id : out EventID) when signalled is
    begin
      id := current_id;
      signalled := false;
    end Wait;

    procedure Signal (id : in EventID) is
    begin
      current_id := id;
      signalled := true;
    end Signal;
  end Event;

  task body EventDispatcherTask is
    last_event : EventID := Idle;
    light_value2 : Integer;
    blackline_detected : Boolean := False;
    last_dir : Dir := Undefined;     -- Stores last robot direction (up/down), important for line crossing
    Next_Time : Time := Clock;
   
  begin
    loop
      light_value2 := read_light_sensor(LS2);

      if light_value2 < BLACKLINE_THRESHOLD and not blackline_detected then
        Event.Signal (On_Blackline);
        blackline_detected := True;
        Put_Line ("Black line detected. Car stopped.");
      elsif light_value2 >= BLACKLINE_THRESHOLD and blackline_detected then
        Event.Signal (Off_Blackline);
        blackline_detected := False;
        Put_Line ("White area detected. Waiting for directional input.");
      elsif blackline_detected then    -- Can move in reverse direction when detects a line
        if last_dir = Up and button_pressed (DownButton) then    
          Event.Signal (DownButtonPressed);
          last_dir := Down;
        elsif last_dir = Down and button_pressed (UpButton) then 
          Event.Signal (UpButtonPressed);
          last_dir:= Up;
        elsif last_dir = Undefined and button_pressed (LeftButton) then   -- If robot spawns at default location (parallel on black line, facing downwards), it should turn right to get inside the black circle
          Event.Signal (LeftButtonPressed);
          last_dir := Down;
        end if;
      end if;

      if not blackline_detected then 
        if button_pressed (UpButton) and (last_event /= UpButtonPressed) then
          Event.Signal (UpButtonPressed);
          last_event := UpButtonPressed;
          last_dir := Up;
          Put_Line ("Up button pressed.");
        elsif button_pressed (DownButton) and (last_event /= DownButtonPressed) then
          Event.Signal (DownButtonPressed);
          last_event := DownButtonPressed;
          last_dir := Down;
          Put_Line ("Down button pressed.");
        elsif button_pressed (RightButton) and (last_event /= RightButtonPressed) then
          Event.Signal (RightButtonPressed);
          last_event := RightButtonPressed;
          Put_Line ("Right button pressed.");
          last_dir := Up;  -- Turning is implemented as a "forward" movement (↓)
        elsif button_pressed (LeftButton) and (last_event /= LeftButtonPressed) then
          Event.Signal (LeftButtonPressed);
          last_event := LeftButtonPressed;
          Put_Line ("Left button pressed.");
          last_dir := Up;  -- Turning is implemented as a "forward" movement (↓)
        elsif not button_pressed (UpButton) and (last_event = UpButtonPressed) then
          Event.Signal (UpButtonReleased);
          last_event := UpButtonReleased;
          Put_Line ("Up button released.");
        elsif not button_pressed (DownButton) and (last_event = DownButtonPressed) then
          Event.Signal (DownButtonReleased);
          last_event := DownButtonReleased;
          Put_Line ("Down button released.");
        elsif not button_pressed (RightButton) and (last_event = RightButtonPressed) then
          Event.Signal (RightButtonReleased);
          last_event := RightButtonReleased;
          Put_Line ("Right button released.");
        elsif not button_pressed (LeftButton) and (last_event = LeftButtonPressed) then
          Event.Signal (LeftButtonReleased);
          last_event := LeftButtonReleased;
          Put_Line ("Left button released.");
        end if;
      end if;

        Next_Time := Next_Time + Period_Display10m;
        delay until Next_Time;

    exit when simulation_stopped;
    end loop;
  end EventDispatcherTask;

  task body MotorControlTask is
    received_event : EventID := Idle;
  begin
     loop
     Event.Wait(received_event); -- Waiting for signalled events
           case received_event is
              when UpButtonPressed =>
                 set_motor_speed(LeftMotor, -MOTORSPEED); -- Could be adjusted between [-999, +999]
                 set_motor_speed(RightMotor, -MOTORSPEED);
              when DownButtonPressed =>
                 set_motor_speed(LeftMotor, MOTORSPEED);
                 set_motor_speed(RightMotor, MOTORSPEED);
              when RightButtonPressed =>
                 set_motor_speed(LeftMotor, 0);
                 set_motor_speed(RightMotor, MOTORSPEED);
              when LeftButtonPressed =>
                 set_motor_speed(LeftMotor, MOTORSPEED);
                 set_motor_speed(RightMotor, 0);
              when UpButtonReleased | DownButtonReleased | RightButtonReleased | LeftButtonReleased =>
                 set_motor_speed(LeftMotor, 0);
                 set_motor_speed(RightMotor, 0);
               when On_Blackline =>
               set_motor_speed(LeftMotor, 0);
               set_motor_speed(RightMotor, 0);
               when Off_Blackline =>
                  null;
              when others =>
                 Put_Line("Unknown Event");
           end case;

    exit when simulation_stopped;
    end loop;
  end MotorControlTask;

  ----------------
  -- Background Procedure --
  ----------------
  procedure Background is
  begin
     while not simulation_stopped loop
        delay 0.25; -- Prevents busy waiting
     end loop;
  end Background;
end Tasks2_2;
