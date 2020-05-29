with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Source_Info;
with Interfaces.C; use Interfaces.C;
with Linux_ioctl_helpers;

package body Linux_GPIO is
   function C_Ioctl (S : Interfaces.C.int; Req : Interfaces.Unsigned_32; Arg : access Linux_GPIO.gpiohandle_data)    return Interfaces.Integer_32;
   function C_Ioctl (S : Interfaces.C.int; Req : Interfaces.Unsigned_32; Arg : access Linux_GPIO.gpiohandle_request) return Interfaces.Integer_32;
   function C_Ioctl (S : Interfaces.C.int; Req : Interfaces.Unsigned_32; Arg : access Linux_GPIO.gpioevent_request)  return Interfaces.Integer_32;
   pragma Import (C, C_Ioctl, "ioctl");

   handle_data     : aliased Linux_GPIO.gpiohandle_data;
   ctrlc           : Boolean := False;
   ioctl_exception : exception;
   read_exception  : exception;
   label_exception : exception;

   function GPIO_GET_CHIPINFO_IOCTL (c : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Linux_ioctl_helpers.C_IOR (16#B4#, 16#1#, c);
   end GPIO_GET_CHIPINFO_IOCTL;

   function GPIO_GET_LINEEVENT_IOCTL (c : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Linux_ioctl_helpers.C_IOWR (16#B4#, 16#4#, c);
   end GPIO_GET_LINEEVENT_IOCTL;

   function GPIO_GET_LINEHANDLE_IOCTL (c : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Linux_ioctl_helpers.C_IOWR (16#B4#, 16#3#, c);
   end GPIO_GET_LINEHANDLE_IOCTL;

   function GPIO_GET_LINEINFO_IOCTL (c : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Linux_ioctl_helpers.C_IOWR (16#B4#, 16#2#, c);
   end GPIO_GET_LINEINFO_IOCTL;

   function GPIOHANDLE_GET_LINE_VALUES_IOCTL (c : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Linux_ioctl_helpers.C_IOWR (16#B4#, 16#8#, c);
   end GPIOHANDLE_GET_LINE_VALUES_IOCTL;

   function GPIOHANDLE_SET_LINE_VALUES_IOCTL (c : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Linux_ioctl_helpers.C_IOWR (16#B4#, 16#9#, c);
   end GPIOHANDLE_SET_LINE_VALUES_IOCTL;

   function  Is_Bit_Set                       (C : Interfaces.Unsigned_32; B : Interfaces.Unsigned_32) return Boolean is
      Count : Integer := 1;
   begin
      while Count <= 32 loop
         if B mod 2 = 1 and then C mod 2 = 1 then
            return True;
         end if;

         Count := Count + 1;
      end loop;

      return False;
   end Is_Bit_Set;

   procedure Monitor_CTRL_C_Called is
   begin
      ctrlc := True;
   end Monitor_CTRL_C_Called;

   function Monitor_CTRL_C_Is_Called return Boolean is
   begin
      return ctrlc;
   end Monitor_CTRL_C_Is_Called;

   procedure Monitor_Device_Close (fd : Linux_GPIO.fd_type) is
      monitor_close   : exception;
      res             : Boolean;
   begin
      GNAT.OS_Lib.Close (GNAT.OS_Lib.File_Descriptor (fd), res);

      if not res then
         raise monitor_close with "Close failed on fd: " & fd'Image & ", errno := " & GNAT.OS_Lib.Errno'Img & ", " & GNAT.OS_Lib.Errno_Message;
      end if;
   end Monitor_Device_Close;

   procedure Monitor_Device_Event_Open (ldevname      : String;
                                  event_request : aliased in out Linux_GPIO.gpioevent_request;
                                  fd            : out fd_type) is
      ret     : Interfaces.Integer_32;
      lmode   : constant GNAT.OS_Lib.Mode := GNAT.OS_Lib.Text;
      monitor_open    : exception;
   begin
      fd := fd_type (GNAT.OS_Lib.Open_Read_Write (ldevname, lmode));
      if fd < 0 then
         raise monitor_open with "Open_Read failed... fd : " & fd'Image & ", path : " & ldevname & ", errno := " & GNAT.OS_Lib.Errno'Img & ", " & GNAT.OS_Lib.Errno_Message;
      end if;

      ret := C_Ioctl (Interfaces.C.int (fd), Linux_GPIO.GPIO_GET_LINEEVENT_IOCTL (event_request'Size / 8), event_request'Access);

      if ret < 0 then
         raise ioctl_exception with "GPIO_GET_LINEEVENT_IOCTL error : " & ret'Image & ", errno := " & GNAT.OS_Lib.Errno'Img & ", " & GNAT.OS_Lib.Errno_Message;
      end if;
   end Monitor_Device_Event_Open;

   procedure Monitor_Device_Request_Open (LDevName       : String;
                                          Lines          : lineoffsets_array;
                                          NLines         : Interfaces.Unsigned_32;
                                          Flags          : flags_type;
                                          Handle_Data    : Linux_GPIO.gpiohandle_data;
                                          Consumer_Label : Ada.Strings.Unbounded.Unbounded_String;
                                          fd             : out fd_type) is
      Request         : aliased gpiohandle_request;
      ret             : Interfaces.Integer_32;
      lmode           : constant GNAT.OS_Lib.Mode := GNAT.OS_Lib.Text;
      monitor_open    : exception;
      Blanks_32       : String := Ada.Characters.Latin_1.NUL & "                               "; --  31 of...
      tmp_integer     : Interfaces.Unsigned_32 := 0;
   begin
      Request.flags          := Flags;
      Request.lineoffsets    := Lines;
      Request.lines          := NLines;

      if Ada.Strings.Unbounded.Length (Consumer_Label) > 31 then
         raise label_exception;
      end if;

      if Ada.Strings.Unbounded.Length (Consumer_Label) < 31 then
         Request.consumer_label := Interfaces.C.To_C (Ada.Strings.Unbounded.To_String (Consumer_Label) & Blanks_32 (1 .. (31 - Ada.Strings.Unbounded.Length (Consumer_Label))));
      else
         Request.consumer_label := Interfaces.C.To_C (Ada.Strings.Unbounded.To_String (Consumer_Label));
      end if;

      fd := fd_type (GNAT.OS_Lib.Open_Read_Write (ldevname, lmode));

      if fd < 0 then
         raise monitor_open with "Open_Read failed... fd : " & fd'Image & ", path : " & ldevname & ", errno := " & GNAT.OS_Lib.Errno'Img & ", " & GNAT.OS_Lib.Errno_Message;
      end if;

      if Is_Bit_Set (Flags, GPIOHANDLE_REQUEST_OUTPUT) then
         while tmp_integer < NLines loop
            Request.default_values.values (Integer (tmp_integer)) := Handle_Data.values (Integer (tmp_integer));
            tmp_integer := tmp_integer + 1;
         end loop;
--         Request.default_values := Handle_Data;
         null;
      end if;

      ret := C_Ioctl (Interfaces.C.int (fd), Linux_GPIO.GPIO_GET_LINEHANDLE_IOCTL (Request'Size / 8), Request'Access);

      if ret < 0 then
         raise ioctl_exception with "GPIO_GET_LINEEVENT_IOCTL error : " & ret'Image & ", errno := " & GNAT.OS_Lib.Errno'Img & ", " & GNAT.OS_Lib.Errno_Message;
      end if;
   end Monitor_Device_Request_Open;

   procedure Monitor_Get_Pins (fd   : Linux_GPIO.fd_type;
                               data : aliased in out Linux_GPIO.gpiohandle_data) is
   begin
      if C_Ioctl (fd, Linux_GPIO.GPIOHANDLE_GET_LINE_VALUES_IOCTL (data'Size / 8), data'Access) < 0 then
         raise ioctl_exception with GNAT.Source_Info.Line'Img;
      end if;
   end Monitor_Get_Pins;

   procedure Monitor_Wait_For_Signal (fd          : Linux_GPIO.fd_type;
                                      event_data  : aliased out Linux_GPIO.gpioevent_data) is
   begin
      if C_Ioctl (fd, Linux_GPIO.GPIOHANDLE_GET_LINE_VALUES_IOCTL (handle_data'Size / 8), handle_data'Access) < 0 then
         raise ioctl_exception with GNAT.Source_Info.Line'Img;
      end if;

      if GNAT.OS_Lib.Read (GNAT.OS_Lib.File_Descriptor (fd), event_data'Address, event_data'Size / 8) < 0 then
         raise read_exception with GNAT.Source_Info.Line'Img;
      end if;
   end Monitor_Wait_For_Signal;

   procedure Monitor_Set_Pins (fd   : Linux_GPIO.fd_type;
                               data : aliased in out Linux_GPIO.gpiohandle_data) is
   begin
      if C_Ioctl (fd, Linux_GPIO.GPIOHANDLE_SET_LINE_VALUES_IOCTL (data'Size / 8), data'Access) < 0 then
         raise ioctl_exception with GNAT.Source_Info.Line'Img;
      end if;
   end Monitor_Set_Pins;
end Linux_GPIO;
