with Ada.Characters.Latin_1;
with GNAT.OS_Lib;
with GNAT.Source_Info;
with Interfaces.C; use Interfaces.C;
with Linux_IOCTL_Helpers;

package body Linux_GPIO is
   function C_Ioctl (S : Interfaces.C.int; Req : Interfaces.Unsigned_32; Arg : access GPIO_Handle_Data)    return Interfaces.Integer_32;
   function C_Ioctl (S : Interfaces.C.int; Req : Interfaces.Unsigned_32; Arg : access GPIO_Handle_Request) return Interfaces.Integer_32;
   function C_Ioctl (S : Interfaces.C.int; Req : Interfaces.Unsigned_32; Arg : access GPIO_Event_Request)  return Interfaces.Integer_32;
   pragma Import (C, C_Ioctl, "ioctl");
   function Get_Label (S : Ada.Strings.Unbounded.Unbounded_String) return Consumer_Type;

   CTRL_C          : Boolean := False;
   Blanks_32       : constant String := Ada.Characters.Latin_1.NUL & "                               "; --  31 of...
   Monitor_Open    : exception;
   IOCTL_Exception : exception;
   Read_Exception  : exception;
   Label_Exception : exception;

   function Get_Label (S : Ada.Strings.Unbounded.Unbounded_String) return Consumer_Type is
   begin
      if Ada.Strings.Unbounded.Length (S) > 31 then
         raise Label_Exception;
      else
         if Ada.Strings.Unbounded.Length (S) < 31 then
            return Interfaces.C.To_C (Ada.Strings.Unbounded.To_String (S) & Blanks_32 (1 .. (31 - Ada.Strings.Unbounded.Length (S))));
         else
            return Interfaces.C.To_C (Ada.Strings.Unbounded.To_String (S));
         end if;
      end if;
   end Get_Label;

   function GPIO_GET_CHIPINFO_IOCTL (C : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Linux_IOCTL_Helpers.C_IOR (16#B4#, 16#1#, C);
   end GPIO_GET_CHIPINFO_IOCTL;

   function GPIO_GET_LINEEVENT_IOCTL (C : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Linux_IOCTL_Helpers.C_IOWR (16#B4#, 16#4#, C);
   end GPIO_GET_LINEEVENT_IOCTL;

   function GPIO_GET_LINEHANDLE_IOCTL (C : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Linux_IOCTL_Helpers.C_IOWR (16#B4#, 16#3#, C);
   end GPIO_GET_LINEHANDLE_IOCTL;

   function GPIO_GET_LINEINFO_IOCTL (C : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Linux_IOCTL_Helpers.C_IOWR (16#B4#, 16#2#, C);
   end GPIO_GET_LINEINFO_IOCTL;

   function GPIOHANDLE_GET_LINE_VALUES_IOCTL (C : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Linux_IOCTL_Helpers.C_IOWR (16#B4#, 16#8#, C);
   end GPIOHANDLE_GET_LINE_VALUES_IOCTL;

   function GPIOHANDLE_SET_LINE_VALUES_IOCTL (C : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Linux_IOCTL_Helpers.C_IOWR (16#B4#, 16#9#, C);
   end GPIOHANDLE_SET_LINE_VALUES_IOCTL;

   function  Is_Bit_Set                       (C : Interfaces.Unsigned_32; B : Interfaces.Unsigned_32) return Boolean is
      Tmp_B : Interfaces.Unsigned_32;
      Tmp_C : Interfaces.Unsigned_32;
      Count : Integer := 1;
   begin
      Tmp_B := B;
      Tmp_C := C;
      while Count <= 32 loop
         if Tmp_B mod 2 = 1 and then Tmp_C mod 2 = 1 then
            return True;
         end if;

         Tmp_B := Shift_Right (Tmp_B, 1);
         Tmp_C := Shift_Right (Tmp_C, 1);
         Count := Count + 1;
      end loop;

      return False;
   end Is_Bit_Set;

   procedure Monitor_CTRL_C_Called is
   begin
      CTRL_C := True;
   end Monitor_CTRL_C_Called;

   function Monitor_CTRL_C_Is_Called return Boolean is
   begin
      return CTRL_C;
   end Monitor_CTRL_C_Is_Called;

   procedure Monitor_Device_Close (FD : FD_Type) is
      Monitor_Close   : exception;
      Res             : Boolean;
   begin
      GNAT.OS_Lib.Close (GNAT.OS_Lib.File_Descriptor (FD), Res);

      if not Res then
         raise Monitor_Close with "Close failed on fd: " & FD'Image & ", errno := " & GNAT.OS_Lib.Errno'Img & ", " & GNAT.OS_Lib.Errno_Message;
      end if;
   end Monitor_Device_Close;

   procedure Monitor_Device_Event_Open (LDev_Name     : String;
                                        Event_Request : aliased in out GPIO_Event_Request;
                                        IOCTL_FD      : out FD_Type) is
      Ret               : Interfaces.Integer_32;
      LMode             : constant GNAT.OS_Lib.Mode := GNAT.OS_Lib.Text;
      Monitor_Excpetion : exception;
      FD                : FD_Type;
   begin
      FD := FD_Type (GNAT.OS_Lib.Open_Read (LDev_Name, LMode));
      if FD < 0 then
         raise Monitor_Excpetion with "Open_Read failed... fd : " & FD'Image & ", path : " & LDev_Name & ", errno := " & GNAT.OS_Lib.Errno'Img & ", " & GNAT.OS_Lib.Errno_Message;
      end if;

      Ret := C_Ioctl (Interfaces.C.int (FD), GPIO_GET_LINEEVENT_IOCTL (Event_Request'Size / 8), Event_Request'Access);
      IOCTL_FD := Event_Request.FD;
      Monitor_Device_Close (FD);

      if Ret < 0 then
         raise IOCTL_Exception with "GPIO_GET_LINEEVENT_IOCTL error : " & Ret'Image & ", errno := " & GNAT.OS_Lib.Errno'Img & ", " & GNAT.OS_Lib.Errno_Message;
      end if;
   end Monitor_Device_Event_Open;

   procedure Monitor_Device_Event_Open (LDev_Name      : String;
                                        Pin            : Pin_Num;
                                        Flags          : Linux_GPIO.Handle_Flags_Type;
                                        Consumer_Label : Ada.Strings.Unbounded.Unbounded_String;
                                        IOCTL_FD       : out FD_Type) is
      Ret             : Interfaces.Integer_32;
      LMode           : constant GNAT.OS_Lib.Mode := GNAT.OS_Lib.Text;
      Event_Request   : aliased GPIO_Event_Request;
      FD              : FD_Type;
   begin
      Event_Request.Line_Offset    := Pin;
      Event_Request.Event_Flags    := Flags;
      Event_Request.Handle_Flags   := Linux_GPIO.GPIOHANDLE_REQUEST_NONE;
      Event_Request.Consumer_Label := Get_Label (Consumer_Label);
      FD := FD_Type (GNAT.OS_Lib.Open_Read (LDev_Name, LMode));

      if FD < 0 then
         raise Monitor_Open with "Open_Read failed... fd : " & FD'Image & ", path : " & LDev_Name & ", errno := " & GNAT.OS_Lib.Errno'Img & ", " & GNAT.OS_Lib.Errno_Message;
      end if;

      Ret      := C_Ioctl (Interfaces.C.int (FD), GPIO_GET_LINEEVENT_IOCTL (Event_Request'Size / 8), Event_Request'Access);
      IOCTL_FD := Event_Request.FD;
      Monitor_Device_Close (FD);

      if Ret < 0 then
         raise IOCTL_Exception with "GPIO_GET_LINEEVENT_IOCTL error : " & Ret'Image & ", errno := " & GNAT.OS_Lib.Errno'Img & ", " & GNAT.OS_Lib.Errno_Message;
      end if;
   end Monitor_Device_Event_Open;

   procedure Monitor_Device_Request_Open (LDev_Name      : String;
                                          Lines          : Line_Offsets_Array;
                                          NLines         : Interfaces.Unsigned_32;
                                          Flags          : Flags_Type;
                                          Handle_Data    : GPIO_Handle_Data;
                                          Consumer_Label : Ada.Strings.Unbounded.Unbounded_String;
                                          IOCTL_FD       : out FD_Type) is
      Request         : aliased GPIO_Handle_Request;
      Ret             : Interfaces.Integer_32;
      FD              : FD_Type;
      LMode           : constant GNAT.OS_Lib.Mode := GNAT.OS_Lib.Text;
   begin
      Request.Flags          := Flags;
      Request.Line_Offsets   := Lines;
      Request.Lines          := NLines;
      Request.Consumer_Label := Get_Label (Consumer_Label);

      FD := FD_Type (GNAT.OS_Lib.Open_Read (LDev_Name, LMode));

      if FD < 0 then
         raise Monitor_Open with "Open_Read failed... fd : " & FD'Image & ", path : " & LDev_Name & ", errno := " & GNAT.OS_Lib.Errno'Img & ", " & GNAT.OS_Lib.Errno_Message;
      end if;

      if Is_Bit_Set (Flags, GPIOHANDLE_REQUEST_OUTPUT) then
         Request.Default_Values := Handle_Data.Values;
      end if;

      Ret := C_Ioctl (Interfaces.C.int (FD), GPIO_GET_LINEHANDLE_IOCTL (Request'Size / 8), Request'Access);
      IOCTL_FD := Request.FD;
      Monitor_Device_Close (FD);

      if Ret < 0 then
         raise IOCTL_Exception with "GPIO_GET_LINEEVENT_IOCTL error : " & Ret'Image & ", errno := " & GNAT.OS_Lib.Errno'Img & ", " & GNAT.OS_Lib.Errno_Message;
      end if;

   end Monitor_Device_Request_Open;

   procedure Monitor_Get_Pins (FD   : FD_Type;
                               Data : aliased in out GPIO_Handle_Data) is
      Ret : Interfaces.Integer_32;
   begin
      Ret := C_Ioctl (FD, GPIOHANDLE_GET_LINE_VALUES_IOCTL (Data'Size / 8), Data'Access);

      if Ret < 0 then
         raise IOCTL_Exception with GNAT.Source_Info.Line'Img;
      end if;
   end Monitor_Get_Pins;

   procedure Monitor_Set_Pins (FD   : FD_Type;
                               Data : GPIO_Handle_Data) is
      IOCTL_Data      : aliased GPIO_Handle_Data;
      Ret             : Interfaces.Integer_32;
   begin
      IOCTL_Data.Values := Data.Values;
      Ret := C_Ioctl (Interfaces.C.int (FD), GPIOHANDLE_SET_LINE_VALUES_IOCTL (IOCTL_Data'Size / 8), IOCTL_Data'Access);

      if Ret < 0 then
         raise IOCTL_Exception with GNAT.Source_Info.Line'Img;
      end if;
   end Monitor_Set_Pins;

   procedure Monitor_Wait_For_Signal (FD          : FD_Type;
                                      Event_Data  : aliased out GPIO_Event_Data) is
      Handle_Data     : aliased GPIO_Handle_Data;
   begin
      if C_Ioctl (FD, GPIOHANDLE_GET_LINE_VALUES_IOCTL (Handle_Data'Size / 8), Handle_Data'Access) < 0 then
         raise IOCTL_Exception with GNAT.Source_Info.Line'Img;
      end if;

      if GNAT.OS_Lib.Read (GNAT.OS_Lib.File_Descriptor (FD), Event_Data'Address, Event_Data'Size / 8) < 0 then
         raise Read_Exception with GNAT.Source_Info.Line'Img;
      end if;
   end Monitor_Wait_For_Signal;

end Linux_GPIO;
