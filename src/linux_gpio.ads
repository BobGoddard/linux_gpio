with Interfaces; use Interfaces;
with Interfaces.C;
with Ada.Strings.Unbounded;

package Linux_GPIO is

   subtype GPIO_REQUEST_EDGE_TYPE is Interfaces.Unsigned_32 range 1 .. 3;
   GPIOEVENT_REQUEST_RISING_EDGE  : constant GPIO_REQUEST_EDGE_TYPE := Shift_Left (1, 0);
   GPIOEVENT_REQUEST_FALLING_EDGE : constant GPIO_REQUEST_EDGE_TYPE := Shift_Left (1, 1);
   GPIOEVENT_REQUEST_BOTH_EDGES   : constant GPIO_REQUEST_EDGE_TYPE := GPIOEVENT_REQUEST_RISING_EDGE + GPIOEVENT_REQUEST_FALLING_EDGE;

   subtype GPIO_EVENT_EDGE_TYPE is Interfaces.Unsigned_32 range 1 .. 2;
   GPIOEVENT_EVENT_RISING_EDGE    : constant GPIO_EVENT_EDGE_TYPE := Shift_Left (1, 0);
   GPIOEVENT_EVENT_FALLING_EDGE   : constant GPIO_EVENT_EDGE_TYPE := Shift_Left (1, 1);

   GPIOLINE_FLAG_NONE             : constant Interfaces.Unsigned_32 := 0;
   GPIOLINE_FLAG_KERNEL           : constant Interfaces.Unsigned_32 := Shift_Left (1, 0);
   GPIOLINE_FLAG_IS_OUT           : constant Interfaces.Unsigned_32 := Shift_Left (1, 1);
   GPIOLINE_FLAG_ACTIVE_LOW       : constant Interfaces.Unsigned_32 := Shift_Left (1, 2);
   GPIOLINE_FLAG_OPEN_DRAIN       : constant Interfaces.Unsigned_32 := Shift_Left (1, 3);
   GPIOLINE_FLAG_OPEN_SOURCE      : constant Interfaces.Unsigned_32 := Shift_Left (1, 4);
   GPIOHANDLE_REQUEST_NONE        : constant Interfaces.Unsigned_32 := 0;
   GPIOHANDLE_REQUEST_INPUT       : constant Interfaces.Unsigned_32 := Shift_Left (1, 0);
   GPIOHANDLE_REQUEST_OUTPUT      : constant Interfaces.Unsigned_32 := Shift_Left (1, 1);
   GPIOHANDLE_REQUEST_ACTIVE_LOW  : constant Interfaces.Unsigned_32 := Shift_Left (1, 2);
   GPIOHANDLE_REQUEST_OPEN_DRAIN  : constant Interfaces.Unsigned_32 := Shift_Left (1, 3);
   GPIOHANDLE_REQUEST_OPEN_SOURCE : constant Interfaces.Unsigned_32 := Shift_Left (1, 4);
   GPIOHANDLES_MAX                : constant Interfaces.Unsigned_32 := 64;

   subtype Pin_Num            is Interfaces.Unsigned_32 range 0 .. 63;
   subtype Flags_Type         is Interfaces.Unsigned_32 range 0 .. 63;
   subtype Lines_Type         is Interfaces.Unsigned_32 range 0 .. 63;
   subtype Line_Offset_Type   is Interfaces.Unsigned_32 range 0 .. 63;
   subtype Handle_Flags_Type  is Interfaces.Unsigned_32 range 0 .. 63;
   subtype Event_Flags_Type   is Interfaces.Unsigned_32 range 0 .. 63;
   subtype Mask_Type          is Interfaces.Unsigned_32 range 0 .. 63;
   subtype ID_Type            is Interfaces.Unsigned_32 range 0 .. 63;
   subtype Timestamp_Type     is Interfaces.Unsigned_64;
   subtype Name_Type          is Interfaces.C.char_array (0 .. 31);
   subtype Label_Type         is Interfaces.C.char_array (0 .. 31);
   subtype Consumer_Type      is Interfaces.C.char_array (0 .. 31);
   subtype FD_Type            is Interfaces.C.int;
   type    Line_Offsets_Array is array (0 .. 63) of Interfaces.Unsigned_32;
   type    Values_Array       is array (0 .. 63) of Interfaces.Integer_8;

   type GPIO_Chip_Info is record
      Name  : Name_Type;
      Label : Label_Type;
      Lines : Lines_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, GPIO_Chip_Info);

   type GPIO_Line_Info is record
      Line_Offset : Line_Offset_Type;
      Flags       : Flags_Type;
      Name        : Name_Type;
      Consumer    : Consumer_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, GPIO_Line_Info);

   type GPIO_Handle_Data is record
      Values : Values_Array;
   end record;
   pragma Convention (C, GPIO_Handle_Data);

   type GPIO_Handle_Request is record
      Line_Offsets   : Line_Offsets_Array;
      Flags          : Flags_Type;
      Default_Values : Values_Array;
      Consumer_Label : Consumer_Type;
      Lines          : Lines_Type;
      FD             : FD_Type;
   end record;
   pragma Convention (C, GPIO_Handle_Request);

   type GPIO_Event_Request is record
      Line_Offset    :  Pin_Num;
      Handle_Flags   :  Handle_Flags_Type;
      Event_Flags    :  Event_Flags_Type;
      Consumer_Label :  Consumer_Type;
      FD             :  FD_Type;
   end record;
   pragma Convention (C, GPIO_Event_Request);

   type GPIO_Event_Data is record
      Timestamp : Timestamp_Type;
      ID        : ID_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, GPIO_Event_Data);

   type GPIO_Flag_Record is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Mask : Mask_Type;
   end record;

   type GPIO_Flag_Type is array (Interfaces.Unsigned_32 range 0 .. 5) of GPIO_Flag_Record;

   GPIO_Flag : constant GPIO_Flag_Type := ((Ada.Strings.Unbounded.To_Unbounded_String ("none"),        GPIOLINE_FLAG_NONE),
                                           (Ada.Strings.Unbounded.To_Unbounded_String ("kernel"),      GPIOLINE_FLAG_KERNEL),
                                           (Ada.Strings.Unbounded.To_Unbounded_String ("is-out"),      GPIOLINE_FLAG_IS_OUT),
                                           (Ada.Strings.Unbounded.To_Unbounded_String ("active-low"),  GPIOLINE_FLAG_ACTIVE_LOW),
                                           (Ada.Strings.Unbounded.To_Unbounded_String ("open-drain"),  GPIOLINE_FLAG_OPEN_DRAIN),
                                           (Ada.Strings.Unbounded.To_Unbounded_String ("open-source"), GPIOLINE_FLAG_OPEN_SOURCE));

   type GPIO_Request_Record is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Mask : Mask_Type;
   end record;

   type GPIO_Request_Type is array (Interfaces.Unsigned_32 range 0 .. 5) of GPIO_Request_Record;

   GPIO_Request : constant GPIO_Request_Type := ((Ada.Strings.Unbounded.To_Unbounded_String ("none"),        GPIOHANDLE_REQUEST_NONE),
                                                 (Ada.Strings.Unbounded.To_Unbounded_String ("input"),       GPIOHANDLE_REQUEST_INPUT),
                                                 (Ada.Strings.Unbounded.To_Unbounded_String ("output"),      GPIOHANDLE_REQUEST_OUTPUT),
                                                 (Ada.Strings.Unbounded.To_Unbounded_String ("active-low"),  GPIOHANDLE_REQUEST_ACTIVE_LOW),
                                                 (Ada.Strings.Unbounded.To_Unbounded_String ("open-drain"),  GPIOHANDLE_REQUEST_OPEN_DRAIN),
                                                 (Ada.Strings.Unbounded.To_Unbounded_String ("open-source"), GPIOHANDLE_REQUEST_OPEN_DRAIN));

   function  GPIO_GET_CHIPINFO_IOCTL          (C : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function  GPIO_GET_LINEEVENT_IOCTL         (C : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function  GPIO_GET_LINEHANDLE_IOCTL        (C : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function  GPIO_GET_LINEINFO_IOCTL          (C : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function  GPIOHANDLE_GET_LINE_VALUES_IOCTL (C : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function  GPIOHANDLE_SET_LINE_VALUES_IOCTL (C : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function  Is_Bit_Set                       (C : Interfaces.Unsigned_32; B : Interfaces.Unsigned_32) return Boolean;
   procedure Monitor_CTRL_C_Called;
   function  Monitor_CTRL_C_Is_Called return Boolean;
   procedure Monitor_Device_Close             (FD             : FD_Type);
   procedure Monitor_Device_Event_Open        (LDev_Name      : String;
                                               Event_Request  : aliased in out GPIO_Event_Request;
                                               IOCTL_FD       : out FD_Type);
   procedure Monitor_Device_Event_Open        (LDev_Name      : String;
                                               Pin            : Pin_Num;
                                               Flags          : Linux_GPIO.Handle_Flags_Type;
                                               Consumer_Label : Ada.Strings.Unbounded.Unbounded_String;
                                               IOCTL_FD       : out FD_Type);
   procedure Monitor_Device_Request_Open      (LDev_Name      : String;
                                               Lines          : Line_Offsets_Array;
                                               NLines         : Interfaces.Unsigned_32;
                                               Flags          : Flags_Type;
                                               Handle_Data    : GPIO_Handle_Data;
                                               Consumer_Label : Ada.Strings.Unbounded.Unbounded_String;
                                               IOCTL_FD       : out FD_Type);
   procedure Monitor_Get_Pins                 (FD             : FD_Type;
                                               Data           : aliased in out GPIO_Handle_Data);
   procedure Monitor_Wait_For_Signal          (FD             : FD_Type;
                                               Event_Data     : aliased out GPIO_Event_Data);
   procedure Monitor_Set_Pins                 (FD             : FD_Type;
                                               Data           : GPIO_Handle_Data);
end Linux_GPIO;
