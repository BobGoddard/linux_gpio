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

   subtype pin_nums          is Interfaces.Unsigned_32 range 0 .. 63;
   subtype flags_type        is Interfaces.Unsigned_32 range 0 .. 63;
   subtype lines_type        is Interfaces.Unsigned_32 range 0 .. 63;
   subtype line_offset_type  is Interfaces.Unsigned_32 range 0 .. 63;
   subtype handle_flags_type is Interfaces.Unsigned_32 range 0 .. 63;
   subtype event_flags_type  is Interfaces.Unsigned_32 range 0 .. 63;
   subtype mask_type         is Interfaces.Unsigned_32 range 0 .. 63;
   subtype id_type           is Interfaces.Unsigned_32 range 0 .. 63;
   subtype timestamp_type    is Interfaces.Unsigned_64;
   subtype name_type         is Interfaces.C.char_array (0 .. 31);
   subtype label_type        is Interfaces.C.char_array (0 .. 31);
   subtype consumer_type     is Interfaces.C.char_array (0 .. 31);
   subtype fd_type           is Interfaces.C.int;
   type lineoffsets_array    is array (0 .. 63) of Interfaces.Unsigned_32;
   type values_array         is array (0 .. 63) of Interfaces.Integer_8;

   type gpiochip_info is record
      name  : name_type;
      label : label_type;
      lines : lines_type;
   end record;
   pragma Convention (C_Pass_By_Copy, gpiochip_info);

   type gpioline_info is record
      line_offset : line_offset_type;
      flags       : flags_type;
      name        : name_type;
      consumer    : consumer_type;
   end record;
   pragma Convention (C_Pass_By_Copy, gpioline_info);

   type gpiohandle_data is record
      values : values_array;
   end record;
   pragma Convention (C, gpiohandle_data);

   type gpiohandle_request is record
      lineoffsets    : lineoffsets_array;
      flags          : flags_type;
      default_values : values_array;
      consumer_label : consumer_type;
      lines          : lines_type;
      fd             : fd_type;
   end record;
   pragma Convention (C, gpiohandle_request);

   type gpioevent_request is record
      lineoffset     :  pin_nums;
      handleflags    :  handle_flags_type;
      eventflags     :  event_flags_type;
      consumer_label :  consumer_type;
      fd             :  fd_type;
   end record;
   pragma Convention (C, gpioevent_request);

   type gpioevent_data is record
      timestamp : timestamp_type;
      id        : id_type;
   end record;
   pragma Convention (C_Pass_By_Copy, gpioevent_data);

   type gpio_flag_record is record
      name : Ada.Strings.Unbounded.Unbounded_String;
      mask : mask_type;
   end record;

   type gpio_flag_type is array (Interfaces.Unsigned_32 range 0 .. 5) of gpio_flag_record;

   gpio_flag : constant gpio_flag_type := ((Ada.Strings.Unbounded.To_Unbounded_String ("none"),        GPIOLINE_FLAG_NONE),
                                           (Ada.Strings.Unbounded.To_Unbounded_String ("kernel"),      GPIOLINE_FLAG_KERNEL),
                                           (Ada.Strings.Unbounded.To_Unbounded_String ("is-out"),      GPIOLINE_FLAG_IS_OUT),
                                           (Ada.Strings.Unbounded.To_Unbounded_String ("active-low"),  GPIOLINE_FLAG_ACTIVE_LOW),
                                           (Ada.Strings.Unbounded.To_Unbounded_String ("open-drain"),  GPIOLINE_FLAG_OPEN_DRAIN),
                                           (Ada.Strings.Unbounded.To_Unbounded_String ("open-source"), GPIOLINE_FLAG_OPEN_SOURCE));

   type gpio_request_record is record
      name : Ada.Strings.Unbounded.Unbounded_String;
      mask : mask_type;
   end record;

   type gpio_request_type is array (Interfaces.Unsigned_32 range 0 .. 5) of gpio_request_record;

   GPIO_Request : constant gpio_request_type := ((Ada.Strings.Unbounded.To_Unbounded_String ("none"),        GPIOHANDLE_REQUEST_NONE),
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
   procedure Monitor_Device_Close             (FD       : fd_type);
   procedure Monitor_Device_Event_Open        (LDevName       : String;
                                               Event_Request  : aliased in out Linux_GPIO.gpioevent_request;
                                               FD             : out fd_type);
   procedure Monitor_Device_Request_Open      (LDevName       : String;
                                               Lines          : lineoffsets_array;
                                               NLines         : Interfaces.Unsigned_32;
                                               Flags          : flags_type;
                                               Handle_Data    : Linux_GPIO.gpiohandle_data;
                                               Consumer_Label : Ada.Strings.Unbounded.Unbounded_String;
                                               FD             : out fd_type;
                                               IOCTL_FD       : out fd_type);
   procedure Monitor_Get_Pins                 (fd             : Linux_GPIO.fd_type;
                                               data           : aliased in out Linux_GPIO.gpiohandle_data);
   procedure Monitor_Wait_For_Signal          (FD             : fd_type;
                                               Event_Data     : aliased out Linux_GPIO.gpioevent_data);
   procedure Monitor_Set_Pins                 (fd             : fd_type;
                                               data           : gpiohandle_data);
end Linux_GPIO;
