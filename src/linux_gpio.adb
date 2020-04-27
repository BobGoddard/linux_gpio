with Linux_ioctl_helpers;

package body Linux_GPIO is

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

end Linux_GPIO;
