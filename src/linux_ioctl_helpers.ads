with Interfaces; use Interfaces;

package Linux_IOCTL_Helpers is

   C_IOC_NRBITS    : constant :=  8;
   C_IOC_TYPEBITS  : constant :=  8;
   C_IOC_SIZEBITS  : constant := 14;
   C_IOC_DIRBITS   : constant :=  2;
   C_IOC_NONE      : constant Interfaces.Unsigned_32 :=  0;
   C_IOC_WRITE     : constant Interfaces.Unsigned_32 :=  1;
   C_IOC_READ      : constant Interfaces.Unsigned_32 :=  2;

   C_IOC_NRMASK    : constant := 2 ** C_IOC_NRBITS - 1;
   C_IOC_TYPEMASK  : constant := 2 ** C_IOC_TYPEBITS - 1;
   C_IOC_SIZEMASK  : constant := 2 ** C_IOC_SIZEBITS - 1;
   C_IOC_DIRMASK   : constant := 2 ** C_IOC_DIRBITS - 1;
   C_IOC_NRSHIFT   : constant := 0;
   C_IOC_TYPESHIFT : constant := C_IOC_NRSHIFT + C_IOC_NRBITS;
   C_IOC_SIZESHIFT : constant := C_IOC_TYPESHIFT + C_IOC_TYPEBITS;
   C_IOC_DIRSHIFT  : constant := C_IOC_SIZESHIFT + C_IOC_SIZEBITS;
   IOC_IN                         : constant Interfaces.Unsigned_32 := Interfaces.Shift_Right (C_IOC_WRITE, C_IOC_DIRSHIFT);
   IOC_OUT                        : constant Interfaces.Unsigned_32 := Interfaces.Shift_Right (C_IOC_READ,  C_IOC_DIRSHIFT);
   IOC_INOUT                      : constant Interfaces.Unsigned_32 := Interfaces.Shift_Right ((C_IOC_WRITE + C_IOC_READ), C_IOC_DIRSHIFT);
   IOCSIZE_MASK                   : constant Interfaces.Unsigned_32 := Interfaces.Shift_Right (C_IOC_SIZEMASK, C_IOC_SIZESHIFT);
   IOCSIZE_SHIFT                  : constant Interfaces.Unsigned_32 := C_IOC_SIZESHIFT;

   function C_IO            (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function C_IOC           (dir   : Interfaces.Unsigned_32; stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function C_IOC_DIR       (nr    : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function C_IOC_NR        (nr    : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function C_IOC_SIZE      (nr    : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function C_IOC_TYPE      (nr    : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function C_IOC_TYPECHECK (t     : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function C_IOR           (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function C_IOR_BAD       (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function C_IOW           (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function C_IOW_BAD       (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function C_IOWR          (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   function C_IOWR_BAD      (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
end Linux_IOCTL_Helpers;
