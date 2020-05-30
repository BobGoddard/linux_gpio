with Logical_Opps;

package body Linux_IOCTL_Helpers is

   function C_IO (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return C_IOC (C_IOC_NONE, stype, nr, 0);
   end C_IO;

   function C_IOC (dir : Interfaces.Unsigned_32; stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Logical_Opps.L_Or (Logical_Opps.L_Or (Shift_Left (dir, C_IOC_DIRSHIFT), Shift_Left (stype, C_IOC_TYPESHIFT)), Logical_Opps.L_Or (Shift_Left (nr, C_IOC_NRSHIFT), Shift_Left (size, C_IOC_SIZESHIFT)));
   end C_IOC;

   function C_IOC_DIR (nr : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Shift_Right (nr, C_IOC_DIRSHIFT) and C_IOC_DIRMASK;
   end C_IOC_DIR;

   function C_IOC_NR (nr : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Shift_Right (nr, C_IOC_NRSHIFT) and C_IOC_NRMASK;
   end C_IOC_NR;

   function C_IOC_SIZE (nr : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Shift_Right (nr, C_IOC_SIZESHIFT) and C_IOC_SIZEMASK;
   end C_IOC_SIZE;

   function C_IOC_TYPE (nr : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return Shift_Right (nr, C_IOC_TYPESHIFT) and C_IOC_TYPEMASK;
   end C_IOC_TYPE;

   function C_IOC_TYPECHECK (t : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return t'Size;
   end C_IOC_TYPECHECK;

   function C_IOR (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return C_IOC (C_IOC_READ, stype, nr, size); -- C_IOC_TYPECHECK (size));
   end C_IOR;

   function C_IOR_BAD (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return C_IOC (C_IOC_READ, stype, nr, size'Size);
   end C_IOR_BAD;

   function C_IOW (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return C_IOC (C_IOC_WRITE, stype, nr, C_IOC_TYPECHECK (size));
   end C_IOW;

   function C_IOW_BAD (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return C_IOC (C_IOC_WRITE, stype, nr, size'Size);
   end C_IOW_BAD;

   function C_IOWR (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return C_IOC (Logical_Opps.L_Or (C_IOC_READ, C_IOC_WRITE), stype, nr, size);
   end C_IOWR;

   function C_IOWR_BAD (stype : Interfaces.Unsigned_32; nr : Interfaces.Unsigned_32; size : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return C_IOC (C_IOC_READ or C_IOC_WRITE, stype, nr, size'Size);
   end C_IOWR_BAD;

end Linux_IOCTL_Helpers;
