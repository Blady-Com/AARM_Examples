with System.Storage_Pools.Subpools;
with System.Storage_Elements;
with Ada.Unchecked_Deallocate_Subpool;
with System.Machine_Code;
with Ada.Streams;
with Ada.Unchecked_Conversion;

procedure AARM_202x_CH13 is

   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      type NTCT is range 0 .. 20;
      type Car is new NTCT;
      type Interruption_Code is new NTCT;
      type Mask is new NTCT;
      type Some_Storage_Pool_Type is new System.Storage_Pools.Root_Storage_Pool with null record;
      procedure Allocate
        (Pool                     : in out Some_Storage_Pool_Type; Storage_Address : out System.Address;
         Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
         Alignment                : in     System.Storage_Elements.Storage_Count) is null;
      procedure Deallocate
        (Pool                     : in out Some_Storage_Pool_Type; Storage_Address : in System.Address;
         Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
         Alignment                : in     System.Storage_Elements.Storage_Count) is null;
      function Storage_Size (Pool : Some_Storage_Pool_Type) return System.Storage_Elements.Storage_Count is (0);
      type Address is new Natural range 0 .. 65535;
      type Designated is new NTCT;
      function Func_Call return Natural is (99);
   end Needed_To_Compile;
   use Needed_To_Compile;

   --                         13   Representation Issues

   --  13.1 Operational and Representation Aspects

   procedure Section_13_1_Paragraph_7c is

      subtype S is Integer range 1 .. 256;
      type A is array (Natural range 1 .. 4) of S with
         Pack;
      X : S := 3;
      Y : A := (1, 2, 3, 4);

   begin
      null;
   end Section_13_1_Paragraph_7c;

   package Section_13_1_Paragraph_13c is

      package Pkg1 is
         type Ifc is interface;
         type T is tagged record
            Fld : Integer;
         end record;
--        for T use record
--           Fld at 0 range 0 .. Integer'Size - 1; --@@ MODIF07 PP error: component overlaps tag field of "T"
--        end record;
      end Pkg1;
   --@          with Pkg1;
      package Pkg2 is
         type NewT is new Pkg1.T and Pkg1.Ifc with null record;
      end Pkg2;

   end Section_13_1_Paragraph_13c;

   procedure Section_13_1_Paragraph_14c is

      package P1 is
         subtype S1 is Integer range 0 .. 2**16 - 1;
           --@  with Size => 16; -- Illegal!
         -- S1'Size would be 16 by default.
         type A1 is access all S1;
         X1 : A1;
      end P1;

      package P2 is
         subtype S2 is Integer range 0 .. 2**16 - 1;
           --@  with Size => 32; -- Illegal!
         type A2 is access all S2;
         X2 : A2;
      end P2;

      procedure Q is
         use P1, P2;
         type Array1 is array (Integer range <>) of aliased S1;
         -- with Pack; --@@ MODIF08 PP error: cannot pack aliased components
         Obj1 : Array1 (1 .. 100);
         type Array2 is array (Integer range <>) of aliased S2;
         -- with Pack; --@@ MODIF08 PP error: cannot pack aliased components
         Obj2 : Array2 (1 .. 100);
      begin
         X1 := Obj2 (17)'Unchecked_Access;
         X2 := Obj1 (17)'Unchecked_Access;
      end Q;

   begin
      null;
   end Section_13_1_Paragraph_14c;

   --  13.1.1 Aspect Specifications

   --  13.2 Packed Types

   --  13.3 Operational and Representation Attributes

   package Section_13_3_Paragraph_79 is
      type T is new NTCT;

--  Examples of attribute definition clauses:

      Byte : constant := 8;
      Page : constant := 2**12;

      type Medium is range 0 .. 65_000;
      for Medium'Size use 2 * Byte;
      for Medium'Alignment use 2;
      Device_Register : Medium;
      for Device_Register'Size use Medium'Size;
      for Device_Register'Address use System.Storage_Elements.To_Address (16#FFFF_0020#);

      type Short is delta 0.01 range -100.0 .. 100.0;
      for Short'Size use 15;

      type Car_Name is access Car;
      for Car_Name'
        Storage_Size use -- specify access type's storage pool size
      2000 * ((Car'Size / System.Storage_Unit) + 1); -- approximately 2000 cars

      function My_Input (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return T;
      for T'Input use My_Input; -- see 13.13.2
   end Section_13_3_Paragraph_79;

   package body Section_13_3_Paragraph_79 is
      function My_Input (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return T is
         LT : T;
      begin
         return LT;
      end My_Input;
   end Section_13_3_Paragraph_79;

--  13.4 Enumeration Representation Clauses

   procedure Section_13_4_Paragraph_11b is

      type T1 is (Red, Green, Blue);
      subtype S1 is T1 range Red .. Green;
      type S2 is new S1;
      for S2 use (Red => 10, Green => 20, Blue => 30);
   begin
      for I in S2'Base loop
         null; --@ ... -- When I equals Blue, the internal code is 30.
      end loop;
   end Section_13_4_Paragraph_11b;

--  Example of an enumeration representation clause:

   type Mix_Code is (ADD, SUB, MUL, LDA, STA, STZ);
   for Mix_Code use (ADD => 1, SUB => 2, MUL => 3, LDA => 8, STA => 24, STZ => 33);

--  13.5 Record Layout

   --  13.5.1 Record Representation Clauses

   package Section_13_5_1_Paragraph_24 is

--  Example of specifying the layout of a record type:

      Word : constant := 4;  --  storage element is byte, 4 bytes per word

      type State is (A, M, W, P);
      type Mode is (Fix, Dec, Exp, Signif);

      type Byte_Mask is array (0 .. 7) of Boolean with
         Component_Size => 1;
      type State_Mask is array (State) of Boolean with
         Component_Size => 1;
      type Mode_Mask is array (Mode) of Boolean with
         Component_Size => 1;

      type Program_Status_Word is record
         System_Mask     : Byte_Mask;
         Protection_Key  : Integer range 0 .. 3;
         Machine_State   : State_Mask;
         Interrupt_Cause : Interruption_Code;
         Ilc             : Integer range 0 .. 3;
         Cc              : Integer range 0 .. 3;
         Program_Mask    : Mode_Mask;
         Inst_Address    : Address;
      end record;

      for Program_Status_Word use record
         System_Mask     at 0 * Word range  0 ..  7;
         Protection_Key  at 0 * Word range 10 .. 11; -- bits 8,9 unused
         Machine_State   at 0 * Word range 12 .. 15;
         Interrupt_Cause at 0 * Word range 16 .. 31;
         Ilc             at 1 * Word range  0 ..  1;  -- second word
         Cc              at 1 * Word range  2 ..  3;
         Program_Mask    at 1 * Word range  4 ..  7;
         Inst_Address    at 1 * Word range  8 .. 31;
      end record;

      for Program_Status_Word'Size use 8 * System.Storage_Unit;
      for Program_Status_Word'Alignment use 8;

   end Section_13_5_1_Paragraph_24;

--  13.5.2 Storage Place Attributes

--  13.5.3 Bit Ordering

--  13.6 Change of Representation

   procedure Section_13_6_Paragraph_3 is

--   Example of change of representation:

      -- Packed_Descriptor and Descriptor are two different types
      -- with identical characteristics, apart from their
      -- representation

      type Descriptor is record
         -- components of a descriptor
         NTCF : NTCT;
      end record;

      type Packed_Descriptor is new Descriptor;

      for Packed_Descriptor use record
      -- component clauses for some or for all components
      end record;

      -- Change of representation can now be accomplished by explicit type conversions:

      D : Descriptor;
      P : Packed_Descriptor;
   begin
      P := Packed_Descriptor (D);  -- pack D
      D := Descriptor (P);         -- unpack P
   end Section_13_6_Paragraph_3;

--  13.7 The Package System

--  13.7.1 The Package System.Storage_Elements

--  13.7.2 The Package System.Address_To_Access_Conversions

--  13.8 Machine Code Insertions

--  Example of a code statement:

   M : Mask;
   procedure Set_Mask with
      Inline;

   procedure Set_Mask is
      use System.Machine_Code; -- assume "with System.Machine_Code;" appears somewhere above
   begin
--            SI_Format'(Code => SSM, B => M'Base_Reg, D => M'Disp);  --@@ MODIF09 PP not defined by GNAT
--  Base_Reg and Disp are implementation-defined attributes
      null;
   end Set_Mask;

--  13.9 Unchecked Type Conversions

   --  13.9.1 Data Validity

   package Section_13_9_1_Paragraph_12d is
      use Ada;

      type My_Int is range 0 .. 99;
      function Safe_Convert is new Unchecked_Conversion (My_Int, Integer);
      function Unsafe_Convert is new Unchecked_Conversion (My_Int, Positive);
      X : Positive := Safe_Convert (0); -- Raises Constraint_Error.
      Y : Positive := Unsafe_Convert (0); -- Bounded Error, may be invalid.
      B : Boolean  := Y'Valid; -- OK, B = False.
      Z : Positive := Y + 1; -- Erroneous to use Y.

   end Section_13_9_1_Paragraph_12d;

--  13.9.2 The Valid Attribute

--  13.10 Unchecked Access Value Creation

--  13.11 Storage Management

   package Section_13_11_Paragraph_32 is

--  To associate an access type with a storage pool object, the user first
--  declares a pool object of some type derived from Root_Storage_Pool. Then, the
--  user defines its Storage_Pool attribute, as follows:

      Pool_Object : Some_Storage_Pool_Type;

      type T is access Designated;
      for T'Storage_Pool use Pool_Object;

--  Another access type may be added to an existing storage pool, via:

      type T2 is access NTCT;
      for T2'Storage_Pool use T'Storage_Pool;

   end Section_13_11_Paragraph_32;

--  13.11.1 Storage Allocation Attributes

--  13.11.2 Unchecked Storage Deallocation

--  13.11.3 Default Storage Pools

--  13.11.4 Storage Subpools

--  13.11.5 Subpool Reclamation

--  13.11.6 Storage Subpool Example

--  The following example is a simple but complete
--  implementation of the classic Mark/Release pool using subpools:

--@     with System.Storage_Pools.Subpools;
--@        with System.Storage_Elements;
--@        with Ada.Unchecked_Deallocate_Subpool;
   package MR_Pool is

      use System.Storage_Pools;
      -- For uses of Subpools.
      use System.Storage_Elements;
      -- For uses of Storage_Count and Storage_Array.

      --  Mark and Release work in a stack fashion, and allocations are not allowed
      --  from a subpool other than the one at the top of the stack. This is also
      --  the default pool.

      subtype Subpool_Handle is Subpools.Subpool_Handle;

      type Mark_Release_Pool_Type (Pool_Size : Storage_Count) is
        new Subpools.Root_Storage_Pool_With_Subpools with private;

      function Mark (Pool : in out Mark_Release_Pool_Type) return not null Subpool_Handle;

      procedure Release (Subpool : in out Subpool_Handle) renames Ada.Unchecked_Deallocate_Subpool;

   private

      type MR_Subpool is new Subpools.Root_Subpool with record
         Start : Storage_Count;
      end record;
      subtype Subpool_Indexes is Positive range 1 .. 10;
      type Subpool_Array is array (Subpool_Indexes) of aliased MR_Subpool;

      type Mark_Release_Pool_Type (Pool_Size : Storage_Count) is new Subpools.Root_Storage_Pool_With_Subpools with
      record
         Storage         : Storage_Array (0 .. Pool_Size);
         Next_Allocation : Storage_Count   := 0;
         Markers         : Subpool_Array;
         Current_Pool    : Subpool_Indexes := 1;
      end record;

      overriding function Create_Subpool (Pool : in out Mark_Release_Pool_Type) return not null Subpool_Handle;

      function Mark (Pool : in out Mark_Release_Pool_Type) return not null Subpool_Handle renames Create_Subpool;

      overriding procedure Allocate_From_Subpool
        (Pool                     : in out Mark_Release_Pool_Type; Storage_Address : out System.Address;
         Size_In_Storage_Elements : in Storage_Count; Alignment : in Storage_Count; Subpool : not null Subpool_Handle);

      overriding procedure Deallocate_Subpool (Pool : in out Mark_Release_Pool_Type; Subpool : in out Subpool_Handle);

      overriding function Default_Subpool_for_Pool
        (Pool : in out Mark_Release_Pool_Type) return not null Subpool_Handle;

      overriding procedure Initialize (Pool : in out Mark_Release_Pool_Type);

      -- We don't need Finalize.

   end MR_Pool;

   package body MR_Pool is

      use type Subpool_Handle;

      procedure Initialize (Pool : in out Mark_Release_Pool_Type) is
      -- Initialize the first default subpool.
      begin
         Pool.Markers (1).Start := 1;
         Subpools.Set_Pool_Of_Subpool (Pool.Markers (1)'Unchecked_Access, Pool);
      end Initialize;

      function Create_Subpool (Pool : in out Mark_Release_Pool_Type) return not null Subpool_Handle is
      -- Mark the current allocation location.
      begin
         if Pool.Current_Pool = Subpool_Indexes'Last then
            raise Storage_Error; -- No more subpools.
         end if;
         Pool.Current_Pool := Pool.Current_Pool + 1; -- Move to the next subpool

         return Result : constant not null Subpool_Handle := Pool.Markers (Pool.Current_Pool)'Unchecked_Access do
            Pool.Markers (Pool.Current_Pool).Start := Pool.Next_Allocation;
            Subpools.Set_Pool_Of_Subpool (Result, Pool);
         end return;
      end Create_Subpool;

      procedure Deallocate_Subpool (Pool : in out Mark_Release_Pool_Type; Subpool : in out Subpool_Handle) is
      begin
         if Subpool /= Pool.Markers (Pool.Current_Pool)'Unchecked_Access then
            raise Program_Error; -- Only the last marked subpool can be released.
         end if;
         if Pool.Current_Pool /= 1 then
            Pool.Next_Allocation := Pool.Markers (Pool.Current_Pool).Start;
            Pool.Current_Pool    := Pool.Current_Pool - 1; -- Move to the previous subpool
         else -- Reinitialize the default subpool:
            Pool.Next_Allocation := 1;
            Subpools.Set_Pool_Of_Subpool (Pool.Markers (1)'Unchecked_Access, Pool);
         end if;
      end Deallocate_Subpool;

      function Default_Subpool_for_Pool (Pool : in out Mark_Release_Pool_Type) return not null Subpool_Handle is
      begin
         return Pool.Markers (Pool.Current_Pool)'Unchecked_Access;
      end Default_Subpool_for_Pool;

      procedure Allocate_From_Subpool
        (Pool                     : in out Mark_Release_Pool_Type; Storage_Address : out System.Address;
         Size_In_Storage_Elements : in Storage_Count; Alignment : in Storage_Count; Subpool : not null Subpool_Handle)
      is
      begin
         if Subpool /= Pool.Markers (Pool.Current_Pool)'Unchecked_Access then
            raise Program_Error; -- Only the last marked subpool can be used for allocations.
         end if;

         -- Check for the maximum supported alignment, which is the alignment of the storage area:
         if Alignment > Pool.Storage'Alignment then
            raise Program_Error;
         end if;
         -- Correct the alignment if necessary:
         Pool.Next_Allocation := Pool.Next_Allocation + ((-Pool.Next_Allocation) mod Alignment);
         if Pool.Next_Allocation + Size_In_Storage_Elements > Pool.Pool_Size then
            raise Storage_Error; -- Out of space.
         end if;
         Storage_Address      := Pool.Storage (Pool.Next_Allocation)'Address;
         Pool.Next_Allocation := Pool.Next_Allocation + Size_In_Storage_Elements;
      end Allocate_From_Subpool;

   end MR_Pool;

--  13.12 Pragma Restrictions and Pragma Profile

--  13.12.1 Language-Defined Restrictions and Profiles

--  13.13 Streams

--  13.13.1 The Streams Subsystem

--  13.13.2 Stream-Oriented Attributes

   package Section_13_13_2_Paragraph_59 is
      use Ada.Streams;
      type My_Integer is new NTCT;

--  Example of user-defined Write attribute:

      procedure My_Write (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : My_Integer'Base);
      for My_Integer'Write use My_Write;

--        Discussion: Example of network input/output using input output attributes:

--@            with Ada.Streams; use Ada.Streams;
      generic
         type Msg_Type (<>) is private;
      package Network_IO is
         -- Connect/Disconnect are used to establish the stream
         procedure Connect (NTCP : NTCT); -- @ ...);
         procedure Disconnect (NTCP : NTCT); -- @ ...);

         -- Send/Receive transfer messages across the network
         procedure Send (X : in Msg_Type);
         function Receive return Msg_Type;
      private
         type Network_Stream is new Root_Stream_Type with null record; --@ ...
         procedure Read
           ( -- @ ...);  -- define Read/Write for Network_Stream
         Stream : in out Network_Stream; Item : out Stream_Element_Array; Last : out Stream_Element_Offset) is null;
         procedure Write
           ( -- @ ...);
         Stream : in out Network_Stream; Item : in Stream_Element_Array) is null;
      end Network_IO;

   end Section_13_13_2_Paragraph_59;

   package body Section_13_13_2_Paragraph_59 is

      procedure My_Write (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : My_Integer'Base) is null;

--@            with Ada.Streams; use Ada.Streams;
      package body Network_IO is
         Current_Stream : aliased Network_Stream;
                    --@ . . .
         procedure Connect (NTCP : NTCT) is null; -- @ ...) is ...;
         procedure Disconnect (NTCP : NTCT) is null; -- @ ...) is ...;

         procedure Send (X : in Msg_Type) is
         begin
            Msg_Type'Output (Current_Stream'Access, X);
         end Send;

         function Receive return Msg_Type is
         begin
            return Msg_Type'Input (Current_Stream'Access);
         end Receive;
      end Network_IO;

   end Section_13_13_2_Paragraph_59;

--  13.14 Freezing Rules

   package Section_13_14_Paragraph_1q is

      type T is record
         NTCF : NTCT; --@ ...
      end record;
      function F return T;
      function G (X : T) return Boolean;
      Y : Boolean := G (F); -- doesn't force T in Ada 83
--     for T use record --@@ MODIF10 PP error: representation item appears too late
--        NTCF at 0 range 0 .. 31; --@ ...
--     end record;

   end Section_13_14_Paragraph_1q;

   package body Section_13_14_Paragraph_1q is
      -- Needed to compile, sometimes dummy
      function F return T is (NTCF => 0);
      function G (X : T) return Boolean is (False);
   end Section_13_14_Paragraph_1q;

   package Section_13_14_Paragraph_1s is

      package P is
         type T is private;
         function F return T;
         function G (X : T) return Boolean;
--        Y : Boolean := G (F); -- doesn't force T in Ada 83 --@@ MODIF11 PP error: type "T" must be fully defined before this point
      private
         type T is record
            NTCF : NTCT; --@ ...
         end record;
      end P;

   end Section_13_14_Paragraph_1s;

   package body Section_13_14_Paragraph_1s is
      -- Needed to compile, sometimes dummy
      package body P is
         function F return T is (NTCF => 0);
         function G (X : T) return Boolean is (False);
      end P;
   end Section_13_14_Paragraph_1s;

   package Section_13_14_Paragraph_10c is

      package P1 is
         type T is private;
         package P2 is
            type Composite (D : Boolean) is record
               case D is
                  when False =>
                     Cf : Integer;
                  when True =>
                     Ct : T;
               end case;
            end record;
         end P2;
--        X : Boolean := P2."=" ((False, 1), (False, 1)); --@@ MODIF12 PP error: "=" not declared in "P2"
      private
         type T is array (1 .. Func_Call) of Integer;
      end P1;

   end Section_13_14_Paragraph_10c;

   package Section_13_14_Paragraph_10j is

      package Pack is

         type Flub is range 0 .. 100;

         function Foo (A : in Natural) return Natural is (A + Flub'Size); -- The expression is not frozen here.

         type Bar is access function (A : in Natural) return Natural;

         P : Bar := Foo'Access; -- (A)

         Val : Natural := P.all (5); -- (B)

      end Pack;

   end Section_13_14_Paragraph_10j;

   package Section_13_14_Paragraph_13c is

      type Pool_Ptr is access System.Storage_Pools.Root_Storage_Pool'Class;
      function F return Pool_Ptr;

      package P is
         type A1 is access Boolean;
         type A2 is new A1;
         type A3 is new A2;
         X : A3 := new Boolean; -- Don't know what pool yet!
--        for A1'Storage_Pool use F.all; --@@ MODIF13 PP error: representation item appears too late
      end P;

   end Section_13_14_Paragraph_13c;

   package body Section_13_14_Paragraph_13c is
      function F return Pool_Ptr is (null);
   end Section_13_14_Paragraph_13c;

   package Section_13_14_Paragraph_19d is

      type T is new NTCT; --@ ...;
      function F return T;
      type R is record
         C : T       := F;
         D : Boolean := F = F;
      end record;
      X : R;

   end Section_13_14_Paragraph_19d;

   package body Section_13_14_Paragraph_19d is
      function F return T is (0);
   end Section_13_14_Paragraph_19d;

   package Section_13_14_Paragraph_20b is

      type A is array (Integer range 1 .. 10) of Boolean;
      subtype S is Integer range A'Range;
      -- not forcing for A

   end Section_13_14_Paragraph_20b;

   package Section_13_14_Paragraph_20f is

      package Outer is
         type T is tagged limited private;
         generic
            type T2 is new T with private; -- Does not freeze T
            -- in Ada 95.
         package Inner is
                        --@ ...
         end Inner;
      private
         type T is tagged limited null record; --@ ...;
      end Outer;
   end Section_13_14_Paragraph_20f;

begin
   null;
end AARM_202x_CH13;
