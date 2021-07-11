with Ada.Finalization;

procedure AARM_202x_CH07 is

   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      type NTCT is range 0 .. 20;
      type Level is (Low, Medium, Urgent);
      type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
      subtype Weekday is Day range Mon .. Fri;
   end Needed_To_Compile;
   use Needed_To_Compile;

   --                                7   Packages

   --  7.1 Package Specifications and Declarations

   --  Example of a package declaration:
   package Rational_Numbers is

      type Rational is record
         Numerator   : Integer;
         Denominator : Positive;
      end record;

      function "=" (X, Y : Rational) return Boolean;

      function "/" (X, Y : Integer) return Rational;  --  to construct a rational number

      function "+" (X, Y : Rational) return Rational;
      function "-" (X, Y : Rational) return Rational;
      function "*" (X, Y : Rational) return Rational;
      function "/" (X, Y : Rational) return Rational;
   end Rational_Numbers;

   --  7.2 Package Bodies

   --   Example of a package body (see 7.1):

   package body Rational_Numbers is

      procedure Same_Denominator (X, Y : in out Rational) is
      begin
         --  reduces X and Y to the same denominator:
         null; --@   ...
      end Same_Denominator;

      function "=" (X, Y : Rational) return Boolean is
         U : Rational := X;
         V : Rational := Y;
      begin
         Same_Denominator (U, V);
         return U.Numerator = V.Numerator;
      end "=";

      function "/" (X, Y : Integer) return Rational is
      begin
         if Y > 0 then
            return (Numerator => X, Denominator => Y);
         else
            return (Numerator => -X, Denominator => -Y);
         end if;
      end "/";

      function "+" (X, Y : Rational) return Rational is
      begin
         return (0, 1); --@ ...
      end "+";
      function "-" (X, Y : Rational) return Rational is
      begin
         return (0, 1); --@ ...
      end "-";
      function "*" (X, Y : Rational) return Rational is
      begin
         return (0, 1); --@ ...
      end "*";
      function "/" (X, Y : Rational) return Rational is
      begin
         return (0, 1); --@ ...
      end "/";

   end Rational_Numbers;

   --  7.3 Private Types and Private Extensions

   procedure Section_7_3_Paragraph_7e is
      package P1 is
         type T1 is tagged limited private;
         procedure Foo (X : in T1'Class);
      private
         --                      type T1 is tagged null record; -- Illegal!
         -- This should say "tagged limited null record".
         type T1 is tagged limited null record;
      end P1;

      package body P1 is
         type A is access T1'Class;
         Global : A;
         procedure Foo (X : in T1'Class) is
         begin
            --                          Global := new T1'Class'(X);
            -- This would be illegal if the full view of
            -- T1 were limited, like it's supposed to be.
            null;
         end Foo;
      end P1;

      --@ with P1;
      package P2 is
         type T2 (D : access Integer) is new P1.T1 with record
            --                              My_Task : Some_Task_Type; -- Trouble!
            NTCF : NTCT;
         end record;
      end P2;

      --@ with P1;
      -- with P2;
      procedure Main is
         Local : aliased Integer;
         Y     : P2.T2 (D => Local'Access);
      begin
         P1.Foo (Y);
      end Main;
   begin
      null;
   end Section_7_3_Paragraph_7e;

   procedure Section_7_3_Paragraph_7l is
      package P is
         type Parent is private;
      private
         type Parent is tagged record
            X : Integer;
         end record;
      end P;

      --@   with P;
      package Q is
         type T is new P.Parent;
      end Q;

      --@    with Q; use Q;
      use Q;
      package body P is
         --@  ... T'Class ... -- Illegal!
         Object : T;
         --@   ... Object.X ... -- Illegal!
         --@ ...
         X : Integer := Parent (Object).X; --@ ... -- OK.
      end P;

   begin
      null;
   end Section_7_3_Paragraph_7l;

   procedure Section_7_3_Paragraph_7q is
      package P is
         package Pkg is
            type Ifc is interface;
            procedure Foo (X : Ifc) is abstract;
         end Pkg;

         type Parent_1 is tagged null record;

         --@                type T1 is new Parent_1 with private;
         type T1 is new Parent_1 and Pkg.Ifc with private;
      private
         type Parent_2 is new Parent_1 and Pkg.Ifc with null record;
         procedure Foo (X : Parent_2) is null; -- Foo #1

         --                type T1 is new Parent_2 with null record; -- Illegal.
         type T1 is new Parent_2 with null record;
      end P;

      --@      with P;
      package P_Client is
         type T2 is new P.T1 and P.Pkg.Ifc with null record;
         procedure Foo (X : T2) is null; -- Foo #2
         X : T2;
      end P_Client;

      --@     with P_Client;
      package body P is
         --@        ...

         procedure Bar (X : T1'Class) is
         begin
            -- Pkg.Foo (X); -- should call Foo #1 or an override thereof
            Pkg.Foo (Pkg.Ifc'Class (X));
         end Bar;

      begin
         Pkg.Foo (Pkg.Ifc'Class (P_Client.X));      -- should call Foo #2
         Bar (T1'Class (P_Client.X));
      end P;

   begin
      null;
   end Section_7_3_Paragraph_7q;

   package Section_7_3_Paragraph_21 is

      --  Examples of private type declarations:

      type Key is private;
      type File_Name is limited private;

      --  Example of a private extension declaration:

      type List is new Ada.Finalization.Controlled with private;
   private
      type Key is new NTCT;
      type File_Name is new NTCT;
      type List is new Ada.Finalization.Controlled with null record;
   end Section_7_3_Paragraph_21;

   --  7.3.1 Private Operations

   procedure Section_7_3_1_Paragraph_5b is
      package P is
         type T is private;
         C : constant T;
      private
         type T is new Integer;
         C : constant T := 42;
      end P;

      --@ with P;
      package Q is
         type T2 is new P.T;  -- T2 is not a descendant of Integer
      end Q;

      --@ with Q;
      --                  package P.Child is
      --                      type T3 is new Q.T2;
      --                  private
      --                      -- Here T3 is known to be indirectly derived from Integer, but inherits
      --                      -- no characteristics from Integer, since T2 inherits no characteristics
      --                      -- from Integer.
      --                      -- However, we allow an explicit conversion of T3 to/from Integer.
      --                      -- Hence, T3 is effectively a descendant of an "incomplete" view of Integer.
      --                      Int : Integer := 52;
      --                      V : T3 := T3(P.C);  -- Legal: conversion allowed
      --                      W : T3 := T3(Int);  -- Legal: conversion allowed
      --                      X : T3 := T3(42);   -- Error: T3 is not a numeric type
      --                      Y : T3 := X + 1;    -- Error: no visible "+" operator
      --                      Z : T3 := T3(Integer(W) + 1);   -- Legal: convert to Integer first
      --                  end P.Child;

   begin
      null;
   end Section_7_3_1_Paragraph_5b;

   procedure Section_7_3_1_Paragraph_7c is
      package Parent is
         type Root is tagged null record;
         procedure Op1 (X : Root) is null;

         type My_Int is range 1 .. 10;
      private
         procedure Op2 (X : Root) is null;

         type Another_Int is new My_Int;
         procedure Int_Op (X : My_Int) is null;
      end Parent;

      --@    with Parent; use Parent;
      use Parent;
      package Unrelated is
         type T2 is new Root with null record;
         procedure Op2 (X : T2) is null;
      end Unrelated;

      --               package Parent.Child is
      --                      type T3 is new Root with null record;
      --                      -- Op1(T3) implicitly declared here.
      --
      --                   package Nested is
      --                          type T4 is new Root with null record;
      --                      private
      --            --@              ...
      --                      end Nested;
      --                  private
      --                      -- Op2(T3) implicitly declared here.
      --                     -- ...
      --                  end Parent.Child;

      --@ with Unrelated; use Unrelated;
      --                  package body Parent.Child is
      --                      package body Nested is
      --                          -- Op2(T4) implicitly declared here.
      --                      end Nested;
      --
      --                   type T5 is new T2 with null record;
      --                  end Parent.Child;
   begin
      null;
   end Section_7_3_1_Paragraph_7c;

   procedure Section_7_3_1_Paragraph_7t is
      package P is
         type Comp1 is private;
      private
         type Comp1 is new Boolean;
      end P;

      --               package P.Q is
      --                      package R is
      --                          type Comp2 is limited private;
      --                          type A is array(Integer range <>) of Comp2;
      --                      private
      --                          type Comp2 is new Comp1;
      --                          -- A becomes nonlimited here.
      --                          -- "="(A, A) return Boolean is implicitly declared here.
      --                  --@        ...
      --                      end R;
      --                  private
      --                      -- Now we find out what Comp1 really is, which reveals
      --                      -- more information about Comp2, but we're not within
      --                      -- the immediate scope of Comp2, so we don't do anything
      --                      -- about it yet.
      --                  end P.Q;
      --
      --               package body P.Q is
      --                      package body R is
      --                          -- Things like "xor"(A,A) return A are implicitly
      --                          -- declared here.
      --                      end R;
      --                  end P.Q;

      -- We say immediately within the
      --  declarative region in order that types do not gain operations
      --   within a nested scope. Consider:

      package Outer is
         package Inner is
            type Inner_Type is private;
         private
            type Inner_Type is new Boolean;
         end Inner;
         type Outer_Type is array (Natural range <>) of Inner.Inner_Type;
      end Outer;

      package body Outer is
         package body Inner is
         -- At this point, we can see that Inner_Type is a Boolean type.
         -- But we don't want Outer_Type to gain an "and" operator here.
         end Inner;
      end Outer;

   begin
      null;
   end Section_7_3_1_Paragraph_7t;

   procedure Section_7_3_1_Paragraph_14 is

      --  Example of a type with private operations:
      package Key_Manager is
         type Key is private;
         Null_Key : constant Key; -- a deferred constant declaration (see 7.4)
         procedure Get_Key (K : out Key);
         function "<" (X, Y : Key) return Boolean;
      private
         type Key is new Natural;
         Null_Key : constant Key := Key'First;
      end Key_Manager;

      package body Key_Manager is
         Last_Key : Key := Null_Key;
         procedure Get_Key (K : out Key) is
         begin
            Last_Key := Last_Key + 1;
            K        := Last_Key;
         end Get_Key;

         function "<" (X, Y : Key) return Boolean is
         begin
            return Natural (X) < Natural (Y);
         end "<";
      end Key_Manager;

   begin
      null;
   end Section_7_3_1_Paragraph_14;

   --  7.3.2 Type Invariants

   -- Example of a work scheduler where only urgent work can be scheduled for weekends:

   package Work_Orders is

      -- See 3.5.1 for type declarations of Level, Day, and Weekday

      type Work_Order is private with
         Type_Invariant => Day_Scheduled (Work_Order) in Weekday or else Priority (Work_Order) = Urgent;

      function Schedule_Work (Urgency : in Level; To_Occur : in Day) return Work_Order with
         Pre => Urgency = Urgent or else To_Occur in Weekday;

      function Day_Scheduled (Order : in Work_Order) return Day;

      function Priority (Order : in Work_Order) return Level;

      procedure Change_Priority (Order : in out Work_Order; New_Priority : in Level; Changed : out Boolean) with
         Post => Changed = (Day_Scheduled (Order) in Weekday or else Priority (Order) = Urgent);

   private

      type Work_Order is record
         Scheduled : Day;
         Urgency   : Level;
      end record;

   end Work_Orders;

   package body Work_Orders is

      function Schedule_Work (Urgency : in Level; To_Occur : in Day) return Work_Order is
        (Scheduled => To_Occur, Urgency => Urgency);

      function Day_Scheduled (Order : in Work_Order) return Day is (Order.Scheduled);

      function Priority (Order : in Work_Order) return Level is (Order.Urgency);

      procedure Change_Priority (Order : in out Work_Order; New_Priority : in Level; Changed : out Boolean) is
      begin
         -- Ensure type invariant is not violated
         if Order.Urgency = Urgent or else (Order.Scheduled in Weekday) then
            Changed       := True;
            Order.Urgency := New_Priority;
         else
            Changed := False;
         end if;
      end Change_Priority;

   end Work_Orders;

   --  7.3.3 Default Initial Conditions

   --  7.3.4 Stable Properties of a Type

   --  7.4 Deferred Constants

   package Section_7_4_Paragraph_12 is
      type Key is new Natural;

      --  Examples of deferred constant declarations:

      Null_Key       : constant Key;      -- see 7.3.1
      CPU_Identifier : constant String (1 .. 8) with
         Import     => True,
         Convention => Assembler,
         Link_Name  => "CPU_ID"; -- see B.1

   private
      Null_Key : constant Key := 99;
   end Section_7_4_Paragraph_12;

   --  7.5 Limited Types

   package Section_7_5_Paragraph_2b is
      type Some_Task_Type is new NTCT;

      package P is
         type T is limited private;
         type R is tagged record -- Illegal!
            -- This should say "limited record".
            --                              X : T;
            NTCF : NTCT;
         end record;
      private
         type T is new Integer; -- R becomes nonlimited here.
      end P;
      use P;
      package Q is
         type R2 is new R with record
            Y : Some_Task_Type;
         end record;
      end Q;
   end Section_7_5_Paragraph_2b;

   procedure Section_7_5_Paragraph_17 is

      --  Example of a package with a limited type:

      package IO_Package is
         type File_Name is limited private;

         procedure Open (F : in out File_Name);
         procedure Close (F : in out File_Name);
         procedure Read (F : in File_Name; Item : out Integer);
         procedure Write (F : in File_Name; Item : in Integer);
      private
         type File_Name is limited record
            Internal_Name : Integer := 0;
         end record;
      end IO_Package;

      package body IO_Package is
         Limit : constant := 200;
         type File_Descriptor is record
            NTCF : NTCT; --@ ...
         end record;
         Directory : array (1 .. Limit) of File_Descriptor;
         --@           ...
         procedure Open (F : in out File_Name) is
         begin
            null; --@ ...
         end Open;
         procedure Close (F : in out File_Name) is
         begin
            null; --@ ...
         end Close;
         procedure Read (F : in File_Name; Item : out Integer) is
         begin
            null; --@ ...
         end Read;
         procedure Write (F : in File_Name; Item : in Integer) is
         begin
            null; --@ ...
         end Write;
      begin
         null;
         --@          ...
      end IO_Package;

   begin
      null;
   end Section_7_5_Paragraph_17;

   --  7.6 Assignment and Finalization

   --  7.6.1 Completion and Finalization

begin
   null;
end AARM_202x_CH07;
