with Ada.IO_Exceptions;
with Ada.Text_IO;

procedure AARM_202x_CH08 is

   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      type NTCT is range 0 .. 20;
      type Month_Name is (Jan, Feb, Mar, Apr, May, Jun, July, Aug, Sep, Oct, Nov, Dec);
      type Gender is (M, F);
      type Date is record
         Day   : Integer range 1 .. 31;
         Month : Month_Name;
         Year  : Integer range 0 .. 4000;
      end record;
      type Person (<>);
      type Person_Name is access Person;
      type Person (Sex : Gender) is record
         Name  : String (1 .. 20);
         Birth : Date;
         Age   : Integer range 0 .. 130;
         case Sex is
            when M =>
               Wife : Person_Name (Sex => F);
            when F =>
               Husband : Person_Name (Sex => M);
         end case;
      end record;
      Leftmost_Person : Person (F);
      type Queue is limited interface;
      procedure Append (Q : in out Queue; Person : in Person_Name) is abstract;
      procedure Remove_First (Q : in out Queue; Person : out Person_Name) is abstract;
      function Cur_Count (Q : in Queue) return Natural is abstract;
      function Max_Count (Q : in Queue) return Natural is abstract;
      package Rational_Numbers is
         type Rational is record
            Numerator   : Integer;
            Denominator : Positive;
         end record;
         function "/" (X, Y : Integer) return Rational is ((0, 1));
      end Rational_Numbers;
      package Table_Manager is
      end Table_Manager;
      type Color is (White, Red, Yellow, Green, Blue, Brown, Black);
      type Real is digits 8;
      type Vector is array (Integer range <>) of Real;
      function Dot_Product (Left, Right : Vector) return Real is (0.0);  --  see 3.6
      type Cell;  --  incomplete type declaration
      type Link is access Cell;
      type Cell is record
         Value : Integer;
         Succ  : Link;
         Pred  : Link;
      end record;
      function Min_Cell (X : Link) return Cell is ((0, null, null));                 --  see 3.10.1
      type Keyboard_ID is (New_ID, TTY_ID, Term_ID);
      type Serial_Device is limited interface;
      task type Keyboard_Driver (ID : Keyboard_ID := New_ID) is new Serial_Device with  -- see 3.9.4
         entry Read (C : out Character);
         entry Write (C : in Character);
         entry Wait_For_Interrupt; --@ needed for §9.7.4
      end Keyboard_Driver;
      Pool : array (1 .. 10) of Keyboard_Driver;
   end Needed_To_Compile;
   package body Needed_To_Compile is
      task body Keyboard_Driver is
      begin
         null;
      end Keyboard_Driver;
   end Needed_To_Compile;
   use Needed_To_Compile;

   --                            8   Visibility Rules

   --  8.1 Declarative Region

   --  8.2 Scope of Declarations

   --  8.3 Visibility

   package Section_8_3_Paragraph_12b is

      package Outer is
         package P1 is
            type Ifc1 is interface;
            procedure Null_Procedure (X : Ifc1) is null;
            procedure Abstract_Subp (X : Ifc1) is abstract;
         end P1;

         package P2 is
            type Ifc2 is interface;
            procedure Null_Procedure (X : Ifc2) is null;
            procedure Abstract_Subp (X : Ifc2) is abstract;
         end P2;

         type T is abstract new P1.Ifc1 and P2.Ifc2 with null record;
      end Outer;

   end Section_8_3_Paragraph_12b;

   procedure Section_8_3_Paragraph_26i is
   --          package A is
   --                     type T is tagged private;
   --                     package B is
   --                        type NT is new T with record
   --                           I: Integer; -- Illegal because T.I is visible in the body.
   --                        end record; -- T.I is not visible here.
   --                     end B;
   --                  private
   --                     type T is tagged record
   --                        I: Integer; -- Illegal because T.I is visible in the body.
   --                     end record;
   --                  end A;

   --   package body A is
   --                     package body B is
   --                        -- T.I becomes visible here.
   --                     end B;
   --                  end A;

   --         package A.C is
   --                     type NT2 is new A.T with record
   --                        I: Integer; -- Illegal because T.I is visible in the private part.
   --                     end record; -- T.I is not visible here.
   --                  private
   --                      -- T.I is visible here.
   --                  end A.C;

      --@        with A;
   --                  package D is
   --                     type NT3 is new A.T with record
   --                        I: Integer; -- Legal because T.I is never visible in this package.
   --                     end record;
   --                  end D;

      --@        with D;
   --                  package A.E is
   --                     type NT4 is new D.NT3 with null record;
   --                     X : NT4;
   --                     I1 : Integer := X.I;        -- D.NT3.I
   --                     I2 : Integer := D.NT3(X).I; -- D.NT3.I
   --                     I3 : Integer := A.T(X).I;   -- A.T.I
   --                  end A.E;

   begin
      null;
   end Section_8_3_Paragraph_26i;

   --  8.3.1 Overriding Indicators

   package Section_8_3_1_Paragraph_9 is
      -- The use of overriding_indicators allows the detection of
      --errors at compile-time that otherwise might not be detected at all. For
      --instance, we might declare a security queue derived from the Queue interface
      --of 3.9.4 as:

      type Security_Queue is new Queue with record
         NTCF : NTCT; --@ ...;
      end record;

      overriding procedure Append (Q : in out Security_Queue; Person : in Person_Name) is null;

      overriding procedure Remove_First (Q : in out Security_Queue; Person : out Person_Name) is null;

      overriding function Cur_Count (Q : in Security_Queue) return Natural is (0);

      overriding function Max_Count (Q : in Security_Queue) return Natural is (0);

      not overriding procedure Arrest (Q : in out Security_Queue; Person : in Person_Name) is null;
   end Section_8_3_1_Paragraph_9;

   --  8.4 Use Clauses

   --  Example of a use clause in a context clause:

   --     with Ada.Calendar; use Ada;

   --  Example of a use type clause:

   use type Rational_Numbers.Rational; -- see 7.1
   Two_Thirds : Rational_Numbers.Rational := 2 / 3;

   --  8.5 Renaming Declarations

   --  8.5.1 Object Renaming Declarations

   procedure Section_8_5_1_Paragraph_7 is
   begin
      --   Example of renaming an object:
      declare
         L : Person renames Leftmost_Person; -- see 3.10.1
      begin
         L.Age := L.Age + 1;
      end;
   end Section_8_5_1_Paragraph_7;

   --  8.5.2 Exception Renaming Declarations

   --   Example of renaming an exception:

   EOF : exception renames Ada.IO_Exceptions.End_Error; -- see A.13

   --  8.5.3 Package Renaming Declarations

   --   Example of renaming a package:

   package TM renames Table_Manager;

   --  8.5.4 Subprogram Renaming Declarations

   package Section_8_5_4_Paragraph_8d is

      package P is
         type T is tagged null record;
         function Predefined_Equal (X, Y : T) return Boolean renames "=";
      private
         function "=" (X, Y : T) return Boolean; -- Override predefined "=".
      end P;

      --@  with P; use P;
      use P;
      package Q is
         function User_Defined_Equal (X, Y : T) return Boolean renames P."=";
      end Q;
   end Section_8_5_4_Paragraph_8d;

   package body Section_8_5_4_Paragraph_8d is
      -- Needed to compile, sometimes dummy
      package body P is
         function "=" (X, Y : T) return Boolean is (True);
      end P;
   end Section_8_5_4_Paragraph_8d;

   package Section_8_5_4_Paragraph_13 is
      K    : constant := 9;
      Head : Link;

      --  Examples of subprogram renaming declarations:

      procedure My_Write (C : in Character) renames Pool (K).Write; --  see 4.1.3

      function Real_Plus (Left, Right : Real) return Real renames "+";
      function Int_Plus (Left, Right : Integer) return Integer renames "+";

      function Rouge return Color renames Red;  --  see 3.5.1
      function Rot return Color renames Red;
      function Rosso return Color renames Rouge;

      function Next (X : Color) return Color renames Color'Succ; -- see 3.5.1

      --  Example of a subprogram renaming declaration with new parameter names:

      function "*" (X, Y : Vector) return Real renames Dot_Product; -- see 6.1

      --  Example of a subprogram renaming declaration with a new default expression:

      function Minimum (L : Link := Head) return Cell renames Min_Cell; -- see 6.1
   end Section_8_5_4_Paragraph_13;

   --  8.5.5 Generic Renaming Declarations

   --   Example of renaming a generic unit:

   generic package Enum_IO renames Ada.Text_IO.Enumeration_IO;  -- see A.10.10

   --  8.6 The Context of Overload Resolution

   package Section_8_6_Paragraph_35h is

      type T;
      type A is access T;
      type T is array (Integer range 1 .. 10) of A;
      I : Integer := 3;
      function F (X : Integer := 7) return A is (null);
      --                  Y : A := F(I); -- Ambiguous? (We hope so.)
   end Section_8_6_Paragraph_35h;

begin
   null;
end AARM_202x_CH08;
