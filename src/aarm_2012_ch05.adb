--  with Ada.Finalization;
with Ada.Text_IO;
procedure AARM_2012_CH05 is
   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      --        K : constant := 99;
      --        G : constant Character := '@';
      --        type Coordinate is (X, Y);
      type Real is digits 8;
      type Matrix is array (Integer range <>, Integer range <>) of Real;
      type Vector is array (Integer range <>) of Real;
      subtype Month_Name is String (1 .. 3);
      type Gender is (MM, FF);
      type Date is record
         Day   : Integer range 1 .. 31;
         Month : Month_Name;
         Year  : Integer range 0 .. 4000;
      end record;
      type Person (<>);    -- incomplete type declaration
      type Car is tagged; -- incomplete type declaration

      type Person_Name is access Person;
      type Car_Name is access all Car'Class;

      type Car is tagged record
         Number : Integer;
         Owner  : Person_Name;
      end record;

      type Person (Sex : Gender) is record
         Name    : String (1 .. 20);
         Birth   : Date;
         Age     : Integer range 0 .. 130;
         Vehicle : Car_Name;
         case Sex is
            when MM =>
               Wife : Person_Name (Sex => FF);
            when FF =>
               Husband : Person_Name (Sex => MM);
         end case;
      end record;
      --        type Binop_Ptr is access all Integer;
      type Bit_Vector is array (Integer range <>) of Boolean;
      type Table is array (1 .. 10) of Integer;
      N  : constant := 199;
      M  : constant := 99;
      PI : constant := 3.14;
      type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
      --        package Key_Manager is
      --           subtype Key is Integer;
      --        end Key_Manager;
--        Max       : constant := 500;                   -- an integer number
      --        Page_Size : constant := 100;
      --        subtype Cylinder_Index is Natural;
      --        subtype Track_Number is Natural;
      --        function Dispersion (X : Real) return Real;
      Max_Value : constant := 99;
      type Color is (White, Red, Yellow, Green, Blue, Brown, Black);
      type Frame is access Matrix;
      function Next_Frame (K : Positive) return Frame;          --  see 3.10
      function Dot_Product (Left, Right : Vector) return Real;  --  see 3.6
      type Device is (Printer, Disk, Drum);
      type State is (Open, Closed);
      Page_Size : constant := 100;
      subtype Cylinder_Index is Natural;
      subtype Track_Number is Natural;
      type Peripheral (Unit : Device := Disk) is record
         Status : State;
         case Unit is
            when Printer =>
               Line_Count : Integer range 1 .. Page_Size;
            when others =>
               Cylinder : Cylinder_Index;
               Track    : Track_Number;
         end case;
      end record;
      Writer : Peripheral (Unit => Printer);
   end Needed_To_Compile;
   package body Needed_To_Compile is
      function Next_Frame (K : Positive) return Frame is
      begin
         return null;
      end Next_Frame;
      function Dot_Product (Left, Right : Vector) return Real is
      begin
         return 0.0;
      end Dot_Product;
   end Needed_To_Compile;

   -- 5.1 Simple and Compound Statements - Sequences of Statements
   procedure Parapraph_5_1_16_18_19 is
      procedure Record_Result (R : Integer) is null;
      task Compute is
         entry Ent (I : out Integer; J : in Integer);
      end Compute;
      task body Compute is
         Sum : Integer := 0;                       -- Compute.Sum
      begin
         Outer :                                      -- Compute.Outer
         for I in 1 .. 10 loop     -- Compute.Outer.I
            Blk :                                     -- Compute.Blk
            declare
               Sum : Integer := 0;                 -- Compute.Blk.Sum
            begin
               accept Ent (I : out Integer; J : in Integer) do
                  -- Compute.Ent.I, Compute.Ent.J
                  Compute.Ent.I := Compute.Outer.I;
                  Inner :                             -- Compute.Blk.Inner
                  for J in 1 .. 10 loop
                     -- Compute.Blk.Inner.J
                     Sum := Sum + Compute.Blk.Inner.J * Compute.Ent.J;
                  end loop Inner;
               end Ent;
               Compute.Sum := Compute.Sum + Compute.Blk.Sum;
            end Blk;
         end loop Outer;
         Record_Result (Sum);
      end Compute;
      X : Integer;
   begin
      <<After>>
      X := 1;
      <<Here>> <<Ici>> <<Aqui>> <<Hier>>
      null;
   end Parapraph_5_1_16_18_19;

   -- 5.2 Assignment Statements
   procedure Parapraph_5_2_4 is
      type R1 is new Integer;
      type R2 is new Float;
      type P1 is access R1;
      type P2 is access R2;

      function F return P1;
      function F return P2;

      function F return P1 is
      begin
         return null;
      end F;
      function F return P2 is
      begin
         return null;
      end F;

      X : R1;
   begin
      F.all := X;  -- Right hand side helps resolve left hand side
   end Parapraph_5_2_4;
   procedure Parapraph_5_2_18 is
      use Needed_To_Compile;
      Value    : Integer;
      Shade    : Color;
      F        : constant         := 2;
      U        : Real;
      V, W     : Vector (1 .. 10) := (others => 1.1);
      Next_Car : Car_Name         := new Car;
   begin
      Value := Max_Value - 1;
      Shade := Blue;

      Next_Frame (F) (M, N) := 2.5;        --  see 4.1.1
      U                     := Dot_Product (V, W);            --  see 6.3

      Writer := (Status => Open, Unit => Printer, Line_Count => 60);  -- see 3.8.1
      --        Next_Car.all := (72074, null);    --  see 3.10.1  -- MODIF PP pb ?
   end Parapraph_5_2_18;

   procedure Parapraph_5_2_22 is

      I, J : Integer range 1 .. 10 := 5;
      K    : Integer range 1 .. 20 := 15;
   begin
      I := J;  --  identical ranges
      K := J;  --  compatible ranges
      J := K;  --  will raise Constraint_Error if K > 10
   end Parapraph_5_2_22;
   procedure Parapraph_5_2_25 is

      A : String (1 .. 31);
      B : String (3 .. 33);

   begin
      A := B;  --  same number of components

      A (1 .. 9)  := "tar sauce";
      A (4 .. 12) := A (1 .. 9);  --  A(1 .. 12) = "tartar sauce"
   end Parapraph_5_2_25;
   procedure Parapraph_5_2_28 is
      type NonLim is new Integer;
      type Lim is limited record
         Field : Float;
      end record;

      type AccNonLim is access NonLim;
      function Foo (Arg : in Integer) return AccNonLim;
      type AccLim is access Lim;
      function Foo (Arg : in Integer) return AccLim;

      function Foo (Arg : in Integer) return AccNonLim is
      begin
         return null;
      end Foo;
      function Foo (Arg : in Integer) return AccLim is
      begin
         return null;
      end Foo;
   begin
      -- Foo(2).all := Foo(1).all; -- illegal since Ada 2005 MODIF PP
      null;
   end Parapraph_5_2_28;
   -- 5.3 If Statements
   procedure Parapraph_5_3_7 is
      use Needed_To_Compile, Ada.Text_IO;
      type Months is (January, November, December); -- to be completed ;-)
      Month                     : Months   := November;
      Day                       : Positive := 11;
      Year                      : Integer  := 2012;
      My_Car                    : Car_Name := new Car;
      Line_Too_Short, Line_Full : Boolean;
      Item                      : String   := "thing";
      Layout_Error : exception;
      procedure Report (V : String) renames Ada.Text_IO.Put;
   begin
      if Month = December and Day = 31 then
         Month := January;
         Day   := 1;
         Year  := Year + 1;
      end if;

      if Line_Too_Short then
         raise Layout_Error;
      elsif Line_Full then
         New_Line;
         Put (Item);
      else
         Put (Item);
      end if;

      if My_Car.Owner.Vehicle /= My_Car then            --  see 3.10.1
         Report ("Incorrect data");
      end if;
   end Parapraph_5_3_7;
   -- 5.4 Case Statements

   procedure Parapraph_5_4_16_18 is
      use Needed_To_Compile;
      type Sensors is (Elevation, Azimuth, Distance, SomeThingElse);
      Sensor       : Sensors;
      Sensor_Value : Integer;
      procedure Record_Elevation (V : Integer) is null;
      procedure Record_Azimuth (V : Integer) is null;
      procedure Record_Distance (V : Integer) is null;
      procedure Compute_Initial_Balance is null;
      procedure Compute_Closing_Balance is null;
      procedure Generate_Report (V : Day) is null;
      procedure Update_Bin (V : Integer) is null;
      procedure Empty_Bin (V : Integer) is null;
      Today : Day;
      function Bin_Number (V : Integer) return Integer is (0);
      Count : Integer;
      Error : exception;
   begin
      case Sensor is
         when Elevation =>
            Record_Elevation (Sensor_Value);
         when Azimuth =>
            Record_Azimuth (Sensor_Value);
         when Distance =>
            Record_Distance (Sensor_Value);
         when others =>
            null;
      end case;

      case Today is
         when Mon =>
            Compute_Initial_Balance;
         when Fri =>
            Compute_Closing_Balance;
         when Tue .. Thu =>
            Generate_Report (Today);
         when Sat .. Sun =>
            null;
      end case;

      case Bin_Number (Count) is
         when 1 =>
            Update_Bin (1);
         when 2 =>
            Update_Bin (2);
         when 3 | 4 =>
            Empty_Bin (1);
            Empty_Bin (2);
         when others =>
            raise Error;
      end case;
      declare
         subtype S is Integer range 1 .. 2;
         function F return S;
         function F return S is (1);
      begin
         case F is
            when 1 =>
               null;
            when 2 =>
               null;
               -- No others needed.
         end case;
      end;
   end Parapraph_5_4_16_18;
   -- 5.5 Loop Statements
   procedure Parapraph_5_5_15 is
      use Ada.Text_IO;
      Current_Character : Character;
      type Cell;  --  incomplete type declaration
      type Link is access Cell;
      type Cell is record
         Value : Integer;
         Succ  : Link;
         Pred  : Link;
         Price : Integer;
      end record;
      Head    : Link               := new Cell'(0, null, null, 0);
      Next    : Link               := Head.Succ;
      Sum     : Integer            := 0;
      Buffer  : String             := "something";
      Space   : constant Character := ' ';
      Bid     : array (1 .. 10) of Cell;
      Cut_Off : Cell;
      N       : Integer            := 1;
      procedure Record_Bid (V : Integer) is null;
   begin
      loop
         Get (Current_Character);
         exit when Current_Character = '*';
      end loop;

      while Bid (N).Price < Cut_Off.Price loop
         Record_Bid (Bid (N).Price);
         N := N + 1;
      end loop;

      for J in Buffer'Range loop     --  works even with a null range
         if Buffer (J) /= Space then
            Put (Buffer (J));
         end if;
      end loop;

      Summation :
      while Next /= Head loop       -- see 3.10.1
         Sum  := Sum + Next.Value;
         Next := Next.Succ;
      end loop Summation;
   end Parapraph_5_5_15;
-- 5.5.2 Generalized Loop Iteration
   procedure Parapraph_5_5_2_14 is
      use Needed_To_Compile;
      Board : Matrix (1 .. 8, 1 .. 8);  --  see 3.6
   begin
      for Element of Board loop  -- See 3.6.1.
         Element := Element * 2.0; -- Double each element of Board, a two-dimensional array.
      end loop;
   end Parapraph_5_5_2_14;
   -- 5.6 Block Statements
   procedure Parapraph_5_6_7 is
      V, U : Integer;
   begin
      Swap : declare
         Temp : Integer;
      begin
         Temp := V;
         V    := U;
         U    := Temp;
      end Swap;
   end Parapraph_5_6_7;
   -- 5.7 Exit Statements
   procedure Parapraph_5_7_8 is
      Max_Num_Items, Terminal_Item : constant := 99;
      procedure Get_New_Item (V : out Integer) is null;
      procedure Merge_Item (V, S : Integer) is null;
      New_Item, Storage_File : Integer;
      Found                  : Boolean;
   begin
      for N in 1 .. Max_Num_Items loop
         Get_New_Item (New_Item);
         Merge_Item (New_Item, Storage_File);
         exit when New_Item = Terminal_Item;
      end loop;

      Main_Cycle :
      loop
         --  initial statements
         exit Main_Cycle when Found;
         --  final statements
      end loop Main_Cycle;
   end Parapraph_5_7_8;
-- 5.8 Goto Statements
   procedure Parapraph_5_8_8 is
      A : constant String := "whatelse";
      N : Positive;
      procedure Exchange (L, R : Character) is null;
   begin
      <<Sort>>
      for I in 1 .. N - 1 loop
         if A (I) > A (I + 1) then
            Exchange (A (I), A (I + 1));
            goto Sort;
         end if;
      end loop;
   end Parapraph_5_8_8;
begin
   null;
end AARM_2012_CH05;
