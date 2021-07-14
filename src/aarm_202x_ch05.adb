with Ada.Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Environment_Variables;
with Ada.Tags;

procedure AARM_202x_CH05 is

   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      type NTCT is range 0 .. 20;
      type Real is digits 8;
      type Matrix is array (Integer range <>, Integer range <>) of Real;
      type Vector is array (Integer range <>) of Real;
      type Month_Name is (January, February, March, April, May, June, July,
                          August, September, October, November, December);
      type Date is record
         Day   : Integer range 1 .. 31;
         Month : Month_Name;
         Year  : Integer range 0 .. 4000;
      end record;
      package Persons_And_Car is
         type Gender is (M, F);
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
               when M =>
                  Wife : Person_Name (Sex => F);
               when F =>
                  Husband : Person_Name (Sex => M);
            end case;
         end record;
      end Persons_And_Car;
      type Bit_Vector is array (Integer range <>) of Boolean;
      type Table is array (1 .. 10) of Integer;
      N  : constant := 199;
      M  : constant := 99;
      PI : constant := 3.14;
      type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
      type Cell;  --  incomplete type declaration
      type Link is access Cell;
      type Cell is record
         Value : Integer;
         Succ  : Link;
         Pred  : Link;
      end record;
      Head      : Link     := new Cell'(0, null, null);
      Next      : Link     := Head.Succ;
      Max_Value : constant := 99;
      Max       : constant := 500;                   -- an integer number
      type Complex is record
         Re : Real := 0.0;
         Im : Real := 0.0;
      end record;
      Board : Matrix (1 .. 8, 1 .. 8);  --  see 3.6
      Count : Integer;
      Grid  : array (1 .. 80, 1 .. 100) of Boolean;
      type Color is (White, Red, Yellow, Green, Blue, Brown, Black);
      type Frame is access Matrix;
      function Next_Frame (K : Positive) return Frame is (null);          --  see 3.10
      function Dot_Product (Left, Right : Vector) return Real is (0.0);  --  see 3.6
      type Expression is tagged null record;
      type Expr_Ptr is access all Expression'Class;
      type Binary_Operation is new Expression with record
         Left, Right : Expr_Ptr;
      end record;
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

   --                               5   Statements

   --  5.1 Simple and Compound Statements - Sequences of Statements

   procedure Section_5_1_Paragraph_19a is
      procedure Record_Result (R : Integer) is null;
      task Compute is
         entry Ent (I : out Integer; J : in Integer);
      end Compute;
      task body Compute is
         Sum : Integer := 0;                       -- Compute.Sum
      begin
         Outer :                                      -- Compute.Outer
         for I in 1 .. 10 loop                     -- Compute.Outer.I
            Blk :                                     -- Compute.Blk
            declare
               Sum : Integer := 0;                 -- Compute.Blk.Sum
            begin
               accept Ent (I : out Integer; J : in Integer) do -- Compute.Ent.I, Compute.Ent.J
                  Compute.Ent.I := Compute.Outer.I;
                  Inner :                             -- Compute.Blk.Inner
                  for J in 1 .. 10 loop              -- Compute.Blk.Inner.J
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

      --  Examples of labeled statements:

      <<Here>>
      <<Ici>>
      <<Aqui>>
      <<Hier>>
      null;
      <<After>>
      X := 1;

   end Section_5_1_Paragraph_19a;

   --  5.2 Assignment Statements

   procedure Section_5_2_Paragraph_4a is
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
   end Section_5_2_Paragraph_4a;

   procedure Section_5_2_Paragraph_18 is
      use Needed_To_Compile;
      Value : Integer;
      Shade : Color;
      F     : constant         := 2;
      U     : Real;
      V, W  : Vector (1 .. 10) := (others => 1.1);
   begin

      --  Examples of assignment statements:

      Value := Max_Value - 1;
      Shade := Blue;

      Next_Frame (F) (M, N) := 2.5;        --  see 4.1.1
      U                     := Dot_Product (V, W);            --  see 6.3

      Writer   := (Status => Open, Unit => Printer, Line_Count => 60);  -- see 3.8.1
      Next.all := (72074, null, Head);    --  see 3.10.1

   end Section_5_2_Paragraph_18;

   procedure Section_5_2_Paragraph_22 is

      --   Examples involving scalar subtype conversions:

      I, J : Integer range 1 .. 10 := 5;
      K    : Integer range 1 .. 20 := 15;
   begin
      I := J;  --  identical ranges
      K := J;  --  compatible ranges
      J := K;  --  will raise Constraint_Error if K > 10
   end Section_5_2_Paragraph_22;

   procedure Section_5_2_Paragraph_25 is

      --  Examples involving array subtype conversions:

      A : String (1 .. 31);
      B : String (3 .. 33);

   begin
      A           := B;  --  same number of components
      A (1 .. 9)  := "tar sauce";
      A (4 .. 12) := A (1 .. 9);  --  A(1 .. 12) = "tartar sauce"
   end Section_5_2_Paragraph_25;

   procedure Section_5_2_Paragraph_28e is
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
      --@ Foo(2).all := Foo(1).all; -- illegal since Ada 2005
      null;
   end Section_5_2_Paragraph_28e;

   --  5.2.1 Target Name Symbols

   procedure Section_5_2_Paragraph_1 is
      use Needed_To_Compile;

      My_Complex_Array : array (1 .. Max) of Complex; -- See 3.3.2, 3.8.
   begin

      -- Examples of the use of target name symbols:

      Board (1, 1) := @ + 1.0;  -- An abbreviation for Board(1, 1) := Board(1, 1) + 1.0;
                                -- (Board is declared in 3.6.1).

      -- Square the element in the Count (see 3.3.1) position:

      My_Complex_Array (Count) := (Re => @.Re**2 - @.Im**2, Im => 2.0 * @.Re * @.Im);

      -- A target_name can be used multiple times and as a prefix if needed.


   end Section_5_2_Paragraph_1;

   --  5.3 If Statements

   procedure Section_5_3_Paragraph_7 is
      use Needed_To_Compile.Persons_And_Car, Ada.Text_IO;
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
   end Section_5_3_Paragraph_7;

   --  5.4 Case Statements

   procedure Section_5_4_Paragraph_16_18c is
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
   end Section_5_4_Paragraph_16_18c;

   --  5.5 Loop Statements

   procedure Section_5_5_Paragraph_15 is
      use Ada.Text_IO, Needed_To_Compile;
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

      --  Example of a loop statement without an iteration scheme:

      loop
         Get (Current_Character);
         exit when Current_Character = '*';
      end loop;

      --  Example of a loop statement with a while iteration scheme:

      while Bid (N).Price < Cut_Off.Price loop
         Record_Bid (Bid (N).Price);
         N := N + 1;
      end loop;

      --  Example of a loop statement with a for iteration scheme:

      for J in Buffer'Range loop     --  works even with a null range
         if Buffer (J) /= Space then
            Put (Buffer (J));
         end if;
      end loop;

      --  Example of a loop statement with a name:

      Summation :
      while Next /= Head loop       -- see 3.10.1
         Sum  := Sum + Next.Value;
         Next := Next.Succ;
      end loop Summation;

      -- Example of a parallel loop:

      -- see 3.6
      --        parallel  --@@ MODIF PP: not yet available
      for I in Grid'Range (1) loop
         Grid (I, 1) := (for all J in Grid'Range (2) => Grid (I, J) = True);
      end loop;

      -- {AI12-0312-1} Example of a parallel loop with a chunk specification:

      declare
         subtype Chunk_Number is Natural range 1 .. 8;
         Chunk : Chunk_Number; --@@ MODIF PP: to be canceled when parallel will be available

         Partial_Sum, Partial_Max : array (Chunk_Number) of Natural := (others => 0);
         Partial_Min              : array (Chunk_Number) of Natural := (others => Natural'Last);

      begin
         --           parallel (Chunk in Chunk_Number)  --@@ MODIF PP: not yet available
         for I in Grid'Range (1) loop
            declare
               True_Count : constant Natural :=
               [for J in Grid'Range(2) => (if Grid (I, J) then 1 else 0)]'Reduce("+",0);
               begin
                  Partial_Sum (Chunk) := @ + True_Count;
                  Partial_Min (Chunk) := Natural'Min (@, True_Count);
                  Partial_Max (Chunk) := Natural'Max (@, True_Count);
               end;
            end loop;

--              Put_Line ("Total=" & Partial_Sum'Reduce ("+", 0)'Image &  --@@ MODIF26 PP: GNAT error: expected type universal integer
--                          ", Min=" & Partial_Min'Reduce(Natural'Min, Natural'Last)'Image &  --@@ MODIF26 PP: GNAT error: expected type universal integer
--                          ", Max=" & Partial_Max'Reduce(Natural'Max, 0)'Image);  --@@ MODIF26 PP: GNAT error: expected type universal integer
            Put_Line ("Total=" & Natural'Image (Partial_Sum'Reduce ("+", 0)) &
                        ", Min=" & Natural'Image (Partial_Min'Reduce(Natural'Min, Natural'Last)) &
                        ", Max=" & Natural'Image (Partial_Max'Reduce(Natural'Max, 0)));
         end;

         -- {AI12-0312-1} For an example of an iterator_filter, see 4.5.8.

   end Section_5_5_Paragraph_15;

      --  5.5.1 User-Defined Iterator Types

      --  5.5.2 Generalized Loop Iteration

   procedure Section_5_5_2_Paragraph_15 is
      use Needed_To_Compile;
      Board : Matrix (1 .. 8, 1 .. 8);  --  see 3.6
   begin

         -- Array component iterator example:

         --        parallel --@@ MODIF PP: not yet available
      for Element of Board loop  -- See 3.6.1.
         Element := Element * 2.0; -- Double each element of Board, a two-dimensional array.
      end loop;
   end Section_5_5_2_Paragraph_15;

      --  5.5.3 Procedural Iterators

   procedure Section_5_5_3_Paragraph_22 is
      use Needed_To_Compile, Ada.Text_IO;
      type My_Key_Type is new NTCT;
      type My_Element_Type is new NTCT;
      package My_Maps is new Ada.Containers.Ordered_Maps (My_Key_Type, My_Element_Type);
      use My_Maps;
      My_Map : Map;
   begin

         -- Example of iterating over a map from My_Key_Type to My_Element_Type (see A.18.4):

         --        for (C : Cursor) of My_Map.Iterate loop   --@@ MODIF PP: not yet available
         --           Put_Line (My_Key_Type'Image (Key (C)) & " => " &
         --                       My_Element_Type'Image (Element (C)));
         --        end loop;

         -- The above is equivalent to:

      declare
         procedure P (C : Cursor) is
         begin
            Put_Line (My_Key_Type'Image (Key (C)) & " => " & My_Element_Type'Image (Element (C)));
         end P;
      begin
         My_Map.Iterate (P'Access);
      end;

         -- Example of iterating over the environment variables (see A.17):

         --        for (Name, Val) of Ada.Environment_Variables.Iterate(<>) loop  --@@ MODIF PP: not yet available
         --           --  "(<>)" is optional because it is the last parameter
         --           Put_Line (Name & " => " & Val);
         --        end loop;

         -- The above is equivalent to:

      declare
         procedure P (Name : String; Val : String) is
         begin
            Put_Line (Name & " => " & Val);
         end P;
      begin
         Ada.Environment_Variables.Iterate (P'Access);
      end;
   end Section_5_5_3_Paragraph_22;

      --  5.6 Block Statements

   procedure Section_5_6_Paragraph_7 is
      V, U : Integer;
   begin
      Swap :
      declare
         Temp : Integer;
      begin
         Temp := V;
         V    := U;
         U    := Temp;
      end Swap;
   end Section_5_6_Paragraph_7;

      --  5.6.1 Parallel Block Statements

   procedure Section_5_6_1_Paragraph_4 is
      use Needed_To_Compile;

      -- Example of a parallel block used to walk a binary tree in parallel:

      procedure Traverse (T : Expr_Ptr) is -- see 3.9.1
      begin
         if T /= null and then T.all in Binary_Operation'Class -- see 3.9.1
         then -- recurse down the binary tree
               --              parallel do   --@@ MODIF PP: not yet available
               --                 Traverse (T.Left);
               --                   and
               --                     Traverse (T.Right);
               --                   and
            Ada.Text_IO.Put_Line ("Processing " & Ada.Tags.Expanded_Name (T'Tag));
               --              end do;
         end if;
      end Traverse;

      -- Example of a parallel block used to search two halves of a string in parallel:

      function Search (S : String; Char : Character) return Boolean is
      begin
         if S'Length <= 1000 then
               -- Sequential scan
            return (for some C of S => C = Char);
         else
               -- Parallel divide and conquer
            declare
               Mid : constant Positive := S'First + S'Length / 2 - 1;
            begin
                  --                    parallel do   --@@ MODIF PP: not yet available
               for C of S (S'First .. Mid) loop
                  if C = Char then
                     return True;  -- Terminates enclosing do
                  end if;
               end loop;
                  --                         and
               for C of S (Mid + 1 .. S'Last) loop
                  if C = Char then
                     return True;  -- Terminates enclosing do
                  end if;
               end loop;
                  --                    end do;
                  -- Not found
               return False;
            end;
         end if;
      end Search;

   begin
      null;
   end Section_5_6_1_Paragraph_4;

      --  5.7 Exit Statements

   procedure Section_5_7_Paragraph_8 is
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
   end Section_5_7_Paragraph_8;

      --  5.8 Goto Statements

   procedure Section_5_8_Paragraph_8 is
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
   end Section_5_8_Paragraph_8;

begin
   null;
end AARM_202x_CH05;
