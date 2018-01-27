procedure AARM_2012_CH06 is

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
               Wife : Person_Name (FF); --@@ gnatpp error (Sex => FF);
            when FF =>
               Husband : Person_Name (MM); --@@ gnatpp error (Sex => MM);
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
      procedure Check (V : Boolean) is null;
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
   -- 6.1 Subprogram Declarations

   package Parapraph_6_1_37 is
      use Needed_To_Compile;
      type Cell;  --  incomplete type declaration
      type Link is access Cell;
      type Cell is record
         Value : Integer;
         Succ  : Link;
         Pred  : Link;
      end record;
      type Line is array (1 .. 99) of Character;
      type Line_Size is range 1 .. 99;
      subtype Probability is Real range 0.0 .. 1.0;   --   a subtype with a smaller range

      procedure Traverse_Tree is null;
      procedure Increment (X : in out Integer) is null;
      procedure Right_Indent (Margin : out Line_Size) is null;          --  see 3.5.4
      procedure Switch (From, To : in out Link) is null;                --  see 3.10.1

      function Random return Probability is (0.0);                      --  see 3.5.7

      function Min_Cell (X : Link) return Cell is ((0, null, null));                 --  see 3.10.1
      function Next_Frame (K : Positive) return Frame is (null);          --  see 3.10
      function Dot_Product (Left, Right : Vector) return Real is (0.0);  --  see 3.6

      function "*" (Left, Right : Matrix) return Matrix is ((1 => (1 => 0.0)));        --  see 3.6

      procedure Print_Header
        (Pages  : in Natural;
         Header : in Line    := (1 .. Line'Last => ' ');  --  see 3.6
         Center : in Boolean := True) is null;
   end Parapraph_6_1_37;

   package Parapraph_6_1_1_27 is
      Tab : array (1 .. 10) of Integer := (others => 0);
      procedure Bar (I : in out Natural);--with Post => I > 0 and then Tab(I)'Old = 1; -- Illegal
   end Parapraph_6_1_1_27;
   package body Parapraph_6_1_1_27 is
      procedure Bar (I : in out Natural) is
      begin
         null;
      end Bar;
   end Parapraph_6_1_1_27;
   -- 6.2 Formal Parameter Modes
   --12.l
   procedure Copy (X : in out String; Y : String) is
   begin
      X := Y;
   end Copy;

   -- 6.3 Subprogram Bodies
   procedure Parapraph_6_3_9 is
      use Needed_To_Compile;
      type Element_Type is new Integer;
      type Table is array (Positive range <>) of Element_Type;
      type Stack (Size : Positive) is record
         Space : Table (1 .. Size);
         Index : Natural := 0;
      end record;
      Stack_Overflow : exception;
      procedure Push (E : in Element_Type; S : in out Stack) is
      begin
         if S.Index = S.Size then
            raise Stack_Overflow;
         else
            S.Index           := S.Index + 1;
            S.Space (S.Index) := E;
         end if;
      end Push;

      function Dot_Product (Left, Right : Vector) return Real is
         Sum : Real := 0.0;
      begin
         Check (Left'First = Right'First and Left'Last = Right'Last);
         for J in Left'Range loop
            Sum := Sum + Left (J) * Right (J);
         end loop;
         return Sum;
      end Dot_Product;
   begin
      null;
   end Parapraph_6_3_9;

   -- 6.3.1 Conformance Rules
   procedure Parapraph_6_3_1_8_21 is
      package P is
         type Root is tagged null record;
         procedure Proc (X : Root) is null;
      end P;
      use P;
      generic
         type Formal (<>) is new Root with private;
         Value : Formal; --@@
      package G is
      -- ...
      end G;

      package body G is
         -- ...
         X : Formal := Value; --@@
         -- ...
         procedure Test (P : access procedure (X : Formal)) is null;
      begin
         Proc (X); -- This is a dispatching call in Instance, because
         -- the actual type for Formal is class-wide.
         -- ...
         -- Proc'Access would be illegal here, because it is of
         -- convention Intrinsic, by the above rule.
         -- Test(Proc'Access); --@@ no GNAT error, yes with compile file not wit check sementic
      end G;

      type Actual is new Root with null record;
      procedure Proc (X : Actual) is null;
      package Instance is new G (Formal => Actual'Class, Value => Actual'(Root with null record)); --@@
      -- It is legal to pass in a class-wide actual, because Formal
      -- has unknown discriminants.

      package A is
         function F (X : Integer := 1) return Boolean is (X = 1);
      end A;
      use A;
      package B is
         package A_View renames A;
         function F_View (X : Integer := 9999) return Boolean renames A.F;
      end B;
      use A, B;
      procedure Main is
         B1 : Boolean := F;
         B2 : Boolean := A.F;
         B3 : Boolean := B.A_View.F;
         B4 : Boolean := A_View.F;
         B5 : Boolean := F_View; -- not fully conformant to the previous ones
      begin
         null;
      end Main;
   begin
      null;
   end Parapraph_6_3_1_8_21;

   -- 6.4 Subprogram Calls
   procedure Parapraph_6_4_Section_22 is
      use Needed_To_Compile;
      type Process_Name is (No_Process, P1, P2);
      procedure Activate
        (Process : in Process_Name;
         After   : in Process_Name := No_Process;
         Wait    : in Duration     := 0.0;
         Prior   : in Boolean      := False) is null;

      procedure Pair (Left, Right : in Person_Name := new Person (MM)) is null;   --  see 3.10.1 --@@ MODIF PP
      X, Y : Process_Name;
   begin
      Activate (X);
      Activate (X, After => Y);
      Activate (X, Wait => 60.0, Prior => True);
      Activate (X, Y, 10.0, False);

      Pair;
      Pair (Left => new Person (FF), Right => new Person (MM)); --@@ MODIF PP
   end Parapraph_6_4_Section_22;

   procedure Parapraph_6_4_Section_26 is
      use Needed_To_Compile;
      type Light is (Red, Green, Orange);

      procedure Put (X : in Integer) is null;
      procedure Put (X : in String) is null;

      procedure Set (Tint : in Color) is null;
      procedure Set (Signal : in Light) is null;

   begin
      Put (28);
      Put ("no possible ambiguity here");

      Set (Tint => Red);
      Set (Signal => Red);
      Set (Color'(Red));
   end Parapraph_6_4_Section_26;
   -- 6.4.1 Parameter Associations

   procedure Parapraph_6_4_1_Section_5c is
      procedure Print (X : in Integer; Y : in Boolean := True) is null;
      procedure Print (Z : in out Integer) is null;
      W : Integer;
   begin
      -- Print(3); -- Ambiguous!
      Print (X => 3); -- Not ambigous
   end Parapraph_6_4_1_Section_5c;

   procedure Parapraph_6_4_1_Section_6i_6l is
      type Tagged_Type is tagged record
         C : Integer;
      end record;
      type Element is new Integer;
      procedure Swap (L, R : in out Element) is null;
      Some_Global_Array : array (1 .. 10) of Element;
      Global            : Tagged_Type;

      procedure Foo (Param : in Tagged_Type := Global) is
         X : Element renames Some_Global_Array (Param.C);
      begin
         Global.C := Global.C + 1;
         Swap (X, Some_Global_Array (Param.C)); --illegal but no GNAT error!
         -- swap (x, x); GNAT error
      end Foo;
      type Some_Type is new Integer;
      function Func_With_Out_Params (V : out Some_Type) return Some_Type is (0);
      procedure P (L, R : Some_Type) is null;
      type Ref is access Some_Type;
      Ptr : Ref := new Some_Type'(10);
      X : Some_Type renames Ptr.all;
   begin
      Ptr := new Some_Type'(20);
      P (Func_With_Out_Params (Ptr.all), X);
   end Parapraph_6_4_1_Section_6i_6l;

   -- 6.5 Return Statements
   procedure Parapraph_6_5_Section_21g is
      type T is tagged null record;
      type Global is access T'Class;
      function F (Ptr : Global) return T'Class is
      begin
         return Ptr.all;
      end F;

      function F return T'Class is
         Local : T'Class := T'(null record);
      begin
         return Local;
      end F;

      X : aliased Integer;

      procedure Foo is null;

      type Component_Type (Discrim : access Integer := X'Access) is limited null record;

      type Undiscriminated is record
         Fld : Component_Type;
      end record;

      function F return Undiscriminated is
         Local : aliased Integer;
      begin
         return X : Undiscriminated := (Fld => (Discrim => Local'Access)) do
            Foo;
         end return;
         -- raises Program_Error after calling Foo.
      end F;
   begin
      null;
   end Parapraph_6_5_Section_21g;

   --     25  Examples of return statements:
   --
   --  26/2    {AI95-00318-02}
--          return;                         -- in a procedure body, entry_body,
   --                                          -- accept_statement
   --          , or extended_return_statement
   --
   --  27      return Key_Value(Last_Index);   -- in a function body
   --
   --  28/2    {AI95-00318-02}
--          return Node : Cell do           -- in a function body, see 3.10.1
   --           for Cell
   --             Node.Value := Result;
   --             Node.Succ := Next_Node;
   --          end return;

   -- 6.5.1 Nonreturning Procedures
   procedure Fail (Msg : String)  -- raises Fatal_Error exception
   with
      No_Return;
   procedure Fail (Msg : String) is
      Fatal_Error : exception;
   begin
      raise Fatal_Error;
   end Fail;
   -- 6.6 Overloading of Operators
--     9       function "+" (Left, Right : Matrix) return Matrix;
--     function "+" (Left, Right : Vector) return Vector;
--
--     --  assuming that A, B, and C are of the type Vector
--     --  the following two statements are equivalent:
--
--     A := B + C;
--     A := "+"(B, C);

   package Parapraph_6_7_Section_6 is
      type Expression is tagged null record;
      procedure Simplify (Expr : in out Expression) is null; -- see 3.9
      -- By default, Simplify does nothing, but it may be overridden in extensions of Expression
   end Parapraph_6_7_Section_6;

   -- 6.8 Expression Functions
   package Parapraph_6_8_Section_9 is
      type Real is digits 8;
      type Point is tagged record
         X, Y : Real := 0.0;
      end record;
      function Is_Origin (P : in Point) return Boolean is -- see 3.9
      (P.X = 0.0 and P.Y = 0.0);
   end Parapraph_6_8_Section_9;

begin
   null;
end AARM_2012_CH06;
