procedure AARM_202x_CH06 is

   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      type Real is digits 8;
      type Matrix is array (Integer range <>, Integer range <>) of Real;
      type Vector is array (Integer range <>) of Real;
      subtype Month_Name is String (1 .. 3);
      type Gender is (M, F);
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
            when M =>
               Wife : Person_Name (F);
            when F =>
               Husband : Person_Name (M);
         end case;
      end record;

      type Bit_Vector is array (Integer range <>) of Boolean;
      type Table is array (1 .. 10) of Integer;
      N  : constant := 199;
      PI : constant := 3.14;
      type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
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

   --                               6   Subprograms

   --  6.1 Subprogram Declarations

   package Section_6_1_Paragraph_37 is
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
      type Element is new Integer;
      type Barrel is tagged record  -- holds objects of type Element
         E : aliased Element;
      end record;

      --  Examples of subprogram declarations:
      procedure Traverse_Tree is null;
      procedure Increment (X : in out Integer) is null;
      procedure Right_Indent (Margin : out Line_Size) is null;          --  see 3.5.4
      procedure Switch (From, To : in out Link) is null;                --  see 3.10.1

      function Random return Probability is (0.0);                      --  see 3.5.7

      function Min_Cell (X : Link) return Cell is ((0, null, null));                 --  see 3.10.1
      function Next_Frame (K : Positive) return Frame is (null);          --  see 3.10
      function Dot_Product (Left, Right : Vector) return Real is (0.0);  --  see 3.6
      function Find (B : aliased in out Barrel; Key : String) return Real is (0.0); --  see 4.1.5
      function "*" (Left, Right : Matrix) return Matrix is ((1 => (1 => 0.0)));        --  see 3.6

      --  Examples of in parameters with default expressions:
      procedure Print_Header
        (Pages  : in Natural;
         Header : in Line := (1 .. Line'Last => ' ');  --  see 3.6

         Center : in Boolean := True) is null;
   end Section_6_1_Paragraph_37;

   --  6.1.1 Preconditions and Postconditions

   package Section_6_1_1_Paragraph_27 is
      Tab : array (1 .. 10) of Integer := (others => 0);
      procedure Bar (I : in out Natural);
      --with Post => I > 0 and then Tab(I)'Old = 1; -- Illegal
   end Section_6_1_1_Paragraph_27;

   package body Section_6_1_1_Paragraph_27 is
      procedure Bar (I : in out Natural) is
      begin
         null;
      end Bar;
   end Section_6_1_1_Paragraph_27;

   --  6.1.2 The Global and Global'Class Aspects

   --  6.2 Formal Parameter Modes

   --         It would be wrong for the compiler to assume that X and Y do not
   --         overlap (unless, of course, it can prove otherwise).
   procedure Copy (X : in out String; Y : String) is
   begin
      X := Y;
   end Copy;

   --  6.3 Subprogram Bodies

   procedure Section_6_3_Paragraph_9 is
      use Needed_To_Compile;
      type Element_Type is new Integer;
      type Table is array (Positive range <>) of Element_Type;
      type Stack (Size : Positive) is record
         Space : Table (1 .. Size);
         Index : Natural := 0;
      end record;
      Stack_Overflow : exception;
      --   Example of procedure body:
      procedure Push (E : in Element_Type; S : in out Stack) is
      begin
         if S.Index = S.Size then
            raise Stack_Overflow;
         else
            S.Index           := S.Index + 1;
            S.Space (S.Index) := E;
         end if;
      end Push;

      --  Example of a function body:
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
   end Section_6_3_Paragraph_9;

   --  6.3.1 Conformance Rules

   procedure Section_6_3_1_Paragraph_8_21 is
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
   end Section_6_3_1_Paragraph_8_21;

   --  6.3.2 Inline Expansion of Subprograms

   --  6.4 Subprogram Calls

   procedure Section_6_4_Paragraph_22 is
      use Needed_To_Compile, Section_6_1_Paragraph_37;
      type Process_Name is (No_Process, P1, P2);
      Title   : constant Line := ('A', 'A', 'R', 'M', others => ' ');
      X, Next : Link;
   begin

      --  Examples of procedure calls:

      Traverse_Tree;                                               --  see 6.1
      Print_Header (128, Title, True);                              --  see 6.1

      Switch (From => X, To => Next);                               --  see 6.1
      Print_Header (128, Header => Title, Center => True);          --  see 6.1
      Print_Header (Header => Title, Center => True, Pages => 128); --  see 6.1

      -- Examples of function calls:

      --       Dot_Product(U, V);   --  see 6.1 and 6.3
      --          Clock;               --  see 9.6
      --          F.all ;              --  presuming F is of an access-to-subprogram type - see 3.10

      --  Examples of procedures with default expressions:

      declare
         procedure Activate
           (Process : in Process_Name; After : in Process_Name := No_Process; Wait : in Duration := 0.0;
            Prior   : in Boolean := False) is null;

         procedure Pair (Left, Right : in Person_Name := new Person (M)) is null;   --  see 3.10.1
         X, Y : Process_Name;

         --  Examples of their calls:

      begin
         Activate (X);
         Activate (X, After => Y);
         Activate (X, Wait => 60.0, Prior => True);
         Activate (X, Y, 10.0, False);

         Pair;
         Pair (Left => new Person (F), Right => new Person (M));
      end;
   end Section_6_4_Paragraph_22;

   procedure Section_6_4_Paragraph_26 is
      use Needed_To_Compile;
      type Light is (Red, Green, Orange);

      --  Examples of overloaded subprograms:

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
      --  Set(Red) would be ambiguous since Red may
      --  denote a value either of type Color or of type Light
   end Section_6_4_Paragraph_26;

   --  6.4.1 Parameter Associations

   procedure Section_6_4_1_Paragraph_5c is
      procedure Print (X : in Integer; Y : in Boolean := True) is null;
      procedure Print (Z : in out Integer) is null;
      W : Integer;
   begin
      -- Print(3); -- Ambiguous!
      Print (X => 3); -- Not ambigous
   end Section_6_4_1_Paragraph_5c;

   procedure Section_6_4_1_Paragraph_6i_6l is
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
      X   : Some_Type renames Ptr.all;
   begin
      Ptr := new Some_Type'(20);
      P (Func_With_Out_Params (Ptr.all), X);
   end Section_6_4_1_Paragraph_6i_6l;

   --  6.5 Return Statements

   procedure Section_6_5_Paragraph_21g is
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
   end Section_6_5_Paragraph_21g;

   --       Examples of return statements:
   --
   --   return;                         -- in a procedure body, entry_body,
   --                                          -- accept_statement
   --          , or extended_return_statement

   --       return Key_Value(Last_Index);   -- in a function body
   --
   --   return Node : Cell do           -- in a function body, see 3.10.1
   --           for Cell
   --             Node.Value := Result;
   --             Node.Succ := Next_Node;
   --          end return;

   --  6.5.1 Nonreturning Procedures

   procedure Fail
     (Msg : String)  -- raises Fatal_Error exception
   with
      No_Return;

   procedure Fail (Msg : String) is
      Fatal_Error : exception;
   begin
      raise Fatal_Error;
   end Fail;

   --  6.6 Overloading of Operators

   --   Examples of user-defined operators:

   --     function "+" (Left, Right : Matrix) return Matrix;
   --     function "+" (Left, Right : Vector) return Vector;
   --
   --     --  assuming that A, B, and C are of the type Vector
   --     --  the following two statements are equivalent:
   --
   --     A := B + C;
   --     A := "+"(B, C);

   --  6.7 Null Procedures

   package Section_6_7_Paragraph_6 is
      type Expression is tagged null record;
      procedure Simplify (Expr : in out Expression) is null; -- see 3.9
      -- By default, Simplify does nothing, but it may be overridden in extensions of Expression
   end Section_6_7_Paragraph_6;

   --  6.8 Expression Functions

   package Section_6_8_Paragraph_9 is
      type Real is digits 8;
      type Point is tagged record
         X, Y : Real := 0.0;
      end record;
      function Is_Origin (P : in Point) return Boolean is -- see 3.9
        (P.X = 0.0 and P.Y = 0.0);
   end Section_6_8_Paragraph_9;

begin
   null;
end AARM_202x_CH06;
