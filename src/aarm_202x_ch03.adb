with System;
with Ada.Numerics; use Ada.Numerics;

procedure AARM_202x_CH03 is

   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      K : constant           := 9;
      G : constant Character := '@';
      type Coordinate is (X, Y);
      type Real is digits 8;
      type Matrix is array (Integer range <>, Integer range <>) of Real;
      type Month_Name is
        (January, February, March, April, May, June, July, August, September, October, November, December);
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
      type Binop_Ptr is access all Integer;
      type Bit_Vector is array (Integer range <>) of Boolean;
      type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
      package Key_Manager is
         subtype Key is Integer;
      end Key_Manager;
      Max       : constant := 500;                   -- an integer number
      Page_Size : constant := 100;
      subtype Cylinder_Index is Natural;
      subtype Track_Number is Natural;
      function Dispersion (X : Real) return Real;
   end Needed_To_Compile;

   package body Needed_To_Compile is
      function Dispersion (X : Real) return Real is
      begin
         return e * X;
      end Dispersion;
   end Needed_To_Compile;

   --                          3   Declarations and Types

   --  3.1 Declarations

   --  3.2 Types and Subtypes

   --  3.2.1 Type Declarations

   package Section_3_2_1_Paragraph_15 is
      type Color is (White, Red, Yellow, Green, Blue, Brown, Black);
      type Column is range 1 .. 72;
      type Table is array (1 .. 10) of Integer;
   end Section_3_2_1_Paragraph_15;

   --  3.2.2 Subtype Declarations

   package Section_3_2_2_Paragraph_15 is
      use Needed_To_Compile, Section_3_2_1_Paragraph_15;
      subtype Rainbow is Color range Red .. Blue;        --  see 3.2.1
      subtype Red_Blue is Rainbow;
      subtype Int is Integer;
      subtype Small_Int is Integer range -10 .. 10;
      subtype Up_To_K is Column range 1 .. K;            --  see 3.2.1
      subtype Square is Matrix (1 .. 10, 1 .. 10);       --  see 3.6
      subtype Male is Person (Sex => M);               --  see 3.10.1
      subtype Binop_Ref is not null Binop_Ptr;             --  see 3.10
   end Section_3_2_2_Paragraph_15;

   --  3.2.3 Classification of Operations

   --  3.2.4 Subtype Predicates

   package Section_3_2_4_Paragraph_39 is
      subtype Basic_Letter is
        Character -- See A.3.2 for "basic letter".
      with
           Static_Predicate => Basic_Letter in 'A' .. 'Z' | 'a' .. 'z' | 'Æ' | 'æ' | 'Ð' | 'ð' | 'Þ' | 'þ' | 'ß';

      subtype Even_Integer is Integer with
           Dynamic_Predicate => Even_Integer mod 2 = 0,
           Predicate_Failure => "Even_Integer must be a multiple of 2";
   end Section_3_2_4_Paragraph_39;

   --  3.3 Objects and Named Numbers

   --  3.3.1 Object Declarations

   package Section_3_3_1_Paragraph_20 is
      use Needed_To_Compile;
      function F (X : Natural := 0) return Natural;

      type R (D : Integer := F) is record
         S : String (1 .. D) := (others => G);
      end record;
      X2 : R;

      type T (<>);
      type T1 (P : access T) is null record;
      type T (D : Natural) is limited record
         C1 : T1 (T'Access);
         C2 : Natural         := F (D);
         C3 : String (1 .. D) := (others => ' ');
      end record;
   end Section_3_3_1_Paragraph_20;

   package body Section_3_3_1_Paragraph_20 is
      -- Needed to compile, sometimes dummy
      function F (X : Natural := 0) return Natural is
      begin
         return X;
      end F;
   end Section_3_3_1_Paragraph_20;

   package Section_3_3_1_Paragraph_25 is
      use Needed_To_Compile, Section_3_2_1_Paragraph_15;
      --  the multiple object declaration
      John, Paul : not null Person_Name := new Person (Sex => M);  --  see 3.10.1
      --  is equivalent to the two single object declarations in the order given
      John2 : not null Person_Name := new Person (Sex => M);
      Paul2 : not null Person_Name := new Person (Sex => M);

      -- Examples of variable declarations:
      Count, Sum  : Integer;
      Size        : Integer range 0 .. 10_000 := 0;
      Sorted      : Boolean                   := False;
      Color_Table : array (1 .. Max) of Color;
      Option      : Bit_Vector (1 .. 10)      := (others => True);
      Hello       : aliased String            := "Hi, world.";
      T, P        : Float range -Pi .. +Pi;

      -- Examples of constant declarations:
      Limit     : constant Integer       := 10_000;
      Low_Limit : constant Integer       := Limit / 10;
      Tolerance : constant Real          := Dispersion (1.15);
      Hello_Msg : constant access String := Hello'Access; -- see 3.10.2
   end Section_3_3_1_Paragraph_25;

   --  3.3.2 Number Declarations

   package Section_3_3_2_Paragraph_9 is
      use Needed_To_Compile;
      Two_Pi        : constant := 2.0 * Ada.Numerics.Pi;   -- a real number (see A.5)
      Max2          : constant := 500;                   -- an integer number
      Max_Line_Size : constant := Max / 6;                 -- the integer 83
      Power_16      : constant := 2**16;                 -- the integer 65_536
      One, Un, Eins : constant := 1;                     -- three different names for 1
   end Section_3_3_2_Paragraph_9;

   --  3.4 Derived Types and Classes

   package Section_3_4_Paragraph_34 is
      use Needed_To_Compile;
      type T1 is range 1 .. 100;
      subtype S1 is T1 range 1 .. 10;
      procedure P (X : in S1) is null;  -- P is a primitive subprogram
      type T2 is new T1 range 11 .. 20;
      -- implicitly declared:
      -- procedure P(X : in T2'Base range 1..10);
      --      X cannot be in T2'First .. T2'Last
   end Section_3_4_Paragraph_34;

   package Section_3_4_Paragraph_37 is
      use Needed_To_Compile;
      type Local_Coordinate is new Coordinate;   --  two different types
      type Midweek is new Day range Tue .. Thu;  --  see 3.5.1
      type Counter is new Positive;              --  same range as Positive

      type Special_Key is new Key_Manager.Key;   --  see 7.3.1
      -- the inherited subprograms have the following specifications:
      --         procedure Get_Key(K : out Special_Key);
      --         function "<"(X,Y : Special_Key) return Boolean;

   end Section_3_4_Paragraph_37;

   procedure Section_3_4_Paragraph_38c is
      package P is
         type T is (A, B, C, D);
         function F (X : T := A) return Integer;
         type NT is new T;
         -- inherits F as
         -- function F( X : NT := A ) return Integer;
         -- in Ada 95 only
         --@ ...
      end P;

      package body P is
         -- Needed to compile, sometimes dummy
         function F (X : T := A) return Integer is
         begin
            return T'Pos (X);
         end F;
      end P;
      --@ ...
      use P;   -- Only one declaration of F from P is use-visible in Ada 83;
      --  two declarations of F are use-visible in Ada 95.
   begin
      --@ ...
      --@      if F > 1 then ... -- legal in Ada 83, ambiguous in Ada 95
      null;
      --@      end if;
   end Section_3_4_Paragraph_38c;

   --  3.4.1 Derivation Classes

   --  3.5 Scalar Types

   --  Examples of ranges:
   --
   --          -10 .. 10
   --          X .. X + 1
   --          0.0 .. 2.0*Pi
   --          Red .. Green     -- see 3.5.1
   --          1 .. 0           -- a null range
   --          Table'Range      -- a range attribute reference (see 3.6)
   --
   --  Examples of range constraints:
   --
   --          range -999.0 .. +999.0
   --          range S'First+1 .. S'Last-1

   --  3.5.1 Enumeration Types

   package Section_3_5_1_Paragraph_14 is
      use Needed_To_Compile;
      type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
      type Month_Name is
        (January, February, March, April, May, June, July, August, September, October, November, December);
      type Suit is (Clubs, Diamonds, Hearts, Spades);
      type Gender is (M, F);
      type Level is (Low, Medium, Urgent);
      type Color is (White, Red, Yellow, Green, Blue, Brown, Black);
      type Light is (Red, Amber, Green); -- Red and Green are overloaded

      type Hexa is ('A', 'B', 'C', 'D', 'E', 'F');
      type Mixed is ('A', 'B', '*', B, None, '?', '%');

      subtype Weekday is Day range Mon .. Fri;
      subtype Major is Suit range Hearts .. Spades;
      subtype Rainbow is Color range Red .. Blue;  --  the Color Red, not the Light
   end Section_3_5_1_Paragraph_14;

   --  3.5.2 Character Types

   package Section_3_5_2_Paragraph_11 is
      type Roman_Digit is ('I', 'V', 'X', 'L', 'C', 'D', 'M');
   end Section_3_5_2_Paragraph_11;

   --  3.5.3 Boolean Types

   --  3.5.4 Integer Types

   package Section_3_5_4_Paragraph_34 is
      use Section_3_3_2_Paragraph_9, Needed_To_Compile;

      type Page_Num is range 1 .. 2_000;
      type Line_Size is range 1 .. Max_Line_Size;

      subtype Small_Int is Integer range -10 .. 10;
      subtype Column_Ptr is Line_Size range 1 .. 10;
      subtype Buffer_Size is Integer range 0 .. Max;

      type Byte is mod 256; -- an unsigned byte
      type Hash_Index is mod 97;  -- modulus is prime
   end Section_3_5_4_Paragraph_34;

   --  3.5.5 Operations of Discrete Types

   --  For the types and subtypes declared in subclause 3.5.1 the following hold:

   --  Color'First   = White,   Color'Last   = Black
   --  Rainbow'First = Red,     Rainbow'Last = Blue

   --  Color'Succ(Blue) = Rainbow'Succ(Blue) = Brown
   --  Color'Pos(Blue)  = Rainbow'Pos(Blue)  = 4
   --  Color'Val(0)     = Rainbow'Val(0)     = White

   --  3.5.6 Real Types

   --  3.5.7 Floating Point Types

   package Section_3_5_7_Paragraph_28 is
      type Coefficient is digits 10 range -1.0 .. 1.0;
      type Real is digits 8;
      type Mass is digits 7 range 0.0 .. 1.0E35;
      subtype Probability is Real range 0.0 .. 1.0;   --   a subtype with a smaller range
   end Section_3_5_7_Paragraph_28;

   --  3.5.8 Operations of Floating Point Types

   --  3.5.9 Fixed Point Types

   package Section_3_5_9_Paragraph_18c is
      type D is delta 0.01 digits 6 range -0.00 .. 9999.99;
   end Section_3_5_9_Paragraph_18c;

   package Section_3_5_9_Paragraph_23 is
      type Fraction is delta 2.0**(-15) range -1.0 .. 1.0;
   end Section_3_5_9_Paragraph_23;

   package Section_3_5_9_Paragraph_26 is
      type Volt is delta 0.125 range 0.0 .. 255.0;

      -- A pure fraction which requires all the available
      -- space in a word can be declared as the type Fraction:
      type Fraction is delta System.Fine_Delta range -1.0 .. 1.0;
      -- Fraction'Last = 1.0 - System.Fine_Delta

      type Money is delta 0.01 digits 15;  -- decimal fixed point
      subtype Salary is Money digits 10;
      -- Money'Last = 10.0**13 - 0.01, Salary'Last = 10.0**8 - 0.01
   end Section_3_5_9_Paragraph_26;

   --  3.5.10 Operations of Fixed Point Types

   --  3.6 Array Types

   package Section_3_6_Paragraph_25 is
      use Section_3_5_2_Paragraph_11, Section_3_3_2_Paragraph_9, Section_3_5_7_Paragraph_28, Section_3_5_1_Paragraph_14;
      type Error_Code is (Too_Big, Too_Small);
      --  Examples of type declarations with unconstrained array definitions:

      type Vector is array (Integer range <>) of Real;
      type Matrix is array (Integer range <>, Integer range <>) of Real;
      type Bit_Vector is array (Integer range <>) of Boolean;
      type Roman is array (Positive range <>) of Roman_Digit; -- see 3.5.2

      --  Examples of type declarations with constrained array definitions:

      type Table is array (1 .. 10) of Integer;
      type Schedule is array (Day) of Boolean;
      type Line is array (1 .. Max_Line_Size) of Character;

      --  Examples of object declarations with array type definitions:

      Grid      : array (1 .. 80, 1 .. 100) of Boolean;
      Mix       : array (Color range Red .. Green) of Boolean;
      Msg_Table : constant array (Error_Code) of access constant String :=
        (Too_Big => new String'("Result too big"), Too_Small => new String'("Result too small"));
      Page : array (Positive range <>) of Line :=  --  an array of arrays
        (1 | 50 => Line'(1 | Line'Last => '+', others => '-'),  -- see 4.3.3

         2 .. 49 => Line'(1 | Line'Last => '|', others => ' '));
      -- Page is constrained by its initial value to (1..50)
   end Section_3_6_Paragraph_25;

   --  3.6.1 Index Constraints and Discrete Ranges

   package Section_3_6_1_Paragraph_11 is
      use Section_3_6_Paragraph_25;
      N : constant := 99;
      --  Examples of array declarations including an index constraint:

      Board     : Matrix (1 .. 8, 1 .. 8);  --  see 3.6
      Rectangle : Matrix (1 .. 20, 1 .. 30);
      Inverse   : Matrix (1 .. N, 1 .. N);  --  N need not be static

      Filter : Bit_Vector (0 .. 31);

      --  Example of array declaration with a constrained array subtype:

      My_Schedule : Schedule;  --  all arrays of type Schedule have the same bounds

      --  Example of record type with a component that is an array:

      type Var_Line (Length : Natural) is record
         Image : String (1 .. Length);
      end record;

      Null_Line : Var_Line (0);  --  Null_Line.Image is a null array
   end Section_3_6_1_Paragraph_11;

   --  Section_3_6_1_Paragraph_18
   --  Filter'First      =   0   Filter'Last       =  31   Filter'Length =  32
   --  Rectangle'Last(1) =  20   Rectangle'Last(2) =  30

   --  3.6.2 Operations of Array Types

   -- Examples (using arrays declared in the examples of subclause 3.6.1):

   --  Filter'First      =   0   Filter'Last       =  31   Filter'Length =  32
   --  Rectangle'Last(1) =  20   Rectangle'Last(2) =  30

   --  3.6.3 String Types

   package Section_3_6_3_Paragraph_7 is
      use Section_3_6_Paragraph_25;
      Stars    : String (1 .. 120) := (1 .. 120 => '*');
      Question : constant String   := "How many characters?";

      -- Question'First = 1, Question'Last = 20

      -- Question'Length = 20 (the number of characters)

      Ask_Twice : String := Question & Question;
      -- constrained to (1..40)
      Ninety_Six : constant Roman := "XCVI";
      -- see 3.5.2 and 3.6
   end Section_3_6_3_Paragraph_7;

   --  3.7 Discriminants

   package Section_3_7_Paragraph_33 is
      use Section_3_5_4_Paragraph_34, Section_3_6_Paragraph_25;
      type Buffer
        (Size : Buffer_Size := 100)
      is        -- see 3.5.4
      record
         Pos   : Buffer_Size := 0;
         Value : String (1 .. Size);
      end record;

      type Matrix_Rec (Rows, Columns : Integer) is record
         Mat : Matrix (1 .. Rows, 1 .. Columns);       -- see 3.6
      end record;

      type Square (Side : Integer) is new Matrix_Rec (Rows => Side, Columns => Side);

      type Double_Square (Number : Integer) is record
         Left  : Square (Number);
         Right : Square (Number);
      end record;

      task type Worker (Prio : System.Priority; Buf : access Buffer)
         --     with Priority => Prio is -- see D.1
         -- discriminants used to parameterize the task type (see 9.1)
         is
         pragma Priority (Prio);  -- see D.1
         entry Fill;
         entry Drain;
      end Worker;
   end Section_3_7_Paragraph_33;

   package body Section_3_7_Paragraph_33 is
      -- Needed to compile, sometimes dummy
      task body Worker is
      begin
         accept Fill;
         accept Drain;
      end Worker;
   end Section_3_7_Paragraph_33;

   --  3.7.1 Discriminant Constraints

   package Section_3_7_1_Paragraph_15 is
      use Section_3_7_Paragraph_33;
      Large : Buffer (200);  --  constrained, always 200 characters
      --   (explicit discriminant value)
      Message : Buffer;       --  unconstrained, initially 100 characters
      --   (default discriminant value)
      Basis : Square (5);    --  constrained, always 5 by 5
      -- Illegal : Square;       --  illegal, a Square has to be constrained
   end Section_3_7_1_Paragraph_15;

   --  3.7.2 Operations of Discriminated Types

   --  3.8 Record Types

   package Section_3_8_Paragraph_27 is
      use Needed_To_Compile;

      type Date is record
         Day   : Integer range 1 .. 31;
         Month : Month_Name;
         Year  : Integer range 0 .. 4000;
      end record;

      type Complex is record
         Re : Real := 0.0;
         Im : Real := 0.0;
      end record; -- Complex; --@@ MODIF PP: not yet available

      --  Examples of record variables:

      Tomorrow, Yesterday : Date;
      A, B, C             : Complex;

      -- both components of A, B, and C are implicitly initialized to zero
   end Section_3_8_Paragraph_27;

   --  3.8.1 Variant Parts and Discrete Choices

   package Section_3_8_1_Paragraph_15b is
      generic
         type T is new Integer;
      package G is
         type Rec (Discrim : T) is record
            case Discrim is
               when -10 .. -1 =>
                  Foo : Float;
               when others =>
                  null;
            end case;
         end record;
      end G;
      package I is new G (Natural); -- Legal
   end Section_3_8_1_Paragraph_15b;

   package Section_3_8_1_Paragraph_23 is
      use Needed_To_Compile;
      --  Example of record type with a variant part:
      type Device is (Printer, Disk, Drum);
      type State is (Open, Closed);

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

      --  Examples of record subtypes:

      subtype Drum_Unit is Peripheral (Drum);
      subtype Disk_Unit is Peripheral (Disk);

      --  Examples of constrained record variables:

      Writer  : Peripheral (Unit => Printer);
      Archive : Disk_Unit;
   end Section_3_8_1_Paragraph_23;

   --  3.9 Tagged Types and Type Extensions

   package Section_3_9_Paragraph_32 is
      use Section_3_5_7_Paragraph_28;
      type Point is tagged record
         X, Y : Real := 0.0;
      end record;

      type Expression is tagged null record;
      -- Components will be added by each extension
   end Section_3_9_Paragraph_32;

   --  3.9.1 Type Extensions

   package Section_3_9_1_Paragraph_4d is
      package P is
         type T is tagged null record;
         function F return T; -- Inherited versions will require overriding.
      end P;
      generic
         type TT is tagged private;
      package Gp is
         type NT is abstract new TT with null record;
         procedure Q (X : in NT) is abstract;
      end Gp;
   end Section_3_9_1_Paragraph_4d;

   package body Section_3_9_1_Paragraph_4d is

      package body Gp is
         --@  type NT2 is new NT with null record; -- Illegal!
         --@  procedure Q(X : in NT2) is begin null; end Q;
      -- Is this legal or not? Can't decide because
      -- we don't know whether TT had any functions that require
      -- overriding on extension.
      end Gp;

      package I is new Gp (TT => P.T);

      package body P is
         -- Needed to compile, sometimes dummy
         function F return T is
            V : T;
         begin
            return V;
         end F;
      end P;
   end Section_3_9_1_Paragraph_4d;

   package Section_3_9_1_Paragraph_11 is
      use Section_3_9_Paragraph_32, Section_3_5_7_Paragraph_28, Section_3_5_1_Paragraph_14;
      type Painted_Point is new Point with record
         Paint : Color := White;
      end record;
      -- Components X and Y are inherited

      Origin : constant Painted_Point := (X | Y => 0.0, Paint => Black);

      type Literal is new Expression with record                 -- a leaf in an Expression tree
         Value : Real;
      end record;

      type Expr_Ptr is access all Expression'Class;
      -- see 3.10

      type Binary_Operation is new Expression with record        -- an internal node in an Expression tree
         Left, Right : Expr_Ptr;
      end record;

      type Addition is new Binary_Operation with null record;
      type Subtraction is new Binary_Operation with null record;
      -- No additional components needed for these extensions

      Tree : Expr_Ptr :=         -- A tree representation of "5.0 + (13.0-7.0)"
        new Addition'
          (Left  => new Literal'(Value => 5.0),
           Right => new Subtraction'(Left => new Literal'(Value => 13.0), Right => new Literal'(Value => 7.0)));
   end Section_3_9_1_Paragraph_11;

   --  3.9.2 Dispatching Operations of Tagged Types

   procedure Section_3_9_2_Paragraph_20c is
      package P1 is
         type T1 is tagged null record;
         procedure Op_A (Arg : in T1) is null;
         procedure Op_B (Arg : in T1) is null;
      end P1;

      -- with P1;
      use P1;
      package P2 is
         type T2 is new T1 with null record;
         procedure Op_A (Param : in T2) is null;
      private
         procedure Op_B (Param : in T2) is null;
      end P2;
      -- with P1; with P2;
      --        procedure Main is
      X : P2.T2;
      Y : P1.T1'Class := X;
   begin
      P2.Op_A (Param => X); -- Nondispatching call to a dispatching operation.
      P1.Op_A (Arg => Y); -- Dispatching call.
      P2.Op_B (Arg => X); -- Nondispatching call to a dispatching operation.
      P1.Op_B (Arg => Y); -- Dispatching call.
   end Section_3_9_2_Paragraph_20c;

   --  3.9.3 Abstract Types and Subprograms

   -- Section_3_9_3_Paragraph_3b
   --     package P is
   --type T is abstract tagged private;
   --function Foo (X : T) return Boolean is abstract; -- Illegal!
   --     private
   --type T is tagged null record; -- Illegal!
   --X : T;
   --Y : Boolean := Foo (T'Class (X));
   --     end ;

   package Section_3_9_3_Paragraph_3e is
      package P is
         type Field_Size is range 0 .. 100;
         type T is abstract tagged null record;
         procedure Print (X : in T; F : in Field_Size := 0) is abstract;
         --. . .
      end P;
      package Q is
         type My_Field_Size is new P.Field_Size;
         -- implicit declaration of Print(X : T; F : My_Field_Size := 0) is abstract;
      end Q;
   end Section_3_9_3_Paragraph_3e;

   package Section_3_9_3_Paragraph_4b is
      package P is
         type I is interface;
         procedure Op (X : I) is abstract;
      end P;

      -- with P;
      package Q is
         type T is abstract new P.I with private;
         -- Op inherited here.
      private
         type T is abstract new P.I with null record;
         procedure Op (X : T) is null;
      end Q;

      -- with Q;
      package R is
      --type T2 is new Q.T with null record;
      -- Illegal. Op inherited here, but requires overriding.
      end R;
   end Section_3_9_3_Paragraph_4b;

   package Section_3_9_3_Paragraph_6c is
      package Pack1 is
         type Ancestor is abstract tagged null record;
         procedure Do_Something (X : in Ancestor) is abstract;
      end Pack1;

      --with Pack1;
      use Pack1;
      package Pack2 is
         type T1 is new Ancestor with null record;
         -- A concrete type.
         procedure Do_Something (X : in T1) is null; -- Have to override.
      end Pack2;

      --with Pack1;
      use Pack1;
      --with Pack2;
      use Pack2;
      package Pack3 is
         type T2 is new Ancestor with private;
         -- A concrete type.
      private
         type T2 is new T1 with -- Parent different from ancestor.
         null record;
         -- Here, we inherit Pack2.Do_Something.
      end Pack3;
   end Section_3_9_3_Paragraph_6c;

   package Section_3_9_3_Paragraph_10d is
      package P1 is
         type T1 is abstract tagged null record;
         procedure P (X : T1) is null; -- (1)
      end P1;

      package P2 is
         type T2 is abstract new P1.T1 with null record;
         -- procedure P (X : T2); -- (2)
         procedure P (X : T2) is abstract; -- (3)
      end P2;

      generic
         type D is abstract new P1.T1 with private;
         -- procedure P (X : D); -- (4)
      procedure G (X : D);

      --@ procedure I is new G (P2.T2); -- Illegal.

      --      Type T2 inherits a nonabstract procedure P (2) from the primitive
      --          procedure P (1) of T1. P (2) is overridden by the explicitly
      --        declared abstract procedure P (3). Type D inherits a nonabstract
      --      procedure P (4) from P (1). In instantiation I, the operation
      --    corresponding to P (4) is the one which is not overridden, that
      --  is, P (3): the overridden operation P (2) does not "reemerge".
      -- Therefore, the instantiation is illegal.

      use P1, P2;
      package Pack is
         type T is abstract new T1 with private;
      private
         type T is abstract new T2 with record
            Dum : Integer;
         end record;
      end Pack;
   end Section_3_9_3_Paragraph_10d;

   package body Section_3_9_3_Paragraph_10d is
      -- Needed to compile, sometimes dummy
      procedure G (X : D) is
      begin
         null;
      end G;
   end Section_3_9_3_Paragraph_10d;

   package Section_3_9_3_Paragraph_15 is
      package Sets is
         subtype Element_Type is Natural;
         type Set is abstract tagged null record;
         function Empty return Set is abstract;
         function Union (Left, Right : Set) return Set is abstract;
         function Intersection (Left, Right : Set) return Set is abstract;
         function Unit_Set (Element : Element_Type) return Set is abstract;
         procedure Take (Element : out Element_Type; From : in out Set) is abstract;
      end Sets;
   end Section_3_9_3_Paragraph_15;

   package Section_3_9_3_Paragraph_17b is
      package P is
         type Public_Part is abstract tagged record
            Dum1 : Integer;
         end record;
         type T is new Public_Part with private;
         -- ...
      private
         type T is new Public_Part with record
            Dum2 : Integer;
         end record;
      end P;
   end Section_3_9_3_Paragraph_17b;

   --  3.9.4 Interface Types

   package Section_3_9_4_Paragraph_20 is
      use Needed_To_Compile;

      -- Example of a limited interface and a synchronized interface extending it:

      package Queues is
         type Queue is limited interface;
         procedure Append (Q : in out Queue; Person : in Person_Name) is abstract;
         procedure Remove_First (Q : in out Queue; Person : out Person_Name) is abstract;
         function Cur_Count (Q : in Queue) return Natural is abstract;
         function Max_Count (Q : in Queue) return Natural is abstract;
         -- See 3.10.1 for Person_Name.
         procedure Transfer (From : in out Queue'Class; To : in out Queue'Class; Number : in Natural := 1);

         Queue_Error : exception;
         -- Append raises Queue_Error if Cur_Count(Q) = Max_Count(Q)
         -- Remove_First raises Queue_Error if Cur_Count(Q) = 0

         type Synchronized_Queue is synchronized interface and Queue; -- see 9.11
         procedure Append_Wait (Q : in out Synchronized_Queue; Person : in Person_Name) is abstract;
         procedure Remove_First_Wait (Q : in out Synchronized_Queue; Person : out Person_Name) is abstract;
      end Queues;
      --   ...
   end Section_3_9_4_Paragraph_20;

   package body Section_3_9_4_Paragraph_20 is
      package body Queues is

         procedure Transfer (From : in out Queue'Class; To : in out Queue'Class; Number : in Natural := 1) is
            Person : Person_Name;
         begin
            for I in 1 .. Number loop
               Remove_First (From, Person);
               Append (To, Person);
            end loop;
         end Transfer;
      end Queues;
   end Section_3_9_4_Paragraph_20;

   procedure Section_3_9_4_Paragraph_28 is
      use Section_3_9_4_Paragraph_20.Queues, Needed_To_Compile;

      -- Example use of the interface:

      type Fast_Food_Queue is new Queue with null record;
      procedure Append (Q : in out Fast_Food_Queue; Person : in Person_Name);
      procedure Remove_First (Q : in out Fast_Food_Queue; Person : out Person_Name);
      function Cur_Count (Q : in Fast_Food_Queue) return Natural;
      function Max_Count (Q : in Fast_Food_Queue) return Natural;

      procedure Append (Q : in out Fast_Food_Queue; Person : in Person_Name) is
      begin
         null;
      end Append;
      procedure Remove_First (Q : in out Fast_Food_Queue; Person : out Person_Name) is
      begin
         null;
      end Remove_First;
      function Cur_Count (Q : in Fast_Food_Queue) return Natural is
      begin
         return 0;
      end Cur_Count;
      function Max_Count (Q : in Fast_Food_Queue) return Natural is
      begin
         return 0;
      end Max_Count;

      Casey : Person_Name := new Person (M);

      --    ...

      -- Example of a task interface:

      type Serial_Device is task interface;  -- see 9.1
      procedure Read (Dev : in Serial_Device; C : out Character) is abstract;
      procedure Write (Dev : in Serial_Device; C : in Character) is abstract;

      Cashier, Counter : Fast_Food_Queue;

   begin
      --    {AI12-0312-1} ...
      -- Add Casey (see 3.10.1) to the cashier's queue:
      Append (Cashier, Casey);
      -- After payment, move Casey to the sandwich counter queue:
      Transfer (Cashier, Counter);
      --     ...
   end Section_3_9_4_Paragraph_28;

   --  3.10 Access Types

   package Section_3_10_Paragraph_9i is
      type T1 is tagged null record;
      procedure P (X : access T1) is null;

      type T2 is new T1 with null record;
      procedure P (X : access T2);
   end Section_3_10_Paragraph_9i;

   package body Section_3_10_Paragraph_9i is
      procedure P (X : access T2) is
      begin
         P (T1 (X.all)'Access);  -- hand off to T1's P
         --. . .     -- now do extra T2-specific processing
      end P;
   end Section_3_10_Paragraph_9i;

   procedure Section_3_10_Paragraph_21 is
      use Section_3_6_Paragraph_25, Section_3_8_1_Paragraph_23, Section_3_9_1_Paragraph_11;
      --  Examples of access-to-object types:
      type Frame is access Matrix;    --  see 3.6
      type Peripheral_Ref is not null access Peripheral;  --  see 3.8.1
      type Binop_Ptr is access all Binary_Operation'Class;
      -- general access-to-class-wide, see 3.9.1

      --  Example of an access subtype:

      subtype Drum_Ref is Peripheral_Ref (Drum);  --  see 3.8.1

      --  Example of an access-to-subprogram type:

      type Message_Procedure is access procedure (M : in String := "Error!");
      procedure Default_Message_Procedure (M : in String);
      Give_Message : Message_Procedure := Default_Message_Procedure'Access;
      --        ...
      procedure Other_Procedure (M : in String);
      --       ...

      procedure Default_Message_Procedure (M : in String) is
      begin
         null;
      end Default_Message_Procedure;
      procedure Other_Procedure (M : in String) is
      begin
         null;
      end Other_Procedure;

   begin
      Give_Message := Other_Procedure'Access;
      --        ...
      Give_Message ("File not found.");  -- call with parameter (.all is optional)
      Give_Message.all;                 -- call with no parameters
   end Section_3_10_Paragraph_21;

   --  3.10.1 Incomplete Type Declarations

   --@@ MODIF01 PP           package Section_3_10_1_Paragraph_2c is
   --
   --             package P is
   --                  private
   --                     type T;
   --                     type PtrT is access T;
   --                  end P;
   --
   --           private package P.C is
   --                     Ptr : PtrT;
   --                  end P.C;
   --
   --           with P.C;
   --                  package body P is
   --                      -- Ptr.all'Size is not legal here, but we are within the scope
   --                      -- of a nonlimited_with_clause for P.
   --                  type T is Integer;
   --                      --  Ptr.all'Size is legal here.
   --                  end P;
   --     end;

   package Section_3_10_1_Paragraph_10f is
      package Pack is
         type T is tagged;
         function F return access T'Class;
         function G (X : access T) return Integer;
         --@  I : Integer := G (F);                 -- Illegal by above rule.
         type T is tagged null record;
      end Pack;
   end Section_3_10_1_Paragraph_10f;

   package body Section_3_10_1_Paragraph_10f is
      -- Needed to compile, sometimes dummy
      package body Pack is
         function F return access T'Class is
         begin
            return new T;
         end F;
         function G (X : access T) return Integer is
         begin
            return 0;
         end G;
      end Pack;
   end Section_3_10_1_Paragraph_10f;

   package Section_3_10_1_Paragraph_14 is
      use Section_3_8_Paragraph_27, Section_3_5_1_Paragraph_14;
      --  Example of a recursive type:

      type Cell;  --  incomplete type declaration
      type Link is access Cell;

      type Cell is record
         Value : Integer;
         Succ  : Link;
         Pred  : Link;
      end record;

      Head : Link := new Cell'(0, null, null);
      Next : Link := Head.Succ;

      --  Examples of mutually dependent access types:

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
      --   {AI12-0312-1}
      My_Car, Your_Car, Next_Car : Car_Name    := new Car;  -- see 4.8
      Casey                      : Person_Name := new Person (M);
      --             ...
   end Section_3_10_1_Paragraph_14;

   procedure Section_3_10_1_Paragraph_23 is
      use Section_3_10_1_Paragraph_14;
   begin
      Casey.Vehicle := Your_Car;
   end Section_3_10_1_Paragraph_23;

   package Section_3_10_1_Paragraph_23d is
      package Pack is
         type Pri is private;
      private
         type Sep;
         type Pri is access Sep;
         X : Pri;
      end Pack;
   end Section_3_10_1_Paragraph_23d;

   package body Section_3_10_1_Paragraph_23d is
      use Section_3_8_Paragraph_27;
      package body Pack is -- Could be separately compiled!
         type Sep is access Date;
         I : Integer := X.all'Size; -- Legal, by AI-00039.
      begin
         X := new Sep;
      end Pack;

      --              pragma Elaborate(Pack);
      --                   package Pack.Child is
      --                      I : Integer := X.all'Size; -- Legal, by AI-00039.
      --                  end Pack.Child;
   end Section_3_10_1_Paragraph_23d;

   --  3.10.2 Operations of Access Types

   package Section_3_10_2_Paragraph_2c is
      type Int_Ptr is access all Integer;
      type Char_Ptr is access all Character;
      type Float_Ptr is access all Float;

      function Zap (Val : Int_Ptr) return Float;   -- (1)
      function Zap (Val : Float_Ptr) return Float; -- (2)
      function Zop return Int_Ptr;  -- (3)
      function Zop return Char_Ptr; -- (4)

      Result : Float := Zap (Zop.all'Access); -- Resolves to Zap (1) and Zop (3).
   end Section_3_10_2_Paragraph_2c;

   package body Section_3_10_2_Paragraph_2c is
      -- Needed to compile, sometimes dummy
      function Zap (Val : Int_Ptr) return Float is
      begin
         return 0.0;
      end Zap;
      function Zap (Val : Float_Ptr) return Float is
      begin
         return 0.0;
      end Zap;
      function Zop return Int_Ptr is
      begin
         return null;
      end Zop;
      function Zop return Char_Ptr is
      begin
         return null;
      end Zop;
   end Section_3_10_2_Paragraph_2c;

   package Section_3_10_2_Paragraph_22hh is
      package Lib_Unit is
      end Lib_Unit;
   end Section_3_10_2_Paragraph_22hh;

   package body Section_3_10_2_Paragraph_22hh is
      package body Lib_Unit is
         type T is tagged null record;
         type A0 is access all T;
         Global : A0 := null;
         procedure P (X : in out T) is
            Y : aliased T;
            type A1 is access all T;
            Ptr0 : A0 := Global; -- OK.
            Ptr1 : A1 := X'Access; -- OK.
         begin
            Ptr1 := Y'Access; -- OK;
            --@ Ptr0 := A0(Ptr1); -- Illegal type conversion!
            --@ Ptr0 := X'Access; -- Illegal reference to Access attribute!
            --@ Ptr0 := Y'Access; -- Illegal reference to Access attribute!
            Global := Ptr0; -- OK.
         end P;
      end Lib_Unit;
   end Section_3_10_2_Paragraph_22hh;

   procedure Section_3_10_2_Paragraph_22jj is
      --    Here's an example involving access parameters of an access-to-object type:
      type Level_1_Type is access all Integer;

      procedure P (X : access Integer) is
         type Nested_Type is access all Integer;
      begin
         --... Nested_Type(X) ... -- (1)
         --... Level_1_Type(X) ... -- (2)
         null;
      end P;

      procedure Q (X : access Integer) is
         procedure Nested (X : access Integer) is
         begin
            P (X);
         end Nested;
      begin
         Nested (X);
      end Q;

      procedure R is
         Level_2 : aliased Integer;
      begin
         Q (Level_2'Access); -- (3)
      end R;

      Level_1 : aliased Integer;
   begin
      Q (Level_1'Access); -- (4)
      R;
   end Section_3_10_2_Paragraph_22jj;

   package Section_3_10_2_Paragraph_22rr is
      package P is
         type Int_Ptr is access all Integer;
         type Rec (D : access Integer) is limited private;
      private
         type Rec_Ptr is access all Rec;
         function F (X : Rec_Ptr) return Boolean;
         function G (X : access Rec) return Boolean;
         type Rec (D : access Integer) is limited record
            --C1: Int_Ptr := Int_Ptr(D); -- Illegal!
            --C2: Rec_Ptr := Rec'Access; -- Illegal!
            --C3: Boolean := F(Rec'Access); -- Illegal!
            C4 : Boolean := G (Rec'Access);
         end record;
      end P;
   end Section_3_10_2_Paragraph_22rr;

   package body Section_3_10_2_Paragraph_22rr is
      -- Needed to compile, sometimes dummy
      package body P is
         function F (X : Rec_Ptr) return Boolean is
         begin
            return X = null;
         end F;
         function G (X : access Rec) return Boolean is
         begin
            return X = null;
         end G;
      end P;
   end Section_3_10_2_Paragraph_22rr;

   package Section_3_10_2_Paragraph_40 is
      use Section_3_10_1_Paragraph_14, Section_3_5_1_Paragraph_14;
      --  Example of use of the Access attribute:

      --    {AI12-0312-1}
      Becky : Person_Name := new Person (F);       -- see 3.10.1
      Cars  : array (1 .. 2) of aliased Car;
      --             ...
   end Section_3_10_2_Paragraph_40;

   package body Section_3_10_2_Paragraph_40 is
   begin
      Becky.Vehicle := Cars (1)'Access;
      Casey.Vehicle := Cars (2)'Access;
   end Section_3_10_2_Paragraph_40;

   package Section_3_10_2_Paragraph_41c is
      type T1 (D1 : Boolean := False) is record
         case D1 is
            when False =>
               C1 : aliased Integer;
            when True =>
               null;
         end case;
      end record;
      type Acc_Int is access all Integer;
   end Section_3_10_2_Paragraph_41c;

   procedure Section_3_10_2_Paragraph_41d is
      use Section_3_10_2_Paragraph_41c;
      A_T : aliased T1;
      --     Ptr : Acc_Int := A_T.C1'Access; -- Illegal in Ada 2005, legal in Ada 95
   begin
      A_T := (D1 => True);             -- Raised Constraint_Error in Ada 95, but does not
      -- in Ada 2005, so Ptr would become invalid when this
      -- is assigned (thus Ptr is illegal).
   end Section_3_10_2_Paragraph_41d;

   package Section_3_10_2_Paragraph_41i is
      type Int_Ptr is access all Integer;
      type Float_Ptr is access all Float;

      function Zap (Val : Int_Ptr) return Float;
      function Zap (Val : Float_Ptr) return Float;

      Value : aliased Integer := 10;

      Result1 : Float := Zap (Value'Access); -- Ambiguous in Ada 95; resolves in Ada 2005.
      Result2 : Float := Zap (Int_Ptr'(Value'Access)); -- Resolves in Ada 95 and Ada 2005.
   end Section_3_10_2_Paragraph_41i;

   package body Section_3_10_2_Paragraph_41i is
      -- Needed to compile, sometimes dummy
      function Zap (Val : Int_Ptr) return Float is
      begin
         return Float (Val.all);
      end Zap;
      function Zap (Val : Float_Ptr) return Float is
      begin
         return Val.all;
      end Zap;
   end Section_3_10_2_Paragraph_41i;

   procedure Section_3_10_2_Paragraph_41cc is
      type Rec is record
         Comp : aliased Integer;
         --                      ...
      end record;

      function F1 (A : aliased in out Rec) return access Integer is
         function F2 (B : access Integer) return access Integer is (B); -- (1)
      begin
         --           return F2 (A.Comp'Access); -- (2) --## error: cannot convert local pointer to non-local access type
         return null;
      end F1;
   begin
      null;
   end Section_3_10_2_Paragraph_41cc;

   --  3.11 Declarative Parts

begin
   null;
end AARM_202x_CH03;
