with Ada.Containers.Ordered_Maps;
with Ada.Streams;

procedure AARM_202x_CH12 is

   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      type NTCT is range 0 .. 20;
      type Real is digits 8;
      type Matrix is array (Integer range <>, Integer range <>) of Real;
      function Matrix_Product (A, B : Matrix) return Matrix is (A);
      type Color is (White, Red, Yellow, Green, Blue, Brown, Black);
   end Needed_To_Compile;
   use Needed_To_Compile;

--                             12   Generic Units

--  12.1 Generic Declarations

   package Section_12_1_Paragraph_14 is

--  Examples of generic formal parts:

      generic     --  parameterless
      procedure P1;

      generic
         Size : Natural;  --  formal object
      procedure P2;

      generic
         Length : Integer := 200;          -- formal object with a default expression
         Area : Integer := Length * Length; -- formal object with a default expression
      procedure P3;

      generic
         type Item is private;                       -- formal type
         type Index is (<>);                          -- formal type
         type Row is array (Index range <>) of Item; -- formal type
         with function "<" (X, Y : Item) return Boolean;    -- formal subprogram
      procedure P4;

--  Examples of generic declarations declaring generic subprograms Exchange and Squaring:

      generic
         type Elem is private;
      procedure Exchange (U, V : in out Elem);

      generic
         type Item (<>) is private;
         with function "*" (U, V : Item) return Item is <>;
      function Squaring (X : Item) return Item;

--  Example of a generic declaration declaring a generic package:

      generic
         type Item is private;
         type Vector is array (Positive range <>) of Item;
         with function Sum (X, Y : Item) return Item;
      package On_Vectors is
         function Sum (A, B : Vector) return Vector;
         function Sigma (A : Vector) return Item;
         Length_Error : exception;
      end On_Vectors;
   end Section_12_1_Paragraph_14;

   --  12.2 Generic Bodies

--   Example of a generic procedure body:

   package body Section_12_1_Paragraph_14 is
      -- Needed to compile, sometimes dummy
      procedure P1 is null;
      procedure P2 is null;
      procedure P3 is null;
      procedure P4 is null;

      procedure Exchange (U, V : in out Elem) is  -- see 12.1
         T : Elem;  --  the generic formal type
      begin
         T := U;
         U := V;
         V := T;
      end Exchange;

--   Example of a generic function body:

      function Squaring (X : Item) return Item is  --  see 12.1
      begin
         return X * X;  --  the formal operator "*"
      end Squaring;

--   Example of a generic package body:

      package body On_Vectors is  --  see 12.1

         function Sum (A, B : Vector) return Vector is
            Result : Vector (A'Range); --  the formal type Vector
            Bias   : constant Integer := B'First - A'First;
         begin
            if A'Length /= B'Length then
               raise Length_Error;
            end if;

            for N in A'Range loop
               Result (N) := Sum (A (N), B (N + Bias)); -- the formal function Sum
            end loop;
            return Result;
         end Sum;

         function Sigma (A : Vector) return Item is
            Total : Item := A (A'First); --  the formal type Item
         begin
            for N in A'First + 1 .. A'Last loop
               Total := Sum (Total, A (N)); --  the formal function Sum
            end loop;
            return Total;
         end Sigma;
      end On_Vectors;
   end Section_12_1_Paragraph_14;

--  12.3 Generic Instantiation

   package Section_12_3_Paragraph_11ff is

      generic
         type Parent is tagged limited private; -- Parent is limited.
         type Comp is limited private;
      package G2 is
         type Extension is new Parent with record
            C : Comp; -- OK.
         end record;
      end G2;

      type Limited_Tagged is tagged limited null record;
      type Non_Limited_Tagged is tagged null record;

      type Limited_Untagged is limited null record;
      type Non_Limited_Untagged is null record;

      package Good_1 is new G2 (Parent => Limited_Tagged, Comp => Limited_Untagged);
      package Good_2 is new G2 (Parent => Non_Limited_Tagged, Comp => Non_Limited_Untagged);
--                  package Bad  is new G2(Parent => Non_Limited_Tagged,
--                                           Comp => Limited_Untagged); -- Illegal!
   end Section_12_3_Paragraph_11ff;

   package Section_12_3_Paragraph_15h is
      type T1 is tagged record
         NTCF1 : NTCT; --@ ...
      end record;

      generic
         type Formal is new T1 with private; --@@ MODIF04 PP added with private
      package G1 is
         type Derived_From_Formal is new Formal with record
            NTCF2 : NTCT; --@ ...
         end record;
         procedure Foo (X : in Derived_From_Formal); -- Does not override anything.
      end G1;

      type T2 is new T1 with record
         NTCF3 : NTCT; --@ ...
      end record;

      procedure Foo (X : in T2);

      package Inst is new G1 (Formal => T2);
   end Section_12_3_Paragraph_15h;

   package body Section_12_3_Paragraph_15h is
      -- Needed to compile, sometimes dummy
      package body G1 is
         procedure Foo (X : in Derived_From_Formal) is null;
      end G1;
      procedure Foo (X : in T2) is null;
   end Section_12_3_Paragraph_15h;

   package Section_12_3_Paragraph_18a is
      type Ancestor is tagged null record;

      generic
         type Formal is new Ancestor with private;
      package G is
         type T is new Formal with null record;
         procedure P (X : in T); -- (1)
      private
         procedure Q (X : in T); -- (2)
      end G;

      type Actual is new Ancestor with null record;
      procedure P (X : in Actual);
      procedure Q (X : in Actual);

      package Instance is new G (Formal => Actual);
   end Section_12_3_Paragraph_18a;

   package body Section_12_3_Paragraph_18a is
      -- Needed to compile, sometimes dummy
      package body G is
         procedure P (X : in T) is null;
         procedure Q (X : in T) is null;
      end G;
      procedure P (X : in Actual) is null;
      procedure Q (X : in Actual) is null;
   end Section_12_3_Paragraph_18a;

   package Section_12_3_Paragraph_22b is
      generic
         type A is (<>);
         type B is private;
      package G1 is
         function Next (X : A) return A;
         function Next (X : B) return B;
      end G1;

      package P1 is new G1 (A => Boolean, B => Boolean);
      -- All calls of P.Next are ambiguous.

      generic
         type T1 is private;
         -- A predefined "=" operator is implicitly declared here:
         -- function "="(Left, Right : T1) return Boolean;
         -- Call this "="(1).
      package G2 is
         subtype S1 is T1; -- So we can get our hands on the type from
         -- outside an instance.
         type T2 is new T1;
         -- An inherited "=" operator is implicitly declared here:
         -- function "="(Left, Right : T2) return Boolean;
         -- Call this "="(2).

         T1_Obj : T1; --@@ ...; MODIF05 PP what value of T1?
         Bool_1 : Boolean := T1_Obj = T1_Obj;

         T2_Obj : T2; --@@ ...; MODIF05 PP what value of T2?
         Bool_2 : Boolean := T2_Obj = T2_Obj;
      end G2;
             --@   ...

      package P2 is
         type My_Int is new Integer;
         -- A predefined "=" operator is implicitly declared here:
         -- function "="(Left, Right : My_Int) return Boolean;
         -- Call this "="(3).
         function "=" (X, Y : My_Int) return Boolean is (False);
         -- Call this "="(4).
         -- "="(3) is hidden from all visibility by "="(4).
         -- Nonetheless, "="(3) can "reemerge" in certain circumstances.
      end P2;
      use P2;
                --@  ...
      package I is new G2 (T1 => My_Int); -- "="(5) is declared in I (see below).
      use I;

      Another_T1_Obj : S1      := 13; -- Can't denote T1, but S1 will do.
      Bool_3         : Boolean := Another_T1_Obj = Another_T1_Obj;

      Another_T2_Obj : T2      := 45;
      Bool_4         : Boolean := Another_T2_Obj = Another_T2_Obj;

      Double : T2 := T2_Obj + Another_T2_Obj;
   end Section_12_3_Paragraph_22b;

   package body Section_12_3_Paragraph_22b is
      -- Needed to compile, sometimes dummy
      package body G1 is
         function Next (X : A) return A is (X);
         function Next (X : B) return B is (X);
      end G1;
   end Section_12_3_Paragraph_22b;

   procedure Section_12_3_Paragraph_23 is
      use Section_12_1_Paragraph_14;
      type Table is array (Positive range <>) of Integer;
      A, B : Integer;

--  Examples of generic instantiations (see 12.1):

      procedure Swap is new Exchange (Elem => Integer);
      procedure Swap is new Exchange (Character);
      --  Swap is overloaded
      function Square is new Squaring (Integer);
      --  "*" of Integer used by default
      function Square1 is new Squaring (Item => Matrix, "*" => Matrix_Product);
      function Square2 is new Squaring (Matrix, Matrix_Product); -- same as previous

      package Int_Vectors is new On_Vectors (Integer, Table, "+");

--  Examples of uses of instantiated units:

      T : Table (1 .. 5) := (10, 20, 30, 40, 50);
      N : Integer        := Int_Vectors.Sigma (T);  --  150 (see 12.2, "Generic Bodies" for the body of Sigma)

      use Int_Vectors;
      M : Integer := Sigma (T);  --  150
   begin
      Swap (A, B);
      A := Square (A);
   end Section_12_3_Paragraph_23;

--  12.4 Formal Objects

--  12.5 Formal Types

   package Section_12_5_Paragraph_8c is
      package Q is
         type T is limited private;
      private
         type T is range 1 .. 10;
      end Q;

--@             generic
--                      type A is array (Positive range <>) of T;
--@                  package Q.G is
--                      A1, A2 : A (1 .. 1);
--                  private
--                      B : Boolean := A1 = A2;
--                  end Q.G;

         --@  with Q.G;
      package R is
         type C is array (Positive range <>) of Q.T;

--@                package I is new Q.G (C); -- Where is the predefined "=" for C?
      end R;
   end Section_12_5_Paragraph_8c;

   package Section_12_5_Paragraph_11 is

      --  Examples of generic formal types:

      generic
         type Item is private;
         type Buffer (Length : Natural) is limited private;

         type Enum is (<>);
         type Int is range <>;
         type Angle is delta <>;
         type Mass is digits <>;

         type Table is array (Enum) of Item;
      procedure P1;

--  Example of a generic formal part declaring a formal integer type:

      generic
         type Rank is range <>;
         First : Rank := Rank'First;
         Second : Rank := First + 1;  --  the operator "+" of the type Rank
      procedure P2;
   end Section_12_5_Paragraph_11;

   package body Section_12_5_Paragraph_11 is
      -- Needed to compile, sometimes dummy
      procedure P1 is null;
      procedure P2 is null;
   end Section_12_5_Paragraph_11;

--  12.5.1 Formal Private and Derived Types

   package Section_12_5_1_Paragraph_23d is
      type T is tagged null record;      -- Needed to compile, sometimes dummy
      function Empty return T;

      generic
         type NT (<>) is new T with private;
         -- Assume T has operation "function Empty return T;"
      package G is
         procedure Test (X : in out NT);
      end G;
   end Section_12_5_1_Paragraph_23d;

   package body Section_12_5_1_Paragraph_23d is
      function Empty return T is
         LT : T;
      begin
         return LT;
      end Empty;

      package body G is
         procedure Test (X : in out NT) is
         begin
            X := Empty;  -- Dispatching based on X'Tag takes
            -- place if actual is class-wide.
            declare
               Y : NT := Empty;
               -- If actual is class-wide, this raises Program_Error
               -- as there is no tag provided by context.
            begin
               X := Y;  -- We never get this far.
            end;
         end Test;
      end G;

      type T1 is new T with null record;
      package I is new G (T1'Class);
   end Section_12_5_1_Paragraph_23d;

--  12.5.2 Formal Scalar Types

--  12.5.3 Formal Array Types

   package Section_12_5_3_Paragraph_9 is

      --   Example of formal array types:

      --  given the generic package

      generic
         type Item is private;
         type Index is (<>);
         type Vector is array (Index range <>) of Item;
         type Table is array (Index) of Item;
      package P is
          --@ ...
      end P;

      --  and the types

      type Mix is array (Color range <>) of Boolean;
      type Option is array (Color) of Boolean;
      --  then Mix can match Vector and Option can match Table

      package R is new P (Item => Boolean, Index => Color, Vector => Mix, Table => Option);
      --  Note that Mix cannot match Table and Option cannot match Vector

   end Section_12_5_3_Paragraph_9;

--  12.5.4 Formal Access Types

   package Section_12_5_4_Paragraph_6 is
      type License_Number is new NTCT;
      type Person is new NTCT;

      --   Example of formal access types:

      --  the formal types of the generic package
      generic
         type Node is private;
         type Link is access Node;
      package P is
           --@ ...
      end P;

      --  can be matched by the actual types
      type Car;
      type Car_Name is access Car;

      type Car is record
         Pred, Succ : Car_Name;
         Number     : License_Number;
         Owner      : Person;
      end record;

      --  in the following generic instantiation
      package R is new P (Node => Car, Link => Car_Name);

   end Section_12_5_4_Paragraph_6;

   --  12.5.5 Formal Interface Types

   package Section_12_5_5_Paragraph_5 is

      type Root_Work_Item is tagged private;

      generic
         type Managed_Task is task interface;
         type Work_Item (<>) is new Root_Work_Item with private;
      package Server_Manager is
         task type Server is new Managed_Task with
            entry Start (Data : in out Work_Item);
         end Server;
      end Server_Manager;

   private
      -- Needed to compile, sometimes dummy
      type Root_Work_Item is tagged null record;
   end Section_12_5_5_Paragraph_5;

   package body Section_12_5_5_Paragraph_5 is
      -- Needed to compile, sometimes dummy
      package body Server_Manager is
         task body Server is
         begin
            accept Start (Data : in out Work_Item) do
               null;
            end Start;
         end Server;
      end Server_Manager;

   end Section_12_5_5_Paragraph_5;

   --  12.6 Formal Subprograms

   package Section_12_6_Paragraph_8g is
      -- Needed to compile, sometimes dummy
      type T is tagged null record;
      procedure Bar (Obj : in T) is null;
      type Something is new T with null record;
      procedure Some_Proc (Obj : in Something) is null;

      generic
         type T (<>) is tagged private;
         with procedure Foo (Obj : in T) is abstract;
      package P  --@ ...
      is
      end P;

      --  package New_P is new P (Something'Class, Some_Proc);  --@@ MODIF06 PP error: no visible subprogram matches the specification for "Foo"

      generic
         type NT (<>) is new T with private;
         -- Presume that T has the following primitive operation:
         -- with procedure Bar (Obj : in T);
      package Gr --@ ...
      is
      end Gr;
   end Section_12_6_Paragraph_8g;

   package body Section_12_6_Paragraph_8g is

      package body Gr is
         package New_P2 is new P (NT, Foo => Bar);
      end Gr;

      --        package New_Gr is new Gr (Something'Class);  --@@ MODIF06 PP error: no visible subprogram matches the specification for "Foo"

   end Section_12_6_Paragraph_8g;

   package Section_12_6_Paragraph_17 is
      type Item is new NTCT;
      type Item_Sequence is new NTCT;
      type Enum is new NTCT;
      type Descriptor is tagged null record;
      procedure Default_Update is null;
      use Ada.Streams;

--  Examples of generic formal subprograms:

      generic
         with function "+" (X, Y : Item) return Item is <>;
         with function Image (X : Enum) return String is Enum'Image;
         with procedure Update is Default_Update;
         with procedure Pre_Action (X : in Item) is null;  -- defaults to no action
         with procedure Write (S : not null access Root_Stream_Type'Class; Desc : Descriptor) is abstract Descriptor'
           Write;  -- see 13.13.2
         -- Dispatching operation on Descriptor with default
      procedure P;

      --  given the generic procedure declaration

      generic
         with procedure Action (X : in Item);
      procedure Iterate (Seq : in Item_Sequence);

      --  and the procedure

      procedure Put_Item (X : in Item);

      --  the following instantiation is possible

      procedure Put_List is new Iterate (Action => Put_Item);

   end Section_12_6_Paragraph_17;

   package body Section_12_6_Paragraph_17 is
      -- Needed to compile, sometimes dummy
      procedure P is null;
      procedure Iterate (Seq : in Item_Sequence) is null;
      procedure Put_Item (X : in Item) is null;
   end Section_12_6_Paragraph_17;

--  12.7 Formal Packages

   package Section_12_7_Paragraph_12 is

      -- Example of a generic package with formal package parameters:

--@    with Ada.Containers.Ordered_Maps;  -- see A.18.6
      generic
         with package Mapping_1 is new Ada.Containers.Ordered_Maps (<>);
         with package Mapping_2 is new Ada.Containers.Ordered_Maps (Key_Type => Mapping_1.Element_Type, others => <>);
      package Ordered_Join is
         -- Provide a "join" between two mappings

         subtype Key_Type is Mapping_1.Key_Type;
         subtype Element_Type is Mapping_2.Element_Type;

         function Lookup (Key : Key_Type) return Element_Type;

      --@ ...
      end Ordered_Join;

-- Example of an instantiation of a package with formal packages:

--@    with Ada.Containers.Ordered_Maps;
      package Symbol_Package is

         subtype Key_String is String (1 .. 5);
         type String_Id is new NTCT;

         type Symbol_Info is new NTCT; --@ ...

         package String_Table is new Ada.Containers.Ordered_Maps (Key_Type => Key_String, Element_Type => String_Id);

         package Symbol_Table is new Ada.Containers.Ordered_Maps (Key_Type => String_Id, Element_Type => Symbol_Info);

         package String_Info is new Ordered_Join (Mapping_1 => String_Table, Mapping_2 => Symbol_Table);

         Apple_Info : constant Symbol_Info := String_Info.Lookup ("Apple");

      end Symbol_Package;

   end Section_12_7_Paragraph_12;

   package body Section_12_7_Paragraph_12 is
      package body Ordered_Join is
         function Lookup (Key : Key_Type) return Element_Type is
            LE : Element_Type;
         begin
            return LE;
         end Lookup;
      end Ordered_Join;
   end Section_12_7_Paragraph_12;

--  12.8 Example of a Generic Package

   package Section_12_8_Paragraph_3 is

      generic
         Size : Positive;
         type Item is private;
      package Stack is
         procedure Push (E : in Item);
         procedure Pop (E : out Item);
         Overflow, Underflow : exception;
      end Stack;

   end Section_12_8_Paragraph_3;

   package body Section_12_8_Paragraph_3 is

      package body Stack is

         type Table is array (Positive range <>) of Item;
         Space : Table (1 .. Size);
         Index : Natural := 0;

         procedure Push (E : in Item) is
         begin
            if Index >= Size then
               raise Overflow;
            end if;
            Index         := Index + 1;
            Space (Index) := E;
         end Push;

         procedure Pop (E : out Item) is
         begin
            if Index = 0 then
               raise Underflow;
            end if;
            E     := Space (Index);
            Index := Index - 1;
         end Pop;

      end Stack;

   end Section_12_8_Paragraph_3;

   procedure Section_12_8_Paragraph_9 is
      use Section_12_8_Paragraph_3;
      N : Integer;

      --   Instances of this generic package can be obtained as follows:

      package Stack_Int is new Stack (Size => 200, Item => Integer);
      package Stack_Bool is new Stack (100, Boolean);

--  Thereafter, the procedures of the instantiated packages can be called as follows:

   begin
      Stack_Int.Push (N);
      Stack_Bool.Push (True);
   end Section_12_8_Paragraph_9;

   package Section_12_8_Paragraph_13 is

      --  Alternatively, a generic formulation of the type Stack can be given as
--  follows (package body omitted):

      generic
         type Item is private;
      package On_Stacks is
         type Stack (Size : Positive) is limited private;
         procedure Push (S : in out Stack; E : in Item);
         procedure Pop (S : in out Stack; E : out Item);
         Overflow, Underflow : exception;
      private
         type Table is array (Positive range <>) of Item;
         type Stack (Size : Positive) is record
            Space : Table (1 .. Size);
            Index : Natural := 0;
         end record;
      end On_Stacks;

   end Section_12_8_Paragraph_13;

   package body Section_12_8_Paragraph_13 is

      package body On_Stacks is
         -- Needed to compile, sometimes dummy
         procedure Push (S : in out Stack; E : in Item) is null;
         procedure Pop (S : in out Stack; E : out Item) is null;
      end On_Stacks;

   end Section_12_8_Paragraph_13;

   procedure Section_12_8_Paragraph_15 is
      use Section_12_8_Paragraph_13;

      --  In order to use such a package, an instance has to be created and
--  thereafter stacks of the corresponding type can be declared:

   begin
      declare
         package Stack_Real is new On_Stacks (Real);
         use Stack_Real;
         S : Stack (100);
      begin
           --@ ...
         Push (S, 2.54);
           --@ ...
      end;
   end Section_12_8_Paragraph_15;

begin
   null;
end AARM_202x_CH12;
