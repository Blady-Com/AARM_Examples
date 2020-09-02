with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

procedure AARM_202x_CH04 is

   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      type Real is digits 8;
      type Matrix is array (Integer range <>, Integer range <>) of Real;
      type Bit_Vector is array (Integer range <>) of Boolean;
      type Table is array (1 .. 10) of Integer;
      M  : constant := 99;
      N  : constant := 199;
      PI : constant := 3.14;
      type Point is tagged record
         X, Y : Real := 0.0;
      end record;
      type Month_Name is (January, February, March, April, May, June, July,
                          August, September, October, November, December);
      type Date is record
         Day   : Integer range 1 .. 31;
         Month : Month_Name;
         Year  : Integer range 0 .. 4000;
      end record;
      Tomorrow, Yesterday : Date;
      type Vector is array (Integer range <>) of Real;
      subtype Count_Type is Ada.Containers.Count_Type;
      type Cell;  --  incomplete type declaration
      type Link is access Cell;
      type Cell is record
         Value : Integer;
         Succ  : Link;
         Pred  : Link;
      end record;
      Head : Link := new Cell'(0, null, null);
      function Min_Cell (X : Link) return Cell is ((0, X, X));
      subtype Probability is Real range 0.0 .. 1.0;
      function Random return Probability is (0.5);
      package Cards_And_Persons is
         type Color is (White, Red, Yellow, Green, Blue, Brown, Black);
         type Suit is (Clubs, Diamonds, Hearts, Spades);
         type Person (<>);
         type Person_Name is access Person;
         type Gender is (M, F);
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
         Casey : constant Person_Name := new Person (M);
      end Cards_And_Persons;
   end Needed_To_Compile;

   --                          4   Names and Expressions

   --  4.1 Names

   --    Examples of direct names:
   --
   --          Pi        -- the direct name of a number                 (see 3.3.2)
   --          Limit     -- the direct name of a constant               (see 3.3.1)
   --          Count     -- the direct name of a scalar variable        (see 3.3.1)
   --          Board     -- the direct name of an array variable        (see 3.6.1)
   --          Matrix    -- the direct name of a type                   (see 3.6)
   --          Random    -- the direct name of a function               (see 6.1)
   --          Error     -- the direct name of an exception             (see 11.1)
   --
   --    Examples of dereferences:
   --
   --        Next_Car.all
   --  explicit dereference denoting the object designated by
   --  the access variable Next_Car (see 3.10.1)
   --          Next_Car.Owner
   --  selected component with implicit dereference;
   --  same as Next_Car.all.Owner
   --  4.1.1 Indexed Components

   --   Examples of indexed components:

   --        My_Schedule(Sat)     --  a component of a one-dimensional array  (see 3.6.1)
   --         Page(10)             --  a component of a one-dimensional array   (see 3.6)
   --         Board(M, J + 1)      --  a component of a two-dimensional array (see 3.6.1)
   --         Page(10)(20)         --  a component of a component  (see 3.6)
   --         Request(Medium)      --  an entry in a family of entries    (see 9.1)
   --         Next_Frame(L)(M, N)  --  a component of a function call      (see 6.1)

   --  4.1.2 Slices

   --  Examples of slices:

   --        Stars(1 .. 15)        --  a slice of 15 characters   (see 3.6.3)
   --          Page(10 .. 10 + Size) --  a slice of 1 + Size components (see 3.6)
   --          Page(L)(A .. B)       --  a slice of the array Page(L)  (see 3.6)
   --          Stars(1 .. 0)         --  a null slice   (see 3.6.3)
   --          My_Schedule(Weekday)  --  bounds given by subtype  (see 3.6.1 and 3.5.1)
   --          Stars(5 .. 15)(K)     --  same as Stars(K)   (see 3.6.3)
   --  provided that K is in 5 .. 15

   --  4.1.3 Selected Components

   --  Examples of selected components:

   --  Tomorrow.Month     --  a record component  (see 3.8)
   --          Next_Car.Owner     --  a record component  (see 3.10.1)
   --          Next_Car.Owner.Age --  a record component   (see 3.10.1)
   --  the previous two lines involve implicit dereferences
   --          Writer.Unit        --  a record component (a discriminant) (see 3.8.1)
   --          Min_Cell(H).Value  --  a record component of the result    (see 6.1)
   --  of the function call Min_Cell(H)
   --          Cashier.Append     --  a prefixed view of a procedure (see 3.9.4)
   --          Control.Seize      --  an entry of a protected object (see 9.4)
   --          Pool(K).Write      --  an entry of the task Pool(K)  (see 9.1)

   --  Examples of expanded names:

   --        Key_Manager."<"      --  an operator of the visible part of a package  (see 7.3.1)
   --          Dot_Product.Sum      --  a variable declared in a function body   (see 6.1)
   --          Buffer.Pool          --  a variable declared in a protected unit  (see 9.11)
   --          Buffer.Read          --  an entry of a protected unit  (see 9.11)
   --          Swap.Temp            --  a variable declared in a block statement (see 5.6)
   --          Standard.Boolean     --  the name of a predefined type  (see A.1)

   --  4.1.4 Attributes

   -- Examples of attributes:

   --      Color'First        -- minimum value of the enumeration type Color   (see 3.5.1)
   --        Rainbow'Base'First -- same as Color'First   (see 3.5.1)
   --        Real'Digits        -- precision of the type Real   (see 3.5.7)
   --        Board'Last(2)      -- upper bound of the second dimension of Board  (see 3.6.1)
   --        Board'Range(1)     -- index range of the first dimension of Board     (see 3.6.1)
   --        Pool(K)'Terminated -- True if task Pool(K) is terminated  (see 9.1)
   --        Date'Size          -- number of bits for records of type Date   (see 3.8)
   --        Message'Address    -- address of the record variable Message   (see 3.7.1)

   --  4.1.5 User-Defined References

   package Section_4_1_5_Paragraph_9 is

      type Element is new Integer;
      type Barrel is tagged record  -- holds objects of type Element
         E : aliased Element;
      end record;

      type Ref_Element (Data : access Element) is limited private with
         Implicit_Dereference => Data;
      -- This Ref_Element type is a "reference" type.
      -- "Data" is its reference discriminant.

      function Find (B : aliased in out Barrel; Key : String) return Ref_Element;
      -- Return a reference to an element of a barrel.

      B : aliased Barrel;
   private
      type Ref_Element (Data : access Element) is null record;
   end Section_4_1_5_Paragraph_9;

   package body Section_4_1_5_Paragraph_9 is
      -- Needed to compile, sometimes dummy
      function Find (B : aliased in out Barrel; Key : String) return Ref_Element is
      begin
         return R : Ref_Element (B.E'Access);
      end Find;

   begin
      Find (B, "grape") := Element'(0); --@ ...); -- Assign through a reference.
      -- This is equivalent to:
      Find (B, "grape").Data.all := Element'(0); --@ ...);
   end Section_4_1_5_Paragraph_9;

   --  4.1.6 User-Defined Indexing

   package Section_4_1_6_Paragraph_18 is

      type Element is new Integer;
      type Indexed_Barrel is tagged --@ ...
      record
         E : aliased Element;
      end record with
         Variable_Indexing => Find;
      -- Indexed_Barrel is an indexable container type,
      -- Find is the generalized indexing operation.

      type Ref_Element (Data : access Element) is limited private with
         Implicit_Dereference => Data;
      -- This Ref_Element type is a "reference" type.
      -- "Data" is its reference discriminant.

      function Find (B : aliased in out Indexed_Barrel; Key : String) return Ref_Element;
      -- Return a reference to an element of a barrel (see 4.1.5).

   private
      type Ref_Element (Data : access Element) is null record;
   end Section_4_1_6_Paragraph_18;

   package body Section_4_1_6_Paragraph_18 is
      -- Needed to compile, sometimes dummy
      function Find (B : aliased in out Indexed_Barrel; Key : String) return Ref_Element is
      begin
         return R : Ref_Element (B.E'Access);
      end Find;

      IB : aliased Indexed_Barrel;
   begin

      -- All of the following calls are then equivalent:

      Find (IB, "pear").Data.all := Element'(2); --@ ...);  -- Traditional call
      IB.Find ("pear").Data.all  := Element'(2); --@ ...);  -- Call of prefixed view
      IB.Find ("pear")           := Element'(2); --@ ...);  -- Implicit dereference (see 4.1.5)
      IB ("pear")                := Element'(2); --@ ...);  -- Implicit indexing and dereference
      IB ("pear").Data.all       := Element'(2); --@ ...);  -- Implicit indexing only

   end Section_4_1_6_Paragraph_18;

   --  4.2 Literals

   --  Examples of literals:
   --        3.14159_26536      --  a real literal
   --        1_345              --  an integer literal
   --        'A'                --  a character literal
   --        "Some Text"        --  a string literal

   --  4.2.1 User-Defined Literals

   procedure Section_4_2_1_Paragraph_6b is
      package Pkg2 is
         type T1 is record
            X, Y : Integer;
         end record with
            Integer_Literal => I_L;

         function I_L (S : String) return T1 is ((0, 0));

         type T2 is new T1;
         function I_L (S : String) return T2 is ((1, 1));
         X : T2 := 123;
      end Pkg2;

      --       {AI12-0342-1} the initial value of Pkg.X is (0,0), not (1,1).
   begin
      null;
   end Section_4_2_1_Paragraph_6b;

   procedure Section_4_2_1_Paragraph_15 is
      type Roman_Digit is ('I', 'V', 'X', 'L', 'C', 'D', 'M');
      for Roman_Digit use ('I' => 1, 'V' => 5, 'X' => 10, 'L' => 50, 'C' => 100, 'D' => 500, 'M' => 1000);

      --                                    Examples

      subtype Roman_Character is Character with
           Static_Predicate => Roman_Character in 'I' | 'V' | 'X' | 'L' | 'C' | 'D' | 'M';

      Max_Roman_Number : constant := 3_999;  -- MMMCMXCIX

      type Roman_Number is range 1 .. Max_Roman_Number;
      --          with String_Literal => To_Roman_Number; --@@ MODIF PP: not yet available

      --        function To_Roman_Number (S : String) return Roman_Number
      --          with Pre => S'Length > 0 and then
      --          (for all Char of S => Char in Roman_Character);

      --        function To_Roman_Number (S : String) return Roman_Number is
      --          (declare
      --           R : constant array (Integer range <>) of Roman_Number :=
      --           (for D in S'Range => Roman_Digit'Enum_Rep
      --            (Roman_Digit'Value (''' & S(D) & '''))); -- See 3.5.2 and 13.4
      --           begin
      --           [for I in R'Range =>  --@@ MODIF24 PP: GNAT error: "R" is undefined
      --             (if I < R'Last and then R(I) < R(I + 1) then -1 else 1) * R(I)]
      --                      'Reduce("+", 0)
      --          );

      --           X : Roman_Number := "III" * "IV" * "XII"; -- 144 (that is, CXLIV) --@@ MODIF PP: not yet available
   begin
      null;
   end Section_4_2_1_Paragraph_15;

   --  4.3 Aggregates

   --  4.3.1 Record Aggregates

   --  Example of a record aggregate with positional associations:

   --      (4, July, 1776)                                       --  see 3.8

   --  Examples of record aggregates with named associations:

   --      (Day => 4, Month => July, Year => 1776)
   --        (Month => July, Day => 4, Year => 1776)

   --      (Disk, Closed, Track => 5, Cylinder => 12)            --  see 3.8.1
   --        (Unit => Disk, Status => Closed, Cylinder => 9, Track => 1)

   -- Examples of component associations with several choices:

   --      (Value => 0, Succ|Pred => new Cell'(0, null, null))  --  see 3.10.1

   --  The allocator is evaluated twice: Succ and Pred designate different cells

   -- (Value => 0, Succ|Pred => <>)        --  see 3.10.1

   --  Succ and Pred will be set to null

   --  Examples of record aggregates for tagged types (see 3.9 and 3.9.1):

   --      Expression'(null record)
   --        Literal'(Value => 0.0)
   --        Painted_Point'(0.0, Pi/2.0, Paint => Red)

   --  4.3.2 Extension Aggregates

   --  Examples of extension aggregates (for types defined in 3.9.1):

   --     Painted_Point'(Point with Red)
   --        (Point'(P) with Paint => Black)

   -- (Expression with Left =>  new Literal'(Value => 1.2),
   --                         Right => new Literal'(Value => 3.4))
   --        Addition'(Binop with null record)
   -- presuming Binop is of type Binary_Operation

   --  4.3.3 Array Aggregates

   --  Examples of array aggregates with positional associations:

   --      (7, 9, 5, 1, 3, 2, 4, 8, 6, 0)
   --        Table'(5, 8, 4, 1, others => 0)  --  see 3.6

   --  Examples of array aggregates with named associations:

   -- (1 .. 5 => (1 .. 8 => 0.0))      --  two-dimensional
   --      [1 .. N => new Cell]             --  N new cells, in particular for N = 0

   -- Table'(2 | 4 | 10 => 1, others => 0)
   --        Schedule'(Mon .. Fri => True,  others => False)  --  see 3.6
   --        Schedule'[Wed | Sun  => False, others => True]
   --        Vector'(1 => 2.5)                                --  single-component vector

   --  Examples of two-dimensional array aggregates:

   -- Three aggregates for the same value of subtype Matrix(1..2,1..3) (see 3.6):

   -- ((1.1, 1.2, 1.3), (2.1, 2.2, 2.3))
   --        (1 => [1.1, 1.2, 1.3], 2 => [2.1, 2.2, 2.3])
   --        [1 => (1 => 1.1, 2 => 1.2, 3 => 1.3), 2 => (1 => 2.1, 2 => 2.2, 3 => 2.3)]

   --  Examples of aggregates as initial values:

   package Section_4_3_3_Paragraph_44 is
      use Needed_To_Compile;
      A : Table           := (7, 9, 5, 1, 3, 2, 4, 8, 6, 0);        -- A(1)=7, A(10)=0
      B : Table           := (2 | 4 | 10 => 1, others => 0);        -- B(1)=0, B(10)=1
      C : constant Matrix := (1 .. 5 => (1 .. 8 => 0.0)); -- C'Last(1)=5, C'Last(2)=8

      D : Bit_Vector (M .. N) := (M .. N => True);         -- see 3.6
      E : Bit_Vector (M .. N) := (others => True);
      F : String (1 .. 1)     := (1 => 'F');  -- a one component aggregate: same as "F"
      G : constant Matrix     :=
          (for I in 1 .. 4 =>
             (for J in 1 .. 4 =>
                (if I=J then 1.0 else 0.0))); -- Identity matrix
   --        Empty_Matrix : constant Matrix := []; -- A matrix without elements --@@ MODIF PP: not yet available
   end Section_4_3_3_Paragraph_44;

   -- Example of an array aggregate with defaulted others
   -- choice and with an applicable index constraint provided by an enclosing record
   -- aggregate:

   --  Buffer'(Size => 50, Pos => 1, Value => ('x', others => <>))  -- see 3.7

   --  4.3.4 Delta Aggregates

   procedure Section_4_3_4_Paragraph_22 is
      use Needed_To_Compile;

      -- Simple use in a postcondition:

      procedure Twelfth (D : in out Date) -- see 3.8 for type Date
        with Post => D = (D'Old with delta Day => 12);

      procedure The_Answer (V : in out Vector; A, B : in Integer) -- see 3.6 for type Vector
        with Post => V = (V'Old with delta A .. B => 42.0, V'First => 0.0);

      -- The base expression can be nontrivial:

      New_Cell : Cell := (Min_Cell (Head) with delta Value => 42);
      -- see 3.10.1 for Cell and Head; 6.1 for Min_Cell

      A1 : Vector := ((0 => 1.0, 1 => 2.0, 2 => 3.0)
                        with delta Integer(Random * 2.0) => 14.2);
      -- see 3.6 for declaration of type Vector
      -- see 6.1 for declaration of Random

      -- The base expression may also be class-wide:

      --        function Translate (P : Point'Class; X, Y : Real) return Point'Class is
      --          (P with delta X => P.X + X,
      --           Y => P.Y + Y); -- see 3.9 for declaration of type Point  --@@ MODIF21 PP: error: type "Point'Class" has no component with this name

      procedure Twelfth (D : in out Date) is null;
      procedure The_Answer (V : in out Vector; A, B : in Integer) is null;

   begin
      Tomorrow := ((Yesterday with delta Day => 12) with delta Month => April); -- see 3.8
   end Section_4_3_4_Paragraph_22;

   --  4.3.5 Container Aggregates

   -- Declarations of Set_Type, Map_Type, and Vector_Type:

   package Section_4_3_5_Paragraph_54 is
      use Needed_To_Compile;

      --  Set_Type is a set-like container type.

      type Set_Type is private; --@@ MODIF PP: not yet available
      --          with Aggregate => (Empty       => Empty_Set,
      --                             Add_Unnamed => Include);

      function Empty_Set return Set_Type;

      subtype Small_Natural is Natural range 0 .. 1000;

      procedure Include (S : in out Set_Type; N : in Small_Natural);

      --  Map_Type is a map-like container type.

      type Map_Type is private; --@@ MODIF PP: not yet available
      --          with Aggregate =>  (Empty     => Empty_Map,
      --                              Add_Named => Add_To_Map);

      procedure Add_To_Map (M : in out Map_Type; Key : in Integer; Value : in String);

      Empty_Map : constant Map_Type;

      -- Vector_Type is an extensible array-like container type.

      type Vector_Type is private; --@@ MODIF PP: not yet available
      --          with Aggregate => (Empty          => Empty_Vector,
      --                             Add_Unnamed    => Append_One,
      --                             New_Indexed    => New_Vector,
      --                             Assign_Indexed => Assign_Element);

      function Empty_Vector (Capacity : Count_Type := 0) return Vector_Type;

      procedure Append_One (V : in out Vector_Type; New_Item : in String);

      procedure Assign_Element (V : in out Vector_Type; Index : in Positive; Item : in String);

      function New_Vector (First, Last : Positive) return Vector_Type with
         Pre => First = Positive'First;
      --                Vectors are always indexed starting at the
      --                  lower bound of their index subtype.

      -- Private part not shown.
      -- Needed to compile, sometimes dummy
   private
      type Set_Type is new Bit_Vector (Small_Natural); -- See 3.6.
      function Empty_Set return Set_Type is (others => False);
      package Int_String_Maps is new Ada.Containers.Indefinite_Ordered_Maps  -- See A.18.14.
        (Key_Type => Integer, Element_Type => String);
      type Map_Type is new Int_String_Maps.Map with null record;

      procedure Add_To_Map (M : in out Map_Type; Key : in Integer; Value : in String) renames Insert;
      Empty_Map : constant Map_Type := (Int_String_Maps.Empty_Map with null record);
      package String_Vectors is new Ada.Containers.Indefinite_Vectors -- See A.18.11.
        (Index_Type => Positive, Element_Type => String);
      type Vector_Type is new String_Vectors.Vector with null record;
   end Section_4_3_5_Paragraph_54;

   -- Examples of container aggregates for Set_Type, Map_Type, and Vector_Type:

   package body Section_4_3_5_Paragraph_54 is

      --  Example aggregates using Set_Type.

      procedure Set_Type_Example is
         S : Set_Type;
      begin
         --  Assign the empty set to S:

         S := [];

         --  Is equivalent to:

         S := Empty_Set;

         --  A positional set aggregate:

         --           S := [1, 2]; --@@ MODIF PP: not yet available

         --  Is equivalent to:

         S := Empty_Set;
         Include (S, 1);
         Include (S, 2);

         --  A set aggregate with an iterated_element_association:

         --           S := [for Item in 1 .. 5 => Item * 2]; --@@ MODIF PP: not yet available

         --  Is equivalent to:

         S := Empty_Set;
         for Item in 1 .. 5 loop
            Include (S, Item * 2);
         end loop;

         --  A set aggregate consisting of two iterated_element_associations:

         --           S := [for Item in 1 .. 5 => Item,
         --             for Item in 1 .. 5 => -Item]; --@@ MODIF PP: not yet available

         --  Is equivalent (assuming set semantics) to:

         S := Empty_Set;
         for Item in -5 .. 5 loop
            if Item /= 0 then
               Include (S, Item);
            end if;
         end loop;

      end Set_Type_Example;

      --  Example aggregates using Map_Type.

      procedure Map_Type_Example is
         M : Map_Type;
      begin

         --  Create an empty map:

         --           M := []; --@@ MODIF PP: not yet available; canceled in draft 24

         --  Is equivalent to:

         M := Empty_Map;

         --  A simple named map aggregate:

         --           M := [12 => "house", 14 => "beige"]; --@@ MODIF PP: not yet available

         --  Is equivalent to:

         M := Empty_Map;
         Add_To_Map (M, 12, "house");
         Add_To_Map (M, 14, "beige");

         --  Define a table of pairs:

         declare
            type Pair is record
               Key   : Integer;
               Value : access constant String;
            end record;

            Table : constant array (Positive range <>) of Pair :=
              [(Key => 33, Value => new String'("a nice string")),
               (Key => 44, Value => new String'("an even better string"))];
         begin

            --  A map aggregate using an iterated_element_association
            --  and a key_expression, built from from a table of key/value pairs:

            --  M := [for P of Table use P.Key => P.Value.all]; --@@ MODIF PP: not yet available

            --  Is equivalent to:

            M := Empty_Map;
            for P of Table loop
               Add_To_Map (M, P.Key, P.Value.all);
            end loop;

         end;

         --  Create an image table for an array of integers:

         declare
            Keys : constant array (Positive range <>) of Integer := [2, 3, 5, 7, 11];
         begin

            --  A map aggregate where the values produced by the
            --  iterated_element_association are of the same type as the key
            --  (eliminating the need for a separate key_expression):

            --              M := [for Key of Keys => Integer'Image (Key)]; --@@ MODIF PP: not yet available

            --  Is equivalent to:

            M := Empty_Map;
            for Key of Keys loop
               Add_To_Map (M, Key, Integer'Image (Key));
            end loop;

            --  The above could have been written using an explicit key_expression:

            --              M := [for Key of Keys use Key => Integer'Image (Key)]; --@@ MODIF PP: not yet available, canceled in draft 24

         end;
      end Map_Type_Example;

      --  Example aggregates using Vector_Type.

      procedure Vector_Type_Example is
         M                     : Map_Type;
         estimate_of_size_of_M : constant Count_Type := M.Length;
         procedure Add_Positional (V : Vector_Type; E : String) is null;

         V : Vector_Type;
      begin

         --  Create an empty vector aggregate:

         --              V := [];  --@@ MODIF PP: not yet available; canceled in draft 24

         --  Is equivalent to:

         V := Empty_Vector (0);

         --  A positional vector aggregate:

         --              V := ["abc", "def"]; --@@ MODIF PP: not yet available

         --  Is equivalent to:

         V := Empty_Vector (2);
         Append_One (V, "abc");
         Append_One (V, "def");

         --  An indexed vector aggregate:

         --              V := [1 => "this", 2 => "is", 3 => "a", 4 => "test"];  --@@ MODIF PP: not yet available

         --  Is equivalent to:

         V := New_Vector (1, 4);
         Assign_Element (V, 1, "this");
         Assign_Element (V, 2, "is");
         Assign_Element (V, 3, "a");
         Assign_Element (V, 4, "test");

         --  A vector of images of dynamic length:

         --              V := [for I in 1 .. N => Integer'Image (I)];  --@@ MODIF PP: not yet available; canceled in draft 24

         --  Is equivalent to:

         V := New_Vector (1, N);
         for I in 1 .. N loop
            Assign_Element (V, I, Integer'Image (I));
         end loop;

         --  A vector made from the elements of a map:

         --              V := [for Elem of M => Elem]; --@@ MODIF PP: not yet available

         --  Is equivalent to:

         V := Empty_Vector (estimate_of_size_of_M);
         for Elem of M loop
            Add_Positional (V, Elem);
         end loop;

      end Vector_Type_Example;
      procedure Include (S : in out Set_Type; N : in Small_Natural) is null;
      function Empty_Vector (Capacity : Count_Type := 0) return Vector_Type is (Empty_Vector (0));
      procedure Append_One (V : in out Vector_Type; New_Item : in String) is null;
      procedure Assign_Element (V : in out Vector_Type; Index : in Positive; Item : in String) is null;
      function New_Vector (First, Last : Positive) return Vector_Type is (Empty_Vector (0));
   end Section_4_3_5_Paragraph_54;

   --  4.4 Expressions

   --  Examples of primaries:
   --      4.0                --  real literal
   --        Pi                 --  named number
   --        (1 .. 10 => 0)     --  array aggregate
   --        Sum                --  variable
   --        Integer'Last       --  attribute
   --        Sine(X)            --  function call
   --        Color'(Blue)       --  qualified expression
   --        Real(M*N)          --  conversion
   --        (Line_Count + 10)  --  parenthesized expression

   --  Examples of expressions:

   -- Volume                      -- primary
   --        not Destroyed               -- factor
   --        2*Line_Count                -- term
   --        -4.0                        -- simple expression
   --        -4.0 + A                    -- simple expression
   --        B**2 - 4.0*A*C              -- simple expression
   --        R*Sin(<Unicode-952>)*Cos(<Unicode-966>)             -- simple expression
   --        Password(1 .. 3) = "Bwv"    -- relation
   --        Count in Small_Int          -- relation
   --        Count not in Small_Int      -- relation
   --        Index = 0 or Item_Hit       -- expression
   --        (Cold and Sunny) or Warm    -- expression (parentheses are required)
   --        A**(B**C)                   -- expression (parentheses are required)

   --  4.5 Operators and Expression Evaluation

   --  Examples of precedence:

   --        not Sunny or Warm    --  same as (not Sunny) or Warm
   --        X > 4.0 and Y > 0.0  --  same as (X > 4.0) and (Y > 0.0)
   --        -4.0*A**2            --  same as -(4.0 * (A**2))
   --        abs(1 + A) + B       --  same as (abs (1 + A)) + B
   --        Y**(-3)              --  parentheses are necessary
   --        A / B * C            --  same as (A/B)*C
   --        A + (B + C)          --  evaluate B + C before adding it to A

   --  4.5.1 Logical Operators and Short-circuit Control Forms

   --  Examples of logical operators:

   --      Sunny or Warm
   --        Filter(1 .. 10) and Filter(15 .. 24)   --   see 3.6.1

   --  Examples of short-circuit control forms:

   --      Next_Car.Owner /= null and then Next_Car.Owner.Age > 25   --   see 3.10.1

   --        N = 0 or else A(N) = Hit_Value

   --  4.5.2 Relational Operators and Membership Tests

   package Section_4_5_2_Paragraph_30c is
      type Root is tagged null record;
      type Ext is new Root with record
         Data : Integer;
      end record;
      function Is_Even (Param : Ext) return Boolean is (Param.Data mod 2 = 0);
      subtype Even_Ext is Ext with
           Dynamic_Predicate => Is_Even (Even_Ext);
      function F (X : Root'Class) return Boolean is (X in Even_Ext);
      Flag : Boolean := F (Root'(null record));
   end Section_4_5_2_Paragraph_30c;

   --  Examples of expressions involving relational operators and membership tests:

   --      X /= Y
   -- S : String := "A";
   --        "" < S and S < "Aa"         --  True
   --        S < "Bb" and S < "A  "      --  True
   --        My_Car = null               -- True if My_Car has been set to null (see 3.10.1)
   --        My_Car = Your_Car           -- True if we both share the same car
   --        My_Car.all = Your_Car.all   -- True if the two cars are identical
   -- N not in 1 .. 10            -- range membership test
   --        Today in Mon .. Fri         -- range membership test
   --        Today in Weekday            -- subtype membership test (see 3.5.1)
   --        Card in Clubs | Spades      -- list membership test (see 3.5.1)
   --        Archive in Disk_Unit        -- subtype membership test (see 3.8.1)
   --        Tree.all in Addition'Class  -- class membership test (see 3.9.1)

   --  4.5.3 Binary Adding Operators

   --  Examples of expressions involving binary adding operators:

   --      Z + 0.1      --  Z has to be of a real type
   --      "A" & "BCD"  --  concatenation of two string literals
   --        'A' & "BCD"  --  concatenation of a character literal and a string literal
   --        'A' & 'A'    --  concatenation of two character literals

   --  4.5.4 Unary Adding Operators

   --  4.5.5 Multiplying Operators

   --  Examples of expressions involving multiplying operators:

   --        I : Integer := 1;
   --        J : Integer := 2;
   --        K : Integer := 3;

   --        X : Real := 1.0;                      --     see 3.5.7
   --        Y : Real := 2.0;

   --        F : Fraction := 0.25;                 --     see 3.5.9
   --        G : Fraction := 0.5;

   --      Expression            Value          Result Type

   --        I*J                   2              same as I and J, that is, Integer
   --        K/J                   1              same as K and J, that is, Integer
   --        K mod J               1              same as K and J, that is, Integer

   --        X/Y                   0.5            same as X and Y, that is, Real
   --        F/2                   0.125          same as F, that is, Fraction

   --        3*F                   0.75           same as F, that is, Fraction
   --        0.75*G                0.375
   --        universal_fixed, implicitly convertible to any fixed point type
   --        Fraction(F*G)         0.125
   --        Fraction, as stated by the conversion
   --        Real(J)*Y             4.0
   --        Real, the type of both operands after conversion of J

   --  4.5.6 Highest Precedence Operators

   --  4.5.7 Conditional Expressions

   package Section_4_5_7_Paragraph_19 is
      generic
         with function Int_Func return Integer;
      package G is
         X : Float := (case Int_Func is when Integer'First .. -1 => -1.0, when 0 => 0.0, when Positive => 1.0);
      end G;

      function Nat_Func return Natural is (123);

      package I is new G (Int_Func => Nat_Func); -- Legal

   end Section_4_5_7_Paragraph_19;

   procedure Section_4_5_7_Paragraph_22 is
      use Ada.Text_IO, Needed_To_Compile.Cards_And_Persons;

      --                                  Examples

      function Card_Color (Card : Suit) return Color is -- see 3.5.1
        (case Card is when Clubs | Spades => Black, when Hearts | Diamonds => Red);

   begin
      Put_Line ("Casey is " & (if Casey.Sex = M then "Male" else "Female")); -- see 3.10.1
   end Section_4_5_7_Paragraph_22;

   --  4.5.8 Quantified Expressions

   package Section_4_5_8_Paragraph_10 is
      use Needed_To_Compile;
      -- The postcondition for a sorting routine on an array A with an index subtype T can be written:

      --    Post => (A'Length < 2 or else
      --         (for all I in A'First .. T'Pred(A'Last) => A (I) <= A (T'Succ (I))))

      -- The assertion that a positive number is composite (as opposed to prime) can be written:

      --              pragma Assert (for some X in 2 .. N when X * X <= N => N mod X = 0); --@@ MODIF PP: not yet available
      -- see iterator_filter in 5.5

   end Section_4_5_8_Paragraph_10;

   --  4.5.9 Declare Expressions

   -- The postcondition for Ada.Containers.Vectors."&" (see A.18.2) could have been written:

   -- with Post =>
   --       (declare
   --           Result renames Vectors."&"'Result;
   --            Length : constant Count_Type := Left.Length + Right.Length;
   --          begin
   --            Result.Length = Length and then
   --           not Tampering_With_Elements_Prohibited (Result) and then
   --           not Tampering_With_Cursors_Prohibited (Result) and then
   --           Result.Capacity >= Length)

   --  4.5.10 Reduction Expressions

   procedure Section_4_5_10_Paragraph_36 is
      use Ada.Text_IO, Needed_To_Compile;

      -- An expression function that returns its result as a Reduction Expression:

      function Factorial(N : Natural) return Natural is
        ([for J in 1..N => J]'Reduce("*", 1));

      -- An expression function that computes the Sine of X using a Taylor expansion:

      function Sine (X : Float; Num_Terms : Positive := 5) return Float is
             ([for I in 1..Num_Terms => (-1.0)**(I-1) * X**(2*I-1)/Float(Factorial(2*I-1))]
                'Reduce("+", 0.0));

      -- A reduction expression that outputs the sum of squares:

      -- An expression function to compute the value of Pi:

      --  See 3.5.7.
      function Pi (Number_Of_Steps : Natural := 10_000) return Real is
                (1.0 / Real (Number_Of_Steps) *
                 [for I in 1 .. Number_Of_Steps =>
                   (4.0 / (1.0 + ((Real (I) - 0.5) * (1.0 / Real (Number_Of_Steps)))**2))]
                     'Reduce("+", 0.0));

      -- Calculate the sum of elements of an array of integers:

      --    A'Reduce("+",0)  -- See 4.3.3.

      -- Determine if all elements in a two dimensional array of booleans are set to true:

      --    Grid'Reduce("and", True)  -- See 3.6.

      -- Calculate the minimum value of an array of integers in parallel:

      --    A'Parallel_Reduce(Integer'Min, Integer'Last)

      -- {AI12-0312-1} A parallel reduction expression used to calculate the mean
      --of the elements of a two-dimensional array of subtype Matrix (see 3.6) that
      --are greater than 100.0:

      type Accumulator is record
         Sum   : Real; -- See 3.5.7.
         Count : Integer;
      end record;

      function Accumulate (L, R : Accumulator) return Accumulator is (Sum => L.Sum + R.Sum, Count => L.Count + R.Count);

      --     function Average_of_Values_Greater_Than_100 (M : Matrix) return Real is
      --             (declare
      --                 Acc : constant Accumulator :=
      --                    [parallel for Val of M when Val > 100.0 => (Val, 1)] --@@ MODIF PP: not yet available
      --                       'Reduce(Accumulate, (Sum => 0, Count => 0));
      --              begin
      --                 Acc.Sum / Real(Acc.Count));

   begin
      Put_Line ("Sum of Squares is" &
                  Integer'Image([for I in 1 .. 10 => I**2]'Reduce("+", 0)));
   end Section_4_5_10_Paragraph_36;

   --  4.6 Type Conversions

   --  Examples of numeric type conversion:

   --      Real(2*J)      --  value is converted to floating point
   --        Integer(1.6)   --  value is 2
   --        Integer(-0.4)  --  value is 0

   --  Example of conversion between derived types:

   --      type A_Form is new B_Form;

   --      X : A_Form;
   --        Y : B_Form;

   --      X := A_Form(Y);
   --        Y := B_Form(X);  --  the reverse conversion

   --  Examples of conversions between array types:

   --      type Sequence is array (Integer range <>) of Integer;
   --        subtype Dozen is Sequence(1 .. 12);
   --        Ledger : array(1 .. 100) of Integer;

   --      Sequence(Ledger)            --  bounds are those of Ledger
   --        Sequence(Ledger(31 .. 42))  --  bounds are 31 and 42
   --        Dozen(Ledger(31 .. 42))     --  bounds are those of Dozen

   --  4.7 Qualified Expressions

   --   Examples of disambiguating expressions using qualification:

   --       type Mask is (Fix, Dec, Exp, Signif);
   --    type Code is (Fix, Cla, Dec, Tnz, Sub);

   --       Print (Mask'(Dec));  --  Dec is of type Mask
   --     Print (Code'(Dec));  --  Dec is of type Code

   --       for J in Code'(Fix) .. Code'(Dec) loop ... -- qualification needed for either Fix or Dec
   --     for J in Code range Fix .. Dec loop ...    -- qualification unnecessary
   --    for J in Code'(Fix) .. Dec loop ...        -- qualification unnecessary for Dec

   --     Dozen'(1 | 3 | 5 | 7 => 2, others => 0) -- see 4.6

   --  4.8 Allocators

   --  Examples of allocators:

   --      new Cell'(0, null, null)                          -- initialized explicitly, see 3.10.1
   --      new Cell'(Value => 0, Succ => null, Pred => null) -- initialized explicitly
   --        new Cell                                          -- not initialized

   --      new Matrix(1 .. 10, 1 .. 20)                      -- the bounds only are given
   --       new Matrix'(1 .. 10 => (1 .. 20 => 0.0))          -- initialized explicitly

   --      new Buffer(100)                                   -- the discriminant only is given
   --        new Buffer'(Size => 80, Pos => 0, Value => (1 .. 80 => 'A')) -- initialized explicitly

   --      Expr_Ptr'(new Literal)                  -- allocator for access-to-class-wide type, see 3.9.1
   --        Expr_Ptr'(new Literal'(Expression with 3.5))      -- initialized explicitly

   --  4.9 Static Expressions and Static Subtypes

   procedure Section_4_9_Paragraph_26b is

      subtype Int10 is Integer range 1 .. 10;

      generic
         F : in out Int10;
      procedure G;

      procedure G is
      begin
         case F is
            when 1 .. 10 =>
               null;
               -- Illegal!
            when others =>
               null; -- so the choices have to cover all values of Integer
         end case;
      end G;

      X : Integer range 1 .. 20;
      procedure I is new G (F => X); -- OK.

   begin
      null;
   end Section_4_9_Paragraph_26b;

   package Section_4_9_Paragraph_37b is

      --        Short-circuit control forms are a special case:

      N  : constant         := 0.0;
      X1 : constant Boolean := (N = 0.0) or else (1.0 / N > 0.5); -- Static.

      --        The declaration of X is legal, since the divide-by-zero part of
      --            the expression is not evaluated. X is a static constant equal to True.

      -- The preceding "statically unevaluated" rule allows

      X2 : constant := (if True then 37 else (1 / 0));

      --      but does not allow

      function If_Then_Else (Flag : Boolean; X, Y : Integer) return Integer is (if Flag then X else Y) with
         Static; -- see 6.8
      --      X3 : constant := If_Then_Else (True, 37, 1 / 0);  --@@ MODIF PP: not yet available

      --      because evaluation of a function call includes evaluation of all of its actual parameters.

   end Section_4_9_Paragraph_37b;

   package Section_4_9_Paragraph_39b is

      X2 : Float := Float'(1.0E+30) + 1.0 - Float'(1.0E+30);   --@@ MODIF22 PP: error: static expression fails Constraint_Check (though it was legal)

   end Section_4_9_Paragraph_39b;

   package Section_4_9_Paragraph_41 is

      --  Examples of static expressions:

      --      1 + 1       -- 2
      --      abs(-10)*3  -- 30

      Kilo : constant := 1000;
      Mega : constant := Kilo * Kilo;   -- 1_000_000
      Long : constant := Float'Digits * 2;
      use Needed_To_Compile;

      Half_Pi    : constant := PI / 2;           -- see 3.3.2
      Deg_To_Rad : constant := Half_Pi / 90;
      Rad_To_Deg : constant := 1.0 / Deg_To_Rad; -- equivalent to 1.0/((3.14159_26536/2)/90)
      --           Bad: constant := 1/0; -- Illegal!

   end Section_4_9_Paragraph_41;

   --  4.9.1 Statically Matching Constraints and Subtypes

   --  4.10 Image Attributes

begin
   null;
end AARM_202x_CH04;
