with Ada.Finalization;
procedure AARM_2012_CH04 is
   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      --        K : constant := 99;
      --        G : constant Character := '@';
      --        type Coordinate is (X, Y);
      type Real is digits 8;
      type Matrix is array (Integer range <>, Integer range <>) of Real;
      --        subtype Month_Name is String (1 .. 3);
      --        type Gender is (M, F);
      --        type Date is record
      --           Day   : Integer range 1 .. 31;
      --           Month : Month_Name;
      --           Year  : Integer range 0 .. 4000;
      --        end record;
      --        type Person (<>);
      --        type Person_Name is access Person;
      --        type Person (Sex : Gender) is record
      --           Name  : String (1 .. 20);
      --           Birth : Date;
      --           Age   : Integer range 0 .. 130;
      --           case Sex is
      --              when M =>
      --                 Wife : Person_Name (Sex => F);
      --              when F =>
      --                 Husband : Person_Name (Sex => M);
      --           end case;
      --        end record;
      --        type Binop_Ptr is access all Integer;
      type Bit_Vector is array (Integer range <>) of Boolean;
      type Table is array (1 .. 10) of Integer;
      M  : constant := 99;
      N  : constant := 199;
      PI : constant := 3.14;
      --        type Day is (
      --           Mon,
      --           Tue,
      --           Wed,
      --           Thu,
      --           Fri,
      --           Sat,
      --           Sun);
      --        package Key_Manager is
      --           subtype Key is Integer;
      --        end Key_Manager;
--        Max       : constant := 500;                   -- an integer number
      --        Page_Size : constant := 100;
      --        subtype Cylinder_Index is Natural;
      --        subtype Track_Number is Natural;
      --        function Dispersion (X : Real) return Real;
   end Needed_To_Compile;
   --     package body Needed_To_Compile is
   --        function Dispersion (X : Real) return Real is
   --        begin
   --           return e * X;
   --        end Dispersion;
   --     end Needed_To_Compile;

   -- 4.1 Names
   --4.1.5 User-Defined References
   package Parapraph_4_1_5_9 is

      type Element is new Integer;
      type Barrel is tagged record  -- holds objects of type Element
         E : aliased Element;
      end record;

      type Ref_Element (Data : access Element) is limited private -- MODIF PP
      --          new Ada.Finalization.Limited_Controlled with private -- MODIF PP
         with
         Implicit_Dereference => Data;
         -- This Ref_Element type is a "reference" type.
         -- "Data" is its reference discriminant.

      function Find (B : aliased in out Barrel; Key : String) return Ref_Element;
      -- Return a reference to an element of a barrel.
      B : aliased Barrel; -- MODIF PP
   private
      type Ref_Element (Data : access Element) is null record;
      --with Implicit_Dereference => Data;
   end Parapraph_4_1_5_9;
   package body Parapraph_4_1_5_9 is
      function Find (B : aliased in out Barrel; Key : String) return Ref_Element is
      begin
         return R : Ref_Element (B.E'Access);
      end Find;

   begin
      Find (B, "grape")          := Element'(0);  -- Assign through a reference.
      Find (B, "grape").Data.all := Element'(1);
   end Parapraph_4_1_5_9;

   -- 4.1.6 User-Defined Indexing
   package Parapraph_4_1_6_18 is

      type Element is new Integer;
      type Indexed_Barrel is tagged record
         E : aliased Element;
      end record with
         Variable_Indexing => Find;
         -- Indexed_Barrel is an indexable container type,
         -- Find is the generalized indexing operation.

      type Ref_Element (Data : access Element) is limited private
         --          new Ada.Finalization.Limited_Controlled with private -- MODIF PP
          with
         Implicit_Dereference => Data;
         -- This Ref_Element type is a "reference" type.
         -- "Data" is its reference discriminant.
      function Find (B : aliased in out Indexed_Barrel; Key : String) return Ref_Element;
      -- Return a reference to an element of a barrel (see 4.1.5).
   private
      type Ref_Element (Data : access Element) is null record;
      --with Implicit_Dereference => Data;
   end Parapraph_4_1_6_18;
   package body Parapraph_4_1_6_18 is
      function Find (B : aliased in out Indexed_Barrel; Key : String) return Ref_Element is
      begin
         return R : Ref_Element (B.E'Access);
      end Find;
      IB : aliased Indexed_Barrel;
   begin
      -- All of the following calls are then equivalent:
      Find (IB, "pear").Data.all := Element'(2); -- Traditional call
      IB.Find ("pear").Data.all  := Element'(3); -- Call of prefixed view
      IB.Find ("pear")           := Element'(4); -- Implicit dereference (see 4.1.5)
      IB ("pear")                := Element'(5); -- Implicit indexing and dereference
      IB ("pear").Data.all       := Element'(6); -- Implicit indexing only
   end Parapraph_4_1_6_18;
   -- 4.3.3 Array Aggregates
   package Parapraph_4_3_3_42 is
      use Needed_To_Compile;
      A : Table           := (7, 9, 5, 1, 3, 2, 4, 8, 6, 0);        -- A(1)=7, A(10)=0
      B : Table           := (2 | 4 | 10 => 1, others => 0);        -- B(1)=0, B(10)=1
      C : constant Matrix := (1 .. 5 => (1 .. 8 => 0.0)); -- C'Last(1)=5, C'Last(2)=8

      D : Bit_Vector (M .. N) := (M .. N => True);         -- see 3.6
      E : Bit_Vector (M .. N) := (others => True);
      F : String (1 .. 1)     := (1 => 'F');  -- a one component aggregate: same as "F"
   end Parapraph_4_3_3_42;

   -- 4.5.7 Conditional Expressions
   package Parapraph_4_5_7_19 is
      generic
         with function Int_Func return Integer;
      package G is
         X : Float := (case Int_Func is when Integer'First .. -1 => -1.0, when 0 => 0.0, when Positive => 1.0);
      end G;

      function Nat_Func return Natural is (123);

--           package I is new G (Int_Func => Nat_Func); -- Legal -- MODIF PP
   end Parapraph_4_5_7_19;
   -- 4.5.8 Quantified Expressions
   package Parapraph_4_5_8_13 is
      use Needed_To_Compile;
      pragma Assert (for some X in 2 .. N / 2 => N mod X = 0);
      -- 4.6 Type Conversions
      -- 70
      type Sequence is array (Integer range <>) of Integer;
      subtype Dozen is Sequence (1 .. 12);
      Ledger : array (1 .. 100) of Integer;
      -- 4.7 Qualified Expressions

   end Parapraph_4_5_8_13;
   -- 4.9 Static Expressions and Static Subtypes
   procedure Parapraph_4_9_26_37_39_43_44 is

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
      N    : constant         := 0.0;
      X1   : constant Boolean := (N = 0.0) or else (1.0 / N > 0.5); -- Static.
      X2   : Float            := Float'(1.0E+32) + 1.0 - Float'(1.0E+32); -- MODIF PP was E+400 overflow GNAT FLoat
      Kilo : constant         := 1000;
      Mega : constant         := Kilo * Kilo;   -- 1_000_000
      Long : constant         := Float'Digits * 2;
      use Needed_To_Compile;

      Half_Pi    : constant := PI / 2;           -- see 3.3.2
      Deg_To_Rad : constant := Half_Pi / 90;
      Rad_To_Deg : constant := 1.0 / Deg_To_Rad; -- equivalent to 1.0/((3.14159_26536/2)/90)
   --           Bad: constant := 1/0; -- Illegal!
   begin
      null;
   end Parapraph_4_9_26_37_39_43_44;
begin
   null;
end AARM_2012_CH04;
