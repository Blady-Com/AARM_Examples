with Ada.Text_IO;

procedure AARM_202x_CH10 is

   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      type NTCT is range 0 .. 20;
   end Needed_To_Compile;
   use Needed_To_Compile;

   --                  10   Program Structure and Compilation Issues

   --  10.1 Separate Compilation

   --  10.1.1 Compilation Units - Library Units

   procedure Section_10_1_1_Paragraph_29 is

      --  Examples of library units:

      --        package Rational_Numbers.IO is  -- public child of Rational_Numbers, see 7.1
      --             procedure Put(R : in  Rational);
      --             procedure Get(R : out Rational);
      --          end Rational_Numbers.IO;
      --
      --        private procedure Rational_Numbers.Reduce(R : in out Rational);
      -- private child of Rational_Numbers

      --@      with Rational_Numbers.Reduce;   -- refer to a private child
      --          package body Rational_Numbers is
      --             --@  ...
      --          end Rational_Numbers;

      --@     with Rational_Numbers.IO; use Rational_Numbers;
      --@      with Ada.Text_io;               -- see A.10
      --          procedure Main is               -- a root library procedure
      --             R : Rational;
      --          begin
      --             R := 5/3;                    -- construct a rational number, see 7.1
      --             Ada.Text_IO.Put("The answer is: ");
      --             IO.Put(R);
      --             Ada.Text_IO.New_Line;
      --          end Main;

      --@      with Rational_Numbers.IO;
      --          package Rational_IO renames Rational_Numbers.IO;
      -- a library unit renaming declaration

      --  Each of the above library_items can be submitted to the compiler separately.

      --        Discussion: Example of a generic package with children:

      generic
         type Element is private;
         with function Image (E : Element) return String;
      package Generic_Bags is
         type Bag is limited private; -- A bag of Elements.
         procedure Add (B : in out Bag; E : Element);
         function Bag_Image (B : Bag) return String;
      private
         type Bag is new NTCT; --@ ...;
      end Generic_Bags;

      --              generic
      --                  package Generic_Bags.Generic_Iterators is
      --                      ... -- various additional operations on Bags.
      --                 generic
      --                        with procedure Use_Element(E : in Element);
      --                           -- Called once per bag element.
      --                     procedure Iterate(B : in Bag);
      --                  end Generic_Bags.Generic_Iterators;

      --        A package that instantiates the above generic units:

      --@   with Generic_Bags;
      --@       with Generic_Bags.Generic_Iterators;
      --                  package My_Abstraction is
      --                      type My_Type is new NTCT; --@ ...;
      --                      function Image(X : My_Type) return String;
      --                      package Bags_Of_My_Type is new Generic_Bags(My_Type, Image);
      --                      package Iterators_Of_Bags_Of_My_Type is new Bags_Of_My_Type.Generic_Iterators;
      --                  end My_Abstraction;

      --        In the above example, Bags_Of_My_Type has a nested generic unit
      --          called Generic_Iterators. The second with_clause makes that nested
      --        unit visible.

      --        Here we show how the generic body could depend on one of its own
      --        children:

      --@            with Generic_Bags.Generic_Iterators;
      package body Generic_Bags is
         procedure Add (B : in out Bag; E : Element) is
         begin
            null; --@ ...
         end Add;

         --                 package Iters is new Generic_Iterators;

         function Bag_Image (B : Bag) return String is
            Buffer : String (1 .. 10_000);
            Last   : Integer := 0;

            procedure Append_Image (E : in Element) is
               Im : constant String := Image (E);
            begin
               if Last /= 0 then -- Insert a comma.
                  Last          := Last + 1;
                  Buffer (Last) := ',';
               end if;
               Buffer (Last + 1 .. Last + Im'Length) := Im;
               Last                                  := Last + Im'Length;
            end Append_Image;

            --                    procedure Append_All is new Iters.Iterate(Append_Image);
         begin
            --                        Append_All(B);
            return Buffer (1 .. Last);
         end Bag_Image;
      end Generic_Bags;
   begin
      null;
   end Section_10_1_1_Paragraph_29;

   --  10.1.2 Context Clauses - With Clauses

   package Section_10_1_2_Paragraph_11d is

      package A is
      end A;

      --              package A.B is
      --                  end A.B;
      --
      --              private package A.B.C is
      --                  end A.B.C;
      --
      --              package A.B.C.D is
      --                  end A.B.C.D;

      --@            with A.B.C; -- (1)
      --                  private package A.B.X is
      --                  end A.B.X;
      --
      --              package A.B.Y is
      --                  end A.B.Y;

      --@            with A.B.C; -- (2)
      --                  package body A.B.Y is
      --                  end A.B.Y;

      --@        private with A.B.C; -- (3)
      --                  package A.B.Z is
      --                  end A.B.Z;
   end Section_10_1_2_Paragraph_11d;

   package Section_10_1_2_Paragraph_16d is

      package A is
         function B return Integer is (0);
      end A;

      function B return Integer is (0);

      --@          with A;
      --@         private with B;
      package C is
         use A;
         V1 : Integer := B; -- (1)
      private
         V2 : Integer := B; -- (2)
      end C;
   end Section_10_1_2_Paragraph_16d;

   package Section_10_1_2_Paragraph_24 is

      -- Examples of use of with clauses, limited with clauses, and private with clauses:

      package Office is
      end Office;

      --@ with Ada.Strings.Unbounded;
      --          package Office.Locations is
      --             type Location is new Ada.Strings.Unbounded.Unbounded_String;
      --          end Office.Locations;

      --@ limited with Office.Departments;  -- types are incomplete
      --@ private with Office.Locations;    -- only visible in private part
      --          package Office.Employees is
      --             type Employee is private;
      --
      --         function Dept_Of(Emp : Employee) return access Departments.Department;
      --             procedure Assign_Dept(Emp  : in out Employee;
      --                                   Dept : access Departments.Department);
      --
      --  --@       ...
      --          private
      --             type Employee is
      --                record
      --                   Dept : access Departments.Department;
      --                   Loc : Locations.Location;
      --                   --...
      --                end record;
      --          end Office.Employees;

      --@ limited with Office.Employees;
      --          package Office.Departments is
      --             type Department is new NTCT; --@ ...;
      --
      --         function Manager_Of(Dept : Department) return access Employees.Employee;
      --             procedure Assign_Manager(Dept : in out Department;
      --                                      Mgr  : access Employees.Employee);
      --             --@ ...
      --          end Office.Departments;
   end Section_10_1_2_Paragraph_24;

   --  10.1.3 Subunits of Compilation Units

   procedure Section_10_1_3_Paragraph_19 is

      -- Example that defines package Parent without subunits:

      package Parent is
         procedure Inner;
      end Parent;

      -- Example showing how the body of procedure Inner may be
      -- turned into a subunit by rewriting the package body as follows (with the
      -- declaration of Parent remaining the same):

      --@      with Ada.Text_IO;
      package body Parent is
         Variable : String := "Hello, there.";
         procedure Inner is
         begin
            Ada.Text_IO.Put_Line (Variable);
         end Inner;
      end Parent;

      --  The body of procedure Inner may be turned into a subunit by rewriting the
      --  package body as follows (with the declaration of Parent remaining the same):

      --        package body Parent is
      --              Variable : String := "Hello, there.";
      --              procedure Inner is separate;
      --          end Parent;

      --@      with Ada.Text_IO;
      --@        separate(Parent)
      --          procedure Inner is
      --          begin
      --              Ada.Text_IO.Put_Line(Variable);
      --          end Inner;

   begin
      null;
   end Section_10_1_3_Paragraph_19;

   --  10.1.4 The Compilation Process

   --  10.1.5 Pragmas and Program Units

   --  10.1.6 Environment-Level Visibility Rules

   --  10.2 Program Execution

   -- 10.2.1 Elaboration Control

begin
   null;
end AARM_202x_CH10;
