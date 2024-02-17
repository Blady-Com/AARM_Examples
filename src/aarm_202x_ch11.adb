with Ada.Text_IO, Ada.IO_Exceptions, Ada.Exceptions;
with GNAT.IO_Aux;

procedure AARM_202x_CH11 is

   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      type NTCT is range 0 .. 20;
   end Needed_To_Compile;
   use Needed_To_Compile;

   --                               11   Exceptions

   --  11.1 Exception Declarations

   --   Examples of user-defined exception declarations:

   Singular            : exception;
   Error               : exception;
   Overflow, Underflow : exception;

   --  11.2 Exception Handlers

   procedure Section_11_2_Paragraph_11 is
      use Ada.Text_IO;
      File : File_Type;

      --  Example of an exception handler:

   begin
      Open (File, In_File, "input.txt");   -- see A.8.2
   exception
      when E : Name_Error =>
         Put ("Cannot open input file : ");
         Put_Line (Ada.Exceptions.Exception_Message (E));  -- see 11.4.1
         raise;
   end Section_11_2_Paragraph_11;

   --  11.3 Raise Statements and Raise Expressions

   package Section_11_3_Paragraph_2a is
      function Func_Call return Boolean is (False);
      TBD_Error, Not_Valid_Error : exception;
      function Is_Valid (P : NTCT) return Boolean is (False);
      Param : NTCT;
      type Some_Tagged is tagged null record;
      type A_Tagged is new Some_Tagged with record
         Comp : Character;
      end record;
      type Some_Array is array (Positive range <>) of NTCT;
      function Func (Val : Integer) return Natural is (0);

      --           Obj1 : Boolean := Func_Call or else raise TBD_Error with Atomic; -- illegal
      Obj2 : Boolean := Func_Call or else (raise TBD_Error) with
         Atomic;
      Obj3 : Boolean := (Func_Call or else raise TBD_Error) with
         Atomic;
         --          Obj4 : Boolean := Func_Call or else (raise TBD_Error with Atomic); --@@ MODIF03 PP error: "Atomic" is undefined
         --                  Obj5 : Boolean := (Func_Call or else raise TBD_Error with Atomic); --@@ MODIF03 PP error: "Atomic" is undefined
      Pre : Boolean  := (if not Is_Valid (Param) then raise Not_Valid_Error);
      A   : A_Tagged := (Some_Tagged'(raise TBD_Error) with Comp => 'A');
      --                  B : Some_Array := (1, 2, 3, others => raise Not_Valid_Error); --@@ MODIF03b PP error: "others" choice not allowed here
      C : Natural  := Func (Val => raise TBD_Error);
      D : A_Tagged := ((raise TBD_Error) with Comp => 'A');

   end Section_11_3_Paragraph_2a;

   procedure Section_11_3_Paragraph_5 is
      Queue_Error : exception;
   begin
      --   Examples of raise statements:

      raise Ada.IO_Exceptions.Name_Error;   -- see A.13
      raise Queue_Error with "Buffer Full"; -- see 9.11
   exception
      when others =>
         raise;                                -- re-raise the current exception
         -- For an example of a raise expression, see the Streams Subsystem definitions in 13.13.1.
   end Section_11_3_Paragraph_5;

   --  11.4 Exception Handling

   --  11.4.1 The Package Exceptions

   --  11.4.2 Pragmas Assert and Assertion_Policy

   --  11.4.3 Example of Exception Handling

   package File_System is
      type Data_Type is new NTCT; --@ ...;
      type File_Handle is limited private;

      File_Not_Found : exception;
      procedure Open (F : in out File_Handle; Name : String);
      -- raises File_Not_Found if named file does not exist

      End_Of_File : exception;
      procedure Read (F : in out File_Handle; Data : out Data_Type);
      -- raises End_Of_File if the file is not open

      --@     ...
      procedure Close (F : in out File_Handle) is null;
   private
      --@ ...
      type File_Handle is limited record
         Current_Position, Last_Position : Natural;
      end record;
   end File_System;

   package body File_System is
      use GNAT.IO_Aux;
      --@ ...

      procedure Open (F : in out File_Handle; Name : String) is
      begin
         if File_Exists (Name) then
            null; --@ ...
         else
            raise File_Not_Found with "File not found: " & Name & ".";
         end if;
      end Open;

      procedure Read (F : in out File_Handle; Data : out Data_Type) is
      begin
         if F.Current_Position <= F.Last_Position then
            null; --@ ...
         else
            raise End_Of_File;
         end if;
      end Read;

      --@           ...

   end File_System;

   --@ with Ada.Text_IO;
   --@      with Ada.Exceptions;
   --@     with File_System; use File_System;
   use File_System;
   use Ada;
   procedure Main is
      Verbosity_Desired : Boolean := True; --@ ...;
      Some_File         : File_Handle;
   begin
      null; --@ ... -- call operations in File_System
   exception
      when End_Of_File =>
         Close (Some_File);
      when Not_Found_Error : File_Not_Found =>
         Text_IO.Put_Line (Exceptions.Exception_Message (Not_Found_Error));
      when The_Error : others =>
         Text_IO.Put_Line ("Unknown error:");
         if Verbosity_Desired then
            Text_IO.Put_Line (Exceptions.Exception_Information (The_Error));
         else
            Text_IO.Put_Line (Exceptions.Exception_Name (The_Error));
            Text_IO.Put_Line (Exceptions.Exception_Message (The_Error));
         end if;
         raise;
   end Main;

   --  11.5 Suppressing Checks

   --  11.6 Exceptions and Optimization

begin
   null;
end AARM_202x_CH11;
