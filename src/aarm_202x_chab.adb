with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with Interfaces.COBOL;
with Interfaces.Fortran;
with Ada.Direct_IO;

procedure AARM_202x_CHAB is

   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      generic package COBOL_Sequential_IO renames Ada.Direct_IO;
   end Needed_To_Compile;
   use Needed_To_Compile;

   --  Annex B (normative) Interface to Other Languages

   --  B.1 Interfacing Aspects

   --     50 Example of interfacing aspects:

   package Fortran_Library is
      function Sqrt (X : Float) return Float with
         Import     => True,
         Convention => Fortran;
      type Matrix is array (Natural range <>, Natural range <>) of Float with
         Convention => Fortran;
      function Invert (M : Matrix) return Matrix with
         Import     => True,
         Convention => Fortran;
   end Fortran_Library;

   --  B.2 The Package Interfaces

   --  B.3 Interfacing with C and C++

   --  76  Example of using the Interfaces.C package:

   --Calling the C Library Function strcpy
   procedure Test is
      package C renames Interfaces.C;
      use type C.char_array;
      -- Call <string.h>strcpy:
      -- C definition of strcpy:  char *strcpy(char *s1, const char *s2);
      --    This function copies the string pointed to by s2 (including the terminating null character)
      --     into the array pointed to by s1. If copying takes place between objects that overlap,
      --     the behavior is undefined. The strcpy function returns the value of s1.

      -- Note: since the C function's return value is of no interest, the Ada interface is a procedure
      procedure Strcpy (Target : out C.char_array; Source : in C.char_array) with
         Import        => True,
         Convention    => C,
         External_Name => "strcpy";

      -- Call <sdtio.h>printf:
      -- C definition of printf:  int printf ( const char * format, ... );
      --    This function writes the C string pointed by format to the standard output (stdout).
      --     If format includes format specifiers (subsequences beginning with %), the additional
      --     arguments following format are formatted and inserted in the resulting string
      --     replacing their respective specifiers. If the number of arguments does not match
      --     the number of format specifiers, or if the types of the arguments do not match
      --     the corresponding format specifier, the behaviour is undefined. On success, the
      --     printf function returns the total number of characters written to the standard output.
      --     If a writing error occurs, a negative number is returned.

      -- Note: since the C function's return value is of no interest, the Ada interface is a procedure
      procedure Printf (Format : in C.char_array; Param1 : in C.char_array; Param2 : in C.int) with
         Import        => True,
         Convention    => C_Variadic_1,
         External_Name => "printf";

      Chars1 : C.char_array (1 .. 20);
      Chars2 : C.char_array (1 .. 20);

   begin
      Chars2 (1 .. 6) := "qwert" & C.nul;

      Strcpy (Chars1, Chars2);

      -- Now Chars1(1..6) = "qwert" & C.Nul

--        Printf("The String=%s, Length=%d", Chars1, Chars1'Length);  --@@ MODIF PP: not yet available
   end Test;

   --     B.3.1 The Package Interfaces.C.Strings

   --     B.3.2 The Generic Package Interfaces.C.Pointers

   --     45  Example of Interfaces.C.Pointers:

   procedure Test_Pointers is
      package C renames Interfaces.C;
      package Char_Ptrs is new C.Pointers
        (Index => C.size_t, Element => C.char, Element_Array => C.char_array, Default_Terminator => C.nul);

      use type Char_Ptrs.Pointer;
      subtype Char_Star is Char_Ptrs.Pointer;

      procedure Strcpy (Target_Ptr, Source_Ptr : Char_Star) is
         Target_Temp_Ptr : Char_Star := Target_Ptr;
         Source_Temp_Ptr : Char_Star := Source_Ptr;
         Element         : C.char;
      begin
         if Target_Temp_Ptr = null or Source_Temp_Ptr = null then
            raise C.Strings.Dereference_Error;
         end if;

         loop
            Element             := Source_Temp_Ptr.all;
            Target_Temp_Ptr.all := Element;
            exit when C."=" (Element, C.nul);
            Char_Ptrs.Increment (Target_Temp_Ptr);
            Char_Ptrs.Increment (Source_Temp_Ptr);
         end loop;
      end Strcpy;
   begin
      null; --@ ...
   end Test_Pointers;

   --     B.3.3 Unchecked Union Types

   package Section_B_3_3_Paragraph_31 is

      type T (Flag : Boolean := False) is record
         case Flag is
            when False =>
               F1 : Float := 0.0;
            when True =>
               F2 : Integer := 0;
         end case;
      end record with
         Unchecked_Union;

      X : T;
      Y : Integer := X.F2; -- erroneous

   end Section_B_3_3_Paragraph_31;

   --  B.4 Interfacing with COBOL

   --  101 Examples of Interfaces.COBOL:

   procedure Test_Call is

      -- Calling a foreign COBOL program
      -- Assume that a COBOL program PROG has the following declaration
      --  in its LINKAGE section:
      --  01 Parameter-Area
      --     05 NAME   PIC X(20).
      --     05 SSN    PIC X(9).
      --     05 SALARY PIC 99999V99 USAGE COMP.
      -- The effect of PROG is to update SALARY based on some algorithm

      package COBOL renames Interfaces.COBOL;

      type Salary_Type is delta 0.01 digits 7;

      type COBOL_Record is record
         Name   : COBOL.Numeric (1 .. 20);
         SSN    : COBOL.Numeric (1 .. 9);
         Salary : COBOL.Binary;  -- Assume Binary = 32 bits
      end record with
         Convention => COBOL;

      procedure Prog (Item : in out COBOL_Record) with
         Import     => True,
         Convention => COBOL;

      package Salary_Conversions is new COBOL.Decimal_Conversions (Salary_Type);

      Some_Salary : Salary_Type  := 12_345.67;
      Some_Record : COBOL_Record :=
        (Name => "Johnson, John       ", SSN => "111223333", Salary => Salary_Conversions.To_Binary (Some_Salary));

   begin
      Prog (Some_Record);
      --@ ...
   end Test_Call;

--          with COBOL_Sequential_IO; -- Assumed to be supplied by implementation
   procedure Test_External_Formats is

      -- Using data created by a COBOL program
      -- Assume that a COBOL program has created a sequential file with
      --  the following record structure, and that we need to
      --  process the records in an Ada program
      --  01 EMPLOYEE-RECORD
      --     05 NAME    PIC X(20).
      --     05 SSN     PIC X(9).
      --     05 SALARY  PIC 99999V99 USAGE COMP.
      --     05 ADJUST  PIC S999V999 SIGN LEADING SEPARATE.
      -- The COMP data is binary (32 bits), high-order byte first

      package COBOL renames Interfaces.COBOL;

      type Salary_Type is delta 0.01 digits 7;
      type Adjustments_Type is delta 0.001 digits 6;

      type COBOL_Employee_Record_Type is  -- External representation
      record
         Name   : COBOL.Alphanumeric (1 .. 20);
         SSN    : COBOL.Alphanumeric (1 .. 9);
         Salary : COBOL.Byte_Array (1 .. 4);
         Adjust : COBOL.Numeric (1 .. 7);  -- Sign and 6 digits
      end record with
         Convention => COBOL;

      package COBOL_Employee_IO is new COBOL_Sequential_IO (COBOL_Employee_Record_Type);
      use COBOL_Employee_IO;

      COBOL_File : File_Type;

      type Ada_Employee_Record_Type is  -- Internal representation
      record
         Name   : String (1 .. 20);
         SSN    : String (1 .. 9);
         Salary : Salary_Type;
         Adjust : Adjustments_Type;
      end record;

      COBOL_Record : COBOL_Employee_Record_Type;
      Ada_Record   : Ada_Employee_Record_Type;

      package Salary_Conversions is new COBOL.Decimal_Conversions (Salary_Type);
      use Salary_Conversions;

      package Adjustments_Conversions is new COBOL.Decimal_Conversions (Adjustments_Type);
      use Adjustments_Conversions;

   begin
      Open (COBOL_File, In_File, Name => "Some_File");

      loop
         Read (COBOL_File, COBOL_Record);

         Ada_Record.Name   := COBOL.To_Ada (COBOL_Record.Name);
         Ada_Record.SSN    := COBOL.To_Ada (COBOL_Record.SSN);
         Ada_Record.Salary := To_Decimal (COBOL_Record.Salary, COBOL.High_Order_First);
         Ada_Record.Adjust := To_Decimal (COBOL_Record.Adjust, COBOL.Leading_Separate);
         --@ ... -- Process Ada_Record
      end loop;
   exception
      when End_Error =>
         null; --@ ...
   end Test_External_Formats;

--  B.5 Interfacing with Fortran

--     29  Example of Interfaces.Fortran:

   use Interfaces.Fortran;
   procedure Ada_Application is

      type Fortran_Matrix is array (Fortran_Integer range <>, Fortran_Integer range <>) of Double_Precision with
         Convention => Fortran;                  -- stored in Fortran's
         -- column-major order
      procedure Invert (Rank : in Fortran_Integer; X : in out Fortran_Matrix) with
         Import     => True,
         Convention => Fortran; -- a Fortran subroutine

      Rank      : constant Fortran_Integer := 100;
      My_Matrix : Fortran_Matrix (1 .. Rank, 1 .. Rank);

      Precision : constant := 6;
      type Standard_Deviation is digits Precision with
         Convention => Fortran;
      Deviation : Standard_Deviation;
      -- Declarations to match the following Fortran declarations:
      --   integer, parameter :: precision = selected_real_kind(p=6)
      --   real(precision) :: deviation

   begin

         --@ ...
      My_Matrix := ((1.0, 1.0 / 2.0), (1.0 / 2.0, 1.0 / 3.0));  --@ ...;
           --@ ...
      Invert (Rank, My_Matrix);
           --@ ...

      Deviation := 9.9;  --@ ...;
           --@ ...

   end Ada_Application;

begin
   null;
end AARM_202x_CHAB;
