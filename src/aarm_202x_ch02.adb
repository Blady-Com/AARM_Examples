procedure AARM_202x_CH02 is

   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      function Exists (F : String) return Boolean is (F /= "");
      File_Name : constant String := "99";
   end Needed_To_Compile;
   use Needed_To_Compile;

--                            2   Lexical Elements

--  2.1 Character Set

--  2.2 Lexical Elements, Separators, and Delimiters

--  2.3 Identifiers

--          Count      X    Get_Symbol   Ethelyn   Marion
--          Snobol_4   X1   Page_Count   Store_Next_Item
--          <Unicode-928><Unicode-955><Unicode-940><Unicode-964><Unicode-969>
--          <Unicode-957>      -- Plato
--          <Unicode-1063><Unicode-1072><Unicode-1081><Unicode-1082>
--          <Unicode-1086><Unicode-1074><Unicode-1089><Unicode-1082>
--          <Unicode-1080><Unicode-1081>  -- Tchaikovsky
--          <Unicode-952>  <Unicode-966>        -- Angles

--  2.4 Numeric Literals

--  2.4.1 Decimal Literals

--          12        0      1E6    123_456    --  integer literals
--          12.0      0.0    0.456  3.14159_26 --  real literals

--  2.4.2 Based Literals

--          2#1111_1111#  16#FF#       016#0ff#   --  integer literals of value 255
--          16#E#E1       2#1110_0000#            --  integer literals of value 224
--          16#F.FF#E+2   2#1.1111_1111_1110#E11  --  real literals of value 4095.0

--  2.5 Character Literals

--          'A'     '*'     '''     ' '
--          'L'     '<Unicode-1051>'     '<Unicode-923>'    -- Various els.
--          '<Unicode-8734>'     '<Unicode-1488>
--          '            -- Big numbers - infinity and aleph.

--  2.6 String Literals

--          "Message of the day:"
--          ""                    --  a null string literal
--          " "   "A"   """"      --  three string literals of length 1
--          "Characters such as $, %, and } are allowed in string literals"
--          "Archimedes said ""<Unicode-917><Unicode-973><Unicode-961>
--          <Unicode-951><Unicode-954><Unicode-945>"""
--          "Volume of cylinder (PIr≤h) = "

--  2.7 Comments

   --  the last sentence above echoes the Algol 68 report
   procedure Line is
   begin
      null;
   end Line;  --  processing of Line is complete
   --  a long comment may be split onto
   --  two or more consecutive lines
   ----------------  the first two hyphens start the comment

--  2.8 Pragmas

   package Section_2_8_Paragraph_28 is
      pragma List (Off); -- turn off listing generation
      pragma Optimize (Off); -- turn off optional optimizations
        --@  pragma Pure(Rational_Numbers); -- set categorization for package
      pragma Assert (Exists (File_Name), Message => "Nonexistent file"); -- assert file exists
   end Section_2_8_Paragraph_28;

begin
   null;
end AARM_202x_CH02;
