procedure AARM_202x_CHAF is

--  Annex F (normative) Information Systems

--  F.1 Machine_Radix Attribute Definition Clause

--     3   Example of Machine_Radix attribute definition clause:

   type Money is delta 0.01 digits 15;
   for Money'Machine_Radix use 10;

--  F.3.2 Edited Output Generation

--  71  In the result string values shown below, 'b' represents the space
--  character.
--
--  72      Item:         Picture and Result Strings:
--
--  73/3    {AI05-0248-1} 123456.78     Picture:  "-###**_***_**9.99"
--                        Result:   "bbb$***123,456.78"
--                                  "bbFF***123.456,78" (currency = "FF",
--                                                       separator = '.',
--                                                       radix mark = ',')
--
--  74/1    {8652/0089} {AI95-00070} 123456.78     Picture:  "-$**_***_**9.99"
--                        Result:   "b$***123,456.78"
--                                 "bFF***123.456,78" (currency = "FF",
--                                                     separator = '.',
--                                                     radix mark = ',')
--
--  75      0.0          Picture: "-$$$$$$.$$"
--                       Result:  "bbbbbbbbbb"
--
--  76      0.20         Picture: "-$$$$$$.$$"
--                       Result:  "bbbbbb$.20"
--
--  77      -1234.565    Picture: "<<<<_<<<.<<###>"
--                       Result:  "bb(1,234.57DMb)"  (currency = "DM")
--
--  78      12345.67     Picture: "###_###_##9.99"
--                       Result:  "bbCHF12,345.67"   (currency = "CHF")

begin
   null;
end AARM_202x_CHAF;
