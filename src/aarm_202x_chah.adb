with Ada.Streams;

procedure AARM_202x_CHAH is

   type My_Integer is new Integer;

--  Annex H (normative) High Integrity Systems

   -- H.7.1 The Use_Formal and Dispatching Aspects

   -- An example of use of the Dispatching aspect:

   procedure My_Write   --  see 13.13.2
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : My_Integer'Base) is null
     with Dispatching => Write(Stream);
   for My_Integer'Write use My_Write;

-- For examples of use of the Use_Formal aspect, see the
-- Element functions of Hashed_Sets in A.18.8.

begin
   null;
end AARM_202x_CHAH;
