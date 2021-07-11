procedure AARM_202x_CHAE is

--  Annex E (normative) Distributed Systems

--     E.4.2 Example of Use of a Remote Access-to-Class-Wide Type

--  1   Example of using a remote access-to-class-wide type to achieve dynamic
--        binding across active partitions:

   package Tapes is
--@     with Pure is
      type Tape is abstract tagged limited private;
      -- Primitive dispatching operations where
      -- Tape is controlling operand
      procedure Copy (From, To : access Tape; Num_Recs : in Natural) is abstract;
      procedure Rewind (T : access Tape) is abstract;
      -- More operations
   private
      type Tape is abstract tagged limited null record; --@ ...
   end Tapes;

--         with Tapes;
   package Name_Server is
--@     with Remote_Call_Interface is
      -- Dynamic binding to remote operations is achieved
      -- using the access-to-limited-class-wide type Tape_Ptr
      type Tape_Ptr is access all Tapes.Tape'Class;
      -- The following statically bound remote operations
      -- allow for a name-server capability in this example
      function Find (Name : String) return Tape_Ptr;
      procedure Register (Name : in String; T : in Tape_Ptr);
      procedure Remove (T : in Tape_Ptr);
      -- More operations
   end Name_Server;

   package body Name_Server is
      -- Needed to compile, sometimes dummy
      function Find (Name : String) return Tape_Ptr is (null);
      procedure Register (Name : in String; T : in Tape_Ptr) is null;
      procedure Remove (T : in Tape_Ptr) is null;
   end Name_Server;

   package Tape_Driver is
   -- Declarations are not shown, they are irrelevant here
   end Tape_Driver;

--         with Tapes, Name_Server;
   package body Tape_Driver is
      type New_Tape is new Tapes.Tape with null record; --@ ...
      overriding procedure Rewind (T : access New_Tape);
      overriding procedure Copy (From, To : access New_Tape; Num_Recs : in Natural) is
      begin
         null; --@ . . .
      end Copy;
      procedure Rewind (T : access New_Tape) is
      begin
         null; --@ . . .
      end Rewind;
      -- Objects remotely accessible through use
      -- of Name_Server operations
      Tape1, Tape2 : aliased New_Tape;
   begin
      Name_Server.Register ("NINE-TRACK", Tape1'Access);
      Name_Server.Register ("SEVEN-TRACK", Tape2'Access);
   end Tape_Driver;

--         with Tapes, Name_Server;
   -- Tape_Driver is not needed and thus not mentioned in the with_clause
   procedure Tape_Client is
      T1, T2 : Name_Server.Tape_Ptr;
   begin
      T1 := Name_Server.Find ("NINE-TRACK");
      T2 := Name_Server.Find ("SEVEN-TRACK");
      Tapes.Rewind (T1);
      Tapes.Rewind (T2);
      Tapes.Copy (T1, T2, 3);
   end Tape_Client;

begin
   null;
end AARM_202x_CHAE;
