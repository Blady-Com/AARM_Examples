with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;

procedure AARM_202x_CHAA is

   package Needed_To_Compile is
      -- Needed to compile, sometimes dummy
      type Real is digits 8;
   end Needed_To_Compile;
   use Needed_To_Compile;

--  Annex A (normative)  Predefined Language Environment

--     A.5.2 Random Number Generation

--     55  Example of a program that plays a simulated dice game:

   procedure Dice_Game is
      subtype Die is Integer range 1 .. 6;
      subtype Dice is Integer range 2 * Die'First .. 2 * Die'Last;
      package Random_Die is new Ada.Numerics.Discrete_Random (Die);
      use Random_Die;
      G : Generator;
      D : Dice;
   begin
      Reset (G);  -- Start the generator in a unique state in each run
      loop
         -- Roll a pair of dice; sum and process the results
         D := Random (G) + Random (G);
              --@ ...
      end loop;
   end Dice_Game;

--  57  Example of a program that simulates coin tosses:

   procedure Flip_A_Coin is
      type Coin is (Heads, Tails);
      package Random_Coin is new Ada.Numerics.Discrete_Random (Coin);
      use Random_Coin;
      G : Generator;
   begin
      Reset (G);  -- Start the generator in a unique state in each run
      loop
         -- Toss a coin and process the result
         case Random (G) is
            when Heads =>
               null; --@ ...
            when Tails =>
               null; --@ ...
         end case;
           --@ ...
      end loop;
   end Flip_A_Coin;

--  59  Example of a parallel simulation of a physical system, with a separate
--        generator of event probabilities in each task:

   procedure Parallel_Simulation is
      use Ada.Numerics.Float_Random;
      task type Worker is
         entry Initialize_Generator (Initiator : in Integer);
              --@ ...
      end Worker;
      W : array (1 .. 10) of Worker;
      task body Worker is
         G                    : Generator;
         Probability_Of_Event : Uniformly_Distributed;
      begin
         accept Initialize_Generator (Initiator : in Integer) do
            Reset (G, Initiator);
         end Initialize_Generator;
         loop
                 --@ ...
            Probability_Of_Event := Random (G);
                 --@ ...
         end loop;
      end Worker;
   begin
      -- Initialize the generators in the Worker tasks to different states
      for I in W'Range loop
         W (I).Initialize_Generator (I);
      end loop;
           --@ ... -- Wait for the Worker tasks to terminate
   end Parallel_Simulation;

--  A.10.6 Get and Put Procedures

   procedure Section_A_10_6_Paragraph_11 is
      use Ada.Integer_Text_IO;

      --  11  In the examples, here and in subclauses A.10.8 and A.10.9, the string
--        quotes and the lower case letter b are not transferred: they are shown only to
--        reveal the layout and spaces.

      N : Integer;
      --@ ...

   begin
      Get (N);

      --                        Characters at input  Sequence input Value of N
      --                        bb-12535b           -12535  -12535
      --                        bb12_535e1b         12_535e1  125350
      --                        bb12_535e;          12_535e  (none) Data_Error raised

--  14  Example of overridden width parameter:

      Put (Item => -23, Width => 2);  --  "-23"
   end Section_A_10_6_Paragraph_11;

--  A.10.8 Input-Output for Integer Types

   procedure Section_A_10_8_Paragraph_26 is
      use Ada.Text_IO;

      -- Examples of use of an instantiation of Text_IO.Integer_IO:

      subtype Byte_Int is Integer range -127 .. 127;
      package Int_IO is new Integer_IO (Byte_Int);
      use Int_IO;
      -- default format used at instantiation,
      -- Default_Width = 4, Default_Base = 10
   begin

      Put (126);                            -- "b126"
      Put (-126, 7);                        -- "bbb-126"
      Put (126, Width => 13, Base => 2);    -- "bbb2#1111110#"

   end Section_A_10_8_Paragraph_26;

--  A.10.9 Input-Output for Real Types

   procedure Section_A_10_9_Paragraph_41 is
      use Ada.Text_IO;

      -- Examples of use of an instantiation of Text_IO.Float_IO:

      package Real_IO is new Float_IO (Real);
      use Real_IO;

      -- default format used at instantiation, Default_Exp = 3

      X : Real := -123.4567;  --  digits 8      (see 3.5.7)

   begin

      Put (X);  -- default format                                   "-1.2345670E+02"
      Put (X, Fore => 5, Aft => 3, Exp => 2);                       -- "bbb-1.235E+2"
      Put (X, 5, 3, 0);                                             -- "b-123.457"

   end Section_A_10_9_Paragraph_41;

--  A.18.33 Example of Container Use

   procedure Section_A_18_33_Paragraph_1 is

--        The following example is an implementation of Dijkstra's
--  shortest path algorithm in a directed graph with positive distances. The graph
--  is represented by a map from nodes to sets of edges.

      use Ada.Containers;
      generic
         type Node is range <>;
      package Shortest_Paths is
         type Distance is new Float range 0.0 .. Float'Last;
         type Edge is record
            To, From : Node;
            Length   : Distance;
         end record;

         package Node_Maps is new Vectors (Node, Node);
         -- The algorithm builds a map to indicate the node used to reach a given
         -- node in the shortest distance.

         package Adjacency_Lists is new Doubly_Linked_Lists (Edge);
         use Adjacency_Lists;

         package Graphs is new Vectors (Node, Adjacency_Lists.List);

         package Paths is new Doubly_Linked_Lists (Node);

         function Shortest_Path (G : Graphs.Vector; Source : Node; Target : Node) return Paths.List with
            Pre => G (Source) /= Adjacency_Lists.Empty_List;

      end Shortest_Paths;

      package body Shortest_Paths is
         function Shortest_Path (G : Graphs.Vector; Source : Node; Target : Node) return Paths.List is
            use Node_Maps, Paths, Graphs;
            Reached : array (Node) of Boolean := (others => False);
            -- The set of nodes whose shortest distance to the source is known.

            Reached_From     : array (Node) of Node;
            So_Far           : array (Node) of Distance := (others => Distance'Last);
            The_Path         : Paths.List               := Paths.Empty_List;
            Nearest_Distance : Distance;
            Next             : Node;
         begin
            So_Far (Source) := 0.0;

            while not Reached (Target) loop
               Nearest_Distance := Distance'Last;

               -- Find closest node not reached yet, by iterating over all nodes.
               -- A more efficient algorithm uses a priority queue for this step.

               Next := Source;
               for N in Node'First .. Node'Last loop
                  if not Reached (N) and then So_Far (N) < Nearest_Distance then
                     Next             := N;
                     Nearest_Distance := So_Far (N);
                  end if;
               end loop;

               if Nearest_Distance = Distance'Last then
                  -- No next node found, graph is not connected
                  return Paths.Empty_List;

               else
                  Reached (Next) := True;
               end if;

               -- Update minimum distance to newly reachable nodes.

               for E of G (Next) loop
                  if not Reached (E.To) then
                     Nearest_Distance := E.Length + So_Far (Next);

                     if Nearest_Distance < So_Far (E.To) then
                        Reached_From (E.To) := Next;
                        So_Far (E.To)       := Nearest_Distance;
                     end if;
                  end if;
               end loop;
            end loop;

            -- Rebuild path from target to source.

            declare
               N : Node := Target;
            begin
               Prepend (The_Path, N);
               while N /= Source loop
                  N := Reached_From (N);
                  Prepend (The_Path, N);
               end loop;
            end;

            return The_Path;
         end Shortest_Path;
      end Shortest_Paths;

      type R_100 is range 1 .. 100;
      package SP_100 is new Shortest_Paths (R_100);
      use SP_100;
      G       : Graphs.Vector;
      L       : Adjacency_Lists.List;
      Next    : R_100;
      Reached : constant array (R_100) of Boolean := (others => False);

   begin
-- Note that the effect of the Constant_Indexing aspect (on
-- type Vector) and the Implicit_Dereference aspect (on type Reference_Type) is that

      L := G (Next);

-- is a convenient shorthand for

      L := G.Constant_Reference (Next).Element.all;

-- Similarly, the effect of the loop:

      for E of G (Next) loop
         if not Reached (E.To) then
            null; --@ ...
         end if;
      end loop;

-- is the same as:

      for C in G (Next).Iterate loop
         declare
            E : Edge renames G (Next) (C);
         begin
            if not Reached (E.To) then
               null; --@ ...
            end if;
         end;
      end loop;

-- which is the same as:

      declare
         L : Adjacency_Lists.List renames G (Next);
         C : Adjacency_Lists.Cursor := L.First;
         use Adjacency_Lists;
      begin
         while Has_Element (C) loop
            declare
               E : Edge renames L (C);
            begin
               if not Reached (E.To) then
                  null; --@ ...
               end if;
            end;
--                C := L.Next (C); --@@ MODIF PP: not yet available
         end loop;
      end;
   end Section_A_18_33_Paragraph_1;

begin
   null;
end AARM_202x_CHAA;
