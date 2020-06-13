with Ada.Finalization;
with Ada.Calendar;
with Ada.Text_IO;

procedure AARM_202x_CH09 is

   package Needed_To_Compile is
      type NTCT is range 0 .. 20;
      type Work_Item is new NTCT;
      type Keyboard_ID is (New_ID, TTY_ID, Term_ID);
      type Serial_Device is limited interface;
      type Level is range 1 .. 10;
      type Lexical_Element is new NTCT;
      type Parser_Action is new NTCT;
      type Item is new NTCT;
      type Index is new NTCT;
      type Item_Array is array (Index range <>) of Item;
      Null_Item : constant Item := 0;
      protected type Some_Other_Protected_Type is
         procedure Some_Op;
      end Some_Other_Protected_Type;
      type Month_Name is (Jan, Feb, Mar, Apr, May, Jun, July, Aug, Sep, Oct, Nov, Dec);
      type Gender is (M, F);
      type Date is record
         Day   : Integer range 1 .. 31;
         Month : Month_Name;
         Year  : Integer range 0 .. 4000;
      end record;
      type Person (<>);
      type Person_Name is access Person;
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
      package Queues is
         type Queue is limited interface;
         procedure Append (Q : in out Queue; Person : in Person_Name) is abstract;
         procedure Remove_First (Q : in out Queue; Person : out Person_Name) is abstract;
         function Cur_Count (Q : in Queue) return Natural is abstract;
         function Max_Count (Q : in Queue) return Natural is abstract;
         -- See 3.10.1 for Person_Name.
         procedure Transfer (From : in out Queue'Class; To : in out Queue'Class; Number : in Natural := 1);

         Queue_Error : exception;
         -- Append raises Queue_Error if Cur_Count(Q) = Max_Count(Q)
         -- Remove_First raises Queue_Error if Cur_Count(Q) = 0

         type Synchronized_Queue is synchronized interface and Queue; -- see 9.11
         procedure Append_Wait (Q : in out Synchronized_Queue; Person : in Person_Name) is abstract;
         procedure Remove_First_Wait (Q : in out Synchronized_Queue; Person : out Person_Name) is abstract;
      end Queues;
   end Needed_To_Compile;

   package body Needed_To_Compile is
      protected body Some_Other_Protected_Type is
         procedure Some_Op is
         begin
            null;
         end Some_Op;
      end Some_Other_Protected_Type;
      package body Queues is
         procedure Transfer (From : in out Queue'Class; To : in out Queue'Class; Number : in Natural := 1) is
            Person : Person_Name;
         begin
            for I in 1 .. Number loop
               Remove_First (From, Person);
               Append (To, Person);
            end loop;
         end Transfer;
      end Queues;
   end Needed_To_Compile;
   use Needed_To_Compile;

   --                         9   Tasks and Synchronization

   --  9.1 Task Units and Task Objects

   package Section_9_1_Paragraph_22 is

      --  Examples of declarations of task types:

      task type Server is
         entry Next_Work_Item (WI : in Work_Item);
         entry Shut_Down;
      end Server;

      task type Keyboard_Driver (ID : Keyboard_ID := New_ID) is new Serial_Device with  -- see 3.9.4
         entry Read (C : out Character);
         entry Write (C : in Character);
         entry Wait_For_Interrupt; --@ needed for §9.7.4
      end Keyboard_Driver;

      --  Examples of declarations of single tasks:

      task Controller is
         entry Request (Level) (D : Item);  --  a family of entries
      end Controller;

      task Parser is
         entry Next_Lexeme (L : in Lexical_Element);
         entry Next_Action (A : out Parser_Action);
      end Parser;

      task User;  --  has no entries

      --  Examples of task objects:

      Agent    : Server;
      Teletype : Keyboard_Driver (TTY_ID);
      Pool     : array (1 .. 10) of Keyboard_Driver;

      --  Example of access type designating task objects:

      type Keyboard is access Keyboard_Driver;
      Terminal : Keyboard := new Keyboard_Driver (Term_ID);
   end Section_9_1_Paragraph_22;

   package body Section_9_1_Paragraph_22 is
      -- Needed to compile, sometimes dummy
      task body Server is
      begin
         null;
      end Server;
      task body Keyboard_Driver is
      begin
         null;
      end Keyboard_Driver;
      task body Controller is
      begin
         null;
      end Controller;
      task body Parser is
      begin
         null;
      end Parser;
      task body User is
      begin
         null;
      end User;
   end Section_9_1_Paragraph_22;

   --  9.2 Task Execution - Task Activation

   --  Example of task activation:

   procedure P is
      use Section_9_1_Paragraph_22;
      A, B : Server;    --  elaborate the task objects A, B
      C    : Server;    --  elaborate the task object C
   begin
      --  the tasks A, B, C are activated together before the first statement
      null; --@         ...
   end P;

   --  9.3 Task Dependence - Termination of Tasks

   procedure Section_9_3_Paragraph_19 is
      use Section_9_1_Paragraph_22;

      --  Example of task dependence:
   begin
      declare
         type Global is access Server;        --  see 9.1
         A, B : Server;
         G    : Global;
      begin
         --  activation of A and B
         declare
            type Local is access Server;
            X : Global := new Server;  --  activation of X.all
            L : Local  := new Server;  --  activation of L.all
            C : Server;
         begin
            --  activation of C
            G := X;  --  both G and X designate the same task object
            --@  ...
         end;  --  await termination of C and L.all (but not X.all)
         --@  ...
      end;  --  await termination of A, B, and G.all
   end Section_9_3_Paragraph_19;

   --  9.4 Protected Units and Protected Objects

   procedure Main is
      task T is
         entry E;
      end T;

      task body T is
         protected PO is
            entry Ee;
         end PO;

         protected body PO is
            entry Ee when False is
            begin
               null;
            end Ee;
         end PO;
      begin
         accept E do
            requeue PO.Ee;
         end E;
      end T;
   begin
      T.E;
   end Main;

   --  The environment task is queued on PO.Ee when PO is finalized.

   procedure Section_9_4_Paragraph_20i is

      --          with Ada.Finalization.Controlled;
      package Window_Manager is
         --@ ...
         type Root_Window is new Ada.Finalization.Controlled with private;
         type Any_Window is access all Root_Window;
         --@ ...
      private
         type Root_Window is new Ada.Finalization.Controlled with null record;                    --@ ...
         procedure Finalize (Object : in out Root_Window);
         --@ ...
      end Window_Manager;

      package body Window_Manager is
         protected type Lock is
            entry Get_Lock;
            procedure Free_Lock;
            --@ ...
         end Lock;

         protected body Lock is
            entry Get_Lock when True is
            begin
               null;
            end Get_Lock;
            procedure Free_Lock is
            begin
               null;
            end Free_Lock;
            --@ ...
         end Lock;

         Window_Lock : Lock;

         procedure Finalize (Object : in out Root_Window) is
         begin
            Window_Lock.Get_Lock;
            --...
            Window_Lock.Free_Lock;
         end Finalize;
         --@ ...
         A_Window : Any_Window := new Root_Window;
      end Window_Manager;
   begin
      null;
   end Section_9_4_Paragraph_20i;

   package Section_9_4_Paragraph_26 is

      --  Example of declaration of protected type and corresponding body:

      protected type Resource is
         entry Seize;
         procedure Release;
      private
         Busy : Boolean := False;
      end Resource;
   end Section_9_4_Paragraph_26;

   package body Section_9_4_Paragraph_26 is
      protected body Resource is
         entry Seize when not Busy is
         begin
            Busy := True;
         end Seize;

         procedure Release is
         begin
            Busy := False;
         end Release;
      end Resource;
   end Section_9_4_Paragraph_26;

   package Section_9_4_Paragraph_30 is

      --  Example of a single protected declaration and corresponding body:

      protected Shared_Array is
         --  Index, Item, and Item_Array are global types
         function Component (N : in Index) return Item;
         procedure Set_Component (N : in Index; E : in Item);
      private
         Table : Item_Array (Index) := (others => Null_Item);
      end Shared_Array;
   end Section_9_4_Paragraph_30;

   package body Section_9_4_Paragraph_30 is
      protected body Shared_Array is
         function Component (N : in Index) return Item is
         begin
            return Table (N);
         end Component;

         procedure Set_Component (N : in Index; E : in Item) is
         begin
            Table (N) := E;
         end Set_Component;
      end Shared_Array;
   end Section_9_4_Paragraph_30;

   package Section_9_4_Paragraph_34 is
      use Section_9_4_Paragraph_26, Section_9_4_Paragraph_30;

      --  Examples of protected objects:

      Control : Resource;
      Flags   : array (1 .. 100) of Resource;

   end Section_9_4_Paragraph_34;

   --  9.5 Intertask Communication

   procedure Section_9_5_Paragraph_4b is

      protected type Pt is
         procedure Op1;
         procedure Op2;
      end Pt;

      PO           : Pt;
      Other_Object : Some_Other_Protected_Type;

      protected body Pt is
         procedure Op1 is
         begin
            null; --@ ...
         end Op1;

         procedure Op2 is
         begin
            Op1; -- An internal call.
            Pt.Op1; -- Another internal call.
            PO.Op1; -- An external call. It the current instance is PO, then
            -- this is a bounded error (see 9.5.1).
            Other_Object.Some_Op; -- An external call.
         end Op2;
      end Pt;

   begin
      null;
   end Section_9_5_Paragraph_4b;

   --  9.5.1 Protected Subprograms and Protected Actions

   procedure Section_9_5_1_Paragraph_24 is
      use Section_9_4_Paragraph_30, Section_9_4_Paragraph_34;
      N : constant Index := 9;
      M : constant Index := 5;
      E : Item           := Null_Item;

      --  Examples of protected subprogram calls (see 9.4):
   begin
      Shared_Array.Set_Component (N, E);
      E := Shared_Array.Component (M);
      Control.Release;
   end Section_9_5_1_Paragraph_24;

   --  9.5.2 Entries and Accept Statements

   procedure Section_9_5_2_Paragraph_13b is
   --              task T is
   --                     entry E(Z : access Integer); -- Illegal!
   --                  end T;
   --
   --              task body T is
   --                  begin
   --                     declare
   --                        type A is access all Integer;
   --                        X : A;
   --                        Int : aliased Integer;
   --                        task Inner;
   --                        task body Inner is
   --                        begin
   --                           T.E(Int'Access);
   --                        end Inner;
   --                     begin
   --                        accept E(Z : access Integer) do
   --                           X := A(Z); -- Accessibility_Check
   --                        end E;
   --                     end;
   --                  end T;

   begin
      null;
   end Section_9_5_2_Paragraph_13b;

   task Section_9_5_2_Paragraph_32 is
      --  Examples of entry declarations:

      entry Read (V : out Item);
      entry Seize;
      entry Request (Level) (D : Item);  --  a family of entries
      entry Shut_Down;

   end Section_9_5_2_Paragraph_32;

   task body Section_9_5_2_Paragraph_32 is
      Local_Item : Item;
      Low        : Level;
   begin
      --  Examples of accept statements:

      accept Shut_Down;

      accept Read (V : out Item) do
         V := Local_Item;
      end Read;

      accept Request (Low) (D : Item) do
         null; --@ ...
      end Request;

   end Section_9_5_2_Paragraph_32;

   --  9.5.3 Entry Calls

   procedure Section_9_5_3_Paragraph_30 is
      use Section_9_1_Paragraph_22, Section_9_4_Paragraph_34;
      E         : Lexical_Element;
      Next_Char : Character;
      Low       : Level;
      Some_Item : Item;
   begin

      --  Examples of entry calls:

      Agent.Shut_Down;                      --  see 9.1
      Parser.Next_Lexeme (E);                --  see 9.1
      Pool (5).Read (Next_Char);              --  see 9.1
      Controller.Request (Low) (Some_Item);   --  see 9.1
      Flags (3).Seize;                       --  see 9.4
   end Section_9_5_3_Paragraph_30;

   --  9.5.4 Requeue Statements

   task Section_9_5_4_Paragraph_18 is
      entry Start;
      entry Request (Level);
   end Section_9_5_4_Paragraph_18;
   task body Section_9_5_4_Paragraph_18 is
      use Section_9_4_Paragraph_34;
      Medium : Level;
      I      : Natural;
   begin
      accept Start do
         --  Examples of requeue statements:

         requeue Request (Medium) with abort;
         -- requeue on a member of an entry family of the current task, see 9.1

         requeue Flags (I).Seize;
         -- requeue on an entry of an array component, see 9.4
      end Start;
   end Section_9_5_4_Paragraph_18;

   --  9.6 Delay Statements, Duration, and Time

   procedure Section_9_6_Paragraph_37 is
      Period : Duration;
   begin

      --  Example of a relative delay statement:

      delay 3.0;  -- delay 3.0 seconds

      --  Example of a periodic task:

      declare
         use Ada.Calendar;
         Next_Time : Time := Clock + Period;
         -- Period is a global constant of type Duration
      begin
         loop               -- repeated every Period seconds
            delay until Next_Time;
            --@ ... -- perform some actions
            Next_Time := Next_Time + Period;
         end loop;
      end;
   end Section_9_6_Paragraph_37;

   --  9.7 Select Statements

   task Section_9_7_Paragraph_3 is
      entry Driver_Awake_Signal;
      entry Stop_The_Train;
   end Section_9_7_Paragraph_3;

   task body Section_9_7_Paragraph_3 is
      Seconds : Duration := 1.0;
   begin

      --   Example of a select statement:

      select
         accept Driver_Awake_Signal;
      or
         delay 30.0 * Seconds;
         Stop_The_Train;
      end select;
   end Section_9_7_Paragraph_3;

   --  9.7.1 Selective Accept

   procedure Section_9_7_1_Paragraph_23 is
      task Server is
         entry Next_Work_Item (WI : in Work_Item);
         entry Shut_Down;
      end Server;
      procedure Process_Work_Item (WI : in Work_Item) is null;

      --  Example of a task body with a selective accept:

      task body Server is
         Current_Work_Item : Work_Item;
      begin
         loop
            select
               accept Next_Work_Item (WI : in Work_Item) do
                  Current_Work_Item := WI;
               end Next_Work_Item;
               Process_Work_Item (Current_Work_Item);
            or
               accept Shut_Down;
               exit;       -- Premature shut down requested
            or
               terminate;  -- Normal shutdown at end of scope
            end select;
         end loop;
      end Server;

   begin
      null;
   end Section_9_7_1_Paragraph_23;

   --  9.7.2 Timed Entry Calls

   procedure Section_9_7_2_Paragraph_6 is
      use Section_9_1_Paragraph_22;
      Medium    : Level;
      Some_Item : Item;
   begin

      --   Example of a timed entry call:

      select
         Controller.Request (Medium) (Some_Item);
      or
         delay 45.0;
         --  controller too busy, try something else
      end select;
   end Section_9_7_2_Paragraph_6;

   --  9.7.3 Conditional Entry Calls
   use Section_9_4_Paragraph_26;

   --   Example of a conditional entry call:

   procedure Spin (R : in out Resource) is  -- see 9.4
   begin
      loop
         select
            R.Seize;
            return;
         else
            null;  --  busy waiting
         end select;
      end loop;
   end Spin;

   --  9.7.4 Asynchronous Transfer of Control

   procedure Section_9_7_4_Paragraph_10 is
      use Section_9_1_Paragraph_22, Ada.Text_IO;
      Command : String (1 .. 10);
      Last    : Natural;
      procedure Process_Command (C : String) is null;
      procedure Horribly_Complicated_Recursive_Function (X, Y : in out Float) is null;
      X, Y : Float;
   begin

      --  Example of a main command loop for a command interpreter:

      loop
         select
            Terminal.Wait_For_Interrupt;
            Put_Line ("Interrupted");
         then abort
            -- This will be abandoned upon terminal interrupt
            Put_Line ("-> ");
            Get_Line (Command, Last);
            Process_Command (Command (1 .. Last));
         end select;
      end loop;

      --  Example of a time-limited calculation:

      select
         delay 5.0;
         Put_Line ("Calculation does not converge");
      then abort
         -- This calculation should finish in 5.0 seconds;
         --  if not, it is assumed to diverge.
         Horribly_Complicated_Recursive_Function (X, Y);
      end select;
   end Section_9_7_4_Paragraph_10;

   --  9.8 Abort of a Task - Abort of a Sequence of Statements

   --  9.9 Task and Entry Attributes

   --  9.10 Shared Variables

   --  9.10.1 Conflict Check Policies

   --  9.11 Example of Tasking and Synchronization

   procedure Section_9_11_Paragraph_7_2 is
      use Queues;
      Queue_Error : exception;

      type Person_Name_Array is array (Positive range <>) of Person_Name; -- see 3.10.1

      protected Buffer is new Synchronized_Queue with  -- see 3.9.4
         entry Append_Wait (Person : in Person_Name);
         entry Remove_First_Wait (Person : out Person_Name);
         function Cur_Count return Natural;
         function Max_Count return Natural;
         procedure Append (Person : in Person_Name);
         procedure Remove_First (Person : out Person_Name);
      private
         Pool                : Person_Name_Array (1 .. 100);
         Count               : Natural  := 0;
         In_Index, Out_Index : Positive := 1;
      end Buffer;

      protected body Buffer is
         entry Append_Wait (Person : in Person_Name) when Count < Pool'Length is
         begin
            Append (Person);
         end Append_Wait;

         procedure Append (Person : in Person_Name) is
         begin
            if Count = Pool'Length then
               raise Queue_Error with "Buffer Full";  -- see 11.3
            end if;
            Pool (In_Index) := Person;
            In_Index        := (In_Index mod Pool'Length) + 1;
            Count           := Count + 1;
         end Append;

         entry Remove_First_Wait (Person : out Person_Name) when Count > 0 is
         begin
            Remove_First (Person);
         end Remove_First_Wait;

         procedure Remove_First (Person : out Person_Name) is
         begin
            if Count = 0 then
               raise Queue_Error with "Buffer Empty"; -- see 11.3
            end if;
            Person    := Pool (Out_Index);
            Out_Index := (Out_Index mod Pool'Length) + 1;
            Count     := Count - 1;
         end Remove_First;

         function Cur_Count return Natural is
         begin
            return Buffer.Count;
         end Cur_Count;

         function Max_Count return Natural is
         begin
            return Pool'Length;
         end Max_Count;
      end Buffer;
      task Producer;

      task body Producer is
         Person : Person_Name; -- see 3.10.1
      begin
         loop
            --@ ... --  simulate arrival of the next customer
            Buffer.Append_Wait (Person);
            exit when Person = null;
         end loop;
      end Producer;

      --   and the consuming task might have the following structure:

      task Consumer;

      task body Consumer is
         Person : Person_Name;
      begin
         loop
            Buffer.Remove_First_Wait (Person);
            exit when Person = null;
            --@ ... --  simulate serving a customer
         end loop;
      end Consumer;

   begin
      null;
   end Section_9_11_Paragraph_7_2;

begin
   null;
end AARM_202x_CH09;
