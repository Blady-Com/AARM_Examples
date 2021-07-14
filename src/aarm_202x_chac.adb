with System;
with System.Atomic_Operations;
with Ada.Interrupts;

procedure AARM_202x_CHAC is

   --  Annex C (normative) Systems Programming

   --  C.3.2 The Package Interrupts

   package Section_C_3_2_Paragraph_27 is

      --     27  Example of interrupt handlers:

      Device_Priority : constant array (Ada.Interrupts.Interrupt_ID range 1 .. 5) of System.Interrupt_Priority :=
        (63, 63, 63, 63, 63);
      protected type Device_Interface
        (Int_Id : Ada.Interrupts.Interrupt_ID) -- with
--           Interrupt_Priority => Device_Priority (Int_Id)     --@@ MODIF25 PP: GNAT error: "Int_Id" is undefined

      is
         procedure Handler; -- with
--              Attach_Handler => Int_Id;    --  --@@ MODIF25 PP: GNAT error: entity for aspect "Attach_Handler" must be library level entity
         --@ ...
      end Device_Interface;
      --@ ...
      Device_1_Driver : Device_Interface (1);
      --@ ...
      Device_5_Driver : Device_Interface (5);
      --@ ...

   end Section_C_3_2_Paragraph_27;

   package body Section_C_3_2_Paragraph_27 is
      protected body Device_Interface is
         procedure Handler is null;
      end Device_Interface;
   end Section_C_3_2_Paragraph_27;

   --  C.6.1 The Package System.Atomic_Operations

   procedure Section_C_6_1_Paragraph_17 is

      --  17 Example of a spin lock using Atomic_Exchange:

      type Atomic_Boolean is new Boolean with
         Atomic;
--        package Exchange is new Atomic_Operations.Exchange (Atomic_Type => Atomic_Boolean);   --@@ MODIF PP: not yet available

      Lock : aliased Atomic_Boolean := False;

      --@ ...

   begin -- Some critical section, trying to get the lock:

      -- Obtain the lock
--        while Exchange.Atomic_Exchange (Item => Lock, Value => True) loop   --@@ MODIF PP: not yet available
--           null;
--        end loop;

      --@ ... -- Do stuff

      Lock := False; -- Release the lock
   end Section_C_6_1_Paragraph_17;

begin
   null;
end AARM_202x_CHAC;
