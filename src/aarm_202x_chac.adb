with System;
with System.Atomic_Operations;
with Ada.Interrupts;

procedure AARM_202x_CHAC is

   --  Annex C (normative) Systems Programming

   --  C.3.2 The Package Interrupts

   package Section_C_3_2_Paragraph_27 is

      --     27  Example of interrupt handlers:

      Device_Priority : constant
        array (Ada.Interrupts.Interrupt_Id range 1..5) of
        System.Interrupt_Priority := (1, 2, 3, 4, 5);
      protected type Device_Interface
        (Int_Id : Ada.Interrupts.Interrupt_Id)
        with Interrupt_Priority => Device_Priority(Int_Id) is
         procedure Handler
           with Attach_Handler => Int_Id;
         --@ ...
      end Device_Interface;
      --@ ...
      Device_1_Driver : Device_Interface(1);
      --@ ...
      Device_5_Driver : Device_Interface(5);
      --@ ...

   end;

   package body Section_C_3_2_Paragraph_27 is
      protected body Device_Interface is
         procedure Handler is null;
      end;
   end;

   --  C.6.1 The Package System.Atomic_Operations

   procedure Section_C_6_1_Paragraph_17 is
      use System;

      --  17 Example of a spin lock using Atomic_Exchange:

      type Atomic_Boolean is new Boolean with Atomic;
      package Exchange is new
        Atomic_Operations.Exchange (Atomic_Type => Atomic_Boolean);

      Lock : aliased Atomic_Boolean := False;

      --@ ...

   begin -- Some critical section, trying to get the lock:

      -- Obtain the lock
      while Exchange.Atomic_Exchange (Item => Lock, Value => True) loop
         null;
      end loop;

      --@ ... -- Do stuff

      Lock := False; -- Release the lock
   end;

begin
   null;
end;
