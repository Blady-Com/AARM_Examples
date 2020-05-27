with Ada.Interrupts;
with Ada.Interrupts.Names;

procedure AARM_202x_CHAJ is

   --  Annex J (normative) Obsolescent Features

   --  J.7.1 Interrupt Entries

   --  22  Example of an interrupt entry:

   task Interrupt_Handler is
      entry Done;
      --            for Done'Address use Ada.Interrupts.Reference(Ada.Interrupts.Names.Device_Done);
      --@@ Note (PP): implementation defined, GNAT: "Device_Done" not declared in "Names"
      --        for Done'Address use Ada.Interrupts.Reference(Ada.Interrupts.Names.SIGINT);
      --@@ MODIF18 PP error: invalid address clause for initialized object "Done"
      --@@ MODIF18 PP error: function "Reference" is not pure (RM 13.1(22))
   end Interrupt_Handler;

   task body Interrupt_Handler is
      -- Needed to compile, sometimes dummy
   begin
      null;
   end Interrupt_Handler;

begin
   null;
end AARM_202x_CHAJ;
