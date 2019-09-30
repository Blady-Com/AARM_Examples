with Ada.Interrupts;
with Ada.Interrupts.Names;

procedure AARM_202x_CHAJ is

   --  Annex J (normative) Obsolescent Features

   --  J.7.1 Interrupt Entries

   --  22  Example of an interrupt entry:

   task Interrupt_Handler is
      entry Done;
      --@@ MODIF16 PP: error: "Device_Done" not declared in "Names"
      --            for Done'Address use Ada.Interrupts.Reference(Ada.Interrupts.Names.Device_Done);
   end Interrupt_Handler;

   task body Interrupt_Handler is
      -- Needed to compile, sometimes dummy
   begin
      null;
   end Interrupt_Handler;

begin
   null;
end AARM_202x_CHAJ;
