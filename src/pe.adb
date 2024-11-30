with ByteFlip;

with Ada.Unchecked_Conversion;
with Ada.Calendar.Conversions;

with Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

package body PE is

   procedure Read_Unsigned_16_Little_Endian (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Unsigned_16_Little_Endian) is
      type U16_String is new String (1 .. 2);
      package U16_ByteFlip is new ByteFlip (Unsigned_16_Little_Endian);
      function U16S_To_U16LE is new Ada.Unchecked_Conversion (U16_String, Unsigned_16_Little_Endian);

      U16S : U16_String;
   begin
      U16_String'Read (Stream, U16S);
      Item := U16S_To_U16LE (U16S);
      U16_ByteFlip.Flip_Little_Endian_Bytes (Item);
   end Read_Unsigned_16_Little_Endian;

   procedure Read_Unsigned_32_Little_Endian (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Unsigned_32_Little_Endian) is
      type U32_String is new String (1 .. 4);
      package U32_ByteFlip is new ByteFlip (Unsigned_32_Little_Endian);
      function U32S_To_U32LE is new Ada.Unchecked_Conversion (U32_String, Unsigned_32_Little_Endian);

      U32S : U32_String;
   begin
      U32_String'Read (Stream, U32S);
      Item := U32S_To_U32LE (U32S);
      U32_ByteFlip.Flip_Little_Endian_Bytes (Item);
   end Read_Unsigned_32_Little_Endian;

   procedure Read_Machine_Type (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Machine_Types) is
      function U16LE_To_MT is new Ada.Unchecked_Conversion (Unsigned_16_Little_Endian, Machine_Types);
   begin
      Item := U16LE_To_MT (Unsigned_16_Little_Endian'Input (Stream));
   end Read_Machine_Type;

   procedure Read_MZ_DOS_Header (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out MZ_DOS_Header) is
   begin
      String'Read (Stream, Item.Magic);
      Unsigned_16_Little_Endian'Read (Stream, Item.Last_Page_Size);
      Unsigned_16_Little_Endian'Read (Stream, Item.Page_Count);
      Unsigned_16_Little_Endian'Read (Stream, Item.Relocation_Count);
      Unsigned_16_Little_Endian'Read (Stream, Item.Header_Size_Paragraphs);
      Unsigned_16_Little_Endian'Read (Stream, Item.Minimum_Extra_Paragraphs_Needed);
      Unsigned_16_Little_Endian'Read (Stream, Item.Maximum_Extra_Paragraphs_Needed);
      Unsigned_16_Little_Endian'Read (Stream, Item.Initial_SS_Value_Rel);
      Unsigned_16_Little_Endian'Read (Stream, Item.Initial_SP_Value);
      Unsigned_16_Little_Endian'Read (Stream, Item.Checksum);
      Unsigned_16_Little_Endian'Read (Stream, Item.Initial_IP_Value);
      Unsigned_16_Little_Endian'Read (Stream, Item.Initial_CS_Value_Rel);
      Unsigned_16_Little_Endian'Read (Stream, Item.Relocation_Table_Pointer);
      Unsigned_16_Little_Endian'Read (Stream, Item.Overlay_Number);
      Reserved_16'Read (Stream, Item.Reserved_1);
      Unsigned_16_Little_Endian'Read (Stream, Item.OEM_Identifier);
      Unsigned_16_Little_Endian'Read (Stream, Item.OEM_Reserved);
      Reserved_16'Read (Stream, Item.Reserved_2);
      Unsigned_16_Little_Endian'Read (Stream, Item.New_Header_Pointer);
   end Read_MZ_DOS_Header;

   procedure Read_COFF_Characteristics (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out COFF_Characteristics) is

      Raw : Unsigned_16_Little_Endian := Unsigned_16_Little_Endian'Input (Stream);

   begin
      for Characteristic in Item'First .. Item'Last loop

         Item (Characteristic) := (Raw and Unsigned_16_Little_Endian (Characteristic'Enum_Rep)) /= 0;

      end loop;
   end Read_COFF_Characteristics;

   procedure COFF_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : COFF_Characteristics) is
   begin
      Output.Put (ASCII.LF & "  (");
      for Characteristic in Value'First .. Value'Last loop
         if Characteristic /= Value'First then
            Output.Put (',' & ASCII.LF & "   ");
         end if;
         Output.Put (Characteristic'Image & " => " & Value (Characteristic)'Image);
      end loop;
      Output.Put (")");
   end COFF_Put_Image;

   function Read_Object_Portable_Executable (File : Ada.Streams.Stream_IO.File_Type; Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Object_Portable_Executable
   is

      New_Object : Object_Portable_Executable;

      discard_2 : String (1 .. 2);
      discard   : String (1 .. 10);

      use type Ada.Streams.Stream_IO.Positive_Count;

   begin
      MZ_DOS_Header'Read (Stream, New_Object.DOS_Header);
      File.Set_Index (Ada.Streams.Stream_IO.Positive_Count (New_Object.DOS_Header.New_Header_Pointer + 1));
      String'Read (Stream, New_Object.Magic);
      Machine_Types'Read (Stream, New_Object.Machine_Type);
      String'Read (Stream, discard_2);
      New_Object.Time_Stamp := Ada.Calendar.Conversions.To_Ada_Time (Interfaces.C.long (Unsigned_32_Little_Endian'Input (Stream)));
      Put_Line (Image (New_Object.Time_Stamp));
      String'Read (Stream, discard);
      COFF_Characteristics'Read (Stream, New_Object.Characteristics);
      return New_Object;
   end Read_Object_Portable_Executable;

end PE;
