with ByteFlip;

with Ada.Unchecked_Conversion;
with Ada.Calendar.Conversions;

with Interfaces.C;

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

package body PE is

   procedure Unsigned_8_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : Unsigned_8) is
      As_Image : String := Integer (Value)'Image;
   begin
      Output.Put (As_Image (2 .. As_Image'Last));
   end Unsigned_8_Put_Image;

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

   procedure Unsigned_16_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : Unsigned_16_Little_Endian) is
      As_Image : String := Integer (Value)'Image;
   begin
      Output.Put (As_Image (2 .. As_Image'Last));
   end Unsigned_16_Put_Image;

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

   procedure Read_Unsigned_64_Little_Endian (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Unsigned_64_Little_Endian) is
      type U64_String is new String (1 .. 8);
      package U64_ByteFlip is new ByteFlip (Unsigned_64_Little_Endian);
      function U64S_To_U64LE is new Ada.Unchecked_Conversion (U64_String, Unsigned_64_Little_Endian);

      U64S : U64_String;
   begin
      U64_String'Read (Stream, U64S);
      Item := U64S_To_U64LE (U64S);
      U64_ByteFlip.Flip_Little_Endian_Bytes (Item);
   end Read_Unsigned_64_Little_Endian;

   procedure Read_Machine_Type (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Machine_Types) is
      function U16LE_To_MT is new Ada.Unchecked_Conversion (Unsigned_16_Little_Endian, Machine_Types);
   begin
      Item := U16LE_To_MT (Unsigned_16_Little_Endian'Input (Stream));
      if not Item'Valid then
         Item := UNKNOWN;
      end if;
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

   procedure Read_COFF_Characteristics_Flags (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out COFF_Characteristics_Flags) is

      Raw : Unsigned_16_Little_Endian := Unsigned_16_Little_Endian'Input (Stream);

   begin
      for Characteristic in Item'First .. Item'Last loop

         Item (Characteristic) := (Raw and Unsigned_16_Little_Endian (Characteristic'Enum_Rep)) /= 0;

      end loop;
   end Read_COFF_Characteristics_Flags;

   procedure COFF_Characteristics_Flags_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : COFF_Characteristics_Flags) is
      Hit_First : Boolean := False;
   begin
      Output.Put ("[");
      for Characteristic in Value'First .. Value'Last loop
         if Value (Characteristic) then
            if Hit_First then
               Output.Put (", ");
            else
               Hit_First := True;
            end if;
            Output.Put (Characteristic'Image);
         end if;
      end loop;
      Output.Put ("]");
   end COFF_Characteristics_Flags_Put_Image;

   procedure Read_Optional_Header_Identifier (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Optional_Header_Identifiers) is
      function U16LE_To_OHI is new Ada.Unchecked_Conversion (Unsigned_16_Little_Endian, Optional_Header_Identifiers);
   begin
      Item := U16LE_To_OHI (Unsigned_16_Little_Endian'Input (Stream));
      if not Item'Valid then
         Item := UNKNOWN;
      end if;
   end Read_Optional_Header_Identifier;

   procedure Read_Header_Version_2 (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Header_Version_2) is
   begin
      Unsigned_8'Read (Stream, Item.Major);
      Unsigned_8'Read (Stream, Item.Minor);
   end Read_Header_Version_2;

   procedure Header_Version_2_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : Header_Version_2) is
   begin
      Output.Put ('v' & Value.Major'Image & '.' & Value.Minor'Image);
   end Header_Version_2_Put_Image;

   procedure Read_Header_Version_4 (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Header_Version_4) is
   begin
      Unsigned_16_Little_Endian'Read (Stream, Item.Major);
      Unsigned_16_Little_Endian'Read (Stream, Item.Minor);
   end Read_Header_Version_4;

   procedure Header_Version_4_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : Header_Version_4) is
   begin
      Output.Put ('v' & Value.Major'Image & '.' & Value.Minor'Image);
   end Header_Version_4_Put_Image;

   procedure Read_Optional_Header_Windows_Subsystem (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Optional_Header_Windows_Subsystems) is
      function U16LE_To_SS is new Ada.Unchecked_Conversion (Unsigned_16_Little_Endian, Optional_Header_Windows_Subsystems);
   begin
      Item := U16LE_To_SS (Unsigned_16_Little_Endian'Input (Stream));
      if not Item'Valid then
         Item := UNKNOWN;
      end if;
   end Read_Optional_Header_Windows_Subsystem;

   procedure DLL_Characteristics_Flags_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : DLL_Characteristics_Flags) is
      Hit_First : Boolean := False;
   begin
      Output.Put ("[");
      for Characteristic in Value'First .. Value'Last loop
         if Value (Characteristic) then
            if Hit_First then
               Output.Put (", ");
            else
               Hit_First := True;
            end if;
            Output.Put (Characteristic'Image);
         end if;
      end loop;
      Output.Put ("]");
   end DLL_Characteristics_Flags_Put_Image;

   procedure Read_DLL_Characteristics_Flags (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out DLL_Characteristics_Flags) is

      Raw : Unsigned_16_Little_Endian := Unsigned_16_Little_Endian'Input (Stream);

   begin
      for Characteristic in Item'First .. Item'Last loop

         Item (Characteristic) := (Raw and Unsigned_16_Little_Endian (Characteristic'Enum_Rep)) /= 0;

      end loop;
   end Read_DLL_Characteristics_Flags;

   procedure Read_Windows_Specific_Optional_Header_64 (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Windows_Specific_Optional_Header_64) is
   begin
      Unsigned_64_Little_Endian'Read (Stream, Item.Image_Base_Pointer);
      Unsigned_32_Little_Endian'Read (Stream, Item.Section_Alignment);
      Unsigned_32_Little_Endian'Read (Stream, Item.File_Alignment);
      Header_Version_4'Read (Stream, Item.Operating_System_Version);
      Header_Version_4'Read (Stream, Item.Image_Version);
      Header_Version_4'Read (Stream, Item.Subsystem_Version);
      Unsigned_32_Little_Endian'Read (Stream, Item.Reserved_Win32_Version);
      Unsigned_32_Little_Endian'Read (Stream, Item.Sizes.Image);
      Unsigned_32_Little_Endian'Read (Stream, Item.Sizes.Headers);
      Unsigned_32_Little_Endian'Read (Stream, Item.Image_Checksum);
      Optional_Header_Windows_Subsystems'Read (Stream, Item.Subsystem);
      DLL_Characteristics_Flags'Read (Stream, Item.DLL_Characteristics);
      Unsigned_64_Little_Endian'Read (Stream, Item.Sizes.Stack_Reserve);
      Unsigned_64_Little_Endian'Read (Stream, Item.Sizes.Stack_Commit);
      Unsigned_64_Little_Endian'Read (Stream, Item.Sizes.Heap_Reserve);
      Unsigned_64_Little_Endian'Read (Stream, Item.Sizes.Heap_Commit);
      Unsigned_32_Little_Endian'Read (Stream, Item.Reserved_Loader_Flags);
      for Index in 1 .. Unsigned_32_Little_Endian'Input (Stream) loop
         declare
            Directory : Image_Data_Directory;
         begin
            Unsigned_32_Little_Endian'Read (Stream, Directory.Virtual_Address);
            Unsigned_32_Little_Endian'Read (Stream, Directory.Size);
            Item.Image_Data_Directories.Append (Directory);
         end;
      end loop;
   end Read_Windows_Specific_Optional_Header_64;

   procedure Read_Windows_Specific_Optional_Header_32 (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Windows_Specific_Optional_Header_32) is
   begin
      Unsigned_32_Little_Endian'Read (Stream, Item.Image_Base_Pointer);
      Unsigned_32_Little_Endian'Read (Stream, Item.Section_Alignment);
      Unsigned_32_Little_Endian'Read (Stream, Item.File_Alignment);
      Header_Version_4'Read (Stream, Item.Operating_System_Version);
      Header_Version_4'Read (Stream, Item.Image_Version);
      Header_Version_4'Read (Stream, Item.Subsystem_Version);
      Unsigned_32_Little_Endian'Read (Stream, Item.Reserved_Win32_Version);
      Unsigned_32_Little_Endian'Read (Stream, Item.Sizes.Image);
      Unsigned_32_Little_Endian'Read (Stream, Item.Sizes.Headers);
      Unsigned_32_Little_Endian'Read (Stream, Item.Image_Checksum);
      Optional_Header_Windows_Subsystems'Read (Stream, Item.Subsystem);
      DLL_Characteristics_Flags'Read (Stream, Item.DLL_Characteristics);
      Unsigned_32_Little_Endian'Read (Stream, Item.Sizes.Stack_Reserve);
      Unsigned_32_Little_Endian'Read (Stream, Item.Sizes.Stack_Commit);
      Unsigned_32_Little_Endian'Read (Stream, Item.Sizes.Heap_Reserve);
      Unsigned_32_Little_Endian'Read (Stream, Item.Sizes.Heap_Commit);
      Unsigned_32_Little_Endian'Read (Stream, Item.Reserved_Loader_Flags);
      for Index in 1 .. Unsigned_32_Little_Endian'Input (Stream) loop
         declare
            Directory : Image_Data_Directory;
         begin
            Unsigned_32_Little_Endian'Read (Stream, Directory.Virtual_Address);
            Unsigned_32_Little_Endian'Read (Stream, Directory.Size);
            Item.Image_Data_Directories.Append (Directory);
         end;
      end loop;
   end Read_Windows_Specific_Optional_Header_32;

   function Read_Optional_Header (File : Ada.Streams.Stream_IO.File_Type; Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Optional_Header is
      New_Object : Optional_Header (Optional_Header_Identifiers'Input (Stream));
   begin
      Header_Version_2'Read (Stream, New_Object.Linker_Version);
      Unsigned_32_Little_Endian'Read (Stream, New_Object.Sizes.Code);
      Unsigned_32_Little_Endian'Read (Stream, New_Object.Sizes.Data.Initialized);
      Unsigned_32_Little_Endian'Read (Stream, New_Object.Sizes.Data.Uninitialized);
      Unsigned_32_Little_Endian'Read (Stream, New_Object.Entry_Pointer);
      Unsigned_32_Little_Endian'Read (Stream, New_Object.Bases.Code);
      if New_Object.Magic = PE_32 then
         Unsigned_32_Little_Endian'Read (Stream, New_Object.Bases.Data);
         Windows_Specific_Optional_Header_32'Read (Stream, New_Object.Image_Base_32);
      else
         Windows_Specific_Optional_Header_64'Read (Stream, New_Object.Image_Base_64);
      end if;
      return New_Object;
   end Read_Optional_Header;

   procedure Section_Characteristics_Flags_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : Section_Characteristics_Flags) is
      Hit_First : Boolean := False;
   begin
      Output.Put ("[");
      for Characteristic in Value'First .. Value'Last loop
         if Value (Characteristic) then
            if Hit_First then
               Output.Put (", ");
            else
               Hit_First := True;
            end if;
            Output.Put (Characteristic'Image);
         end if;
      end loop;
      Output.Put ("]");
   end Section_Characteristics_Flags_Put_Image;

   procedure Read_Section_Characteristics_Flags (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Section_Characteristics_Flags) is

      Raw : Unsigned_32_Little_Endian := Unsigned_32_Little_Endian'Input (Stream);

   begin
      for Characteristic in Item'First .. Item'Last loop

         Item (Characteristic) := (Raw and Unsigned_32_Little_Endian (Characteristic'Enum_Rep)) /= 0;

      end loop;
   end Read_Section_Characteristics_Flags;

   procedure Read_Image_Section (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Image_Section) is
   begin
      String'Read (Stream, Item.Name);
      Unsigned_32_Little_Endian'Read (Stream, Item.Sizes.Virtual);
      Unsigned_32_Little_Endian'Read (Stream, Item.Pointers.Virtual);
      Unsigned_32_Little_Endian'Read (Stream, Item.Sizes.Raw_Data);
      Unsigned_32_Little_Endian'Read (Stream, Item.Pointers.Raw_Data);
      Unsigned_32_Little_Endian'Read (Stream, Item.Pointers.Relocations);
      Unsigned_32_Little_Endian'Read (Stream, Item.Pointers.Line_Numbers);
      Unsigned_16_Little_Endian'Read (Stream, Item.Sizes.Relocations);
      Unsigned_16_Little_Endian'Read (Stream, Item.Sizes.Line_Numbers);
      Section_Characteristics_Flags'Read (Stream, Item.Characteristics);
   end Read_Image_Section;

   function Read_Object_Portable_Executable (File : Ada.Streams.Stream_IO.File_Type; Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Object_Portable_Executable
   is

      New_Object              : Object_Portable_Executable;
      Size_Of_Optional_Header : Unsigned_16_Little_Endian;
      Sections_Count          : Unsigned_16_Little_Endian;

      discard : String (1 .. 8);

      use type Ada.Streams.Stream_IO.Positive_Count;

   begin
      MZ_DOS_Header'Read (Stream, New_Object.DOS_Header);
      File.Set_Index (Ada.Streams.Stream_IO.Positive_Count (New_Object.DOS_Header.New_Header_Pointer + 1));
      String'Read (Stream, New_Object.Magic);
      Machine_Types'Read (Stream, New_Object.Machine_Type);
      Unsigned_16_Little_Endian'Read (Stream, Sections_Count);
      New_Object.Time_Stamp := Ada.Calendar.Conversions.To_Ada_Time (Interfaces.C.long (Unsigned_32_Little_Endian'Input (Stream)));
      Put_Line (Image (New_Object.Time_Stamp));
      String'Read (Stream, discard); -- discard 8
      Unsigned_16_Little_Endian'Read (Stream, Size_Of_Optional_Header);
      COFF_Characteristics_Flags'Read (Stream, New_Object.Characteristics);
      if Size_Of_Optional_Header > 0 then
         New_Object.Optional_Header.Replace_Element (Read_Optional_Header (File, Stream));
      end if;

      for Index in 1 .. Sections_Count loop
         New_Object.Sections.Append (Image_Section'Input (Stream));
      end loop;
      return New_Object;
   end Read_Object_Portable_Executable;

end PE;
