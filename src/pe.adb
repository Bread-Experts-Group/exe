with Ada.Unchecked_Conversion;
with Ada.Calendar.Conversions;

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

with Interfaces.C;

package body PE is

   procedure Read_Machine_Type (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Machine_Types) is
      function U16LE_To_MT is new Ada.Unchecked_Conversion (Unsigned_16.Little_Endian, Machine_Types);
   begin
      Item := U16LE_To_MT (Unsigned_16.Little_Endian'Input (Stream));
      if not Item'Valid then
         Item := UNKNOWN;
      end if;
   end Read_Machine_Type;

   procedure Read_MZ_DOS_Header (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out MZ_DOS_Header) is
   begin
      String'Read (Stream, Item.Magic);
      Unsigned_16.Little_Endian'Read (Stream, Item.Last_Page_Size);
      Unsigned_16.Little_Endian'Read (Stream, Item.Page_Count);
      Unsigned_16.Little_Endian'Read (Stream, Item.Relocation_Count);
      Unsigned_16.Little_Endian'Read (Stream, Item.Header_Size_Paragraphs);
      Unsigned_16.Little_Endian'Read (Stream, Item.Minimum_Extra_Paragraphs_Needed);
      Unsigned_16.Little_Endian'Read (Stream, Item.Maximum_Extra_Paragraphs_Needed);
      Unsigned_16.Little_Endian'Read (Stream, Item.Initial_SS_Value_Rel);
      Unsigned_16.Little_Endian'Read (Stream, Item.Initial_SP_Value);
      Unsigned_16.Little_Endian'Read (Stream, Item.Checksum);
      Unsigned_16.Little_Endian'Read (Stream, Item.Initial_IP_Value);
      Unsigned_16.Little_Endian'Read (Stream, Item.Initial_CS_Value_Rel);
      Unsigned_16.Little_Endian'Read (Stream, Item.Relocation_Table_Pointer);
      Unsigned_16.Little_Endian'Read (Stream, Item.Overlay_Number);
      Reserved_16'Read (Stream, Item.Reserved_1);
      Unsigned_16.Little_Endian'Read (Stream, Item.OEM_Identifier);
      Unsigned_16.Little_Endian'Read (Stream, Item.OEM_Reserved);
      Reserved_16'Read (Stream, Item.Reserved_2);
      Unsigned_16.Little_Endian'Read (Stream, Item.New_Header_Pointer);
   end Read_MZ_DOS_Header;

   procedure Read_COFF_Characteristics_Flags (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out COFF_Characteristics_Flags) is

      Raw : Unsigned_16.Little_Endian := Unsigned_16.Little_Endian'Input (Stream);

   begin
      for Characteristic in Item'First .. Item'Last loop

         Item (Characteristic) := (Raw and Unsigned_16.Little_Endian (Characteristic'Enum_Rep)) /= 0;

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
      function U16LE_To_OHI is new Ada.Unchecked_Conversion (Unsigned_16.Little_Endian, Optional_Header_Identifiers);
   begin
      Item := U16LE_To_OHI (Unsigned_16.Little_Endian'Input (Stream));
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
      Output.Put (f"v{Value.Major'Image}.{Value.Minor'Image}");
   end Header_Version_2_Put_Image;

   procedure Read_Header_Version_4 (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Header_Version_4) is
   begin
      Unsigned_16.Little_Endian'Read (Stream, Item.Major);
      Unsigned_16.Little_Endian'Read (Stream, Item.Minor);
   end Read_Header_Version_4;

   procedure Header_Version_4_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : Header_Version_4) is
   begin
      Output.Put (f"v{Value.Major'Image}.{Value.Minor'Image}");
   end Header_Version_4_Put_Image;

   procedure Read_Optional_Header_Windows_Subsystem (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Optional_Header_Windows_Subsystems) is
      function U16LE_To_SS is new Ada.Unchecked_Conversion (Unsigned_16.Little_Endian, Optional_Header_Windows_Subsystems);
   begin
      Item := U16LE_To_SS (Unsigned_16.Little_Endian'Input (Stream));
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

      Raw : Unsigned_16.Little_Endian := Unsigned_16.Little_Endian'Input (Stream);

   begin
      for Characteristic in Item'First .. Item'Last loop

         Item (Characteristic) := (Raw and Unsigned_16.Little_Endian (Characteristic'Enum_Rep)) /= 0;

      end loop;
   end Read_DLL_Characteristics_Flags;

   procedure Read_Windows_Specific_Optional_Header_64 (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Windows_Specific_Optional_Header_64) is
   begin
      Unsigned_64.Little_Endian'Read (Stream, Item.Image_Base_Pointer);
      Unsigned_32.Little_Endian'Read (Stream, Item.Section_Alignment);
      Unsigned_32.Little_Endian'Read (Stream, Item.File_Alignment);
      Header_Version_4'Read (Stream, Item.Operating_System_Version);
      Header_Version_4'Read (Stream, Item.Image_Version);
      Header_Version_4'Read (Stream, Item.Subsystem_Version);
      Unsigned_32.Little_Endian'Read (Stream, Item.Reserved_Win32_Version);
      Unsigned_32.Little_Endian'Read (Stream, Item.Sizes.Image);
      Unsigned_32.Little_Endian'Read (Stream, Item.Sizes.Headers);
      Unsigned_32.Little_Endian'Read (Stream, Item.Image_Checksum);
      Optional_Header_Windows_Subsystems'Read (Stream, Item.Subsystem);
      DLL_Characteristics_Flags'Read (Stream, Item.DLL_Characteristics);
      Unsigned_64.Little_Endian'Read (Stream, Item.Sizes.Stack_Reserve);
      Unsigned_64.Little_Endian'Read (Stream, Item.Sizes.Stack_Commit);
      Unsigned_64.Little_Endian'Read (Stream, Item.Sizes.Heap_Reserve);
      Unsigned_64.Little_Endian'Read (Stream, Item.Sizes.Heap_Commit);
      Unsigned_32.Little_Endian'Read (Stream, Item.Reserved_Loader_Flags);
      for Index in 1 .. Unsigned_32.Little_Endian'Input (Stream) loop
         Section   : Image_Data_Directory_Section
                        (if Index > Image_Data_Directory_Types'Enum_Rep (Image_Data_Directory_Types'Last) then OTHER else Image_Data_Directory_Types'Enum_Val (Index));
         Directory : Image_Data_Directory;

         Unsigned_32.Little_Endian'Read (Stream, Directory.Virtual_Address);
         Unsigned_32.Little_Endian'Read (Stream, Directory.Size);
         if Directory.Virtual_Address /= 0 or else Directory.Size /= 0 then
            Section.Directory := Directory;
            Item.Image_Data_Directory_Sections.Append (Section);
         end if;
      end loop;
   end Read_Windows_Specific_Optional_Header_64;

   procedure Read_Windows_Specific_Optional_Header_32 (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Windows_Specific_Optional_Header_32) is
   begin
      Unsigned_32.Little_Endian'Read (Stream, Item.Image_Base_Pointer);
      Unsigned_32.Little_Endian'Read (Stream, Item.Section_Alignment);
      Unsigned_32.Little_Endian'Read (Stream, Item.File_Alignment);
      Header_Version_4'Read (Stream, Item.Operating_System_Version);
      Header_Version_4'Read (Stream, Item.Image_Version);
      Header_Version_4'Read (Stream, Item.Subsystem_Version);
      Unsigned_32.Little_Endian'Read (Stream, Item.Reserved_Win32_Version);
      Unsigned_32.Little_Endian'Read (Stream, Item.Sizes.Image);
      Unsigned_32.Little_Endian'Read (Stream, Item.Sizes.Headers);
      Unsigned_32.Little_Endian'Read (Stream, Item.Image_Checksum);
      Optional_Header_Windows_Subsystems'Read (Stream, Item.Subsystem);
      DLL_Characteristics_Flags'Read (Stream, Item.DLL_Characteristics);
      Unsigned_32.Little_Endian'Read (Stream, Item.Sizes.Stack_Reserve);
      Unsigned_32.Little_Endian'Read (Stream, Item.Sizes.Stack_Commit);
      Unsigned_32.Little_Endian'Read (Stream, Item.Sizes.Heap_Reserve);
      Unsigned_32.Little_Endian'Read (Stream, Item.Sizes.Heap_Commit);
      Unsigned_32.Little_Endian'Read (Stream, Item.Reserved_Loader_Flags);
      for Index in 1 .. Unsigned_32.Little_Endian'Input (Stream) loop
         Section   : Image_Data_Directory_Section
                        (if Index > Image_Data_Directory_Types'Enum_Rep (Image_Data_Directory_Types'Last) then OTHER else Image_Data_Directory_Types'Enum_Val (Index));
         Directory : Image_Data_Directory;
         
         Unsigned_32.Little_Endian'Read (Stream, Directory.Virtual_Address);
         Unsigned_32.Little_Endian'Read (Stream, Directory.Size);
         if Directory.Virtual_Address /= 0 or else Directory.Size /= 0 then
            Section.Directory := Directory;
            Item.Image_Data_Directory_Sections.Append (Section);
         end if;
      end loop;
   end Read_Windows_Specific_Optional_Header_32;

   function Read_Optional_Header (File : Ada.Streams.Stream_IO.File_Type; Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Optional_Header is
      New_Object : Optional_Header (Optional_Header_Identifiers'Input (Stream));
   begin
      Header_Version_2'Read (Stream, New_Object.Linker_Version);
      Unsigned_32.Little_Endian'Read (Stream, New_Object.Sizes.Code);
      Unsigned_32.Little_Endian'Read (Stream, New_Object.Sizes.Data.Initialized);
      Unsigned_32.Little_Endian'Read (Stream, New_Object.Sizes.Data.Uninitialized);
      Unsigned_32.Little_Endian'Read (Stream, New_Object.Entry_Pointer);
      Unsigned_32.Little_Endian'Read (Stream, New_Object.Bases.Code);
      if New_Object.Magic = PE_32 then
         Unsigned_32.Little_Endian'Read (Stream, New_Object.Bases.Data);
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

      Raw : Unsigned_32.Little_Endian := Unsigned_32.Little_Endian'Input (Stream);

   begin
      for Characteristic in Item'First .. Item'Last loop

         Item (Characteristic) := (Raw and Unsigned_32.Little_Endian (Characteristic'Enum_Rep)) /= 0;

      end loop;
   end Read_Section_Characteristics_Flags;

   procedure Read_Image_Section (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Image_Section) is
   begin
      String'Read (Stream, Item.Name);
      Unsigned_32.Little_Endian'Read (Stream, Item.Sizes.Virtual);
      Unsigned_32.Little_Endian'Read (Stream, Item.Pointers.Virtual);
      Unsigned_32.Little_Endian'Read (Stream, Item.Sizes.Raw_Data);
      Unsigned_32.Little_Endian'Read (Stream, Item.Pointers.Raw_Data);
      Unsigned_32.Little_Endian'Read (Stream, Item.Pointers.Relocations);
      Unsigned_32.Little_Endian'Read (Stream, Item.Pointers.Line_Numbers);
      Unsigned_16.Little_Endian'Read (Stream, Item.Sizes.Relocations);
      Unsigned_16.Little_Endian'Read (Stream, Item.Sizes.Line_Numbers);
      Section_Characteristics_Flags'Read (Stream, Item.Characteristics);
   end Read_Image_Section;

   procedure Read_Image_Data_Directories_Full
     (File : Ada.Streams.Stream_IO.File_Type; Stream : not null access Ada.Streams.Root_Stream_Type'Class; From : in out Object_Portable_Executable)
   is
      Header         : Optional_Header                             := From.Optional_Header.Element;
      Windows_Header : Windows_Specific_Optional_Header_Base'Class := (if Header.Magic = PE_32_EXTENDED_ADDRESSING then Header.Image_Base_64 else Header.Image_Base_32);

      Section  : Image_Section;
      Location : Ada.Streams.Stream_IO.Positive_Count;
   begin
      for Directory in Windows_Header.Image_Data_Directory_Sections.Iterate loop
         for Object of From.Sections loop
            Actual : Image_Data_Directory_Section := Directory.Element;

            if Actual.Directory.Virtual_Address >= Object.Pointers.Virtual and then Actual.Directory.Virtual_Address <= (Object.Pointers.Virtual + Object.Sizes.Raw_Data) then
               Location := Ada.Streams.Stream_IO.Positive_Count (Actual.Directory.Virtual_Address - Object.Pointers.Virtual + Object.Pointers.Raw_Data);
               File.Set_Index (Location);
               case Actual.Section_Type is
                  when EXPORT_TABLE =>
                     Unsigned_32.Little_Endian'Read (Stream, Actual.Export_Flags);
                     Actual.Export_Time_Stamp := Ada.Calendar.Conversions.To_Ada_Time (Interfaces.C.long (Unsigned_32.Little_Endian'Input (Stream)));
                     Header_Version_4'Read (Stream, Actual.Export_Version);
                     Unsigned_32.Little_Endian'Read (Stream, Actual.Export_Pointers.Name);
                     Unsigned_32.Little_Endian'Read (Stream, Actual.Export_Ordinal_Base);
                     Unsigned_32.Little_Endian'Read (Stream, Actual.Export_Sizes.Address_Table);
                     Unsigned_32.Little_Endian'Read (Stream, Actual.Export_Sizes.Names);
                     Unsigned_32.Little_Endian'Read (Stream, Actual.Export_Pointers.Address_Table);
                     Unsigned_32.Little_Endian'Read (Stream, Actual.Export_Pointers.Export_Name);
                     Unsigned_32.Little_Endian'Read (Stream, Actual.Export_Pointers.Ordinal_Table);
                  when IMPORT_TABLE =>
                     ITD       : Import_Directory;
                     Read_Time : Unsigned_32.Little_Endian;
                     loop
                        Unsigned_32.Little_Endian'Read (Stream, ITD.Pointers.Lookup_Table);
                        Unsigned_32.Little_Endian'Read (Stream, Read_Time);
                        ITD.Time_Stamp := Ada.Calendar.Conversions.To_Ada_Time (Interfaces.C.long (Read_Time));
                        Unsigned_32.Little_Endian'Read (Stream, ITD.Forwarder_Index);
                        Unsigned_32.Little_Endian'Read (Stream, ITD.Pointers.Name);
                        Unsigned_32.Little_Endian'Read (Stream, ITD.Pointers.Address_Table);
                        exit when ITD.Pointers.Lookup_Table + Read_Time + ITD.Forwarder_Index + ITD.Pointers.Name + ITD.Pointers.Address_Table = 0;
                        Actual.Import_Directories.Append (ITD);
                     end loop;
                  when others =>
                     null;
               end case;

               Windows_Header.Image_Data_Directory_Sections.Replace_Element (Directory, Actual);
               exit;
            end if;
         end loop;
      end loop;
      if Header.Magic = PE_32_EXTENDED_ADDRESSING then
         Header.Image_Base_64 := Windows_Specific_Optional_Header_64 (Windows_Header);
      else
         Header.Image_Base_32 := Windows_Specific_Optional_Header_32 (Windows_Header);
      end if;
      From.Optional_Header.Replace_Element (Header);
   end Read_Image_Data_Directories_Full;

   function Read_Object_Portable_Executable (File : Ada.Streams.Stream_IO.File_Type; Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Object_Portable_Executable
   is

      New_Object              : Object_Portable_Executable;
      Size_Of_Optional_Header : Unsigned_16.Little_Endian;
      Sections_Count          : Unsigned_16.Little_Endian;

      discard : String (1 .. 8);

      use type Ada.Streams.Stream_IO.Positive_Count;

   begin
      MZ_DOS_Header'Read (Stream, New_Object.DOS_Header);
      File.Set_Index (Ada.Streams.Stream_IO.Positive_Count (New_Object.DOS_Header.New_Header_Pointer + 1));
      String'Read (Stream, New_Object.Magic);
      Machine_Types'Read (Stream, New_Object.Machine_Type);
      Unsigned_16.Little_Endian'Read (Stream, Sections_Count);
      New_Object.Time_Stamp := Ada.Calendar.Conversions.To_Ada_Time (Interfaces.C.long (Unsigned_32.Little_Endian'Input (Stream)));
      Put_Line (Image (New_Object.Time_Stamp));
      String'Read (Stream, discard); -- discard 8
      Unsigned_16.Little_Endian'Read (Stream, Size_Of_Optional_Header);
      COFF_Characteristics_Flags'Read (Stream, New_Object.Characteristics);
      if Size_Of_Optional_Header > 0 then
         New_Object.Optional_Header.Replace_Element (Read_Optional_Header (File, Stream));
      end if;

      for Index in 1 .. Sections_Count loop
         New_Object.Sections.Append (Image_Section'Input (Stream));
      end loop;
      Read_Image_Data_Directories_Full (File, Stream, New_Object);
      return New_Object;
   end Read_Object_Portable_Executable;

end PE;
