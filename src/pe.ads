with Ada.Streams;
with Ada.Calendar;
with Ada.Streams.Stream_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Text_Buffers;
with Ada.Containers.Indefinite_Holders;

package PE is

   type Unsigned_8 is mod 2**8 with
     Put_Image => Unsigned_8_Put_Image;
   procedure Unsigned_8_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : Unsigned_8);

   type Unsigned_16_Little_Endian is mod 2**16 with
     Put_Image => Unsigned_16_Put_Image;
   procedure Read_Unsigned_16_Little_Endian (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Unsigned_16_Little_Endian);
   for Unsigned_16_Little_Endian'Read use Read_Unsigned_16_Little_Endian;
   procedure Unsigned_16_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : Unsigned_16_Little_Endian);

   type Unsigned_32_Little_Endian is mod 2**32;
   procedure Read_Unsigned_32_Little_Endian (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Unsigned_32_Little_Endian);
   for Unsigned_32_Little_Endian'Read use Read_Unsigned_32_Little_Endian;

   type Unsigned_64_Little_Endian is mod 2**64;
   procedure Read_Unsigned_64_Little_Endian (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Unsigned_64_Little_Endian);
   for Unsigned_64_Little_Endian'Read use Read_Unsigned_64_Little_Endian;

   type Machine_Types is
     (ANY,
      INTEL_386_OR_LATER,
      MIPS_LE,
      MIPS_LE_WCE_V2,
      ALPHA_AXP_32,
      HITACHI_SH3,
      HITACHI_SH3_DSP,
      HITACHI_SH4,
      HITACHI_SH5,
      ARM_LE,
      THUMB,
      ARM_THUMB_2_LE,
      MATSUSHITA_AM33,
      POWERPC_LE,
      POWERPC_FPU,
      ITANIUM_64,
      MIPS_16,
      ALPHA_64,
      MIPS_FPU,
      MIPS_16_FPU,
      EFI,
      RISCV_32,
      RISCV_64,
      RISCV_128,
      LOONG_ARCH_32,
      LOONG_ARCH_64,
      AMD_64,
      MITSUBISHI_M32R_LE,
      ARM_64_LE,
      UNKNOWN);

   for Machine_Types use
     (ANY                => 16#00_00#,
      INTEL_386_OR_LATER => 16#01_4C#,
      MIPS_LE            => 16#01_66#,
      MIPS_LE_WCE_V2     => 16#01_69#,
      ALPHA_AXP_32       => 16#01_84#,
      HITACHI_SH3        => 16#01_A2#,
      HITACHI_SH3_DSP    => 16#01_A3#,
      HITACHI_SH4        => 16#01_A6#,
      HITACHI_SH5        => 16#01_A8#,
      ARM_LE             => 16#01_C0#,
      THUMB              => 16#01_C2#,
      ARM_THUMB_2_LE     => 16#01_C4#,
      MATSUSHITA_AM33    => 16#01_D3#,
      POWERPC_LE         => 16#01_F0#,
      POWERPC_FPU        => 16#01_F1#,
      ITANIUM_64         => 16#02_00#,
      MIPS_16            => 16#02_66#,
      ALPHA_64           => 16#02_84#,
      MIPS_FPU           => 16#03_66#,
      MIPS_16_FPU        => 16#04_66#,
      EFI                => 16#0E_BC#,
      RISCV_32           => 16#50_32#,
      RISCV_64           => 16#50_64#,
      RISCV_128          => 16#51_28#,
      LOONG_ARCH_32      => 16#62_32#,
      LOONG_ARCH_64      => 16#62_64#,
      AMD_64             => 16#86_64#,
      MITSUBISHI_M32R_LE => 16#90_41#,
      ARM_64_LE          => 16#AA_64#,
      UNKNOWN            => 16#FF_FF#);

   procedure Read_Machine_Type (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Machine_Types);
   for Machine_Types'Read use Read_Machine_Type;

   type COFF_Characteristics is
     (NO_BASE_RELOCATION,
      EXECUTABLE,
      DEPRECATED_COFF_DEBUG_NO_LINE_NUMBERS,
      DEPRECATED_COFF_DEBUG_NO_LOCAL_SYMBOLS,
      DEPRECATED_AGRESSIVELY_TRIM_WORKING_SET,
      CAN_HANDLE_OVER_2_GB,
      RESERVED,
      DEPRECATED_LITTLE_ENDIAN,
      WORDS_ARE_32_BIT,
      NO_DEBUGGING_INFO,
      ON_REMOVABLE_MEDIA_COPY_TO_SWAP,
      ON_NETWORK_MEDIA_COPY_TO_SWAP,
      SYSTEM_FILE,
      DYNAMIC_LINK_LIBRARY,
      SINGLE_CORE_ONLY,
      DEPRECATED_BIG_ENDIAN) with
     Size => 16;

   for COFF_Characteristics use
     (NO_BASE_RELOCATION                      => 2**0,
      EXECUTABLE                              => 2**1,
      DEPRECATED_COFF_DEBUG_NO_LINE_NUMBERS   => 2**2,
      DEPRECATED_COFF_DEBUG_NO_LOCAL_SYMBOLS  => 2**3,
      DEPRECATED_AGRESSIVELY_TRIM_WORKING_SET => 2**4,
      CAN_HANDLE_OVER_2_GB                    => 2**5,
      RESERVED                                => 2**6,
      DEPRECATED_LITTLE_ENDIAN                => 2**7,
      WORDS_ARE_32_BIT                        => 2**8,
      NO_DEBUGGING_INFO                       => 2**9,
      ON_REMOVABLE_MEDIA_COPY_TO_SWAP         => 2**10,
      ON_NETWORK_MEDIA_COPY_TO_SWAP           => 2**11,
      SYSTEM_FILE                             => 2**12,
      DYNAMIC_LINK_LIBRARY                    => 2**13,
      SINGLE_CORE_ONLY                        => 2**14,
      DEPRECATED_BIG_ENDIAN                   => 2**15);

   type COFF_Characteristics_Flags is
     array (COFF_Characteristics'First .. COFF_Characteristics'Last)
     of Boolean with
     Put_Image => COFF_Characteristics_Flags_Put_Image;
   procedure Read_COFF_Characteristics_Flags (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out COFF_Characteristics_Flags);
   for COFF_Characteristics_Flags'Read use Read_COFF_Characteristics_Flags;
   procedure COFF_Characteristics_Flags_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : COFF_Characteristics_Flags);

   type Reserved_16 is
     array (Positive range <>)
     of Unsigned_16_Little_Endian;

   type MZ_DOS_Header is record
      Magic                           : String (1 .. 2);
      Last_Page_Size                  : Unsigned_16_Little_Endian;
      Page_Count                      : Unsigned_16_Little_Endian;
      Relocation_Count                : Unsigned_16_Little_Endian;
      Header_Size_Paragraphs          : Unsigned_16_Little_Endian;
      Minimum_Extra_Paragraphs_Needed : Unsigned_16_Little_Endian;
      Maximum_Extra_Paragraphs_Needed : Unsigned_16_Little_Endian;
      Initial_SS_Value_Rel            : Unsigned_16_Little_Endian;
      Initial_SP_Value                : Unsigned_16_Little_Endian;
      Checksum                        : Unsigned_16_Little_Endian;
      Initial_IP_Value                : Unsigned_16_Little_Endian;
      Initial_CS_Value_Rel            : Unsigned_16_Little_Endian;
      Relocation_Table_Pointer        : Unsigned_16_Little_Endian;
      Overlay_Number                  : Unsigned_16_Little_Endian;
      Reserved_1                      : Reserved_16 (1 .. 4);
      OEM_Identifier                  : Unsigned_16_Little_Endian;
      OEM_Reserved                    : Unsigned_16_Little_Endian;
      Reserved_2                      : Reserved_16 (1 .. 10);
      New_Header_Pointer              : Unsigned_16_Little_Endian;
   end record;

   procedure Read_MZ_DOS_Header (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out MZ_DOS_Header);
   for MZ_DOS_Header'Read use Read_MZ_DOS_Header;

   type Optional_Header_Identifiers is
     (READ_ONLY_MEMORY,
      PE_32,
      PE_32_EXTENDED_ADDRESSING,
      UNKNOWN);

   for Optional_Header_Identifiers use
     (READ_ONLY_MEMORY          => 16#01_07#,
      PE_32                     => 16#01_0B#,
      PE_32_EXTENDED_ADDRESSING => 16#02_0B#,
      UNKNOWN                   => 16#FF_FF#);

   procedure Read_Optional_Header_Identifier (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Optional_Header_Identifiers);
   for Optional_Header_Identifiers'Read use Read_Optional_Header_Identifier;

   type Header_Version_2 is record

      Major : Unsigned_8;
      Minor : Unsigned_8;

   end record with
     Put_Image => Header_Version_2_Put_Image;

   procedure Read_Header_Version_2 (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Header_Version_2);
   for Header_Version_2'Read use Read_Header_Version_2;
   procedure Header_Version_2_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : Header_Version_2);

   type Header_Version_4 is record

      Major : Unsigned_16_Little_Endian;
      Minor : Unsigned_16_Little_Endian;

   end record with
     Put_Image => Header_Version_4_Put_Image;

   procedure Read_Header_Version_4 (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Header_Version_4);
   for Header_Version_4'Read use Read_Header_Version_4;
   procedure Header_Version_4_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : Header_Version_4);

   type Optional_Header_Windows_Subsystems is
     (UNKNOWN,
      NATIVE,
      WINDOWS_GUI,
      WINDOWS_CUI,
      OS2_CUI,
      POSIX_CUI,
      NATIVE_WIN9X,
      WINDOWS_CE,
      EFI_APPLICATION,
      EFI_BOOT_SERVICE_DRIVER,
      EFI_RUNTIME_DRIVER,
      EFI_READ_ONLY_MEMORY,
      XBOX,
      WINDOWS_BOOT_APPLICATION) with
     Size => 16;

   for Optional_Header_Windows_Subsystems use
     (UNKNOWN                  => 0,
      NATIVE                   => 1,
      WINDOWS_GUI              => 2,
      WINDOWS_CUI              => 3,
      OS2_CUI                  => 5,
      POSIX_CUI                => 7,
      NATIVE_WIN9X             => 8,
      WINDOWS_CE               => 9,
      EFI_APPLICATION          => 10,
      EFI_BOOT_SERVICE_DRIVER  => 11,
      EFI_RUNTIME_DRIVER       => 12,
      EFI_READ_ONLY_MEMORY     => 13,
      XBOX                     => 14,
      WINDOWS_BOOT_APPLICATION => 16);

   procedure Read_Optional_Header_Windows_Subsystem (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Optional_Header_Windows_Subsystems);
   for Optional_Header_Windows_Subsystems'Read use Read_Optional_Header_Windows_Subsystem;

   type DLL_Characteristics is
     (RESERVED_1,
      RESERVED_2,
      RESERVED_3,
      RESERVED_4,
      UNDEFINED_1,
      SUPPORTS_HIGH_ENTROPY_MEMORY_LAYOUT_RANDOMIZATION,
      SUPPORTS_DLL_LOAD_TIME_RELOCATION,
      ENFORCE_CODE_INTEGRITY_CHECKS,
      SUPPORTS_NO_EXECUTE_BIT,
      DO_NOT_ISOLATE,
      NO_STRUCTURED_EXCEPTION_HANDLING,
      DO_NOT_BIND,
      MUST_EXECUTE_IN_APPCONTAINER,
      WINDOWS_DRIVER_MODEL_DRIVER,
      SUPPORTS_CONTROL_FLOW_GUARD,
      SUPPORTS_TERMINAL_SERVER);

   for DLL_Characteristics use
     (RESERVED_1                                        => 2**0,
      RESERVED_2                                        => 2**1,
      RESERVED_3                                        => 2**2,
      RESERVED_4                                        => 2**3,
      UNDEFINED_1                                       => 2**4,
      SUPPORTS_HIGH_ENTROPY_MEMORY_LAYOUT_RANDOMIZATION => 2**5,
      SUPPORTS_DLL_LOAD_TIME_RELOCATION                 => 2**6,
      ENFORCE_CODE_INTEGRITY_CHECKS                     => 2**7,
      SUPPORTS_NO_EXECUTE_BIT                           => 2**8,
      DO_NOT_ISOLATE                                    => 2**9,
      NO_STRUCTURED_EXCEPTION_HANDLING                  => 2**10,
      DO_NOT_BIND                                       => 2**11,
      MUST_EXECUTE_IN_APPCONTAINER                      => 2**12,
      WINDOWS_DRIVER_MODEL_DRIVER                       => 2**13,
      SUPPORTS_CONTROL_FLOW_GUARD                       => 2**14,
      SUPPORTS_TERMINAL_SERVER                          => 2**15);

   type DLL_Characteristics_Flags is
     array (DLL_Characteristics'First .. DLL_Characteristics'Last)
     of Boolean with
     Put_Image => DLL_Characteristics_Flags_Put_Image;
   procedure DLL_Characteristics_Flags_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : DLL_Characteristics_Flags);

   procedure Read_DLL_Characteristics_Flags (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out DLL_Characteristics_Flags);
   for DLL_Characteristics_Flags'Read use Read_DLL_Characteristics_Flags;

   type Windows_Specific_Optional_Header_Base_Sizes is abstract tagged record
      Image   : Unsigned_32_Little_Endian;
      Headers : Unsigned_32_Little_Endian;
   end record;

   type Image_Data_Directory is record
      Virtual_Address : Unsigned_32_Little_Endian;
      Size            : Unsigned_32_Little_Endian;
   end record;

   package Image_Data_Directory_Vectors is new Ada.Containers.Vectors (Positive, Image_Data_Directory);

   type Windows_Specific_Optional_Header_Base is abstract tagged record
      Section_Alignment        : Unsigned_32_Little_Endian;
      File_Alignment           : Unsigned_32_Little_Endian;
      Operating_System_Version : Header_Version_4;
      Image_Version            : Header_Version_4;
      Subsystem_Version        : Header_Version_4;
      Reserved_Win32_Version   : Unsigned_32_Little_Endian;
      Image_Checksum           : Unsigned_32_Little_Endian;
      Subsystem                : Optional_Header_Windows_Subsystems;
      DLL_Characteristics      : DLL_Characteristics_Flags;
      Reserved_Loader_Flags    : Unsigned_32_Little_Endian;
      Image_Data_Directories   : Image_Data_Directory_Vectors.Vector;
   end record;

   type Windows_Specific_Optional_Header_64_Sizes is new Windows_Specific_Optional_Header_Base_Sizes with record
      Stack_Reserve : Unsigned_64_Little_Endian;
      Stack_Commit  : Unsigned_64_Little_Endian;
      Heap_Reserve  : Unsigned_64_Little_Endian;
      Heap_Commit   : Unsigned_64_Little_Endian;
   end record;

   type Windows_Specific_Optional_Header_64 is new Windows_Specific_Optional_Header_Base with record

      Image_Base_Pointer : Unsigned_64_Little_Endian;
      Sizes              : Windows_Specific_Optional_Header_64_Sizes;

   end record;

   procedure Read_Windows_Specific_Optional_Header_64 (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Windows_Specific_Optional_Header_64);
   for Windows_Specific_Optional_Header_64'Read use Read_Windows_Specific_Optional_Header_64;

   type Windows_Specific_Optional_Header_32_Sizes is new Windows_Specific_Optional_Header_Base_Sizes with record
      Stack_Reserve : Unsigned_32_Little_Endian;
      Stack_Commit  : Unsigned_32_Little_Endian;
      Heap_Reserve  : Unsigned_32_Little_Endian;
      Heap_Commit   : Unsigned_32_Little_Endian;
   end record;

   type Windows_Specific_Optional_Header_32 is new Windows_Specific_Optional_Header_Base with record

      Image_Base_Pointer : Unsigned_32_Little_Endian;
      Sizes              : Windows_Specific_Optional_Header_32_Sizes;

   end record;

   procedure Read_Windows_Specific_Optional_Header_32 (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Windows_Specific_Optional_Header_32);
   for Windows_Specific_Optional_Header_32'Read use Read_Windows_Specific_Optional_Header_32;

   type Optional_Header_Sizes_Data is record

      Initialized   : Unsigned_32_Little_Endian;
      Uninitialized : Unsigned_32_Little_Endian;

   end record;

   type Optional_Header_Sizes is record

      Code : Unsigned_32_Little_Endian;
      Data : Optional_Header_Sizes_Data;

   end record;

   type Optional_Header_Bases (Magic : Optional_Header_Identifiers) is record
      Code : Unsigned_32_Little_Endian;
      case Magic is
         when PE_32 =>
            Data : Unsigned_32_Little_Endian;
         when others =>
            null;
      end case;
   end record;

   type Optional_Header (Magic : Optional_Header_Identifiers) is record

      Linker_Version : Header_Version_2;
      Sizes          : Optional_Header_Sizes;
      Entry_Pointer  : Unsigned_32_Little_Endian;
      Bases          : Optional_Header_Bases (Magic);

      --  Windows-specific fields
      case Magic is
         when PE_32_EXTENDED_ADDRESSING =>
            Image_Base_64 : Windows_Specific_Optional_Header_64;
         when others =>
            Image_Base_32 : Windows_Specific_Optional_Header_32;
      end case;

   end record;

   function Read_Optional_Header (File : Ada.Streams.Stream_IO.File_Type; Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Optional_Header;
   package Optional_Header_Holders is new Ada.Containers.Indefinite_Holders (Optional_Header);

   type Image_Section_Sizes is record
      Virtual      : Unsigned_32_Little_Endian;
      Raw_Data     : Unsigned_32_Little_Endian;
      Relocations  : Unsigned_16_Little_Endian;
      Line_Numbers : Unsigned_16_Little_Endian;
   end record;

   type Image_Section_Pointers is record
      Virtual      : Unsigned_32_Little_Endian;
      Raw_Data     : Unsigned_32_Little_Endian;
      Relocations  : Unsigned_32_Little_Endian;
      Line_Numbers : Unsigned_32_Little_Endian;
   end record;

   type Section_Characteristics is
     (RESERVED_1,
      RESERVED_2,
      RESERVED_3,
      RESERVED_4,
      DEPRECATED_NO_PAD_NEXT_BOUNDARY,
      RESERVED_5,
      EXECUTABLE_CODE,
      INITIALIZED_DATA,
      UNINITIALIZED_DATA,
      RESERVED_OTHER_DATA,
      INFORMATION_DATA,
      RESERVED_7,
      REMOVED_IN_IMAGE,
      COMDAT_DATA,
      UNDEFINED_1,
      UNDEFINED_2,
      GLOBAL_POINTER_REFERENCED_DATA,
      UNDEFINED_3,
      RESERVED_PURGABLE_OR_16_MEMORY,
      RESERVED_LOCKED_MEMORY,
      RESERVED_PRELOAD_MEMORY,
      ALIGN_DATA_1_BYTE_BOUNDARY,
      ALIGN_DATA_2_BYTE_BOUNDARY,
      ALIGN_DATA_4_BYTE_BOUNDARY,
      ALIGN_DATA_8_BYTE_BOUNDARY,
      ALIGN_DATA_16_BYTE_BOUNDARY,
      ALIGN_DATA_32_BYTE_BOUNDARY,
      ALIGN_DATA_64_BYTE_BOUNDARY,
      ALIGN_DATA_128_BYTE_BOUNDARY,
      ALIGN_DATA_256_BYTE_BOUNDARY,
      ALIGN_DATA_512_BYTE_BOUNDARY,
      ALIGN_DATA_1024_BYTE_BOUNDARY,
      ALIGN_DATA_2048_BYTE_BOUNDARY,
      ALIGN_DATA_4096_BYTE_BOUNDARY,
      ALIGN_DATA_8192_BYTE_BOUNDARY,
      EXTENDED_RELOCATIONS,
      DISCARDABLE,
      DO_NOT_CACHE,
      DO_NOT_PAGE,
      SHARABLE,
      EXECUTABLE,
      READABLE,
      WRITABLE);

   for Section_Characteristics use
     (RESERVED_1                      => 0,
      RESERVED_2                      => 2**0,
      RESERVED_3                      => 2**1,
      RESERVED_4                      => 2**2,
      DEPRECATED_NO_PAD_NEXT_BOUNDARY => 2**3,
      RESERVED_5                      => 2**4,
      EXECUTABLE_CODE                 => 2**5,
      INITIALIZED_DATA                => 2**6,
      UNINITIALIZED_DATA              => 2**7,
      RESERVED_OTHER_DATA             => 2**8,
      INFORMATION_DATA                => 2**9,
      RESERVED_7                      => 2**10,
      REMOVED_IN_IMAGE                => 2**11,
      COMDAT_DATA                     => 2**12,
      UNDEFINED_1                     => 2**13,
      UNDEFINED_2                     => 2**14,
      GLOBAL_POINTER_REFERENCED_DATA  => 2**15,
      UNDEFINED_3                     => 2**16,
      RESERVED_PURGABLE_OR_16_MEMORY  => 2**17,
      RESERVED_LOCKED_MEMORY          => 2**18,
      RESERVED_PRELOAD_MEMORY         => 2**19,
      ALIGN_DATA_1_BYTE_BOUNDARY      => 2**20,
      ALIGN_DATA_2_BYTE_BOUNDARY      => 2**21,
      ALIGN_DATA_4_BYTE_BOUNDARY      => 16#00_30_00_00#,
      ALIGN_DATA_8_BYTE_BOUNDARY      => 2**22,
      ALIGN_DATA_16_BYTE_BOUNDARY     => 16#00_50_00_00#,
      ALIGN_DATA_32_BYTE_BOUNDARY     => 16#00_60_00_00#,
      ALIGN_DATA_64_BYTE_BOUNDARY     => 16#00_70_00_00#,
      ALIGN_DATA_128_BYTE_BOUNDARY    => 2**23,
      ALIGN_DATA_256_BYTE_BOUNDARY    => 16#00_90_00_00#,
      ALIGN_DATA_512_BYTE_BOUNDARY    => 16#00_A0_00_00#,
      ALIGN_DATA_1024_BYTE_BOUNDARY   => 16#00_B0_00_00#,
      ALIGN_DATA_2048_BYTE_BOUNDARY   => 16#00_C0_00_00#,
      ALIGN_DATA_4096_BYTE_BOUNDARY   => 16#00_D0_00_00#,
      ALIGN_DATA_8192_BYTE_BOUNDARY   => 16#00_E0_00_00#,
      EXTENDED_RELOCATIONS            => 2**24,
      DISCARDABLE                     => 2**25,
      DO_NOT_CACHE                    => 2**26,
      DO_NOT_PAGE                     => 2**27,
      SHARABLE                        => 2**28,
      EXECUTABLE                      => 2**29,
      READABLE                        => 2**30,
      WRITABLE                        => 2**31);

   type Section_Characteristics_Flags is
     array (Section_Characteristics'First .. Section_Characteristics'Last)
     of Boolean with
     Put_Image => Section_Characteristics_Flags_Put_Image;
   procedure Section_Characteristics_Flags_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : Section_Characteristics_Flags);

   procedure Read_Section_Characteristics_Flags (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Section_Characteristics_Flags);
   for Section_Characteristics_Flags'Read use Read_Section_Characteristics_Flags;

   type Image_Section is record
      Name            : String (1 .. 8);
      Sizes           : Image_Section_Sizes;
      Pointers        : Image_Section_Pointers;
      Characteristics : Section_Characteristics_Flags;
   end record;

   procedure Read_Image_Section (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Image_Section);
   for Image_Section'Read use Read_Image_Section;

   package Image_Section_Vectors is new Ada.Containers.Vectors (Positive, Image_Section);

   type Object_Portable_Executable is record
      DOS_Header      : MZ_DOS_Header;
      Magic           : String (1 .. 4);
      Machine_Type    : Machine_Types;
      Time_Stamp      : Ada.Calendar.Time;
      Optional_Header : Optional_Header_Holders.Holder;
      Characteristics : COFF_Characteristics_Flags;
      Sections        : Image_Section_Vectors.Vector;
   end record;

   function Read_Object_Portable_Executable
     (File : Ada.Streams.Stream_IO.File_Type; Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Object_Portable_Executable;

end PE;
