with Ada.Streams;
with Ada.Calendar;
with Ada.Streams.Stream_IO;
with Ada.Strings.Text_Buffers;

package PE is

   type Unsigned_16_Little_Endian is mod 2**16;
   type Unsigned_32_Little_Endian is mod 2**32;

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

   type Characteristics is
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
      DEPRECATED_BIG_ENDIAN);

   for Characteristics use
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

   type COFF_Characteristics is
     array (Characteristics'First .. Characteristics'Last)
     of Boolean with
     Put_Image => COFF_Put_Image;

   procedure COFF_Put_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; Value : COFF_Characteristics);

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

   type Object_Portable_Executable is tagged record

      DOS_Header : MZ_DOS_Header;

      Magic           : String (1 .. 4);
      Machine_Type    : Machine_Types;
      Time_Stamp      : Ada.Calendar.Time;
      Characteristics : COFF_Characteristics;

   end record;

   function Read_Object_Portable_Executable
     (File : Ada.Streams.Stream_IO.File_Type; Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Object_Portable_Executable;

private

   procedure Read_Unsigned_16_Little_Endian (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Unsigned_16_Little_Endian);

   for Unsigned_16_Little_Endian'Read use Read_Unsigned_16_Little_Endian;

   procedure Read_Unsigned_32_Little_Endian (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Unsigned_32_Little_Endian);

   for Unsigned_32_Little_Endian'Read use Read_Unsigned_32_Little_Endian;

   procedure Read_Machine_Type (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Machine_Types);

   for Machine_Types'Read use Read_Machine_Type;

   procedure Read_MZ_DOS_Header (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out MZ_DOS_Header);

   for MZ_DOS_Header'Read use Read_MZ_DOS_Header;

   procedure Read_COFF_Characteristics (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out COFF_Characteristics);

   for COFF_Characteristics'Read use Read_COFF_Characteristics;

end PE;
