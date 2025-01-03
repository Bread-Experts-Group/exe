with Ada.Text_IO;
with Ada.Directories;
with Ada.Streams.Stream_IO;

with GNAT.OS_Lib;
with GNAT.Strings;
with GNAT.Command_Line; use GNAT.Command_Line;

with PE;

procedure Exe is
   Config        : Command_Line_Configuration;
   File_Location : aliased GNAT.Strings.String_Access;
begin
   Define_Switch (Config, File_Location'Access, "-f=", "--file=", "Read off the specified file");
   Define_Switch (Config, "-h", "--help", "Display help");

   declare
      use type Ada.Directories.File_Kind;
   begin
      Getopt (Config);
      if not Ada.Directories.Exists (File_Location.all) then
         Ada.Text_IO.Put_Line ("A portable executable file was not provided, or does not exist.");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      if Ada.Directories.Kind (File_Location.all) /= Ada.Directories.Ordinary_File then
         Ada.Text_IO.Put_Line (f"The provided file path ({File_Location.all}) is not a file");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   exception
      when Exit_From_Command_Line =>
         Ada.Text_IO.Put_Line (f"\nReport problems to Bread Experts Group [https://github.com/Bread-Experts-Group/exe]");
         GNAT.OS_Lib.OS_Exit (0);

      when E : others =>
         Ada.Text_IO.Put_Line (f"Error reading arguments: {E.Exception_Information}");
         GNAT.OS_Lib.OS_Exit (1);
   end;

   -- File read, decoding time
   declare

      use Ada.Streams.Stream_IO;

      F : File_Type;
      S : Stream_Access;

   begin

      Open (F, In_File, File_Location.all);
      S := Stream (F);

      Ada.Text_IO.Put_Line (PE.Read_Object_Portable_Executable (F, S)'Image);

   end;
end Exe;
