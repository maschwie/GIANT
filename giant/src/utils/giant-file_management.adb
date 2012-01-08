------------------------------------------------------------------------------
-- GIANT - Graphical IML Analysis and Navigation Tool
--
-- Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
-- Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-- First Author: Martin Schwienbacher
--
-- $RCSfile: giant-file_management.adb,v $, $Revision: 1.35 $
-- $Author: koppor $
-- $Date: 2003-10-01 23:00:06 $
--
--

with Ada.Exceptions;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Command_Line;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with Giant.Config;
with Giant.Config.Global_Data;

with Giant.Logger;          -- from GIANT
pragma Elaborate_All (Giant.Logger);

package body Giant.File_Management is

   -- logging functionality
   package Logger is new Giant.Logger ("Giant.File_Management");

   ---------------------------------------------------------------------------
   function Get_Filtered_Files_From_Directory
     (Path_To_Dir   : in String;
      Filter        : in Boolean;
      Filter_String : in String)
     return String_Lists.List is

      GNAT_Directory : GNAT.Directory_Operations.Dir_Type;

      -- The Result - holds all file names found in the directory.
      File_Names_List : String_Lists.List;

      -- (Files and directory)
      Potential_File_Name_String : String(1 .. Max_File_Name_Length);

      Last_Character_Pos : Integer := 0;

      Complete_File_Name : Ada.Strings.Unbounded.Unbounded_String;

      GNAT_File_Name_String_Access : GNAT.OS_Lib.String_Access;

      ADA_Text_IO_File : ADA.Text_IO.File_Type;

   begin

      -- Check whether directory exists
      begin
         GNAT.Directory_Operations.Open
           (GNAT_Directory, Path_To_Dir);
      exception
         when GNAT.Directory_Operations.Directory_Error =>
            raise Invalid_Directory_Exception;
      end;

      -- Creates the empty List about the file names
      File_Names_List := String_Lists.Create;

      -- Search and process (filter etc.) all files in the directory
      loop

         -- read a potential files out of the directory
         GNAT.Directory_Operations.Read
           (GNAT_Directory, Potential_File_Name_String, Last_Character_Pos);

         -- No Files left in directory - leave loop
         exit when (Last_Character_Pos <= 0);

         -- only take real files (filter directories etc.)
         -- (GNAT_File_Access = null for everything else than files).
         GNAT_File_Name_String_Access := GNAT.OS_Lib.Locate_Regular_File
           (File_Name => Potential_File_Name_String(1 .. Last_Character_Pos),
            Path      => Path_To_Dir);

         -- Filter ("remove") File_Names with not matching ending
         -- GNAT_File_Name_String_Acces.all must end with the
         -- character sequence of Filter_String
         --
         -- deallocates file names that are not needed according
         -- to the filter criterion
         if Filter
           and then GNAT.OS_Lib."/="(GNAT_File_Name_String_Access, null) then
            --  Filtering is active and a filename was read
            if
              (GNAT_File_Name_String_Access.all'Length < Filter_String'Length)
             or else
              (GNAT_File_Name_String_Access.all
               (GNAT_File_Name_String_Access.all'Last
                - Filter_String'Length + 1
                ..
                GNAT_File_Name_String_Access.all'Last) /= Filter_String) then
               GNAT.OS_Lib.Free(GNAT_File_Name_String_Access);
            end if;
         end if;

         --  calculate full path and put result into list
         --  "expand to full path"
         if GNAT.OS_Lib."/="(GNAT_File_Name_String_Access, null) then

            -- calculate "real" file name including path
            ADA.Text_IO.Open
              (File => ADA_Text_IO_File,
               Mode => ADA.Text_IO.In_File,
               Name => GNAT_File_Name_String_Access.all);

            -- supposed to calculate a absolute path including
            -- the name of the file (see Ada Reference Manual).
            -- No warantee!
            Complete_File_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
              (ADA.Text_IO.Name(ADA_Text_IO_File));

            ADA.Text_IO.Close(ADA_Text_IO_File);

            -- insert file name into the list:
            String_Lists.Attach
              (File_Names_List,
               Complete_File_Name);

            -- deallocates file name object
            GNAT.OS_Lib.Free(GNAT_File_Name_String_Access);
         end if;

      end loop;

      GNAT.Directory_Operations.Close (GNAT_Directory);
      return File_Names_List;
   end Get_Filtered_Files_From_Directory;

   ---------------------------------------------------------------------------
   function Locate_File_In_Directories
     (Directory_List : in String_Lists.List;
      File_Name      : in String)
     return String is

      Iter : String_Lists.ListIter;
      -- A potential directory
      Pot_Directory : Ada.Strings.Unbounded.Unbounded_String;

   begin

       Iter := String_Lists.MakeListIter (Directory_List);

       while String_Lists.More(Iter) loop

          String_Lists.Next (Iter, Pot_Directory);

          -- ignore not existing / invalid directories
          if GNAT.OS_Lib.Is_Directory
            (Ada.Strings.Unbounded.To_String (Pot_Directory)) then
             begin
                return Get_Absolute_Path_To_File_From_Relative
                  (Ada.Strings.Unbounded.To_String (Pot_Directory),
                   File_Name);
             exception
                when File_Does_Not_Exist_Exception =>
                  null;
             end;
          end if;
       end loop;

       -- return empty String if no file could be located
       return "";
   end Locate_File_In_Directories;

   ---------------------------------------------------------------------------
   procedure Delete_File (File_Name : in String) is

      The_File : Ada.Text_IO.File_Type;
   begin

     if not (GNAT.OS_Lib.Is_Writable_File (File_Name))
        or not (GNAT.OS_Lib.Is_Regular_File (File_Name)) then

        raise File_Cannot_Be_Deleted_Exception;
     end if;

     Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, File_Name);
     Ada.Text_IO.Delete (The_File);
   end Delete_File;

   ---------------------------------------------------------------------------
   procedure Copy_File (Source : in String; Target : in String) is

      use Ada.Streams;

      Source_File : Ada.Streams.Stream_IO.File_Type;
      Target_File :Ada.Streams.Stream_IO.File_Type;
      Trans_Data : Ada.Streams.Stream_Element_Array (1 .. 1000);
      Last_Element : Ada.Streams.Stream_Element_Offset;
   begin

      Ada.Streams.Stream_IO.Open
        (Source_File,
         Ada.Streams.Stream_IO.In_File,
         Source);

      Ada.Streams.Stream_IO.Create
        (Target_File,
         Ada.Streams.Stream_IO.Out_File,
         Target);

      loop
         Ada.Streams.Stream_IO.Read
           (Source_File,
            Trans_Data,
            Last_Element);

         if Last_Element < Trans_Data'Last then
            Ada.Streams.Stream_IO.Write
              (Target_File,
               Trans_Data (Trans_Data'First .. Last_Element));
            exit;
         else

            Ada.Streams.Stream_IO.Write (Target_File, Trans_Data);
         end if;
      end loop;

      Ada.Streams.Stream_IO.Close (Source_File);
      Ada.Streams.Stream_IO.Close (Target_File);
   end Copy_File;

   ---------------------------------------------------------------------------
   procedure Create_Dir_Path (Dir_Path : in String) is

      Dir_Separator : Character := GNAT.OS_Lib.Directory_Separator;
   begin

      for I in Dir_Path'range loop
        if (Dir_Path (I) = Dir_Separator) or (I = Dir_Path'Last) then
           if not GNAT.OS_Lib.Is_Directory (Dir_Path (Dir_Path'First .. I))
           then

              GNAT.Directory_Operations.Make_Dir
                (Dir_Path (Dir_Path'First .. I));
           end if;
        end if;
      end loop;
   end Create_Dir_Path;

   function File_Time_Stamp
     (Filename : String)
      return OS_Time
   is
   begin
      return GNAT.OS_Lib.File_Time_Stamp (Filename);
   end File_Time_Stamp;

   ---------------------------------------------------------------------------
   function Get_Absolute_Path_To_File_From_Relative
     (Start_Dir : in String;
      Relative_Path_To_File : in String) return String is

      -- store "working directory for the execution environment"
      Old_Exec_Dir : String := GNAT.Directory_Operations.Get_Current_Dir;

      -- needed to calculate an absolute path
      ADA_Text_IO_File : ADA.Text_IO.File_Type;

      Abs_Path : Ada.Strings.Unbounded.Unbounded_String;

   begin

      if (GNAT.OS_Lib.Is_Directory (Start_Dir) = False) then

         -- if an invalid start directory is passed the file obviously could
         -- not be found
         raise File_Does_Not_Exist_Exception;
      end if;

      GNAT.Directory_Operations.Change_Dir (Start_Dir);

      begin
         ADA.Text_IO.Open
           (File => ADA_Text_IO_File,
            Mode => ADA.Text_IO.In_File,
            Name => Relative_Path_To_File);

         Abs_Path := Ada.Strings.Unbounded.To_Unbounded_String
           (ADA.Text_IO.Name(ADA_Text_IO_File));

         ADA.Text_IO.Close(ADA_Text_IO_File);
      exception

         when ADA.Text_IO.Name_Error =>
           raise File_Does_Not_Exist_Exception;
      end;

      -- check for a regular file
      if (GNAT.OS_Lib.Is_Regular_File
        (Ada.Strings.Unbounded.To_String (Abs_Path)) = False) then

         raise File_Does_Not_Exist_Exception;
      end if;

      -- switch back to old
      -- "working directory for the execution environment"
      GNAT.Directory_Operations.Change_Dir (Old_Exec_Dir);

      return Ada.Strings.Unbounded.To_String (Abs_Path);

   exception
      when E : others =>
         GNAT.Directory_Operations.Change_Dir (Old_Exec_Dir);
         Ada.Exceptions.Reraise_Occurrence (E);
   end Get_Absolute_Path_To_File_From_Relative;


   ---------------------------------------------------------------------------
   function Get_Relative_Path_From_Absolute
     (Abs_Path_Root : in String;
      Abs_Path      : in String)
     return String is

      Dir_Separator : Character := GNAT.OS_Lib.Directory_Separator;

   begin

      if (Abs_Path_Root'Length > Abs_Path'Length) then
         return Abs_Path;

      elsif (Abs_Path_Root'Length = Abs_Path'Length) and then
        Abs_Path_Root (Abs_Path_Root'Range) = Abs_Path (Abs_Path'Range) then

        return ("." & Dir_Separator);

      -- calculate relative path
      elsif (Abs_Path_Root'Length < Abs_Path'Length) and then
        (Abs_Path_Root (Abs_Path_Root'Range) = Abs_Path
          (Abs_Path'First .. Abs_Path'First + Abs_Path_Root'Length - 1)) then

         if (Abs_Path (Abs_Path'First + Abs_Path_Root'Length)
           = Dir_Separator) then

            return ("." & Abs_Path
              (Abs_Path'First + Abs_Path_Root'Length .. Abs_Path'Last));
         else

            return ("." & Dir_Separator & Abs_Path (Abs_Path'First +
              Abs_Path_Root'Length .. Abs_Path'Last));
         end if;
      else

         return Abs_Path;
      end if;

   end Get_Relative_Path_From_Absolute;

   ---------------------------------------------------------------------------
   function Get_Absolute_Path_To_Directory_From_Relative
     (Start_Dir    : in String;
      Rel_Dir_Path : in String)
     return String is

      Old_Exec_Dir : String := GNAT.Directory_Operations.Get_Current_Dir;
      Abs_Dir_Path : Ada.Strings.Unbounded.Unbounded_String;
   begin

      GNAT.Directory_Operations.Change_Dir (Start_Dir);
      GNAT.Directory_Operations.Change_Dir (Rel_Dir_Path);

      -- should return an absolute path
      Abs_Dir_Path := Ada.Strings.Unbounded.To_Unbounded_String
        (GNAT.Directory_Operations.Get_Current_Dir);

      GNAT.Directory_Operations.Change_Dir (Old_Exec_Dir);

      return Ada.Strings.Unbounded.To_String (Abs_Dir_Path);
   exception
      when GNAT.Directory_Operations.Directory_Error =>
         raise Directory_Does_Not_Exist_Exception;
   end Get_Absolute_Path_To_Directory_From_Relative;

   ---------------------------------------------------------------------------
   function Get_Absolute_Path_From_Relative
     (Start_Dir    : in String;
      Rel_Path : in String)
     return String
   is
      -- store "working directory for the execution environment"
      Old_Exec_Dir : String := GNAT.Directory_Operations.Get_Current_Dir;

      -- needed to calculate an absolute path
      ADA_Text_IO_File_or_Dir : ADA.Text_IO.File_Type;

      Abs_Path : Ada.Strings.Unbounded.Unbounded_String;

   begin
      if (GNAT.OS_Lib.Is_Directory (Start_Dir) = False) then
         -- if an invalid start directory is passed the file
         -- obviously could not be found
         raise Abs_Path_Could_Not_Be_Calculated_Exception;
      end if;

      GNAT.Directory_Operations.Change_Dir (Start_Dir);

      begin
         ADA.Text_IO.Open
           (File => ADA_Text_IO_File_or_Dir,
            Mode => ADA.Text_IO.In_File,
            Name => Rel_Path);

         Abs_Path := Ada.Strings.Unbounded.To_Unbounded_String
           (ADA.Text_IO.Name(ADA_Text_IO_File_or_Dir));

         ADA.Text_IO.Close(ADA_Text_IO_File_or_Dir);
      exception

         when ADA.Text_IO.Name_Error =>
           raise Abs_Path_Could_Not_Be_Calculated_Exception;
      end;

      GNAT.Directory_Operations.Change_Dir (Old_Exec_Dir);

      return Ada.Strings.Unbounded.To_String (Abs_Path);
   exception
      when E : others =>
         GNAT.Directory_Operations.Change_Dir (Old_Exec_Dir);
         Ada.Exceptions.Reraise_Occurrence (E);
   end Get_Absolute_Path_From_Relative;

   ---------------------------------------------------------------------------
   procedure Set_Currunt_Working_Dir_To_Exec_Dir is

      Exec_File_Call_String : String := Ada.Command_Line.Command_Name;

   begin

      GNAT.Directory_Operations.Change_Dir
        (Return_Dir_Path_For_File_Path (Exec_File_Call_String));

   end Set_Currunt_Working_Dir_To_Exec_Dir;

   ---------------------------------------------------------------------------
   function Return_Dir_Path_For_File_Path (File_Path : String)
                                          return String is

      -- Position of last directory spearator in "File_Path"
      -- that separates the path from the file name;
      Dir_Separator : Character := GNAT.OS_Lib.Directory_Separator;
   begin
      for I in reverse File_Path'Range loop
         if (File_Path(I) = Dir_Separator) then
            if (GNAT.OS_Lib.Is_Directory
                (File_Path(File_Path'First .. I)) = False) then
               raise Directory_Could_Not_Be_Calculated_Exception;
            end if;
            return File_Path(File_Path'First .. I);
         end if;
      end loop;

      -- Return current "working directory for the execution environment"
      -- if only a filename with no path (directory) was passed.
      return GNAT.Directory_Operations.Get_Current_Dir;
   end Return_Dir_Path_For_File_Path;

   ---------------------------------------------------------------------------
   function Calculate_Name_For_File (File_Name : in String)
     return String is

       Name : Ada.Strings.Unbounded.Unbounded_String;

       Dir_Separator : Character := GNAT.OS_Lib.Directory_Separator;

       Cut_Dot_Index : Integer := 0;
       Dot_Found : Boolean := False;

       Cut_Dir_Sep_Index : Integer := 0;
       Dir_Sep_Found : Boolean := False;

   begin

      for I in reverse File_Name'Range loop

         -- search firs dot "." from behind
         if (Dot_Found = False)
           and then (File_Name(I) = '.') then

            Dot_Found := True;
            Cut_Dot_Index := I;
         end if;

         -- search first directory separator from behind
         if (Dir_Sep_Found = False)
           and then (File_Name(I) = Dir_Separator) then

            Dir_Sep_Found := True;
            Cut_Dir_Sep_Index := I;

            -- nessary because the dot must be behind the first
            -- directory separator
            exit;
         end if;

      end loop;

      -- calculate name
      if (Dir_Sep_Found = True) and (Dot_Found = True)  then

         Name := Ada.Strings.Unbounded.To_Unbounded_String
           (File_Name (Cut_Dir_Sep_Index + 1 .. Cut_Dot_Index - 1));
      elsif (Dir_Sep_Found = True) and (Dot_Found = False) then

         Name := Ada.Strings.Unbounded.To_Unbounded_String
           (File_Name (Cut_Dir_Sep_Index + 1 .. File_Name'Last));
      elsif (Dir_Sep_Found = False) and (Dot_Found = True) then

         Name := Ada.Strings.Unbounded.To_Unbounded_String
           (File_Name (File_Name'First .. Cut_Dot_Index - 1));
      else
         Name := Ada.Strings.Unbounded.To_Unbounded_String (File_Name);
      end if;

      return Ada.Strings.Unbounded.To_String (Name);
   end Calculate_Name_For_File;

   ---------------------------------------------------------------------------
   function Append_Dir_Separator_If_Necessary
     (Directory : in String)
     return String is
      Dir_Separator : Character := GNAT.OS_Lib.Directory_Separator;
   begin
      if (Directory = "") then
         return "";
      end if;

      -- append directory separator if necessary
      if (Directory (Directory'Last) = Dir_Separator) then

         return Directory;
      else

         return (Directory & Dir_Separator);
      end if;
   end Append_Dir_Separator_If_necessary;

   User_Config_Path : constant String
     := Append_Dir_Separator_If_Necessary (GNAT.OS_Lib.Getenv ("HOME").all)
     & ".giant" & GNAT.OS_Lib.Directory_Separator;

   ---------------------------------------------------------------------------
   function Get_User_Config_Path
     return String
   is
   begin
      return User_Config_Path;
   end Get_User_Config_Path;

   ---------------------------------------------------------------------------
   function Get_Shared_Path
     (Sub_Path : in String := "";
      Resource : in String := "")
     return String
   is
   begin
      if (Resource /= "") then
         return Config.Global_Data.Get_Resources_Directory & Sub_Path
           & GNAT.OS_Lib.Directory_Separator & Resource;
      else
         return Config.Global_Data.Get_Resources_Directory & Sub_Path;
      end if;
   end Get_Shared_Path;

   ---------------------------------------------------------------------------
   procedure Deallocate_Argument_List_Content
     (The_Arg_List : in out GNAT.OS_Lib.Argument_List) is

   begin

      for I In The_Arg_List'Range loop

         GNAT.OS_Lib.Free (The_Arg_List (I));
      end loop;
   end Deallocate_Argument_List_Content;

   ---------------------------------------------------------------------------
   procedure Free_Argument_List_Access is new Ada.Unchecked_Deallocation
     (GNAT.OS_Lib.Argument_List, GNAT.OS_Lib.Argument_List_Access);

   ---------------------------------------------------------------------------
   function Substitute_Sub_Strings
     (Source : in String;
      Needle : in String;
      Fork   : in String)
     return String is

      use Ada.Strings.Unbounded;

      Result : Ada.Strings.Unbounded.Unbounded_String;
      Pos    : Integer;
   begin

      if (Source'Length >= Needle'Length) then

         Result     := Ada.Strings.Unbounded.Null_Unbounded_String;
         Pos := Source'First;

         loop
            exit when not (Pos <= (Source'Last));

            if (Pos <= Source'Last - Needle'Length + 1)
              and then (Source (Pos .. (Pos + Needle'Length - 1)) = Needle)
              then

               -- Substitution
               Ada.Strings.Unbounded.Append (Result, Fork);
               Pos := Pos + Needle'Length;
            else
               Ada.Strings.Unbounded.Append (Result, Source (Pos));
               Pos := Pos + 1;
            end if;
         end loop;

         return Ada.Strings.Unbounded.To_String (Result);
      else

        return Source;
      end if;
   end Substitute_Sub_Strings;

   ---------------------------------------------------------------------------
   procedure Execute
     (Command : in String)
   is
      use GNAT.OS_Lib;

      Args   : GNAT.OS_Lib.Argument_List_Access;
      Pid    : GNAT.OS_Lib.Process_Id;
      Prog   : GNAT.OS_Lib.String_Access;
   begin
      Args := GNAT.OS_Lib.Argument_String_To_List (Command);
      Prog := GNAT.OS_Lib.Locate_Exec_On_Path (Args (Args'First).all);

      if Prog /= null then
         Pid := GNAT.OS_Lib.Non_Blocking_Spawn
           (Prog.all, Args (Args'First + 1 .. Args'Last));
         Free (Prog);
      end if;

      if Args /= null then
         for J in Args'Range loop
            GNAT.OS_Lib.Free (Args (J));
         end loop;

         Deallocate_Argument_List_Content (Args.all);
         Free_Argument_List_Access (Args);
      end if;
   end Execute;

   ---------------------------------------------------------------------------
   procedure Execute_External_Editor
     (Command  : in String;
      Filename : in String;
      Line     : in Natural;
      Column   : in Natural) is

      Parsed_Command_String : Ada.Strings.Unbounded.Unbounded_String;
   begin

      Parsed_Command_String :=
        Ada.Strings.Unbounded.To_Unbounded_String (Command);

      Parsed_Command_String := Ada.Strings.Unbounded.To_Unbounded_String
        (Substitute_Sub_Strings
          (Ada.Strings.Unbounded.To_String (Parsed_Command_String),
           "%l",
           Ada.Strings.Fixed.Trim (Integer'Image(Line), Ada.Strings.Both)));

      Parsed_Command_String := Ada.Strings.Unbounded.To_Unbounded_String
        (Substitute_Sub_Strings
          (Ada.Strings.Unbounded.To_String (Parsed_Command_String),
           "%c",
           Ada.Strings.Fixed.Trim (Integer'Image(Column), Ada.Strings.Both)));

      Parsed_Command_String := Ada.Strings.Unbounded.To_Unbounded_String
        (Substitute_Sub_Strings
          (Ada.Strings.Unbounded.To_String (Parsed_Command_String),
           "%f",
           Filename));

      Execute (Ada.Strings.Unbounded.To_String (Parsed_Command_String));
   end Execute_External_Editor;

   ---------------------------------------------------------------------------
   function Get_Path
     (Filename : in String)
     return String
   is
      Path : Ada.Strings.Unbounded.Unbounded_String;
      Dir_Separator : Character := GNAT.OS_Lib.Directory_Separator;
   begin
      for I in reverse Filename'Range loop
         if (Filename (I) = Dir_Separator) then
            return Filename (Filename'First .. I);
         end if;
      end loop;
      return "";
   end Get_Path;

   ---------------------------------------------------------------------------
   function Get_File
     (Filename : in String)
     return String
   is
      Path : Ada.Strings.Unbounded.Unbounded_String;
      Dir_Separator : Character := GNAT.OS_Lib.Directory_Separator;
   begin
      for I in reverse Filename'Range loop
         if (Filename (I) = Dir_Separator) then
            return Filename (I + 1 .. Filename'Last);
         end if;
      end loop;
      return Filename;
   end Get_File;

end Giant.File_Management;
