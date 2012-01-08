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
-- $RCSfile: giant-file_management.ads,v $, $Revision: 1.23 $

-- $Author: squig $
-- $Date: 2003-09-20 20:27:37 $
--
-- -----------------------------------------------
--
-- This package offers basic functionality used by GIANT to handle files.
--
with Ada.Text_IO;
with Ada.Strings.Unbounded;
-- with Ada.Command_Line;

with GNAT.OS_Lib;

with String_Lists; -- from Bauhaus IML "Reuse.src"

package Giant.File_Management is

   ---------------------------------------------------------------------------
   -- Wraps GNAT type.
   subtype OS_Time is GNAT.OS_Lib.OS_Time;

   ---------------------------------------------------------------------------
   -- The system path separator.
   Path_Separator : constant String (1 .. 1)
     := (1 => GNAT.OS_Lib.Path_Separator);

   ---------------------------------------------------------------------------
   -- The maximum length of a file name (only the name - no path)
   Max_File_Name_Length : constant integer := 512;

   ---------------------------------------------------------------------------
   -- Raised if an not existing of incorrect directory is passed
   -- as parameter.
   Invalid_Directory_Exception : exception;

   ---------------------------------------------------------------------------
   -- Returns file names from a Directory.
   -- Each returned unbounded String in the list corresponds
   -- to the string returned by the function
   -- "function Name(File : in File_Type) return String;"
   -- from the package "Ada.Sequential_IO";
   -- It is not absolute certain but string is expected to be
   -- the absolute path of the file.
   -- This string is specified in the Ada 95 Reference Manual
   -- (according to the International Standard ISO/IEC 8652:1995(E)).
   --
   -- See ARM "A.8.2 File Management":
   -- 22 "Returns a string which uniquely identifies the external file
   --     currently associated with the given file (and may thus be used in
   --     an Open operation). If an external environment allows alternative
   --     specifications of the name (for example, abbreviations), the
   --     string returned by the function should correspond
   --     to a full specification of the name."
   --
   -- If Filter = True, then
   -- only Files which name ends with the Filter String are returned.
   -- E.g. Filter_String = ".xml" returns all Files "*.xml"
   -- in the given directory.
   -- If Filter = FALSE then all Files in the directory are
   -- returned regardeless of the ending.
   --
   -- Parameter:
   --   Path_To_Dir - A Path to a directory
   --     where the files should be searched.
   --   Filter - Determines whether all files found in the directory
   --     should be returned (False) or whether only the files
   --     with the ending passed in "Filter_String" should be
   --     returned (True).
   --   Filter_String - If "Filter" is "True" then only files
   --   which end with the string "Filter_String" are returned.
   -- Return:
   --   A List of all file names (incl. absolute path)
   --   that comply to the filter criterion.
   --   The returned list may be empty if no appropriate files
   --   were found.
   -- Raises:
   --   Invalid_Directory_Exception - Raised if the passed Path "Path_To_Dir"
   --   is no correct or existing path.
   function Get_Filtered_Files_From_Directory
     (Path_To_Dir    : in String;
      Filter         : in Boolean;
      Filter_String  : in String)
     return String_Lists.List;

   ---------------------------------------------------------------------------
   -- This function tries to locate a file in a passed list holding
   -- absolute paths to directories (relative paths will be expanded
   -- towards the working directory of the current execution
   -- environment).
   --
   -- The Directory_List is traversed in sequential order from the
   -- beginning. As soon as a file File_Name is found in a directory
   -- it will be returned - the absolute path will be calculated on
   -- "the first hit".
   --
   -- Invalid / not existing directories in Directory_List will be
   -- ignored.
   --
   -- Parameters:
   --   Directory_List - A list holding paths to directories.
   --   File_Name - The name of a file (incl. ending).
   -- Returns:
   --   An absolute path to the file File_Name if it is found in one
   --   of the passed directories. If the file is not found, then
   --   an empty String ("") will be returned.
   function Locate_File_In_Directories
      (Directory_List : in String_Lists.List;
       File_Name      : in String)
      return String;

   ---------------------------------------------------------------------------
   -- Raised if a file does not exist
   File_Does_Not_Exist_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if a file could not be deleted (not existing, not sufficient
   -- rights, no regular file ...)
   File_Cannot_Be_Deleted_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if a directory does not exist
   Directory_Does_Not_Exist_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if an existing directory path could not be calculated out
   -- of a path to a file name.
   Directory_Could_Not_Be_Calculated_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if no exsiting absolute path (to file or dir) could be
   -- calculated out of passed parameters.
   Abs_Path_Could_Not_Be_Calculated_Exception : exception;

   ---------------------------------------------------------------------------
   -- Deletes a File.
   --
   -- Parameters:
   --   File_Name - The file (incl. Path) that should be deleted.
   -- Raises
   --   File_Cannot_Be_Deleted_Exception - Raised if the file could not be
   --   found, accessed or deleted.
   procedure Delete_File (File_Name : in String);

   ---------------------------------------------------------------------------
   -- Copies a file, existing Target files will be overwritten
   --
   -- Parameters:
   --   Source - The name of the source file.
   --   Target - The name of the target file.
   procedure Copy_File (Source : in String; Target : in String);

   ---------------------------------------------------------------------------
   -- For a passed path to a directory this subprogram will check whether
   -- all directories that are part of the path exist, if not the missing
   -- directories (at the ende of "Dir_Path") will be created.
   --
   -- Parameters:
   --   Dir_Path: A path to a directory - you should only pass absolute
   --     paths. A path to a file may cause unpredictable errors.
   --     - If you pass a relative path it will be expanded regarding
   --       the working directory of the current execution environment.
   procedure Create_Dir_Path (Dir_Path : in String);

   ---------------------------------------------------------------------------
   --  Returns the timestamp of Filename. Filename needs to be unopened.
   function File_Time_Stamp
     (Filename : String)
      return OS_Time;

   ---------------------------------------------------------------------------
   -- For a given realtive Path to a file and a path to a directory there the
   -- relative path may begin, this subprogram calculates the aboslute path
   -- to that file as returned by:
   -- ---
   -- "function Name(File : in File_Type) return String;"
   -- from the package "Ada.Sequential_IO";
   -- ---
   -- For further Details see internal docu
   -- "function Get_Filtered_Files_From_Directory".
   --
   -- Parameters:
   --   Start_Dir - The directory there the relative path
   --     "Relative_Path_To_File" begins. "Start_Dir" may also
   --     be a relative path - then the absolute path will
   --     be calculated based on the "current working directory
   --     of the execution environment".
   --   Relative_Path_To_File - A relative path to a file
   --     having "Start_Dir" as root.
   --     If an absolute path is passed, this will cause no problems
   --     (then an equivalent absolute path will be returned).
   --
   -- Returns:
   --    A absolute Path to the File that is calculated based on
   --    "Start_Dir" and "Relative_Path_To_File".
   -- Raises:
   --   File_Does_Not_Exist_Exception - Raised if Start_Dir and
   --     Relative_Path_To_File do together not describe a path
   --     to an existing file.
   function Get_Absolute_Path_To_File_From_Relative
     (Start_Dir             : in String;
      Relative_Path_To_File : in String)
     return String;

   --------------------------------------------------------------------------
   -- Calculates an relative path out of an absolute path using a absolute
   -- path root.
   --
   -- Simply tries to Cut a way the Abs_Path_Root from Abs_Path.
   --
   -- Will return Abs_Path if no relative Path could be calculated.
   --
   -- Example:
   --  Abs_Path_Root = "/home"
   --  Abs_Path      = "/home/my_home_dir"
   --  --> Result    = "./my_home_dir"
   --
   -- Parameters:
   --   Abs_Path_Root - An absolute path describing the root towards the
   --     relative one should be calculated.
   --   Abs_Path - An absolute path for that a relative one should
   --    be calculated.
   -- Returns:
   --   A String describing a relative path if such a path could be calculated,
   --   if not Abs_Path will be returned.
   function Get_Relative_Path_From_Absolute
     (Abs_Path_Root : in String;
      Abs_Path      : in String)
     return String;

   ---------------------------------------------------------------------------
   --  Calculates an absolute path from an relative one for a directory.
   --
   --  Parameters:
   --    Start_Dir - The directory there the relative path
   --      "Rel_Dir_Path" begins. "Start_Dir" may also
   --      be a relative path - then the absolute path will
   --      be calculated based on the "current working directory
   --      of the execution environment".
   --    Rel_Dir_Path - A relative path to a directory. If an absolute path is
   --      passed the path will not be changed.
   --  Returns:
   --    An absolute Path for the passed relative path.
   --  Raises:
   --    Directory_Does_Not_Exist_Exception - Raised if Start_Dir and
   --      Rel_Dir_Path do not describe an existing directory.
   function Get_Absolute_Path_To_Directory_From_Relative
     (Start_Dir    : in String;
      Rel_Dir_Path : in String)
     return String;

   ----------------------------------------------------------------------------
   --  Tries to calculate absolute paths without differing between files
   --  and directories.
   --
   --  Directories are regarded as files by this subprogram therefore it
   --  is not garanted that this will work for other operating systems
   --  than Linux or Sun Solaris - use on your own risk.
   --
   --  Parameters:
   --    Start_Dir - The directory there the relative path
   --      "Rel_Dir_Path" begins. "Start_Dir" may also
   --      be a relative path - then the absolute path will
   --      be calculated based on the "current working directory
   --      of the execution environment".
   --    Rel_Path - A relative path to a file or directory.
   --      If an absolute path is passed the path will not be changed.
   --  Raises:
   --    Abs_Path_Could_Not_Be_Calculated_Exception - Raised if
   --      Start_Dir and Rel_Path together do not form a path to
   --      an existing file or directory.
   function Get_Absolute_Path_From_Relative
     (Start_Dir    : in String;
      Rel_Path : in String)
     return String;

   ---------------------------------------------------------------------------
   -- This procedure changes the current working directory for the execution
   -- environment so that it matches the directory there the Executable
   -- file of the program is located.
   procedure Set_Currunt_Working_Dir_To_Exec_Dir;

   ---------------------------------------------------------------------------
   -- Returns the "path" out of a string holding a file name including
   -- a path.
   --
   -- Only the path must exist. It is not checked whether the file exists
   -- too.
   -- If the passed path in "File_Path" is relative then a relative path
   -- to the directory will be returned if it is absolute then a absolute
   -- path will be returned.
   --
   -- Parameters:
   --   File_Path - A File Name String that holds a file name and a path.
   -- Returns:
   --   The Path to the directory there the file is loacted.
   --   If only a file name is passed (without a path) then the
   --   working directory for the current execution environment
   --   will be returned.
   -- Raises:
   --   Directory_Could_Not_Be_Calculated_Exception - Raised if the
   --   directory that is part of the file name "File_Path" does
   --   not exist.
   function Return_Dir_Path_For_File_Path (File_Path : in String)
     return String;

   ---------------------------------------------------------------------------
   -- Calculates a name out of a file name by neglecting the path and the
   -- ending that may be part of a file name,
   -- i.e. the ending (all characters after the last dot "." incl. the dot
   -- itself and the path is removed from "File_Name".
   --
   -- Does not check whether the file realy exists.
   --
   -- Paramters:
   --   File_Name - The full name (optional incl. path) of a file.
   -- Returns:
   --   The Name calculated for that file.
   -- Examples:
   --   - "./test/my_file.xml" --> "my_file"
   --   - "a.data"             --> "a"
   --   - "./../../data"       --> "data"
   function Calculate_Name_For_File (File_Name : in String)
     return String;

   ---------------------------------------------------------------------------
   -- Appends a directory separator if
   -- necessary (the passed String should be a path).
   --
   -- Example:
   -- "c:\my_dir" --> "c:\my_dir\" (for Windows Users)
   -- "./dir"     --> "./dir/"
   -- "/dir/"     --> "/dir/"
   --
   -- Parameters:
   --   Directory - A String describing a directory.
   -- Returns:
   --   A path ending with a directory separator.
   function Append_Dir_Separator_If_Necessary
     (Directory : in String)
     return String;

   ---------------------------------------------------------------------------
   --  Returns the path where the user settings are stored.
   --
   --  A trailing directory separator is appended.
   --
   function Get_User_Config_Path
     return String;

   ---------------------------------------------------------------------------
   --  Returns the path where the resources are located.
   --
   function Get_Shared_Path
     (Sub_Path : in String := "";
      Resource : in String := "")
     return String;

   ---------------------------------------------------------------------------
   -- Replaces Substrings in a String.
   -- Source is evaluated from Left to right, everytime Needle is found
   -- in Source it will be replaced by Fork.
   --
   --Example Source => "*A* is *A* ..."
   --        Needle => "*A*"
   --        Fork   => "Life"
   --    --> Result : "Life is Life ..."
   --
   -- Paramters:
   --   Source - The String where Needle should be replaced.
   --   Needle - The Substring in Fork that should be replaced.
   --   Fork   - The String with that Needle should be replaced.
   -- Returns:
   --   A new String
   function Substitute_Sub_Strings
     (Source : in String;
      Needle : in String;
      Fork   : in String)
     return String;


   ---------------------------------------------------------------------------
   -- Calls an external editor.
   -- Parameters are not checked for correctness.
   --
   -- Parameters:
   --   Command - A command line call string for an
   --     editor holding vild cards for a filename ("%f"),
   --     a column ("%c") and a line indication ("%l").
   --   Filename - An absolute path to a file thart should be opened
   --     (replaces "%f" in the Command parameter.
   --   Line - The line number where the cursor should be placed on.
   --   Column - The Column there the cursor should be placed on.
   procedure Execute_External_Editor
     (Command  : in String;
      Filename : in String;
      Line     : in Natural;
      Column   : in Natural);

   ---------------------------------------------------------------------------
   --  Returns the directory portion of the Filename. If Filename does
   --  contain a directory separator the empty string is returned.
   function Get_Path
     (Filename : in String)
     return String;

   ---------------------------------------------------------------------------
   --  Returns the filename portion of the Filename. If Filename does
   --  contain a directory separator Filename is returned.
   function Get_File
     (Filename : in String)
     return String;

end Giant.File_Management;
