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
-- $RCSfile: giant-config_settings.ads,v $, $Revision: 1.19 $
-- $Author: squig $
-- $Date: 2003-09-02 20:11:08 $
--
-- -----
-- This package holds the functionality needed to access and handle
-- xml configuration files used for giant.
--
-- The Implementation is quite independent from giant itself.
-- Therefore the settings are simply read from the files
-- into that ADO -
-- If necessary special interpretation is done by other packages.
--
with Ada.Strings.Unbounded;

with String_Lists;  -- from Bauhaus Reuse.src

package Giant.Config_Settings is

   ---------------------------------------------------------------------------
   -- Used to identify a config file by the top level node (document node)
   -- of a xml file. Only files with this top level node will be accepted
   -- duiring initialisation of this ADO.
   Config_File_Identifier : constant String := "giant_config_file";

   ---------------------------------------------------------------------------
   -- 0.1 Validators
   --
   -- Validators check settings (String values) that are read from
   -- xml config files whether they correspond to a required format etc.
   -- If a validator function returns "False" the Default Setting will be
   -- used instead of the read settings.
   --
   -- In this version validators are only used while reading a config
   -- file, not while writing one or changing config settings.
   --
   -- The default settings itself are not checked in any way.
   type Validator is access function (Value : in String) return Boolean;

   ---------------------------------------------------------------------------
   -- Checks whether "Val" may be converted to Integer by
   -- Integer'Value(Value).
   function Validate_Integer (Value : in String) return Boolean;

   ---------------------------------------------------------------------------
   -- 0.2 Default Settings
   --
   -- These settings are used if the setting is not found in the config files
   -- or if a read setting is not valid (see validators).
   ---------------------------------------------------------------------------

   -- Only used to build the array "Default_Settings"
   function To_UStr (Value : String)
     return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   type Default_Setting_Element is record
     -- the name of a setting
     Name      : Ada.Strings.Unbounded.Unbounded_String;
     -- the default value for that setting
     Def_Value : Ada.Strings.Unbounded.Unbounded_String;
     -- used to check whether a read setting is valid
     -- (may be a null pointer if no validation should be done)
     Validator_Function : Validator;
   end record;

   type Default_Settings_Array is array (integer range <>)
     of Default_Setting_Element;

   ---------------------------------------------------------------------------
   -- Here you may enter default values for necessary settings.
   -- If a setting is not found in the config files, these default
   -- values will be used.
   Default_Settings : constant Default_Settings_Array :=
     (
       (To_UStr ("Resources_Directory"),
        To_Ustr ("."), null),

       (To_UStr ("Icon_For_Node_Annotations"),
        To_UStr ("my_icon.xpm"), null),

       (To_UStr ("Current_Selection_Highlight_Color"),
        To_UStr ("#AAAAAA"), null),

       (To_UStr ("Selection_Highlight_Color_1"),
        To_UStr ("#AAAAAA"), null),

       (To_UStr ("Selection_Highlight_Color_2"),
        To_UStr ("#AAAAAA"), null),

       (To_UStr ("Selection_Highlight_Color_3"),
        To_UStr ("#AAAAAA"), null),

       (To_UStr ("IML_Subgraph_Highlight_Color_1"),
        To_UStr ("#AAAAAA"), null),

       (To_UStr ("IML_Subgraph_Highlight_Color_2"),
        To_UStr ("#AAAAAA"), null),

       (To_UStr ("IML_Subgraph_Highlight_Color_3"),
        To_UStr ("#AAAAAA"), null),

       -- You may enter a sequence of paths separated by the OS'
       -- path separator for environment variables
       (To_UStr ("GSL.Include_Paths"),
        To_UStr ("."), null),

       (To_UStr ("Main_Window.Height"),
        To_UStr ("400"),
        Validate_Integer'Access),

       (To_UStr ("Main_Window.Width"),
        To_UStr ("200"),
        Validate_Integer'Access),

       (To_UStr ("Main_Window.Separator"),
        To_UStr ("230"),
        Validate_Integer'Access),

       (To_UStr ("Confirm.Delete"),
        To_UStr ("True"),
        null),

       (To_UStr ("Editor.Source"),
        To_UStr ("/usr/bin/emacs +%l:%c %f"),
        null),

      (To_UStr ("GSL.No_Param"),
       To_UStr (""),
       null),

      (To_UStr ("GSL.No_Param_Context"),
       To_UStr (""),
       null),

      (To_UStr ("GSL.Node_Id_Param"),
       To_UStr (""),
       null),

      (To_UStr ("GSL.Edge_Id_Param"),
       To_UStr (""),
       null),

      (To_UStr ("GSL.Subgraph_Param"),
       To_UStr (""),
       null),

      (To_UStr ("GSL.Selection_Param"),
       To_UStr (""),
       null)
      );

   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and finalisation
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Raised if a passed config file could not be parsed as a
   -- valid xml file.
   Config_File_Not_Correct_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if subprograms are called before the ADO that holds the config
   -- was initialized.
   Config_Settings_ADO_Not_Initialized_Exception : exception;


   ---------------------------------------------------------------------------
   -- Initializes the ADO and reads the configuration data from the given
   -- config files.
   --
   -- Each setting has a unique identifier - a "name" string.
   --
   -- The settings are read in sequential order following the
   -- xml node hierarchy. First the file "GIANT_Config_File" is
   -- read, second the file "User_Config_File".
   -- If a setting with the same indentifier is found
   -- twice (e.g. in GIANT_Config_File and in User_Config_File)
   -- the setting first read is replaced
   -- by the setting that is later read.
   --
   -- After processing the files the validators will replace all non
   -- valid setting values by the default value.
   -- For all settings not found in the config files that have a default
   -- value this default value will be taken.
   --
   -- Parameters:
   --   GIANT_Config_File - The file (filename and path) where
   --     the GIANT config file is stored. Such a config file must
   --     exist. This may be an empty string ("") when no file will be read.
   --
   --   User_Config_File - A userdefined config file.
   --     "User_Config_File may be an empty string (""), then this parameter
   --      will be ignored and no user defined config file will be loaded.
   --
   -- Raises:
   --   Config_File_Not_Correct_Exception - Raised when the file passed
   --     (parameter "GIANT_Config_File" and "User_Config_File") is
   --     not a correct (valid) config file.
   procedure Initialize_Config_Settings
     (GIANT_Config_File : in String;
      User_Config_File  : in String);

   ---------------------------------------------------------------------------
   -- Finalizes the ADO;
   -- Deallocates all memory used by the ADO.
   -- After the call of this procedure "Initialize_Config_Settings" may be
   -- called without causing memory leaks.
   -- After the call the ADO is regarded as not initialized.
   --
   -- Note
   --  The types "Chars_Ptr_Array_Access" and "Color_Access", that
   --  are returned by some functions of this package, are pointers
   --  that point directly into the internal data structure of
   --  the ADO. Because of that you should not use these pointers
   --  after the finalisation of the ADO, otherwise
   --  you will definitely have dangling pointers
   --
   -- Raises:
   --   Config_Settings_ADO_Not_Initialized_Exception -
   --     raised if this subprogram is called before "
   --     Initialize_Config_Settings".
   procedure Clear_Config_Data;


   ---------------------------------------------------------------------------
   -- B
   -- Access to configuration data
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Raised if a config setting identified by its unique name does not
   -- exist.
   Config_Setting_Does_Not_Exist_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if a setting demanded as an integer value could not be converted
   -- into an integer value.
   Config_Setting_Is_Not_An_Integer_Value : exception;

   ---------------------------------------------------------------------------
   -- Raised if a setting demanded as an integer value could not be converted
   -- into an integer value.
   Invalid_Type_Exception : exception;

   ---------------------------------------------------------------------------
   -- This function is used to determine whether a config setting
   -- exists or not.
   --
   -- Parameters:
   --   Name - The name (each setting has an unique name) of
   --     the setting.
   -- Returns:
   --   True, if the config setting exists; False, otherwise.
   -- Raises:
   --   Config_Settings_ADO_Not_Initialized_Exception -
   --     raised if this subprogram is called before "
   --     Initialize_Config_Settings".
   function Does_Setting_Exist
     (Name : in String) return Boolean;

   ---------------------------------------------------------------------------
   -- This method returns a config setting as a string.
   -- As described above each config setting is identified by a unique
   -- name (a sting).
   --
   -- Parameters:
   --   Name - The unique identifier of a config setting.
   -- Returns:
   --   The the config setting corresponding to "Name_of_Setting"
   -- Raises:
   --   Config_Settings_ADO_Not_Initialized_Exception -
   --     raised if this subprogram is called before "
   --     Initialize_Config_Settings".
   --   Config_Setting_Does_Not_Exist_Exception - raised if there is
   --     no config setting with the name "Name";
   function Get_Setting_As_String
     (Name : in String) return String;

   ---------------------------------------------------------------------------
   -- This method returns a config setting.
   --
   -- This setting must hold an path to a directory or a file.
   -- If the path is an relative path it will be expanded regarding the
   -- following rules:
   --
   -- Config Settings that describe a path to directory must end with
   -- a directory separator.
   --
   -- 1. Expansion towards the value of the "root_directory" attribute
   --    of the <absolute_path_root> - node if exists in the config
   --    file.
   -- 2. If there is no <absolute_path_root> - node or no existing
   --    path could be calculated. Then expansion is done based on
   --    the directory of the config file from that the setting
   --    was read.
   -- 3. If still no path to an existing directory of file could be
   --    calculated then an empty String ("") will be returned.
   --
   -- Parameters:
   --   Name - The unique identifier of a config setting.
   -- Returns:
   --   A path to a directory or file calculated to the rules described
   --   above.
   -- Raises:
   --   Config_Settings_ADO_Not_Initialized_Exception -
   --     raised if this subprogram is called before "
   --     Initialize_Config_Settings".
   --   Config_Setting_Does_Not_Exist_Exception - raised if there is
   --     no config setting with the name "Name";
   function Get_Setting_With_Path_Expanded (Name : in String) return String;

   ---------------------------------------------------------------------------
   -- Same functionality as Get_Setting_With_Path_Expanded - used for
   -- settings that hold several paths to directories or files.
   --
   -- Splitts a setting holding several paths, each element of the list
   -- will hold an expanded path.
   --
   -- Not expandable paths will be ignored.
   --
   -- Paths have to be separated by the OS'
   -- path separator for environment variables.
   --
   -- Parameters:
   --   Name - The unique identifier of a config setting.
   -- Returns:
   --   A list holding all expanded paths. May return an empty list
   --   if no expan dable paths are found. You are responsible for
   --   the deallocation of the result.
   -- Raises:
   --   Config_Settings_ADO_Not_Initialized_Exception -
   --     raised if this subprogram is called before "
   --     Initialize_Config_Settings".
   --   Config_Setting_Does_Not_Exist_Exception - raised if there is
   --     no config setting with the name "Name";
   function Get_Setting_As_Expanded_Path_List
     (Name : in String)
     return String_Lists.List;

   ---------------------------------------------------------------------------
   -- This method returns a config setting as a boolean value.
   -- As described above, each config setting is identified by a unique
   -- name (a string).
   --
   -- Parameters:
   --   Name - The unique identifier of a config setting.
   -- Returns:
   --   The config setting corresponding to "Name_of_Setting"
   -- Raises:
   --   Config_Settings_ADO_Not_Initialized_Exception -
   --     raised if this subprogram is called before "
   --     Initialize_Config_Settings".
   --   Config_Setting_Does_Not_Exist_Exception - raised if there is
   --     no config setting with the name "Name".
   --   Config_Setting_Is_Not_An_Integer_Value - raised if the
   --     value if the setting "Name_Of_Setting" is not an integer value.
   function Get_Setting_As_Boolean (Name : in String)
     return Boolean;

   ---------------------------------------------------------------------------
   -- This method returns a config setting as a integer value.
   -- As described above, each config setting is identified by a unique
   -- name (a string).
   --
   -- Parameters:
   --   Name - The unique identifier of a config setting.
   -- Returns:
   --   The config setting corresponding to "Name_of_Setting"
   -- Raises:
   --   Config_Settings_ADO_Not_Initialized_Exception -
   --     raised if this subprogram is called before "
   --     Initialize_Config_Settings".
   --   Config_Setting_Does_Not_Exist_Exception - raised if there is
   --     no config setting with the name "Name".
   --   Config_Setting_Is_Not_An_Integer_Value - raised if the
   --     value if the setting "Name_Of_Setting" is not an integer value.
   function Get_Setting_As_Integer (Name : in String)
     return Integer;

   ---------------------------------------------------------------------------
   -- Adds a new setting or changes the value of an existing setting.
   --
   -- Parameters:
   --   Name - The name of the setting that should
   --     be added or those value should be changed (if already
   --     exists).
   --   Value - The new value for that setting.
   -- Raises:
   --   Config_Settings_ADO_Not_Initialized_Exception -
   --     raised if this subprogram is called before "
   --     Initialize_Config_Settings".
   procedure Set_Setting (Name : in String; Value : in String);

   ---------------------------------------------------------------------------
   -- Adds a new setting or changes the value of an existing setting.
   --
   -- Parameters:
   --   Name - The name of the setting that should
   --     be added or those value should be changed (if already
   --     exists).
   --   Value - A integer value for the setting - will be converted
   --     to String by Integer'Image (Value).
   -- Raises:
   --   Config_Settings_ADO_Not_Initialized_Exception -
   --     raised if this subprogram is called before "
   --     Initialize_Config_Settings".
   procedure Set_Setting (Name : in String; Value : in Integer);

   ---------------------------------------------------------------------------
   -- Adds a new setting or changes the value of an existing setting.
   --
   -- Parameters:
   --   Name - The name of the setting that should
   --     be added or those value should be changed (if already
   --     exists).
   --   Value - A boolean value for the setting - will be converted
   --     to String by Integer'Image (Value).
   -- Raises:
   --   Config_Settings_ADO_Not_Initialized_Exception -
   --     raised if this subprogram is called before "
   --     Initialize_Config_Settings".
   procedure Set_Setting (Name : in String; Value : in Boolean);

   ---------------------------------------------------------------------------
   -- Returns an absolute path to the "User_Config_File" if no
   -- "User_Config_File" was parsed duiring initialisation
   -- (e.g. >Initialize_Config_Settings ("/My_Giant_Config__File", "");< )
   -- an empty String ("") will be returned.
   --
   -- Returns:
   --   An absolute path to the "User_Config_File".
   -- Raises:
   --   Config_Settings_ADO_Not_Initialized_Exception -
   --     raised if this subprogram is called before
   --     "Initialize_Config_Settings".
   function Get_User_Config_File return String;

   ---------------------------------------------------------------------------
   -- Writes all config settings that have been read from "User_Config_File"
   -- or that have been added/changed by ("Set_Setting") into
   -- a file.
   --
   -- Does not create a DTD (Document Type Definition) for the file.
   --
   -- Parameters:
   --   File_Name - The name of the file into that the settings should be
   --     written.
   -- Raises:
   --   Config_Settings_ADO_Not_Initialized_Exception -
   --     raised if this subprogram is called before
   --     "Initialize_Config_Settings".
   procedure Store_User_Config_File (File_Name : in String);

end Giant.Config_Settings;
