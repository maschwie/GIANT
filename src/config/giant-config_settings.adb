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
-- $RCSfile: giant-config_settings.adb,v $, $Revision: 1.22 $
-- $Author: schwiemn $
-- $Date: 2003-09-15 20:12:55 $
--
with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

with GNAT.Directory_Operations; -- from GNAT
with GNAT.OS_Lib;               -- from GNAT

with Hashed_Mappings;       -- from Bauhaus Reuse.src
pragma Elaborate_All (Hashed_Mappings);
with Unbounded_String_Hash; -- from Bauhaus Reuse.src

with Tree_Readers;       -- from xmlada
with DOM.Core;           -- from xmlada
with DOM.Core.Nodes;     -- from xmlada
with DOM.Core.Documents; -- from xmlada
with DOM.Core.Elements;  -- from xmlada

with Giant.Logger;          -- from GIANT
pragma Elaborate_All (Giant.Logger);
with Giant.XML_File_Access; -- from GIANT
with Giant.File_Management; -- from GIANT
with Giant.String_Split;    -- from GIANT

package body Giant.Config_Settings is

   -- logging functionality
   package Logger is new Giant.Logger ("Giant.Config_Settings");

   --------------------------------------------------------------------------
   --  Internal Datastructure of the ADO
   --------------------------------------------------------------------------

   package Setting_Hashs is new Hashed_Mappings
     (Key_Type   => Ada.Strings.Unbounded.Unbounded_String,
      Equal      => Ada.Strings.Unbounded."=",
      Hash       => Unbounded_String_Hash,
      Value_Type => Ada.Strings.Unbounded.Unbounded_String);

   type Config_Data_Element is record
      -- holds all settings
      Settings_Map         : Setting_Hashs.Mapping;
      -- the absolute path to the config file - needed for path expansion
      -- a file
      Abs_Config_File_Path : Ada.Strings.Unbounded.Unbounded_String;
      -- the optional root for relative paths - needed for expansion
      -- a directory
      Abs_Path_Root        : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   -- Settings read from "GIANT_Config_File"
   GIANT_Config : Config_Data_Element;

   -- Settings read from "User_Config_File"
   User_Config : Config_Data_Element;

   -- Used to check initialisation
   ADO_Initialized : Boolean := False;

   --------------------------------------------------------------------------
   -- 0.1 Validators
   --------------------------------------------------------------------------

   --------------------------------------------------------------------------
   function Validate_Integer (Value : in String) return Boolean is
      Test_Int : Integer;
   begin
      begin
         Test_Int := Integer'Value (Value);
      exception
         when Constraint_Error =>

            return False;
      end;

      return True;
   end Validate_Integer;


   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and finalisation
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Adds defaults values for settings - if no entry is found in both Maps
   procedure Add_Default_Values_If_Necessary is
   begin
      for I in Default_Settings'Range loop
         if not Does_Setting_Exist
           (Ada.Strings.Unbounded.To_String (Default_Settings (I).Name))
         then
            -- add entry
            Setting_Hashs.Bind
              (GIANT_Config.Settings_Map,
               Default_Settings (I).Name,
               Default_Settings (I).Def_Value);
         end if;
      end loop;
   end Add_Default_Values_If_Necessary;

   ---------------------------------------------------------------------------
   procedure Validate_Read_Settings
     (Config_Data  : in out Config_Data_Element) is

   begin

      for I in Default_Settings'Range loop

         if (Default_Settings (I).Validator_Function /= null) and then
           Setting_Hashs.Is_Bound
           (Config_Data.Settings_Map, Default_Settings (I).Name) then

            -- replace with default if validation failes
            if not Default_Settings (I).Validator_Function
              (Ada.Strings.Unbounded.To_String
               (Setting_Hashs.Fetch
                (Config_Data.Settings_Map, Default_Settings (I).Name)))
            then

               Logger.Info
                 ("Validation failed for config setting with name: """
                  & Ada.Strings.Unbounded.To_String
                  (Default_Settings (I).Name)
                  & """ - value: """
                  & Ada.Strings.Unbounded.To_String
                  (Setting_Hashs.Fetch
                   (Config_Data.Settings_Map, Default_Settings (I).Name))
                  & """ not valid "
                  & "- in config file: "
                  & Ada.Strings.Unbounded.To_String
                  (Config_Data.Abs_Config_File_Path));

               Setting_Hashs.Update_Value
                 (Config_Data.Settings_Map,
                  Default_Settings (I).Name,
                  Default_Settings (I).Def_Value);
            end if;
         end if;
      end loop;
   end Validate_Read_Settings;

   ---------------------------------------------------------------------------
   procedure Read_Setting_Entries
     (Source_XML_Document : in     Dom.Core.Document;
      Config_Data         : in out Config_Data_Element) is

      Setting_Nodes_List : DOM.Core.Node_List;
      A_Setting_Node     : DOM.Core.Node;

      Setting_Name       : Ada.Strings.Unbounded.Unbounded_String;
      Setting_Value      : Ada.Strings.Unbounded.Unbounded_String;
   begin

      -- determine absolute path root
      -------------------------------
      Setting_Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Source_XML_Document, "absolute_path_root");

      -- Element <absolute_path_root> is not required
      if not (DOM.Core.Nodes.Length(Setting_Nodes_List) <= 0) then

         -- only holds one Element
         A_Setting_Node := DOM.Core.Nodes.Item (Setting_Nodes_List,0);

         -- expand when necessary if path not found take path of
         -- the directory of the config file
         begin
            Config_Data.Abs_Path_Root :=
              Ada.Strings.Unbounded.To_Unbounded_String
              (File_Management.Get_Absolute_Path_To_Directory_From_Relative
               (GNAT.Directory_Operations.Get_Current_Dir,
                DOM.Core.Elements.Get_Attribute
                (A_Setting_Node, "root_directory")));
         exception
            when File_Management.Directory_Does_Not_Exist_Exception =>
               Config_Data.Abs_Path_Root :=
                 Ada.Strings.Unbounded.To_Unbounded_String
                 (File_Management.Return_Dir_Path_For_File_Path
                  (Ada.Strings.Unbounded.To_String
                   (Config_Data.Abs_Config_File_Path)));

               Logger.Info
                 ("Invalid path defined in <absolute_path_root> node "
                  &"in config file: "
                  & Ada.Strings.Unbounded.To_String
                  (Config_Data.Abs_Config_File_Path));
         end;
      else
         Logger.Info ("No <absolute_path_root> node defined in config file: "
                      & Ada.Strings.Unbounded.To_String
                      (Config_Data.Abs_Config_File_Path));
         Config_Data.Abs_Path_Root :=
           Ada.Strings.Unbounded.To_Unbounded_String
           (File_Management.Return_Dir_Path_For_File_Path
            (Ada.Strings.Unbounded.To_String
             (Config_Data.Abs_Config_File_Path)));
      end if;

      DOM.Core.Free (Setting_Nodes_List);

      -- get all <setting> nodes
      --------------------------
      Setting_Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Source_XML_Document, "setting");

      -- insert new setting with "a possibly already
      -- existing setting with the same name will be replaced
      -------------------------------------------------------
      for I in 0 ..  DOM.Core.Nodes.Length(Setting_Nodes_List) - 1 loop

         A_Setting_Node := DOM.Core.Nodes.Item(Setting_Nodes_List,I);

         Setting_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String
           (DOM.Core.Elements.Get_Attribute
            (A_Setting_Node, "name"));

         Setting_Value :=
           Ada.Strings.Unbounded.To_Unbounded_String
           (DOM.Core.Elements.Get_Attribute
            (A_Setting_Node, "value"));

         -- check if setting already exists and remove it if that
         -- is the case
         if Setting_Hashs.Is_Bound
           (Config_Data.Settings_Map, Setting_Name) then
            Logger.Info ("Config Setting with name: """
                         & Ada.Strings.Unbounded.To_String (Setting_Name)
                         & """ already read - value replaced by new one: """
                         & Ada.Strings.Unbounded.To_String (Setting_Value)
                         & """ - last value read from config file: "
                         & Ada.Strings.Unbounded.To_String
                         (Config_Data.Abs_Config_File_Path));
            Setting_Hashs.Unbind (Config_Data.Settings_Map, Setting_Name);
         end if;

         Setting_Hashs.Bind
           (Config_Data.Settings_Map, Setting_Name, Setting_Value);
      end loop;

      -- deallocate used memory for list
      ----------------------------------
      DOM.Core.Free (Setting_Nodes_List);
   end Read_Setting_Entries;

   ------------------------------------------------------------------------
   procedure Access_Config_File
     (Config_File  : in     String;
      Config_Data  : in out Config_Data_Element) is

      -- Needed for deallocation
      The_Tree_Reader  : Tree_Readers.Tree_Reader;

      -- top level node (document node)
      The_XML_Document : Dom.Core.Document;

      Process_File : Boolean := True;
   begin

      -- Load and Parse Config_File
      begin
         XML_File_Access.Load_XML_File_Validated
           (Config_File, The_Tree_Reader, The_XML_Document);
      exception
         when XML_File_Access.XML_File_Access_Error_Exception =>

            Logger.Info ("Procedure: Access_Config_File - "
                          & "Cannot access file: " & Config_File);

            Process_File := False;
         when XML_File_Access.XML_File_Parse_Fatal_Error_Exception =>

            Logger.Fatal ("Procedure: Access_Config_File - "
                          & "XML Error while parsing file: " & Config_File);

            raise Config_File_Not_Correct_Exception;
      end;

      if Process_File then
         -- check for correct type
         if (XML_File_Access.Does_XML_Document_Belong_To_Type
             (Config_File_Identifier, The_XML_Document) = False) then

            Logger.Fatal ("Procedure: Access_Config_File - XML File is not"
                          & "of correct type: "
                          & Config_File);
            raise Config_File_Not_Correct_Exception;
         end if;

         begin
            -- set abs path to config file
            Config_Data.Abs_Config_File_Path :=
              Ada.Strings.Unbounded.To_Unbounded_String
              (File_Management.Get_Absolute_Path_To_File_From_Relative
               (GNAT.Directory_Operations.Get_Current_Dir, Config_File));

            -- read settings into internal data structures
            Read_Setting_Entries (The_XML_Document, Config_Data);
            -- deallocate memory
            Tree_Readers.Free(The_Tree_Reader);
         exception
            when others =>
               Logger.Fatal
                 ("Procedure: Access_Config_File - XML Error while "
                  & "reading entries of parsed xml file: "
                  & Config_File);

               Tree_Readers.Free(The_Tree_Reader);
               raise Config_File_Not_Correct_Exception;
         end;
      end if; --end if Process_File
   end Access_Config_File;

   ---------------------------------------------------------------------------
   procedure Initialize_Config_Settings
     (GIANT_Config_File : in String;
      User_Config_File  : in String) is

   begin

      ADO_Initialized := True;

      -- Basic Initialisation
      -- must happen at this point
      ----------------------------
      GIANT_Config.Settings_Map := Setting_Hashs.Create;
      GIANT_Config.Abs_Config_File_Path :=
        Ada.Strings.Unbounded.Null_Unbounded_String;
      GIANT_Config.Abs_Path_Root :=
        Ada.Strings.Unbounded.Null_Unbounded_String;

      User_Config.Settings_Map := Setting_Hashs.Create;
      User_Config.Abs_Config_File_Path :=
        Ada.Strings.Unbounded.Null_Unbounded_String;
      User_Config.Abs_Path_Root :=
        Ada.Strings.Unbounded.Null_Unbounded_String;

      -- Read files
      -------------
      if not (GIANT_Config_File = "") then
         Access_Config_File (GIANT_Config_File, GIANT_Config);
      end if;

      if not (User_Config_File = "") then
         Access_Config_File (User_Config_File, User_Config);
      end if;

      -- Validate settings
      --------------------
      Validate_Read_Settings (GIANT_Config);
      Validate_Read_Settings (User_Config);

      -- Add default values for not read settings into GIANT_Config_Data
      -------------------------------------------------------------------
      Add_Default_Values_If_Necessary;

   end Initialize_Config_Settings;

   ---------------------------------------------------------------------------
   procedure Clear_Config_Data is
   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      Setting_Hashs.Destroy (GIANT_Config.Settings_Map);
      Setting_Hashs.Destroy (User_Config.Settings_Map);

      ADO_Initialized := False;
   end Clear_Config_Data;


   ---------------------------------------------------------------------------
   -- B
   -- Access to configuration data.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Does_Setting_Exist_In_Data_Element
     (Config_Data : in Config_Data_Element;
      Name        : in String)
     return Boolean is
   begin

      return Setting_Hashs.Is_Bound
        (Config_Data.Settings_Map,
         Ada.Strings.Unbounded.To_Unbounded_String (Name));
   end Does_Setting_Exist_In_Data_Element;

   ---------------------------------------------------------------------------
   function Does_Setting_Exist (Name : in String)
                               return Boolean is
   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      if Does_Setting_Exist_In_Data_Element (User_Config, Name) or
        Does_Setting_Exist_In_Data_Element (GIANT_Config, Name) then

         return True;
      else

         return False;
      end if;
   end Does_Setting_Exist;

   ---------------------------------------------------------------------------
   function Get_Setting_As_String_From_Data_Element
     (Config_Data : in Config_Data_Element;
      Name        : in String)
     return String is
   begin

      return Ada.Strings.Unbounded.To_String
        (Setting_Hashs.Fetch
         (Config_Data.Settings_Map,
          Ada.Strings.Unbounded.To_Unbounded_String (Name)));
   end Get_Setting_As_String_From_Data_Element;


   ---------------------------------------------------------------------------
   function Get_Setting_As_String
     (Name : in String)
     return String is

   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      if not Does_Setting_Exist (Name) then
         raise Config_Setting_Does_Not_Exist_Exception;
      end if;

      -- first search User_Config !!!
      if Does_Setting_Exist_In_Data_Element (User_Config, Name) then

         return Get_Setting_As_String_From_Data_Element (User_Config, Name);
      else

         return Get_Setting_As_String_From_Data_Element (GIANT_Config, Name);
      end if;
   end Get_Setting_As_String;

   ---------------------------------------------------------------------------
   function Internal_Try_Path_Expansion
     (Abs_Path_Root        : in String;
      Abs_Config_File_Dir  : in String;
      Unexpanded_Path      : in String)
     return String is

      Abs_Path : Ada.Strings.Unbounded.Unbounded_String;
   begin

      -- 1. try expansion regarding abs_path_root
      begin
      
         return File_Management.Get_Absolute_Path_From_Relative
           (Abs_Path_Root,
            Unexpanded_Path);
      exception
         when File_Management.Abs_Path_Could_Not_Be_Calculated_Exception =>
            null;            
      end;

      -- 2. try expansion regarding directory of config file      
      begin

          return File_Management.Get_Absolute_Path_From_Relative
            (Abs_Config_File_Dir,
             Unexpanded_Path);
      exception
          when File_Management.Abs_Path_Could_Not_Be_Calculated_Exception =>
             null;
      end;

      -- 3. retrun empty string if nothing works
      return "";
   end Internal_Try_Path_Expansion;

   ---------------------------------------------------------------------------
   function Get_Setting_With_Path_Expanded (Name : in String) return String is

      Unexp_Path          : Ada.Strings.Unbounded.Unbounded_String;
      Source_Data         : Config_Data_Element;
      Found               : Boolean := False;
   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      if not Does_Setting_Exist (Name) then
         raise Config_Setting_Does_Not_Exist_Exception;
      end if;

      -- first search User_Config !!!
      if Does_Setting_Exist_In_Data_Element (User_Config, Name) then
         Source_Data := User_Config;
      else
         Source_Data := GIANT_Config;
      end if;

      Unexp_Path := Ada.Strings.Unbounded.To_Unbounded_String
        (Get_Setting_As_String_From_Data_Element
         (Source_Data, Name));

      return Internal_Try_Path_Expansion
        (Ada.Strings.Unbounded.To_String (Source_Data.Abs_Path_Root),
         File_Management.Return_Dir_Path_For_File_Path
             (Ada.Strings.Unbounded.To_String
               (Source_Data.Abs_Config_File_Path)),
         Ada.Strings.Unbounded.To_String (Unexp_Path));
   end Get_Setting_With_Path_Expanded;

   ---------------------------------------------------------------------------
   function Get_Setting_As_Expanded_Path_List
     (Name : in String)
     return String_Lists.List is

     use Ada.Strings.Unbounded;

     Source_Data   : Config_Data_Element;

     Splitted_List      : String_Lists.List;
     Splitted_List_Iter : String_Lists.ListIter;
     Result_List   : String_Lists.List;
     A_Pot_Path    : Ada.Strings.Unbounded.Unbounded_String;
     A_Exp_Path    : Ada.Strings.Unbounded.Unbounded_String;
   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      if not Does_Setting_Exist (Name) then
         raise Config_Setting_Does_Not_Exist_Exception;
      end if;

      -- first search User_Config !!!
      if Does_Setting_Exist_In_Data_Element (User_Config, Name) then
         Source_Data := User_Config;
      else
         Source_Data := GIANT_Config;
      end if;

      Result_List := String_Lists.Create;

      Splitted_List := String_Split.Split_String
        (Get_Setting_As_String_From_Data_Element (Source_Data, Name),
         File_Management.Path_Separator);

      Splitted_List_Iter := String_Lists.MakeListIter (Splitted_List);

      while String_Lists.More (Splitted_List_Iter) loop

         String_Lists.Next (Splitted_List_Iter, A_Pot_Path);

         if A_Pot_Path /= Ada.Strings.Unbounded.Null_Unbounded_String then

            A_Exp_Path := Ada.Strings.Unbounded.To_Unbounded_String
              (Internal_Try_Path_Expansion
                (Ada.Strings.Unbounded.To_String
                  (Source_Data.Abs_Path_Root),
                 File_Management.Return_Dir_Path_For_File_Path
                   (Ada.Strings.Unbounded.To_String
                     (Source_Data.Abs_Config_File_Path)),
                 Ada.Strings.Unbounded.To_String (A_Pot_Path)));

            if A_Exp_Path /= Ada.Strings.Unbounded.Null_Unbounded_String then

               String_Lists.Attach (Result_List, A_Exp_Path);
            end if;
         end if;
      end loop;

      String_Lists.Destroy (Splitted_List);

      return Result_List;
   end Get_Setting_As_Expanded_Path_List;

   ---------------------------------------------------------------------------
   function Get_Setting_As_Boolean (Name : in String)
                                   return Boolean is

   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      if not Does_Setting_Exist (Name) then
         raise Config_Setting_Does_Not_Exist_Exception;
      end if;

      declare
         --  trim and lower case
         Value : String := Ada.Strings.Fixed.Translate
           (Ada.Strings.Fixed.Trim
            (Get_Setting_As_String (Name),
             Side => Ada.Strings.Both),
            Ada.Strings.Maps.Constants.Lower_Case_Map);
      begin
         return (Value = "true" or else Value = "on");
      exception
         when Constraint_Error =>
            raise Invalid_Type_Exception;
      end;
   end Get_Setting_As_Boolean;

   ---------------------------------------------------------------------------
   function Get_Setting_As_Integer (Name : in String)
                                   return Integer is

   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      if not Does_Setting_Exist (Name) then
         raise Config_Setting_Does_Not_Exist_Exception;
      end if;

      begin
         return Integer'Value
           (Get_Setting_As_String (Name));
      exception
         when Constraint_Error =>
            raise Config_Setting_Is_Not_An_Integer_Value;
      end;
   end Get_Setting_As_Integer;

   ---------------------------------------------------------------------------
   procedure Set_Setting (Name : in String; Value : in String) is
   begin
      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      -- insert or replace setting in User_Config
      if Setting_Hashs.Is_Bound
        (User_Config.Settings_Map,
         Ada.Strings.Unbounded.To_Unbounded_String (Name)) then

         Setting_Hashs.Unbind
           (User_Config.Settings_Map,
            Ada.Strings.Unbounded.To_Unbounded_String (Name));
      end if;

      Setting_Hashs.Bind
        (User_Config.Settings_Map,
         Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Ada.Strings.Unbounded.To_Unbounded_String (Value));
   end Set_Setting;

   ---------------------------------------------------------------------------
   procedure Set_Setting (Name : in String; Value : in Integer) is
   begin
      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      Set_Setting (Name, Integer'Image (Value));
   end Set_Setting;

   ---------------------------------------------------------------------------
   procedure Set_Setting (Name : in String; Value : in Boolean) is
   begin
      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      Set_Setting (Name, Boolean'Image (Value));
   end Set_Setting;

   ---------------------------------------------------------------------------
   function Get_User_Config_File return String is
   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      -- if no User_Config_File was passed for the initialisation when
      -- the value "User_Config.Abs_Config_File_Path" contains a Null_String
      return Ada.Strings.Unbounded.To_String
        (User_Config.Abs_Config_File_Path);
   end Get_User_Config_File;

   ---------------------------------------------------------------------------
   procedure Write_DTD_File (File_Name : in String) is

      DTD_File : Ada.Text_IO.File_Type;
   begin

      --  create the file
      -------------------
      Ada.Text_IO.Create
        (DTD_File,
         Ada.Text_IO.Out_File,
         File_Name);

      Ada.Text_IO.Set_Output (DTD_File);

      Ada.Text_IO.Put_Line
        ("<!ELEMENT giant_config_file (absolute_path_root?, (setting)*)>");

      Ada.Text_IO.Put_Line
        ("  <!ELEMENT absolute_path_root EMPTY>");

      Ada.Text_IO.Put_Line
        ("  <!ATTLIST absolute_path_root");
      Ada.Text_IO.Put_Line
        ("    root_directory  CDATA  #REQUIRED");
      Ada.Text_IO.Put_Line
        ("  >");
      Ada.Text_IO.Put_Line
        ("  <!ELEMENT setting EMPTY>");
      Ada.Text_IO.Put_Line
        ("  <!ATTLIST setting");
      Ada.Text_IO.Put_Line
        ("    name   CDATA  #REQUIRED");
      Ada.Text_IO.Put_Line
        ("    value  CDATA  #REQUIRED");
      Ada.Text_IO.Put_Line
        ("  >");

      --  close down resources
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      Ada.Text_IO.Close (DTD_File);
   end Write_DTD_File;

   ---------------------------------------------------------------------------
   procedure Store_User_Config_File (File_Name : in String) is

      Config_File : Ada.Text_IO.File_Type;
      User_Settings_Iter : Setting_Hashs.Bindings_Iter;

      Setting_Name  : Ada.Strings.Unbounded.Unbounded_String;
      Setting_Value : Ada.Strings.Unbounded.Unbounded_String;
   begin

      if not ADO_Initialized then
         raise Config_Settings_ADO_Not_Initialized_Exception;
      end if;

      --  create the file
      -------------------
      Ada.Text_IO.Create
        (Config_File,
         Ada.Text_IO.Out_File,
         File_Name);
      Ada.Text_IO.Set_Output (Config_File);

      --  write xml file header for external dtd
      ------------------------------------------
      Ada.Text_IO.Put_Line
        ("<?xml version=""1.0"" encoding=""ISO-8859-1""?>");
      Ada.Text_IO.Put_Line
        ("<!DOCTYPE giant_config_file");
      Ada.Text_IO.Put_Line
        ("  SYSTEM ""giant_config_file.dtd"">");
      Ada.Text_IO.New_Line;

      --  open top level xml node (document node)
      Ada.Text_IO.Put_Line
        ("<giant_config_file>");

      --  write absolute_path_root
      ----------------------------
      Ada.Text_IO.Put_Line
        ("  <absolute_path_root root_directory  = """
         & Ada.Strings.Unbounded.To_String (User_Config.Abs_Path_Root)
         & """ />");

      --  write setting entries
      -------------------------------------------
      User_Settings_Iter :=
        Setting_Hashs.Make_Bindings_Iter (User_Config.Settings_Map);

      while Setting_Hashs.More (User_Settings_Iter) loop
         Setting_Hashs.Next
           (User_Settings_Iter, Setting_Name, Setting_Value);

         Ada.Text_IO.Put_Line
           ("  <setting name  = """
            & Ada.Strings.Unbounded.To_String
            (Setting_Name)
            & """");
         Ada.Text_IO.Put_Line
           ("           value = """
            & Ada.Strings.Unbounded.To_String
            (Setting_Value)
            & """ />");
      end loop;

      -- last entry in file - close top level xml node (document node)
      Ada.Text_IO.Put_Line
        ("</giant_config_file>");

      --  close down resources
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
      Ada.Text_IO.Close (Config_File);

      -- write DTD File into the same directory
      Write_DTD_File
        (File_Management.Return_Dir_Path_For_File_Path (File_Name)
         & "giant_config_file.dtd");

   end Store_User_Config_File;

end Giant.Config_Settings;
