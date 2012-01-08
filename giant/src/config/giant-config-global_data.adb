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
-- $RCSfile: giant-config-global_data.adb,v $, $Revision: 1.4 $
-- $Author: schwiemn $
-- $Date: 2003-07-03 13:15:38 $
--
with Ada.Unchecked_Deallocation;

with Giant.File_Management;  -- from GIANT
with Giant.Config_Settings; -- from GIANT
with Giant.Ptr_Normal_Hashs; -- from GIANT
pragma Elaborate_All (Giant.Ptr_Normal_Hashs);

package body Giant.Config.Global_Data is


   ---------------------------------------------------------------------------
   -- 0.1
   -- The internal data structure of the ADO
   ---------------------------------------------------------------------------
   
   type Selection_Highlight_Array is array (Selection_High_Light_ID) 
      of Color_Access;
      
   type Subgraphs_Highlight_Array is array (Subgraph_High_Light_ID) 
      of Color_Access;

   type Processed_Config_Settings_Element is record   
   
      Resources_Directory   : Ada.Strings.Unbounded.Unbounded_String;            
      Selection_Highlight   : Selection_Highlight_Array;
      Subgraph_Highlight    : Subgraphs_Highlight_Array;
                             
      Node_Annotations_Icon : Ada.Strings.Unbounded.Unbounded_String;              
   end record;

   -- Holds all further processed config data (settings are read from 
   -- Giant.Config.Config_Settings;
   Proc_Settings : Processed_Config_Settings_Element;
   
   -- determines the status of the ADO - initialized or false
   ADO_Initialized : Boolean := False;
   
   
   ---------------------------------------------------------------------------
   -- 0.2
   -- Internal subprograms
   ---------------------------------------------------------------------------

   function Is_Config_ADO_Initialized return Boolean is
   begin
      return ADO_Initialized;
   end Is_Config_ADO_Initialized;


   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and finalisation
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   procedure Initialize_Config_Data is
   begin
   
      Proc_Settings.Resources_Directory := 
        Ada.Strings.Unbounded.To_Unbounded_String
          (Config_Settings.Get_Setting_With_Path_Expanded
            ("Resources_Directory"));
                             
      Proc_Settings.Selection_Highlight (Current_Selection) :=
        new Ada.Strings.Unbounded.Unbounded_String'
          (Ada.Strings.Unbounded.To_Unbounded_String
            (Config_Settings.Get_Setting_As_String
              ("Current_Selection_Highlight_Color"))); 
                                
      Proc_Settings.Selection_Highlight (Color_1) :=
        new Ada.Strings.Unbounded.Unbounded_String'
          (Ada.Strings.Unbounded.To_Unbounded_String
            (Config_Settings.Get_Setting_As_String
              ("Selection_Highlight_Color_1")));         

      Proc_Settings.Selection_Highlight (Color_2) :=
        new Ada.Strings.Unbounded.Unbounded_String'        
          (Ada.Strings.Unbounded.To_Unbounded_String
            (Config_Settings.Get_Setting_As_String
              ("Selection_Highlight_Color_2")));   

      Proc_Settings.Selection_Highlight (Color_3) :=
        new Ada.Strings.Unbounded.Unbounded_String'
          (Ada.Strings.Unbounded.To_Unbounded_String
            (Config_Settings.Get_Setting_As_String
              ("Selection_Highlight_Color_3")));                       
            
      Proc_Settings.Subgraph_Highlight (Color_1) :=
        new Ada.Strings.Unbounded.Unbounded_String'
          (Ada.Strings.Unbounded.To_Unbounded_String
            (Config_Settings.Get_Setting_As_String
              ("IML_Subgraph_Highlight_Color_1")));
                    
      Proc_Settings.Subgraph_Highlight (Color_2) :=
        new Ada.Strings.Unbounded.Unbounded_String'        
          (Ada.Strings.Unbounded.To_Unbounded_String
            (Config_Settings.Get_Setting_As_String
              ("IML_Subgraph_Highlight_Color_2")));       
      
      Proc_Settings.Subgraph_Highlight (Color_3) :=
        new Ada.Strings.Unbounded.Unbounded_String'
          (Ada.Strings.Unbounded.To_Unbounded_String
            (Config_Settings.Get_Setting_As_String
              ("IML_Subgraph_Highlight_Color_3")));            
            
      -- check whether icon exists
      if not (Config_Settings.Get_Setting_With_Path_Expanded
        ("Icon_For_Node_Annotations") = "") then
            
         Proc_Settings.Node_Annotations_Icon := 
           Ada.Strings.Unbounded.To_Unbounded_String
             (Config_Settings.Get_Setting_With_Path_Expanded
               ("Icon_For_Node_Annotations"));                 
      else
            
         Proc_Settings.Node_Annotations_Icon := 
           Ada.Strings.Unbounded.Null_Unbounded_String; 
      end if;  
         
      ADO_Initialized := True;
   end Initialize_Config_Data;

   ---------------------------------------------------------------------------
   procedure Clear_Config_Data is

   begin

      if (Is_Config_ADO_Initialized = False) then

         raise Config_ADO_Not_Initialized_Exception;
      end if;
                                    
      for I in Proc_Settings.Selection_Highlight'Range loop
      
         Config.Free_Color_Access 
           (Proc_Settings.Selection_Highlight (I));
      end loop;
      
      for I in Proc_Settings.Subgraph_Highlight'Range loop
      
         Config.Free_Color_Access 
           (Proc_Settings.Subgraph_Highlight (I));
      end loop;      
     
   end Clear_Config_Data;


   ---------------------------------------------------------------------------
   -- B
   -- Access to processed config data.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_Resources_Directory return String is

   begin
                    
      if (Ada.Strings.Unbounded.To_String 
        (Proc_Settings.Resources_Directory) = "") then 
         
         return "";
      else 
      
        return File_Management.Append_Dir_Separator_If_Necessary
          (Ada.Strings.Unbounded.To_String 
            (Proc_Settings.Resources_Directory));
      end if;
   end Get_Resources_Directory;

   ---------------------------------------------------------------------------
   -- C
   -- Access to the configuration data about colors.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Get_Selection_Highlight_Color
     (Highlight_ID : in Selection_High_Light_ID)
     return Color_Access is

   begin
      if (Is_Config_ADO_Initialized = False) then
         raise Config_ADO_Not_Initialized_Exception;
      end if;

      return Proc_Settings.Selection_Highlight (Highlight_ID);
   end Get_Selection_Highlight_Color;

   ---------------------------------------------------------------------------
   function Get_Subgraph_Highlight_Color
     (Highlight_ID : in Subgraph_High_Light_ID)
     return Color_Access is
   begin
      if (Is_Config_ADO_Initialized = False) then
         raise Config_ADO_Not_Initialized_Exception;
      end if;

      return Proc_Settings.Subgraph_Highlight (Highlight_ID);
   end Get_Subgraph_Highlight_Color;

   ---------------------------------------------------------------------------
   function Get_Node_Annotations_Icon
     return String is

   begin
      if (Is_Config_ADO_Initialized = False) then
         raise Config_ADO_Not_Initialized_Exception;
      end if;

      return Ada.Strings.Unbounded.To_String
        (Proc_Settings.Node_Annotations_Icon);
   end Get_Node_Annotations_Icon;

end Giant.Config.Global_Data;
