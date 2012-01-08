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
-- $RCSfile: giant-config-global_data.ads,v $, $Revision: 1.5 $
-- $Author: schwiemn $
-- $Date: 2003-07-03 13:15:38 $
--
-- -----
-- This package holds the functionality needed to access the
-- data stored in the global configuration file
-- (see GIANT Specification "13.2 Die globale Konfigurationsdatei").
--
-- This package simply uses the information from the package
-- Giant.Config_Settings and does some further processing in order
-- to simplify the access on the information.
--
-- The purpose of this packe is to other necessary config settings in
-- a performant and simple way.
--
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with String_Lists; -- from Baushaus Reuse.src

package Giant.Config.Global_Data is

   ---------------------------------------------------------------------------
   -- According to the specification, there are three different
   -- userdefined colors used to higlight Selections
   -- (see GIANT Specification "13.2.2 Farbe von Hervorhebungen").
   type Selection_High_Light_ID is 
     (Current_Selection, Color_1, Color_2, Color_3);

   ---------------------------------------------------------------------------
   -- There are also three different colors used to highlight
   -- Subgrapghs.
   type Subgraph_High_Light_ID is (Color_1, Color_2, Color_3);


   ---------------------------------------------------------------------------
   -- Raised on attempt to access a not initialized instance of the ADO
   Config_ADO_Not_Initialized_Exception : exception;
   
   
   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and finalisation
   ---------------------------------------------------------------------------         

   ---------------------------------------------------------------------------
   -- Initializes the ADO and processes the information hold in
   -- the ADO Giant.Config_Settings
   --
   -- ADO Giant.Config_Settings must have been initialized before this
   -- subprogramm is called.
   procedure Initialize_Config_Data;

   ---------------------------------------------------------------------------
   -- Finalizes the ADO;
   -- Deallocates all memory used by the ADO.
   -- After the call of this procedure "Initialize_Config_Data" may be
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
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --     is called before "Initialize_Config_Data"
   procedure Clear_Config_Data;


   ---------------------------------------------------------------------------
   -- B
   -- Access to processed config data.
   ---------------------------------------------------------------------------
     
   ---------------------------------------------------------------------------
   -- Returns an absolute path for the resources directory setting.
   --
   -- May return an empty String ("") if the setting is not correctly
   -- defined.
   --
   -- Returns:
   --   An absolute path that ends with a directory separator
   --   (e.g. "/home/donald/resources/").
   -- Raises:
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --     is called before "Initialize_Config_Data"
   function Get_Resources_Directory return String;
   
   ---------------------------------------------------------------------------
   -- C
   -- Access to the configuration data about colors.
   --
   -- The following functions return data needed to visualize the iml graph
   -- in visualisation windows. The implementation is not performance
   -- optimized as this settings only needs to be read once.
   --
   -- Note: Color_Access
   -- The functions returning "Color_Access" do not guarentee that
   -- for the same color the pointer "Color_Access" points to the
   -- same object on the heap.
   -- So for two equal colors, C1 = C2 is not guaranted
   -- (C1: Color_Access; C2: Color_Access).
   -- This does not apply to the functions of the subpackage
   -- Config.Vis_Styles (see internal documentation of this package).
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- This function returns the color used to highlight selections.
   -- As this function is not supposed to be used frequently, the
   -- implementation is not very performant.
   --
   -- Note
   --   The returned value is a pointer. After the finalisation of
   --   this ADO, you will have a dangling pointer.
   --
   -- Parameters:
   --   Highlight_ID - As there are several different colors to highlight
   --     more than one selection at once, this parameter is used to
   --     determine which highlight color should be returned.
   -- Returns:
   --   A pointer to a String which describes a color.
   -- Raises:
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --     is called before "Initialize_Config_Data"
   function Get_Selection_Highlight_Color
     (Highlight_ID : in Selection_High_Light_ID)
     return Color_Access;

   ---------------------------------------------------------------------------
   -- This function returns the color used to highlight subgraphs.
   -- As this function is not supposed to be used frequently, the
   -- implementation is not very performant.
   --
   -- Note
   --   The returned value is a pointer. After the finalisation of
   --   this ADO, you will have a dangling pointer.
   --
   -- Parameters:
   --   Highlight_ID - As there are several different colors to highlight
   --     more than one selection at once, this parameter is used to
   --     determine which highlight color should be returned.
   -- Returns:
   --   A pointer to a String which describes a color.
   -- Raises:
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --     is called before "Initialize_Config_Data".
   function Get_Subgraph_Highlight_Color
     (Highlight_ID : in Subgraph_High_Light_ID)
     return Color_Access;

   ---------------------------------------------------------------------------
   -- Returns an absolute path to the file that holds the Icon for
   -- Node Annotations.
   --
   -- Note
   --   It is not checked whether the returned file is really a pixmap
   --   file.
   -- Returns:
   --   A path to a file.
   --   May return an null string ("") if the file is not found.
   -- Raises:
   --   Config_ADO_Not_Initialized_Exception - raised if this subprogram
   --     is called before "Initialize_Config_Data".
   function Get_Node_Annotations_Icon
     return String;

end Giant.Config.GlobaL_Data;
