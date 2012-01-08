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
-- $RCSfile: giant-config-vis_styles.ads,v $, $Revision: 1.9 $
-- $Author: schwiemn $
-- $Date: 2003-08-01 13:59:39 $
--
-- ----------------
-- This package provides the functionality needed to manage
-- the Visualisation Styles.
-- Visualisation Styles determine how window edges and window nodes should
-- visualized on the window content of a visualisation window.
--
-- Each Visualisation Style is realized as an Abstract Data Object (ADO).
--
-- Protocol description:
--
-- 1. The internal data structures must be initialized by calling
-- -> procedure Initialize_Config_Vis_Styles
--
-- After this procedure executed successful, all visualisation styles
-- are stored in the internal data structure. The xml files used to
-- define the visualisation are only accessed by this procedure.
-- There are now abstract data objects for an unlimted number of
-- visualisation styles (barring memory restrictions), at least
-- there is one abstract data object for the default vis style.
--
-- 2. You may now query for the abstract data objects describing the
-- visualisation styles using the functionality of part
-- "B - Mangement of Visualisation Styles".
--
-- 3. Before a variable of type Visualisation_Style may be used as
-- a parameter for the functions specified in part C.0, C.1, C.2 and C.3,
-- the variable has to be initialized by calling
-- -> function Initialize_Vis_Style_By_Name
--
-- 4. After that, the information needed to draw window nodes and window
-- edges my be retrivied by the functions specified in part C.1, C.2
-- and C.3
--
-- 5. Finally, the memory needed for the management of the visualisation
-- styles has to be deallocated by calling
-- the procedure Clear_Config_Vis_Styles
--
with String_Lists;    -- from Bauhaus IML "Reuse.src"
with Hashed_Mappings; -- from Bauhaus IML "Reuse.src"
pragma Elaborate_All (Hashed_Mappings);

with Giant.Graph_Lib; -- from GIANT
with Giant.Graph_Lib.Node_Attribute_Filters; -- from GIANT

package Giant.Config.Vis_Styles is

   ---------------------------------------------------------------------------
   -- Before a variable of this type may be used, it
   -- has to be connected (i.e. initialized) to a Visualisation_Style by
   -- calling "Get_Vis_Style_By_Name (Vis_Style_Name : in String)"
   type Visualisation_Style_Access is private;

   ---------------------------------------------------------------------------
   -- Describes the possible line styles used to draw a window edge
   type Edge_Line_Style is (Continuous_Line, Dotted_Line, Dashed_Line);

   ---------------------------------------------------------------------------
   -- Thrown if an invalid directory is given as paramter
   Invalid_Directory_Exception : exception;

   ---------------------------------------------------------------------------
   -- Thrown if the xml file describing the Default
   -- Visualisation Style is not correct (e.g. not valid).
   --
   -- Note:
   -- The author does not guarantee that this exception
   -- is always thrown if a xml file describing a
   -- visualisation file is not correct.
   --
   -- As specified in the Specification of GIANT, the
   -- consequences of reading incorrect data from
   -- files may be undefined.
   Illegal_Default_Vis_Style_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if the given xml file for the Default
   -- Visualisation Style is not found.
   Default_Vis_Style_Not_Found_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if a subprogram is called before the data describing the
   -- visualisation styles was read from the files.
   Config_Vis_Styles_Not_Initialized_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if no corresponding visualisation style for passed name
   -- exists.
   Visualisation_Style_Not_Found_Exception : exception;

   ---------------------------------------------------------------------------
   -- Raised if a parameter of type "Visualisation_Style_Access" is
   -- passed by a subprogram before its initialisation.
   Visualisation_Style_Access_Not_Initialized_Exception : exception;


   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and finalisation of the internal data structure
   -- that holds all known visualisation styles (each visualisation style
   -- is regarded as an ADO).
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Initializes all ADOs describing the visualisation styles.
   -- This procedure initializes the internal data strcuture and reads the
   -- data describing the visualisation styles from the xml files.
   -- The xml Files are only accessed by this procedure,
   -- the other methods all work on a internal data structure.
   -- This procedure must have been executed before
   -- any other procedure or function of this package may be executed.
   --
   -- ----
   -- Important: The name of a visualisation style is defined by the name of
   -- its file (e.g. name: "my_vis" -> file: "my_vis.xml)
   -- The name of a visualisation style is unique,
   -- each vis-style is identified by its name.
   --
   -- To guarantee the uniqueness of the name, the following rules are defined:
   --
   -- First, all visualisation styles found in the directory
   -- GIANT_VIS_Directory are loaded.
   -- Second, all visualisation styles found in the directory
   -- User_Vis_Directory are loaded.
   -- If a vis-style that already exists is loaded; as
   -- both directories have a xml-file with the same filename, then the
   -- vis-style first loaded from the directory "GIANT_Vis_Directory" is
   -- replaced by the vis-style loaded from the directory "User_Vis_Directory"
   --
   -- Third, the Default-Vis-Style is loaded.
   -- The vis-style loaded from the file "Default_Vis_Style_File" replaces
   -- an already loaded vis-style with the same name (if such a vis-style was
   -- loaded in step one or two).
   --
   -- A correct "Default_Vis_Style_File" must exist, as there has to be
   -- a default-vis-style.
   -- It does not matter if there are no vis-styles in "GIANT_Vis_Directory"
   -- and "User_Vis_Directory".
   -- ----
   --
   -- This subrogram will try to expand relative paths inside a vis style
   -- (e.g. the node class icons) this expansion is done the following
   -- way:
   --
   --   1. Expand using the directory there the xml file for the
   --      visualisation style is located. If no file is found:
   --   2. Expand using "Resources_Root_Dir" as root. 
   --
   --   If no file is found then:
   --   3. Try to ignore that setting.
   --
   -- Note
   --   All xml files in the directories passed by the parameters
   --   "GIANT_Vis_Directory" and "User_Vis_Directory" are regarded
   --   as xml files describing a visualisation style.
   --   GIANT will try to ignore XML files that do not describe a
   --   visualisation style - but there is no guarantee this will work.
   --
   --   User_Vis_Directory may be an empty string. In that case, this
   --   parameter will be ignored.
   --
   -- Parameters:
   --   You should only pass ABSOLUTE PATHS for the following parameters.
   --
   --   Resources_Root_Dir - The path to a directory towards that realtive
   --     paths should be expanded if expansion towards the directory there
   --     the read xml file is stored fails.
   --   GIANT_Vis_Directory - A path to a directory there the visualisation
   --     styles for all users of an installation of GIANT are found.
   --     GIANT_Vis_Directory may be an empty string, then this parameter
   --     will be ignored.
   --   User_Vis_Directory - A path to a directory there the visualisation
   --     styles for one user are loacted.
   --     User_Vis_Directory may be an empty string (""), then this parameter
   --     will be ignored.
   --   Default_Vis_Style_File - The required default visualisation style.
   -- Raises:
   --   Invalid_Directory_Exception - raised if no correct (existing)
   --     directory is passed by the parameters "GIANT_Vis_Directory" and
   --     "User_Vis_Directory"
   --   Default_Vis_Style_Not_Found_Exception - raised if the file passed
   --     by the parameter "Default_Vis_Style_File" is not found.
   --   Illegal_Default_Vis_Style_Exception - raised if the xml file for
   --     the default visualisation style does not correspond to the
   --     requirements for a xml file describing visualisation styles.
   procedure Initialize_Config_Vis_Styles
     (Resources_Root_Dir     : in String;
      GIANT_Vis_Directory    : in String;
      User_Vis_Directory     : in String;
      Default_Vis_Style_File : in String);

   ---------------------------------------------------------------------------
   -- Finalisation.
   -- Deallocates all memory needed for the management of
   -- the visualisation Styles.
   -- After calling this perocedure, Initialize_Config_Vis_Styles
   -- may be called again without causing memory leaks.
   --
   -- Note
   --   The types Visualisation_Style_Access, Color_Access and
   --   Chars_Ptr_Array_Access are pointers that point directly
   --   into the internal data structures. After calling this
   --   procedure, all pointers will be dangling pointers.
   --
   -- Raises:
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   procedure Clear_Config_Vis_Styles;


   ---------------------------------------------------------------------------
   -- B
   -- Mangement of Visualisation Styles
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Returns the number of all known Visualisation Styles
   -- As there has to be at least the Default Visualisation Style
   -- a value between 1 and N is returned.
   --
   -- Returns:
   --   The number of known visualisation styles.
   --
   -- Raises:
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   function Get_Number_Of_Known_Vis_Styles return Integer;

   ---------------------------------------------------------------------------
   -- Returns a list holding the names of all known visualisation
   -- styles (loaded from the directoiries "GIANT_Vis_Directory"
   -- and "User_Vis_Directory" as well as from the file
   -- "Default_Vis_Style_File").
   -- The name of the Default Visualisation Style is the first entry in
   -- this list.
   --
   -- Returns:
   --   A list holding the names of all known visualisation styles.
   --
   -- Raises:
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   function Get_All_Vis_Styles
     return String_Lists.List;

   ---------------------------------------------------------------------------
   -- Checks whether there is a Visualisation Style with the
   -- passed name or not.
   --
   -- Parameters:
   --   Vis_Style_Name - The name of a visualisation style
   -- Returns:
   --   True, if there is a visualisation style with the given name;
   --   False, otherwise.
   -- Raises:
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   function Does_Vis_Style_Exist (Vis_Style_Name : in String)
                                 return Boolean;

   ---------------------------------------------------------------------------
   -- Used to initialize "Visualisation_Style_Access"
   -- (the pointer is bound to an existing visualisation style).
   --
   -- Parameters:
   --   Vis_Style_Name - the name of a visualisation style.
   -- Returns:
   --   The Visualisation Style having the passed name.
   -- Raises:
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   --   Visualisation_Style_Not_Found_Exception - raised
   --     if there is no visualisation style
   --     with the passed name "Vis_Style_Name"
   --     (i.e. "Does_Vis_Style_Exist (Vis_Style_Name)" will return "False").
   function Initialize_Vis_Style_By_Name
     (Vis_Style_Name : in String)
     return Visualisation_Style_Access;

   ---------------------------------------------------------------------------
   -- Returns the name of an existing visualisation style
   --
   -- Parameters:
   --   Vis_Style - The visualisation style.
   -- Returns:
   --   The name of a visualisation style as string.
   -- Raises
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   --   Visualisation_Style_Access_Not_Initialized_Exception - raised
   --     if the passed parameter was not initialized.
   function Get_Name_Of_Vis_Style
     (Vis_Style : in Visualisation_Style_Access)
     return Ada.Strings.Unbounded.Unbounded_String;

   ---------------------------------------------------------------------------
   -- Returns the default Visualisation Style.
   -- There is always a default Visualisation Style.
   -- Returns:
   --   The default visualisation style
   -- Raises
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   function Get_Default_Vis_Style return Visualisation_Style_Access;


   ---------------------------------------------------------------------------
   -- C.0
   -- Encoded Colors
   --
   -- All functions returneing color values for e.g. the line color
   -- of an edge return integer id's
   -- by using this id as index for an Color_Access_Array_Access
   -- you may retrieve an pointer to the string describing the color.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- holds all colors

   type Color_Access_Array is array (Positive range <>) 
     of Color_Access;

   type Color_Access_Array_Access is access Color_Access_Array;

   ---------------------------------------------------------------------------
   -- Returns an pointer to an array that holds all Colors used for
   -- the visualisation styles.
   -- Using the encoding integer id as index you may use this array for
   -- retrieving the real color e.g. used to draw the line of an edge.
   --
   -- The array holds pointers to string (in GIANT we use RGB Strings
   -- to describe colores) but this is not checked at this point,
   -- therefore the array may hold strings that do not describe colors
   -- according to your color moddel.
   --
   -- You may not deallocate the result.
   --
   -- Returns:
   --   A Pointer to an array holding all colors.
   --   May return a pointer
   --   to an array with Length=0 if no icons files have been found.
   --   Bounds of Array: 1 .. Ammount of diffrent Colors
   -- Raises:
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles.
   function Get_All_Colors return Color_Access_Array_Access;


   ---------------------------------------------------------------------------
   -- C.1
   -- Global visualisation data
   -- Access to visualisation data for the hole visualisation window
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Returns the background color for a visualisation window specified
   -- by the visualisation style.
   --
   -- Parameters:
   --   Vis_Style - A visualisation style.
   -- Returns:
   --   An encoding id integer for the color.
   -- Raises
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   --   Visualisation_Style_Access_Not_Initialized_Exception - raised
   --     if the passed parameter was not initialized.
   function Get_Vis_Window_Background_Color
     (Vis_Style : in Visualisation_Style_Access)
     return Integer;


   ---------------------------------------------------------------------------
   -- C.2
   -- Node Class specific visualisation data
   -- Access to visualisation data for node classes which is
   -- defined by a Visualisation Style
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- holds absolute paths for all icon files
   type Node_Icons_Array is array (Positive range <>) 
     of Ada.Strings.Unbounded.Unbounded_String;

   type Node_Icons_Array_Access is access Node_Icons_Array;

   ---------------------------------------------------------------------------
   -- Returns an pointer to an array that holds all absolute paths to
   -- files that describe icons (xpm). We only garantee that this file
   -- exists but not that it is a valid xpm file.
   --
   -- Using the encoding integer returned for a Node_Class
   -- as index you may retrieve the incon file for that node class.
   --
   -- You may not deallocate the result.
   --
   -- Returns:
   --   An Array holding absolute paths to files.
   --   May return a pointer
   --   to an array with Length=0 if no icons files have been found.
   --   Bounds of Array: 1 .. Ammount of diffrent Icons
   -- Raises:
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   function Get_All_Node_Icons return Node_Icons_Array_Access;

   ---------------------------------------------------------------------------
   -- Returns the encoding id of the icon used for this node class.
   --
   -- Note you may retrieve the icon file itself with:
   --   "Get_All_Node_Icons (Get_Node_Icon (My_Vis_Style, A_Node_Class))"
   --   i.e. means the encoding id returned by this function is the index
   --   of the unbounded string holding the absolute path to an icon file
   --   in the corresponding instance of "Node_Icons_Array_Access"
   --
   -- Parameters:
   --   Vis_Style - A visulisation style.
   --   Node_Class - An ID of an existing node class of the iml graph.
   -- Returns:
   --   An Integer Value that represents an Icon.
   --   May return "0" if no icon for this node is defined.
   -- Raises:
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   --   Visualisation_Style_Access_Not_Initialized_Exception - raised
   --     if the passed parameter "Vis_Style" was not initialized.
   function Get_Node_Icon_Encoding
     (Vis_Style  : in Visualisation_Style_Access;
      Node_Class : in Graph_Lib.Node_Class_Id)
     return Integer;

   ---------------------------------------------------------------------------
   -- Returns a special filter that is used to determine
   -- which attributes of the node class should directly
   -- be visible inside the node rectangle of a window node.
   --
   -- Note
   --  Under no circumstances you are allowed to DEALLOCATE the returned
   --  Filter.
   --
   --  After the deallocation of this ADO, the returned Filter will
   --  be a dangling pointer.
   --
   -- Parameters:
   --   Vis_Style - A visulisation style.
   --   Node_Class - An ID of an existing node class of the iml graph.
   -- Returns:
   --   The filter.
   -- Raises:
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   --   Visualisation_Style_Access_Not_Initialized_Exception - raised
   --     if the passed parameter "Vis_Style" was not initialized.
   function Get_Attribute_Filter
     (Vis_Style  : in Visualisation_Style_Access;
      Node_Class : in Graph_Lib.Node_Class_Id)
     return Graph_Lib.Node_Attribute_Filters.Filter;

   ---------------------------------------------------------------------------
   -- Returns the border color of the node rectangle.
   --
   -- Parameters:
   --   Vis_Style - A visualisation style.
   --   Node_Class - An ID of an existing node class of the iml graph.
   -- Returns:
   --   An encoding id integer for the color.
   -- Raises
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   --   Visualisation_Style_Access_Not_Initialized_Exception - raised
   --     if the passed parameter was not initialized.
   function Get_Border_Color
     (Vis_Style  : in Visualisation_Style_Access;
      Node_Class : in Graph_Lib.Node_Class_Id)
     return Integer;

   ---------------------------------------------------------------------------
   -- Returns the fill color for the node rectangle of a window node.
   --
   -- Parameters:
   --   Vis_Style - A visualisation style.
   --   Node_Class - An ID of an existing node class of the iml graph.
   -- Returns:
   --   An encoding id integer for the color.
   -- Raises
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   --   Visualisation_Style_Access_Not_Initialized_Exception - raised
   --     if the passed parameter was not initialized.
   function Get_Fill_Color
     (Vis_Style  : in Visualisation_Style_Access;
      Node_Class : in Graph_Lib.Node_Class_Id)
     return Integer;

   ---------------------------------------------------------------------------
   -- Returns the color used to display node attributes
   -- and the values of this attributes inside the
   -- node rectangle of a window node.
   --
   -- Parameters:
   --   Vis_Style - A visualisation style.
   --   Node_Class - An ID of an existing node class of the iml graph.
   -- Returns:
   --   An encoding id integer for the color.
   -- Raises
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   --   Visualisation_Style_Access_Not_Initialized_Exception - raised
   --     if the passed parameter was not initialized.
   function Get_Text_Color
     (Vis_Style  : in Visualisation_Style_Access;
      Node_Class : in Graph_Lib.Node_Class_Id)
     return Integer;


   ---------------------------------------------------------------------------
   -- C.3
   -- Edge Class specific visualisation data
   -- Access to visualisation data for edge classes which is
   -- defined by a Visualisation Style
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   -- Returns the color for a edge class specified by a
   -- visualisation style. This color is used to draw the line of a window
   -- edge.
   --
   -- Parameters:
   --   Vis_Style - A visualisation style.
   --   Edge_Class - An ID of an existing edge class of the iml graph.
   -- Returns:
   --   An encoding id integer for the color.
   -- Raises
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   --   Visualisation_Style_Access_Not_Initialized_Exception - raised
   --     if the passed parameter was not initialized.
   function Get_Line_Color
     (Vis_Style  : in Visualisation_Style_Access;
      Edge_Class : in Graph_Lib.Edge_Class_Id)
     return Integer;

   ---------------------------------------------------------------------------
   -- Returns the color used to display the name of the edge class
   -- in the visualisation window.
   --
   -- Parameters:
   --   Vis_Style - A visualisation style.
   --   Edge_Class - An ID of an existing edge class of the iml graph.
   -- Returns:
   --   An encoding id integer for the color.
   -- Raises
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles"
   --   Visualisation_Style_Access_Not_Initialized_Exception - raised
   --     if the passed parameter was not initialized.
   function Get_Text_Color
     (Vis_Style  : in Visualisation_Style_Access;
      Edge_Class : in Graph_Lib.Edge_Class_Id)
     return Integer;

   ---------------------------------------------------------------------------
   -- Returns the line style for an edge class.
   --
   -- Parameters:
   --   Vis_Style - A visualisation style.
   --   Edge_Class - An ID of an existing edge class of the iml graph.
   -- Returns:
   --   The line style.
   -- Raises
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles".
   --   Visualisation_Style_Access_Not_Initialized_Exception - raised
   --     if the passed parameter was not initialized.
   function Get_Line_Style
     (Vis_Style  : in Visualisation_Style_Access;
      Edge_Class : in Graph_Lib.Edge_Class_Id)
     return Edge_Line_Style;

   ---------------------------------------------------------------------------
   -- Determines whether the edge class should
   -- be shown as a label on the visible window content (with color
   -- "Get_Text_Color_For_Edge_Class") or not.
   --
   -- Parameters:
   --   Vis_Style - A visualisation style.
   --   Edge_Class - An ID of an existing edge class of the iml graph.
   -- Returns:
   --   True, if the edge class label should be shown; False, otherwise.
   -- Raises
   --   Config_Vis_Styles_Not_Initialized_Exception - raised if this
   --     subprogram is called before "Initialize_Config_Vis_Styles".
   --   Visualisation_Style_Access_Not_Initialized_Exception - raised
   --     if the passed parameter was not initialized.
   function Show_Label_For_Edge_Class_Name
     (Vis_Style  : in Visualisation_Style_Access;
      Edge_Class : in Graph_Lib.Edge_Class_Id)
     return Boolean;

------------------------------------------------------------------------------
private

   -- Global settings in the space of a visualisation style
   type Global_Vis_Data_Rec is record

      -- Background Color of the Visualisation WIndow
      Visualisation_Window_Background_Color_Encoding : Integer;
   end record;

   -- settings for a node class in the space of a visualisation style
   type Node_Class_Vis_Data is record

      -- the Ecncoded Icon of a Node Class
      Icon_Encoding         : Integer := 0; -- (zero means not initialized)

      -- represents the attributes of a node class that are
      -- directly visible on the visualisation window content
      Attribute_Filter : Graph_Lib.Node_Attribute_Filters.Filter;

      -- the border color of the node rectangle (the rectangle
      -- representing a window node)
      Border_Color_Encoding : Integer := 0;

      -- the fill color of the node rectangle
      Fill_Color_Encoding   : Integer := 0;

      -- the text color for the data displayed inside the node rectangle
      -- (the attributes and their values).
      Text_Color_Encoding   : Integer := 0;
   end record;

   -- settings for a edge class in the space of a visualisation style
   type Edge_Class_Vis_Data is record

      -- The color of the edge line
      Line_Color_Encoding : Integer := 0;

      -- The color of the text for the edge class
      Text_Color_Encoding : Integer := 0;

      -- The line style
      Line_Style          : Edge_Line_Style;

      -- determines whether the name of the edge class should be visible
      -- on the visible window content
      Label               : Boolean;
   end record;

   package Node_Class_Id_Hashed_Mappings is new Hashed_Mappings
     (Key_Type   => Graph_Lib.Node_Class_Id,
      Hash       => Graph_Lib.Hash_Node_Class_Id,
      Value_Type => Node_Class_Vis_Data);

   package Edge_Class_Id_Hashed_Mappings is new Hashed_Mappings
     (Key_Type   => Graph_Lib.Edge_Class_Id,
      Hash       => Graph_Lib.Hash_Edge_Class_Id,
      Value_Type => Edge_Class_Vis_Data);

   -- describes one Visualisation Style
   type Vis_Style_Data is record
      Style_Name              : Ada.Strings.Unbounded.Unbounded_String;
      Global_Vis_Data         : Global_Vis_Data_Rec;
      Node_Class_Specific_Vis : Node_Class_Id_Hashed_Mappings.Mapping;
      Edge_Class_Specific_Vis : Edge_Class_Id_Hashed_Mappings.Mapping;
   end record;

   type Visualisation_Style_Access is access Vis_Style_Data;

end Giant.Config.Vis_Styles;
