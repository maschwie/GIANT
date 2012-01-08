-----------------------------------------------------------------------------
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License version 2 as
--  published by the Free Software Foundation.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
--  As a special exception, if you link this unit with the Bauhaus toolkit
--  to produce an executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
--  First Author: Martin Schwienbacher
--
--  $RCSfile: giant-vis_windows.ads,v $, $Revision: 1.28 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
--  ----------------
--  This package realizes a container that administrates the components
--  that decribe a visualisation window, i.e. this is the data model
--  for a visualisation_window.
--
--  Each Container is an Abstract Data Type (ADT), the administrated
--  Components are selections, pins and a graph_widget.
--  The container holds all data needed to sava a visual window
--  persistent into a file.
--
--  This "data model" is absolute independent from any kind
--  of a graphical represenation of a visualiasation window.
--  --> Zhe user of this package has to take care for the
--  consistency between this "data model" and any graphical
--  representation.
--
--  The purpose of this package is to combine all components
--  describing the not redundant data needed for the persistence of
--  a visualisation window.
--
with Ada.Strings.Unbounded;

with String_Lists; -- from Bauhaus IML "Reuse.src"
with Bauhaus_IO;   -- from Bauhaus IML "Reuse.src"
with Ordered_Sets; -- from Bauhaus IML "Reuse.src"

with Giant.Graph_Lib;            -- from GIANT
with Giant.Graph_Lib.Selections; -- from GIANT
with Giant.Graph_Widgets;        -- from GIANT
with Giant.Vis;                  -- from GIANT
with Giant.Node_Annotations;     -- from GIANT

package Giant.Vis_Windows is

   ---------------------------------------------------------------------------
   --  The ADT offered by this package.
   --  A Pointer to a data object that describes a visual window
   type Visual_Window_Access is private;

   ---------------------------------------------------------------------------
   --  This type describes the highlight status of all selections.
   type Selection_Highlight_Status is 
     (None, Color_1, Color_2, Color_3, Current_Selection);
     
   ---------------------------------------------------------------------------
   --  Necessary to prevent user from changing the Highlightstatus to current
   --  selection (this is done automatically).
   subtype Changable_Highlight_Status is Selection_Highlight_Status 
     range None .. Color_3;

   ---------------------------------------------------------------------------
   --  This exception is raised if a not initialized instance of the
   --  ADT "Visual_Window_Access" is passed as parameter to one
   --  of the subprograms in this package.
   Visual_Window_Access_Not_Initialized_Exception : exception;

   ---------------------------------------------------------------------------
   --  A
   --  Initialisation, Finalisation and Persistence
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  This subprogram initializes the ADT Vis_Window_Data_Access
   --  by creating a new empty instance of the data model for
   --  visualisation windows.
   --
   --  An empty standard selection will be created too, this will also
   --  become the current selection.
   --
   --  Per Default the "default visualisation style"
   --  (determinded by Giant.Config.Vis_styles.Get_Default_Vis_Style)
   --  will be used for that visualisation window.
   --
   --  Parameters:
   --    Vis_Window_Name - The name of the visualisation window.
   --    Annotations - The Node Annotations used for the nodes showed
   --      in the Graph_Widget that belongs to this window.
   --  Returns:
   --    A pointer that points to a new data object describing a
   --    visualisation window.
   function Create_New
     (Vis_Window_Name : in String;
      Annotations     : in Node_Annotations.Node_Annotation_Access :=
        Node_Annotations.Create_Empty)
     return Visual_Window_Access;

   ---------------------------------------------------------------------------
   --  This subprogram initializes the ADT by creating a new instance
   --  based on the data read from the stream.
   --
   --  If no visualisation style with the
   --  stored name is not found, the "default
   --  visualisation style" will be taken (determinded by
   --  Giant.Config.Vis_styles.Get_Default_Vis_Style).
   --
   --  Uses platform independent streams from Bauhaus_IO;
   --
   --  Parameters:
   --    Stream - the stream where the data is read.
   --    Item - the new Instance of the ADT.
   procedure Visual_Window_Access_Read
     (Stream      : in  Bauhaus_IO.In_Stream_Type;
      Vis_Window  : out Visual_Window_Access;
      Annotations : in Node_Annotations.Node_Annotation_Access :=
        Node_Annotations.Create_Empty);

   ---------------------------------------------------------------------------
   --  This subprogram writes the Container including all its components
   --  into a stream.
   --
   --  Uses platform independent streams from Bauhaus_IO;
   --
   --  Parameters:
   --    Stream - the stream into that the data should be written.
   --    Item - the Instance of the ADT that should be written into
   --      a stream.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   procedure Visual_Window_Access_Write
     (Stream     : in Bauhaus_IO.Out_Stream_Type;
      Vis_Window : in Visual_Window_Access);

   ---------------------------------------------------------------------------
   --  Deallocates the ADT.
   --
   --  Note:
   --    This procedure performs a DEEP DEALLOCATION,
   --    i.e. all selections that belong to
   --    that visualisation window are deallocated too.
   --    As Selections are realized as pointers you should
   --    beware of dangling pointers.
   --
   --    The Graph_Widget (a part of the ADT) will only be
   --    deallocated (automatically by GTK) if its
   --    Reference Counter is set to 1 before this subprogram is
   --    executed.
   --
   --  Parameters:
   --    Vis_Window - the instance of the ADT that should be deallocated.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   procedure Deallocate_Vis_Window_Deep
     (Vis_Window : in out Visual_Window_Access);


   ---------------------------------------------------------------------------
   --  B
   --  General access on Visual_Window_Access
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Returns the name of a visualisation window.
   --
   --  Parameters:
   --    Visual_Window - The instance of the ADT whose name should
   --      be returned.
   --  Returns:
   --    The name of the visualisation window as a unbounded string.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   function Get_Name
     (Vis_Window : in Visual_Window_Access)
      return String;

   ---------------------------------------------------------------------------
   --  Changes the name of a visualisation window.
   --
   --  Parameters:
   --    Visual_Window - The instance of the ADT whose name should
   --      be changed.
   --    New_Name - The new name for that visualisation window.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   procedure Change_Name
     (Vis_Window : in Visual_Window_Access;
      New_Name   : in String);

   ---------------------------------------------------------------------------
   --  Equal function - to instances of the ADT having the
   --  same name are regarded as equal. This is necessary because of the
   --  fact, that each visualisation window of a project must have a
   --  unique name.
   --
   --  It is garanted that:
   --  (Equal (Vis_Window_A, Vis_Window_B) = TRUE) if and only if
   --  (Get_Vis_Window_Name(Vis_Window_A) = Get_Vis_Window_Name(Vis_Window_B))
   --
   --  Parameters:
   --    Left : An instance of the ADT.
   --    Right: An instance of the ADT:
   --  Returns:
   --    True, if the two instances are equal; False, otherwise.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   function Is_Equal
     (Left  : in Visual_Window_Access;
      Right : in Visual_Window_Access)
     return Boolean;

   ---------------------------------------------------------------------------
   --  A Hash Function for the ADT. The Hash Value is calculated based on
   --  the name of "Vis_Window".
   --
   --  It is garanted that if
   --    Equal (Window_A, Window_B)  ->
   --      (Get_Hash_Value (Window_A) = Get_Hash_Value (Window_B));
   --
   --  Parameters:
   --    Vis_Window : An instance of the ADT for that an hash value should
   --      be calculated.
   --  Returns:
   --    A hash value for "Vis_Window".
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   function Get_Hash_Value
     (Vis_Window : in Visual_Window_Access)
     return Integer;
     
     
   ---------------------------------------------------------------------------
   --  Returns the Graph Widget associatet to this visualisation window.
   --
   --  Note:
   --    You are not allowed to deallocate the returned Graph Widget.
   --  Returns:
   --    A pointer to a graph_widget.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   function Get_Graph_Widget      
     (Vis_Window : in Visual_Window_Access)
     return Graph_Widgets.Graph_Widget;


   ---------------------------------------------------------------------------
   --  C
   --  Management of the Selections that belong to a visualisation window
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  This exception is raised if a selection with a passed name is not found.
   Selection_With_Passed_Name_Not_Found_Exception : exception;

   ---------------------------------------------------------------------------
   --  Each selection must have a unique name - this exception is raised
   --  if subprogram call will violate this principle.
   Selection_Is_Already_Part_Of_Window_Exception : exception;

   ---------------------------------------------------------------------------
   --  Raised on trial to remove the standard selection
   Standard_Selection_May_Not_Be_Removed_Exception : exception;

   ---------------------------------------------------------------------------
   --  Raised on Trial to change the Highlight-Status of a selection if
   --  that is not allowed.
   Highlight_Status_Of_Selection_May_Not_Be_Changed_Exception : exception;

   ---------------------------------------------------------------------------
   --  Raised on attempt to make a selection to current selection that
   --  is faded out.
   Illegal_Current_Selection_Exception : exception;

   ---------------------------------------------------------------------------
   --  Raised on attempt to change a selection's name to a name that already
   --  exists.
   New_Selection_Name_Does_Already_Exist_Exception : exception;

   ---------------------------------------------------------------------------
   --  Raised on attempt of changing the name of the standard selection.
   Standard_Selection_Name_May_Not_Be_Changed_Exception : exception;

   ---------------------------------------------------------------------------
   --  Checks whether the data model vor a visualisation window has a
   --  selection with the given name.
   --
   --  Parameters:
   --    Visual_Window - An instance of the ADT.
   --    Selection_Name - The name of a selection.
   --  Returns:
   --    True, if "Vis_Window" has a selection with the given name
   --    "Selection_Name"; False, otherwise.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   function Does_Selection_Exist
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String)
     return Boolean;

   ---------------------------------------------------------------------------
   --  This function returns a selection.
   --
   --  Note
   --    As only a pointer to a "selection data object" is returned
   --    several calls of this function with the same parameters will
   --    cause aliases.
   --
   --    You may NOT DEALLOCATE the returned Selection
   --    before it is removed from the Visualisation Window
   --    by calling "Remove_Selection_From_Vis_Window".
   --
   --  Parameters:
   --    Vis_Window - The "model" for visual window to that the selection
   --      belongs.
   --    Selection_Name - The name of the searched selection.
   --  Returns:
   --    The selection with the given name.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   --    Selection_With_Passed_Name_Not_Found_Exception - Raised if
   --      "Vis_Window" has no selection with the name "Selection_Name".
   function Get_Selection
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String)
     return Graph_Lib.Selections.Selection;

   ---------------------------------------------------------------------------
   --  Returns a list (not sorted in any way) holding the names
   --  of a all selections that belong to "Vis_Window".
   --  The standard selection is the first selection in this list.
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window.
   --  Returns:
   --    A list holding the names of all known selctions.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   function Get_All_Selections
     (Vis_Window : in Visual_Window_Access)
     return String_Lists.List;

   ---------------------------------------------------------------------------
   --  Changes the name of a selection.
   --
   --  While a selection is part of a visualisation window you may only
   --  change its name using this subprogram.
   --
   --  You may not change the name of the standard selection.
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window.
   --    Selection_Name - The name of the selection whose name should be
   --      changed to "New_Selection_Name".
   --    New_Selection_Name - The new selection name.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   --    Selection_With_Passed_Name_Not_Found_Exception - Raised if a
   --      selection with the name "Selection_Name" is not part of
   --      "Visual_Window_Access"
   --    New_Selection_Name_Does_Already_Exist_Exception - Raised
   --      if there is already a selection with the name
   --      "New_Selection_Name".
   --    Standard_Selection_Name_May_Not_Be_Changed_Exception - Raised
   --      on attempt of changing the name of the standard selection.
   procedure Change_Selection_Name
     (Vis_Window         : in Visual_Window_Access;
      Selection_Name     : in String;
      New_Selection_Name : in String);

   ---------------------------------------------------------------------------
   --  Adds a Selection to the visualisation window.
   --
   --  The passed Selection Selection "Selection" must be initialized
   --  else the bahaviour of this procedure us not defined.
   --
   --  The added selection is regarded as:
   --    - not as the current selection
   --    - not as the standard selection
   --    - not as faded out
   --    - not as highlighted.
   --
   --  Parameters:
   --    Vis_Window - The "model" for visual window.
   --    Selection  - The selection that should be added.
   --  Returns:
   --    A list about all selections.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   --    Selection_Is_Already_Part_Of_Window_Exception - Raised if there
   --      is already a selection with the same name in "Vis_Window".
   procedure Add_Selection
     (Vis_Window : in Visual_Window_Access;
      Selection  : in Graph_Lib.Selections.Selection);

   ---------------------------------------------------------------------------
   --  Removes a selection with the passed name.
   --
   --  The Selection is only removed from the visualisation window
   --  NO DEALLOCATION is done for the selection.
   --  After the call of that subprogram a selection may be deallocated
   --  without affecting the ADT Vis_Window.
   --
   --  The Standard Selection may not be removed.
   --  If the Current Selection is removed, the standard selection will
   --  become the current selection.
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window.
   --    Selection_Name  - The name of the selection that should be removed.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   --    Selection_With_Passed_Name_Not_Found_Exception - Raised if
   --      "Vis_Window" has no selection with the name "Selection_Name".
   --    Standard_Selection_May_Not_Be_Removed_Exception - Raised on Trail
   --      to remove the standard selection.
   --    Selection_With_Passed_Name_Not_Found_Exception - Raised if a
   --      selection with the name "Selection_Name" is not part of
   --      "Visual_Window_Access".
   procedure Remove_Selection
      (Vis_Window     : in Visual_Window_Access;
       Selection_Name : in String);

   ---------------------------------------------------------------------------
   --  Returns the name of the current selection.
   --  There is always a current selection.
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window whose current
   --      selection should be returned.
   --  Returns:
   --    The name of the current seclection.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   function Get_Current_Selection
     (Vis_Window : in Visual_Window_Access)
     return String;

   ---------------------------------------------------------------------------
   --  Sets the current selection.
   --  The old current Selection will loose the status "current selection"
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window whose current
   --      selection should be changed.
   --    Selection_Name  - The name of the selection
   --      that should become the new current selection.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   --    Selection_With_Passed_Name_Not_Found_Exception - Raised if a
   --      selection with the name "Selection_Name" is not part of
   --      "Visual_Window_Access".
   --    Illegal_Current_Selection_Exception - Raised if a selection
   --      that is faded out should be made to the current selection
   --      or if the selection that should become the current selection
   --      has a highlight status diffrent from "None".
   procedure Set_Current_Selection
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String);

   ---------------------------------------------------------------------------
   --  Returns the name of the standard selection.
   --  There is always a standard selection.
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window whose standard
   --      selection should be returned.
   --  Returns:
   --    The name of the standard selection.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   function Get_Standard_Selection
     (Vis_Window : in Visual_Window_Access)
     return String;

   ---------------------------------------------------------------------------
   --  Returns the Highlight-Status of a selection.
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window.
   --    Selection_Name - The name of the selection
   --      whose "Highlight-Status" should be returned.
   --  Returns:
   --    The highlight status.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   --    Selection_With_Passed_Name_Not_Found_Exception - Raised if a
   --      selection with the name "Selection_Name" is not part of
   --      "Visual_Window_Access"
   function Get_Highlight_Status
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String)
     return Selection_Highlight_Status;

   ---------------------------------------------------------------------------
   --  Determines whether the "Highlight-Status" of a selection may be
   --  changed.
   --  This is necessary as you are not allowed to change the highlight status
   --  of the current selection.
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window.
   --    Selection_Name - The name of the selection whose
   --      "Highlight-Status" should be changed.
   --  Returns:
   --    True, if the "Highlight-Status" may be changed; False, otherwise.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   --    Selection_With_Passed_Name_Not_Found_Exception - Raised if a
   --      selection with the name "Selection_Name" is not part of
   --      "Visual_Window_Access"
   function May_Highlight_Status_Be_Changed
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Changes the "Higlight-Status" of a selection.
   --
   --  You are not allowed to set the Highlight Status to "Current_Selection".
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window.
   --    Selection_Name - The name of the selection whose "Highlight-Status"
   --      is changed.
   --    New_Highlight_Status - The new Highlight-Status for the "Selection".
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   --    Selection_With_Passed_Name_Not_Found_Exception - Raised if a
   --      selection with the name "Selection_Name" is not part of
   --      "Visual_Window_Access"
   --    Highlight_Status_Of_Selection_May_Not_Be_Changed_Exception - raised
   --      if it is not allowed to change the Highlight-Status of
   --      "Selection".
   procedure Set_Highlight_Status
     (Vis_Window           : in Visual_Window_Access;
      Selection_Name       : in String;
      New_Highlight_Status : in Changable_Highlight_Status);


   ---------------------------------------------------------------------------
   -- D
   -- Filters
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Raised on attempt to fade out a selection that can not be faded out.
   Selection_May_Not_Be_Faded_Out_Exception : exception;

   ---------------------------------------------------------------------------
   --  Raised on attemot to fade in a selection that is not faded out.
   Selection_Is_Not_Faded_Out_Exception : exception;

   ---------------------------------------------------------------------------
   --  Determines whether it is possible to fade out a selection
   --  (for further information see GIANT Spec
   --  "7.1. UC: Selektionen ausblenden").
   --
   --  The current selection and the standard selection may not be faded
   --  out.
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window.
   --    Selection_Name - The name of a selection.
   --  Returns:
   --    True, if the selection may be faded out; False, otherwise.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   --    Selection_With_Passed_Name_Not_Found_Exception - Raised if a
   --      selection with the name "Selection_Name" is not part of
   --      "Visual_Window_Access"
   function May_Be_Faded_Out
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String)
     return Boolean;

   --------------------------------------------------------------------------
   -- Determines whether a selection is faded out.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Selection_Name - The name of a selection.
   -- Returns:
   --   True, if the selection is faded out; False, otherwise.
   -- Raises:
   --   Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Selection_With_Passed_Name_Not_Found_Exception - Raised if a
   --     selection with the name "Selection_Name" is not part of
   --     "Visual_Window_Access"
   function Is_Faded_Out
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String)
     return Boolean;

   --------------------------------------------------------------------------
   --  Fades a selection out.
   --
   --  You are not allowed to fade out the current selection and
   --  the standard selection.
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window.
   --    Selection_Name - The name of the selection that should be
   --      faded out.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   --    Selection_With_Passed_Name_Not_Found_Exception - Raised if a
   --      selection with the name "Selection_Name" is not part of
   --      "Visual_Window_Access"
   --    Selection_May_Not_Be_Faded_Out_Exception - Raised if the Selection
   --      may not be faded out.
   procedure Fade_Out_Selection
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String);

   --------------------------------------------------------------------------
   -- Fades in a selection (only selections that are currently faded out).
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Selection_Name - The name of the selection that should be
   --     faded in.
   -- Raises:
   --   Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Selection_With_Passed_Name_Not_Found_Exception - Raised if a
   --     selection with the name "Selection_Name" is not part of
   --     "Visual_Window_Access".
   --   Selection_Is_Not_Faded_Out_Exception - Raised if the Selection is
   --     currently not faded out.
   procedure Fade_In_Selection
     (Vis_Window     : in Visual_Window_Access;
      Selection_Name : in String);


   --------------------------------------------------------------------------
   -- E
   -- Pin Management
   -- Provides functionality for the managent of pins.
   --------------------------------------------------------------------------

   --------------------------------------------------------------------------
   -- Raised if a pin with the passed name does not exist.
   Pin_With_Passed_Name_Not_Found_Exception : exception;

   --------------------------------------------------------------------------
   -- Raised if a added pin already exists.
   Pin_Does_Already_Exist_Exception : exception;

   --------------------------------------------------------------------------
   -- Determines whether a Pin with the passed name exists.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Pin_Name - The name of a pin.
   -- Returns:
   --   True, if the pin exists; False, otherwise.
   -- Raises:
   --   Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   function Does_Pin_Exist
     (Vis_Window : in Visual_Window_Access;
      Pin_Name   : in String)
     return Boolean;

   --------------------------------------------------------------------------
   -- Returns the data describing the position of the visible window
   -- content.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Pin_Name - The name of a pin.
   -- Returns:
   --   The data for the position of the visual window content
   --   stored in a pin.
   -- Raises
   --   Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Pin_With_Passed_Name_Not_Found_Exception - Raised if the
   --     pin "Pin_Name" is not found.
   function Get_Position
     (Vis_Window : in Visual_Window_Access;
      Pin_Name   : in String)
     return Vis.Logic.Vector_2d;

   --------------------------------------------------------------------------
   -- Returns the data describing the zoom level of the visible window
   -- content.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Pin_Name - The name of a pin.
   -- Returns:
   --   A variable describing the zoomlevel stored in the pin.
   -- Raises
   --   Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Pin_With_Passed_Name_Not_Found_Exception - Raised if the
   --     pin "Pin_Name" is not found.
   function Get_Zoom
     (Vis_Window : in Visual_Window_Access;
      Pin_Name   : in String)
     return Vis.Zoom_Level;

   --------------------------------------------------------------------------
   --  Returns all Pins of a visualisation window.
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window.
   --  Returns:
   --    The names of all pins. The list is not sorted in any way.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   function Get_All_Pins
     (Vis_Window : in Visual_Window_Access)
     return String_Lists.List;

   ---------------------------------------------------------------------------
   -- Adds a pin to a visualisation window.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Name - The name of the new pin.
   --   Position - The data describing the position a visible window
   --     content
   --   Zoom_Level - The data describing the zoom level of a visible
   --     window content.
   -- Raises:
   --   Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Pin_Does_Already_Exist_Exception - raised if "Pin_Name" does
   --     already exist.
   procedure Add_Pin
     (Vis_Window : in Visual_Window_Access;
      Name       : in String;
      Position   : in Vis.Logic.Vector_2d;
      Zoom_Level : in Vis.Zoom_Level);

   ---------------------------------------------------------------------------
   --  Changes the name of a pin.
   --  
   --  Parameters:
   --    Vis_Window   - A visualisation window.
   --    Pin_Name     - The name of the pin whose name should be changed.
   --    New_Pin_Name - The new name for the pin.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   --    Pin_Does_Already_Exist_Exception - Raised if "New_Pin_Name" does
   --      already exist.
   procedure Change_Pin_Name 
      (Vis_Window   : in Visual_Window_Access;
       Pin_Name     : in String;
       New_Pin_Name : in String);
       
   ---------------------------------------------------------------------------
   -- Removes a pin from a visualisation window.
   --
   -- Parameters:
   --   Vis_Window - The "model" for a visualisation window.
   --   Pin_Name - The name of a pin.
   -- Raises
   --   Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --     initialized instance of "Vis_Window_Data_Access" is passed
   --     as parameter.
   --   Pin_With_Passed_Name_Not_Found_Exception - Raised if the
   --     pin "Pin_Name" is not found.
   procedure Remove_Pin
      (Vis_Window : in Visual_Window_Access;
       Pin_Name   : in String);


   ---------------------------------------------------------------------------
   --  F
   --  Visualisation Styles
   --
   --  Note a visualisation window knows only the name of its visualisation
   --  style. As visualisation styles are realized independently from
   --  project files and may be changed by the user, it is only garanted
   --  that after a visualisation window is loaded from its a
   --  visualsiation style with the stored name is set for this
   --  window. That does not garantee that the window nodes look
   --  like they have as the user stored the window, because in between
   --  the use may have changed the visualisation styles.
   --  If there is no appropriate visualisation style found then the
   --  Standard visualisation style will be used.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Raised if passed Vis_Style does not exist.
   Vis_Style_Does_Not_Exist_Exception : exception;

   ---------------------------------------------------------------------------
   --  Returns the name of the visualisation style assigned to this
   --  visualisation window.
   --
   --  As already mentioned above it is not garanted that a
   --  visualisation style with the returned name exists.
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window.
   --  Returns:
   --    The name of the visuliastion style assigned to the window.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   function Get_Vis_Style
     (Vis_Window : in Visual_Window_Access)
     return String;

   ---------------------------------------------------------------------------
   --  Sets the name of the visualisation style of this visualisation window.
   --
   --  Parameters:
   --    Vis_Window - The "model" for a visualisation window.
   --    Vis_Style_Name - The name of a visualisation style.
   --  Raises:
   --    Visual_Window_Access_Not_Initialized_Exception - Raised if a not
   --      initialized instance of "Vis_Window_Data_Access" is passed
   --      as parameter.
   --    Vis_Style_Does_Not_Exist_Exception - Raised if "Vis_Style" is
   --      not known by the package "Giant.Config.Vis_Styles".
   procedure Set_Vis_Style
     (Vis_Window     : in Visual_Window_Access;
      Vis_Style_Name : in String);

------------------------------------------------------------------------------
private


   ---------------------------------------------------------------------------
   --  Management of the pins of a visualisation window.
   ---------------------------------------------------------------------------
   type Pin is record
     Pin_Name : Ada.Strings.Unbounded.Unbounded_String;
     Pin_Pos  : Vis.Logic.Vector_2d;
     Pin_Zoom : Vis.Zoom_Level;
   end record;

   --  pins are only compared based on "Pin_Name"
   function Pin_Equal (Left : in Pin; Right : in Pin) return Boolean;

   --  pins are only compared based on "Pin_Name"
   function Pin_Less_Than (Left : in Pin; Right : in Pin) return Boolean;

   package Pin_Sets is new
     Ordered_Sets (Item_Type => Pin,
                   "="       => Pin_Equal,
                   "<"       => Pin_Less_Than);


   ---------------------------------------------------------------------------
   --  Management of Selections inside a visualisation window
   ---------------------------------------------------------------------------

   type Selection_Data_Elemet is record
     The_Selection : Graph_Lib.Selections.Selection;
     Highlight_Status : Selection_Highlight_Status;
     Is_Faded_Out : boolean;
   end record;

   --  selections are only compared based on the name
   function Selection_Data_Equal
     (Left  : in Selection_Data_Elemet;
      Right : in Selection_Data_Elemet)
     return Boolean;

   --  selections are only compared based on the name
   function Selection_Data_Less_Than
     (Left  : in Selection_Data_Elemet;
      Right : in Selection_Data_Elemet)
     return Boolean;

   package Selection_Data_Sets is new Ordered_Sets
     (Item_Type => Selection_Data_Elemet,
      "="       => Selection_Data_Equal,
      "<"       => Selection_Data_Less_Than);


   ---------------------------------------------------------------------------
   --  The data model for a visualisation window
   ---------------------------------------------------------------------------

   type Visual_Window_Element;

   type Visual_Window_Access is access Visual_Window_Element;

   type Visual_Window_Element is record

      Vis_Window_Name         : Ada.Strings.Unbounded.Unbounded_String;

      --  The Visualisation Style used for that window (only known by name)
      The_Visualisation_Style : Ada.Strings.Unbounded.Unbounded_String;

      The_Graph_Widget        : Graph_Widgets.Graph_Widget;

      Set_Of_All_Pins         : Pin_Sets.Set;

      --  The name of the "standard selection"
      --  (See GIANT Specification 3.4.2. Standard-Selektion)
      Standard_Selection      : Ada.Strings.Unbounded.Unbounded_String;

      --  The name of the "current selection"
      -- (See GIANT Specification 3.4.3. Aktuelle Selektion)
      Current_Selection       : Ada.Strings.Unbounded.Unbounded_String;

      --  A ordered set of all selections (including extra
      --  management data) that belong to this
      --  visulalisation window.
      All_Managed_Selections  : Selection_Data_Sets.Set;
   end record;

end Giant.Vis_Windows;



