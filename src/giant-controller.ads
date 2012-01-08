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
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-controller.ads,v $, $Revision: 1.53 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
------------------------------------------------------------------------------
--
--  Contains the controller. The controller is responsible for maintaing
--  data consistency. All operations that modify global data i.e. data
--  that is stored in the project must be included in this package.
--
--  Think of the controller as a facade for the framework of the
--  application. Sadly the facade is incomplete and only contains
--  methods that modify global data the inspectors are missing and
--  need to be called directly.
--
--  The controller also stores the currently loaded project. The
--  controller can only handle a singe project.
--
--  Frequently used parameters:
--    Ask_For_Confirmation -
--      If True, the content was modified and the gui is visible, the
--      user is prompted for confirmation. Usually the dialog contains a
--      Yes, No and Cancel button. If the user selects cancel the called
--      function usually returns False.
--      Otherwise, the requested operation is executed and True is
--      returned.
--

with Ada.Exceptions;
with Ada.IO_Exceptions;

with Giant.Evolutions;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Lib.Subgraphs;
with Giant.Graph_Widgets;
with Giant.Gsl;
with Giant.Gsl.Interpreters;
with Giant.Projects;
with Giant.Valid_Names;
with Giant.Vis;
with Giant.Vis_Windows;

package Giant.Controller is

   ---------------------------------------------------------------------------
   --  Application
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Exits the application
   procedure Exit_Application;

   ---------------------------------------------------------------------------
   --  Shows an error dialog for io exceptions.
   --
   --  These io errors are considered programming errors and not
   --  caught:
   --    Ada.IO_Exceptions.Mode_Error
   --    Ada.IO_Exceptions.Use_Error
   --
   --  See:
   --    Show_Error
   procedure Handle_IO_Exception
     (Error    : in Ada.Exceptions.Exception_Occurrence;
      Filename : in String);

   ---------------------------------------------------------------------------
   --  Shows an error dialog for exceptions while opening a project.
   --
   --  Invokes Handle_IO_Exception as a fallback.
   --
   --  See:
   --    Handle_IO_Exception
   procedure Handle_Project_Exception
     (Error    : in Ada.Exceptions.Exception_Occurrence;
      Filename : in String);

   -------------------------------------------------------------------------
   --  Shows an error dialog. Shows error message on console, if gui is
   --  not initialized.
   procedure Show_Error
     (Message : in String);

   -------------------------------------------------------------------------
   --  Shows an input dialog.
   function Show_Input
     (Message : in String)
      return String;

   ---------------------------------------------------------------------------
   --  Shows an error dialog or error message, if gui is not
   --  initialized.
   --
   --  Returns:
   --    True, if editor was launched; False, if Node has no sloc
   --    attribute.
   --  See:
   --    File_Management.Execute_External_Editor
   function Show_Source
     (Node : in Graph_Lib.Node_Id)
     return Boolean;

   ---------------------------------------------------------------------------
   --  GUI
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Shows the main window.
   --
   --  See:
   --    Giant.Gui_Manager.Show
   procedure Show_Gui;

   ---------------------------------------------------------------------------
   --  Closes all windows. The current project is not closed.
   --
   --  Returns:
   --    True, if gui was hidden; False, if the user cancelled.
   --  See:
   --    Giant.Gui_Manager.Hide
   function Hide_Gui
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   ---------------------------------------------------------------------------
   --  GSL
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Executes a gsl script from a file. A shared interpreter
   --  instance is used for all scripts.
   --
   --  If gui is shown a progress dialog is displayed.
   --
   --  Exceptions:
   --    Throws any IO Exception when script file access fails. Other
   --    exceptions are handled by the interpreter.
   --  See:
   --    Gsl.Interpreters.Execute_Script
   --    Gsl.Interpreters.Start_Calculation
   procedure Execute_GSL
     (Filename             : in String);

   procedure Execute_GSL
     (Script_Name : in String;
      Context     : in String;
      Parameter   : in Gsl.Interpreters.Gsl_Params);

   ---------------------------------------------------------------------------
   -- Only a wrapper function for the gsl runtime library to get
   -- the content of a vis_window.
   --
   -- See:
   --   Gsl.Runtime.Runtime_Get_Window_Content
   function Gsl_Get_Window_Content
     (Window_Name : in String)
      return Graph_Lib.Subgraphs.Subgraph;

   ---------------------------------------------------------------------------
   --  Layout
   ---------------------------------------------------------------------------

   procedure Apply_Layout
     (Layout_Name           : in String;
      Window_Name           : in String;
      Selection_Name        : in String;
      Position              : in Vis.Logic.Vector_2d;
      Additional_Parameters : in String);

   ---------------------------------------------------------------------------
   --  Node Annotations
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Returns the annotation for Node.
   function Get_Node_Annotation
     (Node : in Graph_Lib.Node_Id)
     return String;

   ---------------------------------------------------------------------------
   --  Returns True, if Node is annotated.
   function Is_Node_Annotated
     (Node : in Graph_Lib.Node_Id)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Sets the annotation for Node to Text.
   procedure Set_Node_Annotation
     (Node : in Graph_Lib.Node_Id;
      Text : in String);

   ---------------------------------------------------------------------------
   --  Removes the annotation for Node.
   procedure Remove_Node_Annotation
     (Node : in Graph_Lib.Node_Id);

   ---------------------------------------------------------------------------
   --  Pins
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Creates a pin that stores the current center position and zoom
   --  level of the graph widget.
   procedure Create_Pin
     (Window_Name : in String;
      Name        : in String);

   procedure Create_Pin
     (Window_Name : in String;
      Name        : in String;
      Position    : in Vis.Logic.Vector_2d;
      Zoom_Level  : in Vis.Zoom_Level);

   function Remove_Pin
     (Window_Name          : in String;
      Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Pin
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String);

   procedure Show_Pin
     (Window_Name : in String;
      Name        : in String);

   ---------------------------------------------------------------------------
   --  Projects
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Closes the currently open project.
   --
   --  This method needs to be called prior to Create_Project and
   --  Open_Project if Is_Project_Loaded returns True.
   function Close_Project
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Creates a new project.
   --
   --  See:
   --    Close_Project
   --    Giant.Graph_Lib.Load
   --    Giant.Projects.Create_Empty_Project_For_File
   procedure Create_Project
     (Project_Filename : in String;
      Graph_Filename   : in String);

   ---------------------------------------------------------------------------
   --  Returns the currently open project.
   function Get_Project
     return Projects.Project_Access;

   ---------------------------------------------------------------------------
   --  Returns a unique name for the current project by appending
   --  digits to Name. This is useful for the creation of project
   --  global object like Vis_Windows and Subgraphs.
   function Get_Unique_Name
     (Name : in String := "Unknown")
      return String;

   ---------------------------------------------------------------------------
   --  Returns True, if the current project was modified. The return
   --  value is undefined if no project is loaded.
   function Has_Project_Changed
     return Boolean;

   ---------------------------------------------------------------------------
   --  Returns True, if a project is loaded; False, otherwise.
   function Is_Project_Loaded
     return Boolean;

   ---------------------------------------------------------------------------
   --  Opens a project.
   --
   --  See:
   --    Close_Project
   --    Giant.Projects.Get_Bauhaus_IML_Graph_Data_File
   --    Giant.Graph_Lib.Load
   --    Giant.Projects.Load_Project_File
   procedure Open_Project
     (Filename : in String);

   ---------------------------------------------------------------------------
   --  Saves the current project.
   --
   --  See:
   --    Giant.Projects.Store_Whole_Project
   procedure Save_Project;

   ---------------------------------------------------------------------------
   --  Saves the current project with in a different directory.
   --
   --  See:
   --    Giant.Projects.Store_Whole_Project_As_For_File
   procedure Save_Project
     (Filename : in String);

   ---------------------------------------------------------------------------
   --  Selections
   ---------------------------------------------------------------------------

   procedure Add_Selection
     (Window_Name     : in     String;
      Selection       : in     Graph_Lib.Selections.Selection;
      Replace_Content : in     Boolean);

   procedure Create_Selection
     (Window_Name : in String;
      Name        : in String);

   procedure Create_Selection_From_Subgraph
     (Subgraph_Name  : in String;
      Window_Name    : in String;
      Selection_Name : in String);

   procedure Duplicate_Selection
     (Window_Name : in String;
      Source_Name : in String;
      Target_Name : in String);

   ---------------------------------------------------------------------------
   --  Returns True, if a selection with Name exists.
   --
   --  See:
   --    Giant.Vis_Windows.Does_Selection_Exist
   function Exists_Selection
     (Window_Name : in String;
      Name        : in String)
     return Boolean;

   function Get_Selection
     (Window_Name    : in String;
      Selection_Name : in String)
     return Graph_Lib.Selections.Selection;

   function Get_Current_Selection
     (Window_Name : in String)
     return Graph_Lib.Selections.Selection;

   procedure Hide_Selection
     (Window_Name : in String;
      Name        : in String);

   procedure Highlight_Selection
     (Window_Name       : in String;
      Name              : in String;
      Highlight_Status  : in Vis_Windows.Selection_Highlight_Status;
      Unhighligt_Others : in Boolean                                := True);

   procedure Insert_Selection
     (Window_Name           : in String;
      Selection_Name        : in String;
      Selection             : in Graph_Lib.Selections.Selection;
      Layout_Name           : in String;
      Position              : in Vis.Logic.Vector_2d := Vis.Logic.Zero_2d;
      Additional_Parameters : in String;
      Parent_Evolution      : in Evolutions.Iterative_Evolution_Class_Access :=
        null);

   ---------------------------------------------------------------------------
   --  Removes the content of Selection from window named Window_Name
   procedure Remove_Selection_Content
     (Window_Name  : in String;
      Selection    : in Graph_Lib.Selections.Selection;
      Do_Not_Touch : in String                         := "");

   function Remove_Selection
     (Window_Name          : in String;
      Name                 : in String;
      Remove_Content       : in Boolean;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Selection
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String);

   procedure Selection_Difference
     (Window_Name : in String;
      Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String);

   procedure Selection_Intersection
     (Window_Name : in String;
      Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String);

   procedure Selection_Union
     (Window_Name : in String;
      Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String);

   procedure Set_Current_Selection
     (Window_Name : in String;
      Name        : in String);

   procedure Show_All_Selections
     (Window_Name    : in String);

   procedure Show_Selection
     (Window_Name : in String;
      Name        : in String);

   procedure Unhighlight_Selection
     (Window_Name : in String;
      Name        : in String);

   ---------------------------------------------------------------------------
   --  Unhighlights all selections that are hightlighted with
   --  Highlight_Status.
   procedure Unhighlight_Selections_By_Color
     (Window_Name      : in String;
      Highlight_Status : in Vis_Windows.Selection_Highlight_Status);

   ---------------------------------------------------------------------------
   --  Subgraphs
   ---------------------------------------------------------------------------

   procedure Add_Subgraph
     (Subgraph : in Graph_Lib.Subgraphs.Subgraph);

   procedure Create_Subgraph
     (Name : in String);

   procedure Create_Subgraph_From_Selection
     (Window_Name    : in String;
      Selection_Name : in String;
      Subgraph_Name  : in String);

   procedure Duplicate_Subgraph
     (Source_Name : in String;
      Target_Name : in String);

   ---------------------------------------------------------------------------
   --  Returns True, if a subgraph with Name exists.
   --
   --  See:
   --    Giant.Projects.Does_Subgraph_Exist
   function Exists_Subgraph
     (Name : in String)
     return Boolean;

   function Get_Subgraph
     (Name : in String)
     return Graph_Lib.Subgraphs.Subgraph;

   function Remove_Subgraph
     (Name : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   procedure Rename_Subgraph
     (Old_Name : in String;
      New_Name : in String);

   procedure Highlight_Subgraph
     (Name              : in String;
      Highlight_Status  : in Projects.Subgraph_Highlight_Status;
      Unhighligt_Others : in Boolean                            := True);

   procedure Subgraph_Difference
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String);

   procedure Subgraph_Intersection
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String);

   procedure Subgraph_Union
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String);

   procedure Unhighlight_Subgraph
     (Name : in String);

   procedure Unhighlight_Subgraphs_By_Color
     (Highlight_Status : in Projects.Subgraph_Highlight_Status);

   ---------------------------------------------------------------------------
   --  Vis Styles
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Sets the visualization style for window named Window_Name to
   --  Vis_Style_Name.
   --
   --  Returns:
   --    True, if style was changed or already active; False, if style
   --    was not found
   --  See:
   --    Config.Vis_Styles.Initialize_Vis_Style_By_Name
   --    Graph_Widgets.Set_Vis_Style
   function Set_Vis_Style
     (Window_Name    : in String;
      Vis_Style_Name : in String)
      return Boolean;
   ---------------------------------------------------------------------------
   --  Windows
   ---------------------------------------------------------------------------

   function Close_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Creates a window with a unique name. The window is opened if the
   --  gui is shown.
   --
   --  See:
   --    Get_Unique_Name
   --    Giant.Vis_Windows.Create_New
   --    Giant.Projects.Add_Visualisation_Window
   --    Open_Window
   procedure Create_Window
     (Name : in String);

   ---------------------------------------------------------------------------
   --  Returns True, if a window with Name exists.
   --
   --  See:
   --    Giant.Projects.Does_Vis_Window_Exist
   function Exists_Window
     (Name : in String)
     return Boolean;

   function Get_Window
     (Name : in String)
     return Vis_Windows.Visual_Window_Access;

   procedure Make_Room
     (Window_Name : in String;
      Center      : in Vis.Logic.Vector_2d;
      Width       : in Vis.Logic_Float;
      Height      : in Vis.Logic_Float);

   ---------------------------------------------------------------------------
   --  Opens a window, if gui is shown.
   --
   --  Parameters:
   --    Name - The name of the window
   --  See:
   --    Giant.Projects.Get_Visualisation_Window
   --    Giant.Gui_Manager.Open
   procedure Open_Window
     (Name : in String);

   ---------------------------------------------------------------------------
   --  Removes a window.
   --
   --  Parameters:
   --    Name - The name of the window
   --  See:
   --    Giant.Projects.Get_Visualisation_Window
   --    Giant.Gui_Manager.Remove_Window
   function Remove_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean;

   ---------------------------------------------------------------------------
   --  Renames a window.
   --
   --  Parameters:
   --    Old_Name - The name of the window
   --    New_Name - The new name of the window
   --  See:
   --    Giant.Projects.Get_Visualisation_Window
   --    Giant.Projects.Change_Vis_Window_Name
   procedure Rename_Window
     (Old_Name : in String;
      New_Name : in String);

   ---------------------------------------------------------------------------
   --  Save a window.
   --
   --  Parameters:
   --    Name - The name of the window
   --  See:
   --    Giant.Projects.Get_Visualisation_Window
   --    Giant.Gui_Manager.Open
   procedure Save_Window
     (Name : in String);

   ---------------------------------------------------------------------------
   --  Zoom and Location
   ---------------------------------------------------------------------------

   procedure Center_On_Node
     (Window_Name : in String;
      Node        : in Graph_Lib.Node_Id);

   procedure Set_Zoom_Level
     (Window_Name : in     String;
      Zoom_Level  : in out Vis.Zoom_Level);

   procedure Zoom_To_All
     (Window_Name : in String);

   procedure Zoom_To_Edge
     (Window_Name : in String;
      Edge        : in Graph_Lib.Edge_Id);

   procedure Zoom_To_Selection
     (Window_Name    : in String;
      Selection_Name : in String);

private

   procedure Add_Selection
     (Window_Name     : in String;
      Selection       : in Graph_Lib.Selections.Selection;
      Replace_Content : in Boolean;
      Lock            :    out Graph_Widgets.Lock_Type);


   Current_Project : Projects.Project_Access := Projects.Null_Project;
   Project_Loaded : Boolean := False;
   Project_Changed : Boolean := False;

   Gsl_Interpreter : Gsl.Interpreters.Interpreter := null;

end Giant.Controller;
