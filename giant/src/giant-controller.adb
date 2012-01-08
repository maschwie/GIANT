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
--  $RCSfile: giant-controller.adb,v $, $Revision: 1.102 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with String_Lists;

with Giant.Basic_Evolutions;
with Giant.Config;
with Giant.Config_Settings;
with Giant.Config.Global_Data;
with Giant.Config.Vis_Styles;
with Giant.Dialogs;
with Giant.File_Management;
with Giant.Graph_Lib;
with Giant.Graph_Lib.Selections;
with Giant.Graph_Lib.Subgraphs;
with Giant.Graph_Widgets;
with Giant.Gui_Manager;
with Giant.Gsl_Support;
with Giant.Layout_Factory;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);
with Giant.Node_Annotations;
with Giant.Vis_Windows;

package body Giant.Controller is

   type Selection_Operation_Type is access function
     (Left        : in Graph_Lib.Selections.Selection;
      Right       : in Graph_Lib.Selections.Selection;
      Target_Name : in String)
     return Graph_Lib.Selections.Selection;

   type Subgraph_Operation_Type is access function
     (Left        : in Graph_Lib.Subgraphs.Subgraph;
      Right       : in Graph_Lib.Subgraphs.Subgraph;
      Target_Name : in String)
     return Graph_Lib.Subgraphs.Subgraph;


   package Logger is new Giant.Logger("giant.controller");

   ---------------------------------------------------------------------------
   --  Application
   ---------------------------------------------------------------------------

   procedure Exit_Application
   is
      use type Gsl.Interpreters.Interpreter;
   begin
      if (Gsl_Interpreter /= null) then
         Gsl.Interpreters.Destroy (Gsl_Interpreter);
      end if;
   end Exit_Application;

   procedure Handle_IO_Exception
     (Error    : in Ada.Exceptions.Exception_Occurrence;
      Filename : in String)
   is
   begin
      Ada.Exceptions.Reraise_Occurrence (Error);
   exception
     when Ada.IO_Exceptions.Data_Error =>
        Show_Error (-"Unexpected data type: " & Filename);
     when Ada.IO_Exceptions.Device_Error =>
        Show_Error (-"Device error during io operation: " & Filename);
     when Ada.IO_Exceptions.End_Error =>
        Show_Error (-"Unexpected end of file: " & Filename);
     when Ada.IO_Exceptions.Layout_Error =>
        Show_Error (-"Unexpected file layout: " & Filename);
     when Ada.Io_Exceptions.Name_Error =>
        Show_Error(-"File not found: " & Filename);
     when Ada.IO_Exceptions.Status_Error =>
        Show_Error (-"File already open: " & Filename);
     when Ada.IO_Exceptions.Use_Error =>
        Show_Error (-"Could not open file: " & Filename);
   end Handle_IO_Exception;

   procedure Handle_Project_Exception
     (Error    : in Ada.Exceptions.Exception_Occurrence;
      Filename : in String)
   is
   begin
      Ada.Exceptions.Reraise_Occurrence (Error);
   exception
     when Giant.Graph_Lib.Load_Error =>
        Show_Error (-"The IML graph could not be loaded.");
     when Projects.Invalid_Project_Directory_Excpetion =>
        Show_Error (-"The selected directory is invalid.");
     when Projects.Wrong_IML_Graph_Loaded_Exception =>
        Show_Error (-"The IML graph is invalid.");
     when Projects.Project_Does_Not_Exist_Exception =>
        Show_Error (-"The project file is invalid.");
     when Projects.Directory_Holds_Already_A_Project_File_Exception =>
        Show_Error (-"The project could not be created. The directory already contains a project.");
     when E : others =>
        Logger.Error ("An exceptions has occured while processing projects.");
        Logger.Error (E);

        Controller.Handle_IO_Exception (E, Filename);
   end Handle_Project_Exception;

   procedure Show_Error
     (Message : in String)
   is
   begin
      if (Gui_Manager.Is_Initialized) then
         Dialogs.Show_Error_Dialog (Message);
      else
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);
      end if;
   end Show_Error;

   function Show_Input
     (Message : in String)
      return String is
   begin
      if (Gui_Manager.Is_Initialized) then
         return Dialogs.Show_Input_Dialog (Message);
      else
         declare
            Input : String (1 .. 1024);
            Last : Natural;
         begin
            Ada.Text_IO.Put (Message & ": ");
            Ada.Text_IO.Get_Line (Ada.Text_IO.Standard_Input, Input, Last);
            return Input (1 .. Last);
         end;
      end if;
   end Show_Input;

   function Show_Source
     (Node : in Graph_Lib.Node_Id)
     return Boolean
   is
      use type Graph_Lib.Node_Attribute_Class_Id;
      Iterator : Graph_Lib.Node_Attribute_Iterator;
      Attribute : Graph_Lib.Node_Attribute_Id := null;
   begin
      Iterator := Graph_Lib.Make_Attribute_Iterator (Node);
      while (Graph_Lib.More (Iterator)) loop
         Graph_Lib.Next (Iterator, Attribute);
         if (Graph_Lib.Get_Node_Attribute_Class_Id (Attribute)
             = Graph_Lib.Class_SLoc) then
            --  launch editor
            File_Management.Execute_External_Editor
              (Command  =>
                 Config_Settings.Get_Setting_As_String ("Editor.Source"),
               Filename =>
                 Graph_Lib.Get_Node_Attribute_SLoc_Path_Value
               (Node, Attribute)
               & Graph_Lib.Get_Node_Attribute_SLoc_Filename_Value
               (Node, Attribute),
               Line     =>
                 Graph_Lib.Get_Node_Attribute_SLoc_Line_Value
               (Node, Attribute),
               Column   =>
                 Graph_Lib.Get_Node_Attribute_SLoc_Column_Value
               (Node, Attribute));
            return True;
         end if;
      end loop;

      return False;
   end Show_Source;

   ---------------------------------------------------------------------------
   --  GUI
   ---------------------------------------------------------------------------

   function Hide_Gui
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
   begin
      return Gui_Manager.Hide (Ask_For_Confirmation);
   end Hide_Gui;

   procedure Show_Gui
   is
   begin
      Gui_Manager.Show;
   end Show_Gui;

   ---------------------------------------------------------------------------
   --  GSL
   ---------------------------------------------------------------------------

   procedure Pre_Execute_Gsl
   is
      use type Gsl.Interpreters.Interpreter;
   begin
      if (Gsl_Interpreter = null) then
         --  lazily instanciate
         Gsl_Interpreter := Gsl.Interpreters.Create_Interpreter;
      end if;
   end Pre_Execute_Gsl;

   procedure Start_Interpreter
   is
      Started : Boolean;
   begin
      Gsl.Interpreters.Start_Calculation
        (Individual => Gsl_Interpreter,
         Started    => Started,
         Dialog     => Gui_Manager.Create_Progress_Dialog
         (-"Executing GSL Script", -"Script is running..."));
      -- XXX: Evaluate started: show error, if not started
   end Start_Interpreter;

   procedure Execute_GSL
     (Filename : in String)
   is
   begin
      Pre_Execute_Gsl;
      Gsl.Interpreters.Execute_Gsl_File
        (Individual => Gsl_Interpreter,
         Name       => Filename,
         Context    => "");
      Start_Interpreter;
   end Execute_GSL;

   procedure Execute_GSL
     (Script_Name : in String;
      Context     : in String;
      Parameter   : in Gsl.Interpreters.Gsl_Params)
   is
   begin
      Pre_Execute_Gsl;
      Gsl.Interpreters.Execute_Script
        (Individual => Gsl_Interpreter,
         Name       => Script_Name,
         Context    => Context,
         Params     => Parameter);
      Start_Interpreter;
   exception
     when Giant.Gsl_Support.Gsl_Script_Not_Found_Exception =>
       Show_Error ("The file " & Script_Name & ".gsl could not be "
                   & "found in the include path.");
   end Execute_GSL;

   function Gsl_Get_Window_Content
     (Window_Name : in String)
      return Graph_Lib.Subgraphs.Subgraph is

      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      return Graph_Widgets.Get_Content
        (Vis_Windows.Get_Graph_Widget (Window));
   end Gsl_Get_Window_Content;

   ---------------------------------------------------------------------------
   --  Layout
   ---------------------------------------------------------------------------

   procedure Apply_Layout
     (Window                : in Vis_Windows.Visual_Window_Access;
      Selection             : in Graph_Lib.Selections.Selection;
      Lock                  : in Graph_Widgets.Lock_Type;
      Layout_Name           : in String;
      Position              : in Vis.Logic.Vector_2d              := Vis.Logic.Zero_2d;
      Additional_Parameters : in String;
      Parent_Calculation    : in Evolutions.Iterative_Evolution_Class_Access)
   is
      Evolution : Evolutions.Evolution_Class_Access;
      Started : Boolean;
   begin
      Logger.Debug ("Applying layout: " & Layout_Name & "["
                    & Additional_Parameters & "]");
      begin
         Layout_Factory.Create (Algorithm => Layout_Name,
                                Selection_To_Layout => Selection,
                                Widget => Vis_Windows.Get_Graph_Widget (Window),
                                Widget_Lock => Lock,
                                Target_Position => Position,
                                Additional_Parameters =>
                                  Additional_Parameters,
                                Layout_Evolution => Evolution);
      exception
         --  "when others" to ALWAYS free lock
         when others =>
            Graph_Widgets.Release_Lock
              (Vis_Windows.Get_Graph_Widget (Window), Lock);
            raise;
      end;
      if Evolutions."=" (Parent_Calculation, null) then
         Evolutions.Start_Calculation (Evolution,
                                       Gui_Manager.Create_Progress_Dialog
                                       (-"Applying Layout",
                                        -"Layout is calculated..."),
                                       Started);
      else
         Evolutions.Start_Sub_Calculation
           (Parent_Calculation, Evolution);
      end if;
      --  Evolutions.Start_Calculation_Blocked (Evolution);
   end Apply_Layout;

   procedure Apply_Layout
     (Layout_Name           : in String;
      Window_Name           : in String;
      Selection_Name        : in String;
      Position              : in Vis.Logic.Vector_2d;
      Additional_Parameters : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Selection : Graph_Lib.Selections.Selection
        := Vis_Windows.Get_Selection (Window, Selection_Name);
      Lock : Graph_Widgets.Lock_Type;
   begin
      Graph_Widgets.Lock_Selection (Vis_Windows.Get_Graph_Widget (Window),
                                    Selection, Lock);
      Apply_Layout (Window, Selection, Lock, Layout_Name, Position,
                    Additional_Parameters, null);
   end Apply_Layout;

   ---------------------------------------------------------------------------
   --  Node Annotations
   ---------------------------------------------------------------------------

   function Get_Node_Annotation
     (Node : in Graph_Lib.Node_Id)
     return String
   is
   begin
      if (Is_Node_Annotated (Node)) then
         return Node_Annotations.Get_Annotation_Text
           (Projects.Get_Node_Annotations (Current_Project), Node);
      else
         return "";
      end if;
   end;

   function Is_Node_Annotated
     (Node : in Graph_Lib.Node_Id)
     return Boolean
   is
   begin
      return Node_Annotations.Is_Annotated
        (Projects.Get_Node_Annotations (Current_Project), Node);
   end;

   procedure Set_Node_Annotation
     (Node : in Graph_Lib.Node_Id;
      Text : in String)
   is
   begin
      if (Is_Node_Annotated (Node)) then
         Node_Annotations.Change_Node_Annotation
           (Projects.Get_Node_Annotations (Current_Project), Node, Text);
      else
         Node_Annotations.Add_Node_Annotation
           (Projects.Get_Node_Annotations (Current_Project), Node, Text);
         Gui_Manager.Update_Node_Annotation (Node);
      end if;
   end;

   procedure Remove_Node_Annotation
     (Node : in Graph_Lib.Node_Id)
   is
   begin
      if (Is_Node_Annotated (Node)) then
         Node_Annotations.Remove_Node_Annotation
           (Projects.Get_Node_Annotations (Current_Project), Node);
         Gui_Manager.Update_Node_Annotation (Node);
      end if;
   end;

   ---------------------------------------------------------------------------
   --  Projects
   ---------------------------------------------------------------------------

   procedure Initialize_Project
   is
   begin
      Project_Loaded := True;

      Gui_Manager.Initialize_Project;
      Logger.Info (-"Project initialized");
   end;

   function Close_Project
     (Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
   begin
      if (not Project_Loaded) then
         return True;
      end if;

      if (not Gui_Manager.Close_Project (Ask_For_Confirmation)) then
         return False;
      end if;

      Logger.Info (-"Closing current project");

      Project_Loaded := False;

      Projects.Deallocate_Project_Deep (Current_Project);
      Current_Project := Projects.Null_Project;

      Graph_Lib.Unload;

      return True;
   end;

   procedure Create_Project
     (Project_Filename : in String;
      Graph_Filename   : in String)
   is
      Checksum : Integer;
      Closed : Boolean;
      Individual : Basic_Evolutions.Basic_Evolution_Access;
   begin
      --  check this now to avoid loading the graph
      if (Projects.Is_Already_A_Project_File_In_Directory
          (File_Management.Return_Dir_Path_For_File_Path (Project_Filename)))
          then
         raise Projects.Directory_Holds_Already_A_Project_File_Exception;
      end if;

      --  create directory
      File_Management.Create_Dir_Path
        (File_Management.Get_Path (Project_Filename));

      --  should not fail
      Closed := Close_Project;

      --  progress dialog
      Individual := Basic_Evolutions.Create
        (Gui_Manager.Create_Progress_Dialog
         (-"Creating Project", "Loading Graph"));

      begin
         --  create graph
         Graph_Lib.Load (Graph_Filename, Individual);
         Checksum := Graph_Lib.Get_Graph_Hash;

         Logger.Info (-"Creating project " & Project_Filename);

         --  create project
         Current_Project := Projects.Create_Empty_Project_For_File
           (Project_Filename, Graph_Filename, Checksum);

         --  update application
         Initialize_Project;
      exception
        when others =>
           --  close progress dialog
           Basic_Evolutions.Destroy (Individual);
           raise;
      end;

      --  close progress dialog
      Basic_Evolutions.Destroy (Individual);
   end Create_Project;

   function Get_Project
     return Projects.Project_Access
   is
   begin
      return Current_Project;
   end;

   function Get_Unique_Name
     (Name : in String := "Unknown")
      return String
   is
      I : Natural := 1;
   begin
      --  try the plain name first
      if (not Projects.Exists_Name (Current_Project, Name)) then
         return Name;
      end if;

      while (True) loop
         declare
            Unique_Name : String := Name & Integer'Image (I);
         begin
            Logger.Debug ("Get_Unique_Name: trying name " & Unique_Name);

            if (not Projects.Exists_Name (Current_Project, Unique_Name)) then
               return Unique_Name;
            end if;
         end;
         I := I + 1;
      end loop;

      --  this can never happen, we will get a constrained error instead
      --  when I runs out of bounds
      return "";
   end;

   function Has_Project_Changed
     return Boolean
   is
   begin
      return Project_Changed;
   end Has_Project_Changed;

   function Is_Project_Loaded
     return Boolean
   is
   begin
      return Project_Loaded;
   end Is_Project_Loaded;

   procedure Open_Project
     (Filename : in String)
   is
      Graph_Filename : String
        := Projects.Get_Bauhaus_IML_Graph_File (Filename);
      Closed : Boolean;
      Individual : Basic_Evolutions.Basic_Evolution_Access;
   begin
      --  check this now to avoid loading the graph
      if (not Projects.Does_Project_Exist_File (Filename)) then
         raise Projects.Project_Does_Not_Exist_Exception;
      end if;

      --  should not fail
      Closed := Close_Project;

      --  progress dialog
      Individual := Basic_Evolutions.Create
        (Gui_Manager.Create_Progress_Dialog
         (-"Loading Project", "Loading Graph"));

      begin
         --  load graph
         Logger.Info (-"Loading graph " & Graph_Filename);
         Giant.Graph_Lib.Load (Graph_Filename, Individual);

         Logger.Info (-"Opening project " & Filename);
         Current_Project := Projects.Load_Project_File (Filename);

         --  update application
         Initialize_Project;
      exception
         when others =>
            --  close progress dialog
            Basic_Evolutions.Destroy (Individual);
            raise;
      end;

      --  close progress dialog
      Basic_Evolutions.Destroy (Individual);
   end;

   procedure Save_Project
   is
   begin
      Projects.Store_Whole_Project (Current_Project);
      Logger.Info (-"Project saved");
   end Save_Project;

   procedure Save_Project
     (Filename : in String)
   is
   begin
      Projects.Store_Whole_Project_As_For_File (Current_Project, Filename);
      Logger.Info (-"Project saved");
   end Save_Project;

   ---------------------------------------------------------------------------
   --  Pins
   ---------------------------------------------------------------------------

   procedure Create_Pin
     (Window_Name : in String;
      Name        : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Position : Vis.Logic.Vector_2d
        := Graph_Widgets.Get_Location (Vis_Windows.Get_Graph_Widget (Window));
      Zoom_Level : Vis.Zoom_Level
        := Graph_Widgets.Get_Zoom_Level (Vis_Windows.Get_Graph_Widget (Window));
   begin
      Create_Pin (Window_Name, Name, Position, Zoom_Level);
   end Create_Pin;

   procedure Create_Pin
     (Window_Name : in String;
      Name        : in String;
      Position    : in Vis.Logic.Vector_2d;
      Zoom_Level  : in Vis.Zoom_Level)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      Vis_Windows.Add_Pin (Window, Name, Position, Zoom_Level);
      Gui_Manager.Add_Pin (Window_Name, Name);
   end Create_Pin;

   function Remove_Pin
     (Window_Name          : in String;
      Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      if (Gui_Manager.Remove_Pin (Window_Name, Name)) then
         Vis_Windows.Remove_Pin (Window, Name);
         return True;
      else
         return False;
      end if;
   end Remove_Pin;

   procedure Rename_Pin
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      if (New_Name /= Old_Name) then
         Vis_Windows.Change_Pin_Name (Window, Old_Name, New_Name);
         Gui_Manager.Rename_Pin (Window_Name, Old_Name, New_Name);
      end if;
   end Rename_Pin;

   procedure Show_Pin
     (Window_Name : in String;
      Name        : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      Graph_Widgets.Set_Location_And_Zoom_Level
        (Vis_Windows.Get_Graph_Widget (Window),
         Vis_Windows.Get_Position (Window, Name),
         Vis_Windows.Get_Zoom (Window, Name));
      Gui_Manager.Update_Zoom_Level (Window_Name);
   end Show_Pin;

   ---------------------------------------------------------------------------
   --  Selections
   ---------------------------------------------------------------------------

   procedure Add_Selection
     (Window_Name     : in     String;
      Selection       : in     Graph_Lib.Selections.Selection;
      Replace_Content : in     Boolean)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Lock : Graph_Widgets.Lock_Type;
   begin
      Add_Selection (Window_Name, Selection, Replace_Content, Lock);
      Graph_Widgets.Release_Lock (Vis_Windows.Get_Graph_Widget (Window), Lock);
   end Add_Selection;

   procedure Add_Selection
     (Window_Name     : in     String;
      Selection       : in     Graph_Lib.Selections.Selection;
      Replace_Content : in     Boolean;
      Lock            :    out Graph_Widgets.Lock_Type)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project,
                                              Window_Name);
      Name : constant String := Graph_Lib.Selections.Get_Name (Selection);
      Replace : Boolean;
      Removed : Boolean;
      Old_Highlight_Status : Vis_Windows.Selection_Highlight_Status;
   begin
      if (Name = Vis_Windows.Get_Standard_Selection (Window)) then
         raise Vis_Windows.Standard_Selection_Name_May_Not_Be_Changed_Exception;
      end if;

      Replace := Vis_Windows.Does_Selection_Exist (Window, Name);
      if (Replace) then
         --  save status
         Old_Highlight_Status := Vis_Windows.Get_Highlight_Status (Window, Name);

         Removed := Remove_Selection (Window_Name, Name,
                                      Ask_For_Confirmation => False,
                                      Remove_Content       => Replace_Content);
         pragma Assert (Removed);
      end if;

      Vis_Windows.Add_Selection (Window, Selection);
      Graph_Widgets.Insert_Selection
        (Vis_Windows.Get_Graph_Widget (Window), Selection, Lock);
      Gui_Manager.Add_Selection (Window_Name, Name);

      if (Replace) then
         Highlight_Selection (Window_Name, Name, Old_Highlight_Status);
      end if;
   end Add_Selection;

   procedure Create_Selection
     (Window_Name : in String;
      Name        : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Selection : Graph_Lib.Selections.Selection
        := Graph_Lib.Selections.Create (Name);
   begin
      Vis_Windows.Add_Selection (Window, Selection);
      Gui_Manager.Add_Selection (Window_Name, Name);
   end Create_Selection;

   procedure Create_Selection_From_Subgraph
     (Subgraph_Name  : in String;
      Window_Name    : in String;
      Selection_Name : in String)
   is
      Subgraph : Graph_Lib.Subgraphs.Subgraph
        := Projects.Get_Subgraph (Current_Project, Subgraph_Name);
      Selection : Graph_Lib.Selections.Selection
        := Graph_Lib.Subgraphs.Create_Selection (Subgraph,
                                                 Selection_Name);
   begin
      Add_Selection
        (Window_Name,
         Selection,
         Replace_Content => False);
   end Create_Selection_From_Subgraph;

   procedure Duplicate_Selection
     (Window_Name : in String;
      Source_Name : in String;
      Target_Name : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Source : Graph_Lib.Selections.Selection
        := Vis_Windows.Get_Selection (Window, Source_Name);
      Target : Graph_Lib.Selections.Selection
        := Graph_Lib.Selections.Clone (Source, Target_Name);
   begin
      Vis_Windows.Add_Selection (Window, Target);
      Gui_Manager.Add_Selection (Window_Name, Target_Name);
   end Duplicate_Selection;

   function Exists_Selection
     (Window_Name : in String;
      Name        : in String)
     return Boolean
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      return Vis_Windows.Does_Selection_Exist (Window, Name);
   end Exists_Selection;

   function Get_Current_Selection
     (Window_Name : in String)
      return Graph_Lib.Selections.Selection
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      return Vis_Windows.Get_Selection
        (Window, Vis_Windows.Get_Current_Selection (Window));
   end Get_Current_Selection;

   function Get_Selection
     (Window_Name    : in String;
      Selection_Name : in String)
     return Graph_Lib.Selections.Selection
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      return Vis_Windows.Get_Selection (Window, Selection_Name);
   end Get_Selection;

   function To_Selection_Hightlight_ID
     (Highlight_Status : in Vis_Windows.Selection_Highlight_Status)
     return Config.Global_Data.Selection_High_Light_ID
   is
   begin
      case (Highlight_Status) is
        when Vis_Windows.Color_1 =>
           return Config.Global_Data.Color_1;
        when Vis_Windows.Color_2 =>
           return Config.Global_Data.Color_2;
        when Vis_Windows.Color_3 =>
           return Config.Global_Data.Color_3;
        when Vis_Windows.Current_Selection =>
           return Config.Global_Data.Current_Selection;
        when others =>
           raise Constraint_Error;
      end case;
   end To_Selection_Hightlight_ID;

   procedure Hide_Selection
     (Window_Name : in String;
      Name        : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      if (not Vis_Windows.Is_Faded_Out (Window, Name)) then
         Vis_Windows.Fade_Out_Selection (Window, Name);
      end if;
   end Hide_Selection;

   procedure Highlight_Selection
     (Window_Name       : in String;
      Name              : in String;
      Highlight_Status  : in Vis_Windows.Selection_Highlight_Status;
      Unhighligt_Others : in Boolean                                := True)
   is
      use type Vis_Windows.Selection_Highlight_Status;

      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Selection : Graph_Lib.Selections.Selection
        := Get_Selection (Window_Name, Name);
      Old_Highlight_Status : Vis_Windows.Selection_Highlight_Status
        := Vis_Windows.Get_Highlight_Status (Window, Name);
   begin
      if (Old_Highlight_Status /= Highlight_Status) then
         if (Unhighligt_Others) then
            --  unhighlight other selections with the same color
            Unhighlight_Selections_By_Color (Window_Name, Highlight_Status);
         end if;

         Vis_Windows.Set_Highlight_Status (Window, Name, Highlight_Status);
         if (Highlight_Status = Vis_Windows.None) then
            Graph_Widgets.Remove_Local_Highlighting
              (Vis_Windows.Get_Graph_Widget (Window), Selection,
               To_Selection_Hightlight_ID (Old_Highlight_Status));
         else
            if (Old_Highlight_Status /= Vis_Windows.None) then
               Graph_Widgets.Remove_Local_Highlighting
                 (Vis_Windows.Get_Graph_Widget (Window), Selection,
                  To_Selection_Hightlight_ID (Old_Highlight_Status));
            end if;
            Graph_Widgets.Add_Local_Highlighting
              (Vis_Windows.Get_Graph_Widget (Window), Selection,
               To_Selection_Hightlight_ID (Highlight_Status));
         end if;
      end if;
      Gui_Manager.Update_Selection (Window_Name, Name);
   end;

   procedure Insert_Selection
     (Window_Name           : in String;
      Selection_Name        : in String;
      Selection             : in Graph_Lib.Selections.Selection;
      Layout_Name           : in String;
      Position              : in Vis.Logic.Vector_2d      := Vis.Logic.Zero_2d;
      Additional_Parameters : in String;
      Parent_Evolution      : in Evolutions.Iterative_Evolution_Class_Access :=
        null)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Lock : Graph_Widgets.Lock_Type;
   begin
      Add_Selection
        (Window_Name     => Window_Name,
         Selection       => Selection,
         Replace_Content => True,
         Lock            => Lock);
      if (Layout_Name /= "") then
         Apply_Layout (Window, Selection, Lock, Layout_Name, Position,
                       Additional_Parameters, Parent_Evolution);
      end if;
   end Insert_Selection;

   function Remove_Selection
     (Window_Name          : in String;
      Name                 : in String;
      Remove_Content       : in Boolean;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Selection : Graph_Lib.Selections.Selection
        := Get_Selection (Window_Name, Name);
   begin
      if (Vis_Windows.Get_Standard_Selection (Window) = Name) then
         --  we need to raise this before Gui_Manager.Remove_Selection
         --  is called, otherwise we loose gui consistency
         raise Vis_Windows.Standard_Selection_May_Not_Be_Removed_Exception;
      end if;

      if (Gui_Manager.Remove_Selection (Window_Name, Name,
                                        Ask_For_Confirmation)) then
         if (Vis_Windows.Get_Current_Selection (Window) = Name) then
            Set_Current_Selection
              (Window_Name, Vis_Windows.Get_Standard_Selection (Window));
         else
            Unhighlight_Selection (Window_Name, Name);
         end if;

         if (Remove_Content) then
            --  remove the selection from graph widget before it is destroyed
            Graph_Widgets.Remove_Selection
              (Vis_Windows.Get_Graph_Widget (Window), Selection);
            Remove_Selection_Content
              (Window_Name  => Window_Name,
               Selection    => Selection,
               Do_Not_Touch => Name);
         end if;
         Vis_Windows.Remove_Selection (Window, Name);
         return True;
      end if;
      return False;
   end Remove_Selection;

   procedure Remove_Selection_Content
     (Window_Name  : in String;
      Selection    : in Graph_Lib.Selections.Selection;
      Do_Not_Touch : in String := "")
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      List : String_Lists.List;
      Iterator : String_Lists.ListIter;
      Target_Name : Ada.Strings.Unbounded.Unbounded_String;
      Target_Selection : Graph_Lib.Selections.Selection;
   begin
         --  iterate through all selections
      List := Vis_Windows.Get_All_Selections (Window);
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Target_Name);
         if (Ada.Strings.Unbounded.To_String (Target_Name) /=
             Do_Not_Touch) then
            Target_Selection := Get_Selection
              (Window_Name,
               Ada.Strings.Unbounded.To_String (Target_Name));
            Graph_Lib.Selections.Remove (Target_Selection, Selection);
            Gui_Manager.Update_Selection
              (Window_Name,
               Ada.Strings.Unbounded.To_String (Target_Name));
         end if;
      end loop;
      String_Lists.Destroy (List);
   end Remove_Selection_Content;

   procedure Rename_Selection
     (Window_Name : in String;
      Old_Name    : in String;
      New_Name    : in String)
   is
      Window : constant Vis_Windows.Visual_Window_Access
        := Get_Window (Window_Name);
   begin
      if (New_Name /= Old_Name) then
          Vis_Windows.Change_Selection_Name
           (Window, Old_Name, New_Name);
         Gui_Manager.Rename_Selection (Window_Name, Old_Name, New_Name);
      end if;
   end Rename_Selection;

   procedure Set_Current_Selection
     (Window_Name : in String;
      Name        : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project,
                                              Window_Name);
      Previous_Selection_Name : String
        := Vis_Windows.Get_Current_Selection (Window);
      Selection : Graph_Lib.Selections.Selection
        := Get_Selection (Window_Name, Name);
      Previous_Selection : Graph_Lib.Selections.Selection
        := Get_Selection (Window_Name, Previous_Selection_Name);
   begin
      if (Name /= Previous_Selection_Name) then
         Unhighlight_Selection (Window_Name, Name);
         Vis_Windows.Set_Current_Selection (Window, Name);

         --  do not call Highlight_Selection, the highlight status of
         --  the current selection can not be modified
         Graph_Widgets.Remove_Local_Highlighting
           (Vis_Windows.Get_Graph_Widget (Window), Previous_Selection,
            To_Selection_Hightlight_ID (Vis_Windows.Current_Selection));
         Graph_Widgets.Add_Local_Highlighting
           (Vis_Windows.Get_Graph_Widget (Window), Selection,
            To_Selection_Hightlight_ID (Vis_Windows.Current_Selection));

         Gui_Manager.Update_Selection (Window_Name, Previous_Selection_Name);
         Gui_Manager.Update_Selection (Window_Name, Name);
      else
         Logger.Info (-"The selection is already active.");
      end if;
   end Set_Current_Selection;

   procedure Show_All_Selections
     (Window_Name    : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      List : String_Lists.List;
      Iterator : String_Lists.ListIter;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  iterate through all selections
      List := Vis_Windows.Get_All_Selections (Window);
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Name);
         if (Vis_Windows.Is_Faded_Out
             (Window, Ada.Strings.Unbounded.To_String (Name))) then
            Vis_Windows.Fade_In_Selection
              (Window, Ada.Strings.Unbounded.To_String (Name));
         end if;
      end loop;
      String_Lists.Destroy (List);
   end;

   procedure Show_Selection
     (Window_Name : in String;
      Name        : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      if (Vis_Windows.Is_Faded_Out (Window, Name)) then
         Vis_Windows.Fade_In_Selection (Window, Name);
      end if;
   end;

   procedure Selection_Operation
     (Window_Name : in String;
      Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String;
      Operation   : in Selection_Operation_Type)
   is
      Window : Vis_Windows.Visual_Window_Access := Get_Window (Window_Name);
      Left : Graph_Lib.Selections.Selection;
      Right : Graph_Lib.Selections.Selection;
      Target : Graph_Lib.Selections.Selection;
   begin
      Left := Vis_Windows.Get_Selection (Window, Left_Name);
      Right := Vis_Windows.Get_Selection (Window, Right_Name);

      --  operation
      Target := Operation (Left, Right, Target_Name);

      --  add to project
      Vis_Windows.Add_Selection (Window, Target);
      Gui_Manager.Add_Selection (Window_Name, Target_Name);
   end;

   procedure Selection_Difference
     (Window_Name : in String;
      Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String)
   is
   begin
      Selection_Operation (Window_Name, Left_Name, Right_Name, Target_Name,
                           Graph_Lib.Selections.Difference'Access);
   end;

   procedure Selection_Intersection
     (Window_Name : in String;
      Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String)
   is
   begin
      Selection_Operation (Window_Name, Left_Name, Right_Name, Target_Name,
                           Graph_Lib.Selections.Intersection'Access);
   end;

   procedure Selection_Union
     (Window_Name : in String;
      Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String)
   is
   begin
      Selection_Operation (Window_Name, Left_Name, Right_Name, Target_Name,
                           Graph_Lib.Selections.Union'Access);
   end;

   procedure Unhighlight_Selection
     (Window_Name : in String;
      Name        : in String)
   is
   begin
      Highlight_Selection (Window_Name, Name, Vis_Windows.None);
   end Unhighlight_Selection;

   procedure Unhighlight_Selections_By_Color
     (Window_Name      : in String;
      Highlight_Status : in Vis_Windows.Selection_Highlight_Status)
   is
      use type Vis_Windows.Selection_Highlight_Status;
      use Ada.Strings.Unbounded;

      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      List : String_Lists.List;
      Iterator : String_Lists.ListIter;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      List := Vis_Windows.Get_All_Selections (Window);
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Name);
         if (Vis_Windows.Get_Highlight_Status (Window, To_String (Name))
             = Highlight_Status) then
            --  unhighlight
            Highlight_Selection (Window_Name, To_String (Name),
                                 Vis_Windows.None, Unhighligt_Others => False);
         end if;
      end loop;
      String_Lists.Destroy (List);
   end Unhighlight_Selections_By_Color;

   ---------------------------------------------------------------------------
   --  Subgraphs
   ---------------------------------------------------------------------------

   procedure Add_Subgraph
     (Subgraph : in Graph_Lib.Subgraphs.Subgraph)
   is
      Name : constant String := Graph_Lib.Subgraphs.Get_Name (Subgraph);
      Replace : Boolean;
      Removed : Boolean;
      Old_Highlight_Status : Projects.Subgraph_Highlight_Status;
   begin
      Replace := Projects.Does_Subgraph_Exist (Current_Project, Name);
      if (Replace) then
         Old_Highlight_Status
           := Projects.Get_Highlight_Status (Current_Project, Name);
         Removed := Remove_Subgraph
           (Name,
            Ask_For_Confirmation => False);
         pragma Assert (Removed);
      end if;

      Projects.Add_Subgraph (Current_Project, Subgraph);
      Gui_Manager.Add_Subgraph (Name);

      if (Replace) then
         Highlight_Subgraph (Name, Old_Highlight_Status);
      end if;
   end Add_Subgraph;

   procedure Create_Subgraph
     (Name : in String)
   is
      Subgraph : Graph_Lib.Subgraphs.Subgraph;
   begin
      Subgraph := Graph_Lib.Subgraphs.Create (Name);
      Add_Subgraph (Subgraph);
   end Create_Subgraph;

   procedure Create_Subgraph_From_Selection
     (Window_Name    : in String;
      Selection_Name : in String;
      Subgraph_Name  : in String)
   is
      Selection : Graph_Lib.Selections.Selection
        := Get_Selection (Window_Name, Selection_Name);
      Subgraph : Graph_Lib.Subgraphs.Subgraph
        := Graph_Lib.Subgraphs.Create (Subgraph_Name, Selection);
   begin
      Add_Subgraph (Subgraph);
   end Create_Subgraph_From_Selection;

   procedure Duplicate_Subgraph
     (Source_Name : in String;
      Target_Name : in String)
   is
      Source : Graph_Lib.Subgraphs.Subgraph
        := Projects.Get_Subgraph (Current_Project, Source_Name);
      Target : Graph_Lib.Subgraphs.Subgraph
        := Graph_Lib.Subgraphs.Clone (Source, Target_Name);
   begin
      Projects.Add_Subgraph (Current_Project, Target);
      Gui_Manager.Add_Subgraph (Target_Name);
   end Duplicate_Subgraph;

   function Exists_Subgraph
     (Name : in String)
     return Boolean
   is
   begin
      return Projects.Does_Subgraph_Exist (Current_Project, Name);
   end Exists_Subgraph;

   function Get_Subgraph
     (Name        : in String)
     return Graph_Lib.Subgraphs.Subgraph
   is
   begin
      return Projects.Get_Subgraph (Current_Project, Name);
   end Get_Subgraph;

   function To_Subgraph_Hightlight_ID
     (Highlight_Status : in Projects.Subgraph_Highlight_Status)
     return Config.Global_Data.Subgraph_High_Light_ID
   is
   begin
      case (Highlight_Status) is
        when Projects.Color_1 =>
           return Config.Global_Data.Color_1;
        when Projects.Color_2 =>
           return Config.Global_Data.Color_2;
        when Projects.Color_3 =>
           return Config.Global_Data.Color_3;
        when others =>
           raise Constraint_Error;
      end case;
   end To_Subgraph_Hightlight_ID;

   procedure Highlight_Subgraph
     (Name              : in String;
      Highlight_Status  : in Projects.Subgraph_Highlight_Status;
      Unhighligt_Others : in Boolean                            := True)
   is
     use type Projects.Subgraph_Highlight_Status;
     use Ada.Strings.Unbounded;

     Subgraph : Graph_Lib.Subgraphs.Subgraph
       := Get_Subgraph (Name);
     Old_Highlight_Status : Projects.Subgraph_Highlight_Status
       := Projects.Get_Highlight_Status (Current_Project, Name);

     procedure Highlight_Subgraph
       (Window_Name : in String)
     is
        Window : Vis_Windows.Visual_Window_Access
          := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
     begin
        if (Highlight_Status = Projects.None) then
           Graph_Widgets.Remove_Global_Highlighting
             (Vis_Windows.Get_Graph_Widget (Window), Subgraph,
              To_Subgraph_Hightlight_ID (Old_Highlight_Status));
        else
           if (Old_Highlight_Status /= Projects.None) then
              Graph_Widgets.Remove_Global_Highlighting
                (Vis_Windows.Get_Graph_Widget (Window), Subgraph,
                 To_Subgraph_Hightlight_ID (Old_Highlight_Status));
           end if;
           Graph_Widgets.Add_Global_Highlighting
             (Vis_Windows.Get_Graph_Widget (Window), Subgraph,
              To_Subgraph_Hightlight_ID (Highlight_Status));
        end if;
     end Highlight_Subgraph;

     List : String_Lists.List;
     Iterator : String_Lists.ListIter;
     Window_Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      if (Old_Highlight_Status /= Highlight_Status) then
         if (Unhighligt_Others) then
            --  unhighlight other subgraphs with the same color
            Unhighlight_Subgraphs_By_Color (Highlight_Status);
         end if;

         Projects.Change_Highlight_Status (Current_Project, Name,
                                           Highlight_Status);

         List := Projects.Get_All_Visualisation_Window_Names (Current_Project);
         Iterator := String_Lists.MakeListIter (List);
         while String_Lists.More (Iterator) loop
            String_Lists.Next (Iterator, Window_Name);
            if (Projects.Is_Vis_Window_Memory_Loaded
                (Current_Project, To_String (Window_Name))) then

               Highlight_Subgraph (To_String (Window_Name));
            end if;
         end loop;
         String_Lists.Destroy (List);
         Gui_Manager.Update_Subgraph (Name);
      end if;
   end Highlight_Subgraph;

   function Remove_Subgraph
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
   begin
      if (Gui_Manager.Remove_Subgraph (Name, Ask_For_Confirmation)) then
         Unhighlight_Subgraph (Name);
         Projects.Remove_Subgraph (Current_Project, Name);
         return True;
      end if;
      return False;
   end Remove_Subgraph;

   procedure Rename_Subgraph
     (Old_Name : in String;
      New_Name : in String)
   is
   begin
      if (New_Name /= Old_Name) then
         Projects.Change_Subgraph_Name
           (Current_Project, Old_Name, New_Name);
         Gui_Manager.Rename_Subgraph (Old_Name, New_Name);
      end if;
   end Rename_Subgraph;

   procedure Subgraph_Operation
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String;
      Operation   : in Subgraph_Operation_Type)
   is
      Left : Graph_Lib.Subgraphs.Subgraph;
      Right : Graph_Lib.Subgraphs.Subgraph;
      Target : Graph_Lib.Subgraphs.Subgraph;
   begin
      Left := Projects.Get_Subgraph (Current_Project, Left_Name);
      Right := Projects.Get_Subgraph (Current_Project, Right_Name);

      --  operation
      Target := Operation (Left, Right, Target_Name);

      --  add to project
      Projects.Add_Subgraph (Current_Project, Target);
      Gui_Manager.Add_Subgraph (Target_Name);
   end;

   procedure Subgraph_Difference
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String)
   is
   begin
      Subgraph_Operation (Left_Name, Right_Name, Target_Name,
                          Graph_Lib.Subgraphs.Difference'Access);
   end;

   procedure Subgraph_Intersection
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String)
   is
   begin
      Subgraph_Operation (Left_Name, Right_Name, Target_Name,
                          Graph_Lib.Subgraphs.Intersection'Access);
   end;

   procedure Subgraph_Union
     (Left_Name   : in String;
      Right_Name  : in String;
      Target_Name : in String)
   is
   begin
      Subgraph_Operation (Left_Name, Right_Name, Target_Name,
                          Graph_Lib.Subgraphs.Union'Access);
   end;

   procedure Unhighlight_Subgraph
     (Name : in String)
   is
   begin
      Highlight_Subgraph (Name, Projects.None);
   end Unhighlight_Subgraph;

   procedure Unhighlight_Subgraphs_By_Color
     (Highlight_Status : in Projects.Subgraph_Highlight_Status)
   is
      use type Projects.Subgraph_Highlight_Status;
      use Ada.Strings.Unbounded;

      List : String_Lists.List;
      Iterator : String_Lists.ListIter;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      List := Projects.Get_All_Subgraphs (Current_Project);
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Name);
         if (Projects.Get_Highlight_Status (Current_Project, To_String (Name))
             = Highlight_Status) then
            --  unhighlight
            Highlight_Subgraph (To_String (Name), Projects.None,
                                Unhighligt_Others => False);
         end if;
      end loop;
      String_Lists.Destroy (List);
   end Unhighlight_Subgraphs_By_Color;

   ---------------------------------------------------------------------------
   --  Vis Styles
   ---------------------------------------------------------------------------

   function Set_Vis_Style
     (Window_Name    : in String;
      Vis_Style_Name : in String)
     return Boolean
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Style : Config.Vis_Styles.Visualisation_Style_Access;
   begin
      if not Config.Vis_Styles.Does_Vis_Style_Exist (Vis_Style_Name) then
         return False;
      end if;

      if Vis_Windows.Get_Vis_Style (Window) /= Vis_Style_Name then
         Logger.Debug ("Setting vis style for " & Window_Name & ": "
                       & Vis_Style_Name);
         Style
           := Config.Vis_Styles.Initialize_Vis_Style_By_Name (Vis_Style_Name);
         Vis_Windows.Set_Vis_Style (Window, Vis_Style_Name);
         Graph_Widgets.Set_Vis_Style (Vis_Windows.Get_Graph_Widget (Window),
                                      Style);
         Gui_Manager.Update_Vis_Style (Window_Name);
      end if;
      return True;
   end Set_Vis_Style;

   ---------------------------------------------------------------------------
   --  Windows
   ---------------------------------------------------------------------------

   function Close_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
      Removed : Boolean;
   begin
      if (not Projects.Is_Vis_Window_Memory_Loaded
          (Current_Project, Name)) then
         --  the window is not even loaded
         return True;
      end if;

      if (Gui_Manager.Close (Name, Ask_For_Confirmation)) then
         Projects.Free_Memory_For_Vis_Window (Current_Project, Name);

         if (Projects.Does_Vis_Window_Exist (Current_Project, Name)) then
            Gui_Manager.Update_Window (Name);
         else
            --  the window was never saved
            Removed :=
              Gui_Manager.Remove_Window (Name, Ask_For_Confirmation => False);
         end if;
         return True;
      end if;
      return False;
   end Close_Window;

   procedure Create_Window
     (Name : in String)
   is
      Window : Vis_Windows.Visual_Window_Access;
   begin
      Window := Vis_Windows.Create_New
        (Name, Projects.Get_Node_Annotations (Current_Project));
      Projects.Add_Visualisation_Window (Current_Project, Window);
      Gui_Manager.Add_Window (Name);
      Open_Window (Name);
   end Create_Window;

   function Exists_Window
     (Name : in String)
     return Boolean
   is
   begin
      return Projects.Does_Vis_Window_Exist (Current_Project, Name);
   end Exists_Window;

   function Get_Window
     (Name : in String)
     return Vis_Windows.Visual_Window_Access
   is
   begin
      return Projects.Get_Visualisation_Window (Current_Project, Name);
   end Get_Window;

   procedure Make_Room
     (Window_Name : in String;
      Center      : in Vis.Logic.Vector_2d;
      Width       : in Vis.Logic_Float;
      Height      : in Vis.Logic_Float)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      Graph_Widgets.Make_Room (Vis_Windows.Get_Graph_Widget (Window),
                               Center, Width, Height);
   end Make_Room;

   procedure Open_Window
     (Name : in String)
   is
      use type Projects.Subgraph_Highlight_Status;

      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Name);
      List : String_Lists.List;
      Iterator : String_Lists.ListIter;
      Subgraph_Name : Ada.Strings.Unbounded.Unbounded_String;
      Subgraph : Graph_Lib.Subgraphs.Subgraph;
      Highlight_Status : Projects.Subgraph_Highlight_Status;
   begin
      Gui_Manager.Open (Window);

      --  set global highlighting
      List := Projects.Get_All_Subgraphs (Current_Project);
      Iterator := String_Lists.MakeListIter (List);
      while String_Lists.More (Iterator) loop
         String_Lists.Next (Iterator, Subgraph_Name);
         Subgraph := Get_Subgraph
           (Ada.Strings.Unbounded.To_String (Subgraph_Name));
         Highlight_Status := Projects.Get_Highlight_Status
           (Current_Project, Ada.Strings.Unbounded.To_String (Subgraph_Name));
         if (Highlight_Status /= Projects.None) then
            Graph_Widgets.Add_Global_Highlighting
              (Vis_Windows.Get_Graph_Widget (Window), Subgraph,
               To_Subgraph_Hightlight_ID (Highlight_Status));
         end if;
      end loop;
      String_Lists.Destroy (List);
   end Open_Window;

   function Remove_Window
     (Name                 : in String;
      Ask_For_Confirmation : in Boolean := True)
     return Boolean
   is
   begin
      if (Gui_Manager.Remove_Window (Name, Ask_For_Confirmation)) then
         if (Projects.Is_Vis_Window_Memory_Loaded (Current_Project, Name)) then
            Projects.Free_Memory_For_Vis_Window (Current_Project, Name);
         end if;

         --  Need to check if window still exists in project, in case
         --  it was never written to disk, Free_Memory_For_Vis_Window will
         --  remove it right away
         if (Exists_Window (Name)) then
            Projects.Remove_Visualisation_Window (Current_Project, Name);
         end if;

         return True;
      end if;

      return False;
   end Remove_Window;

   procedure Rename_Window
     (Old_Name : in String;
      New_Name : in String)
   is
      --  make sure the window is loaded
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Old_Name);
   begin
      if (New_Name /= Old_Name) then
         Projects.Change_Vis_Window_Name
           (Current_Project, Old_Name, New_Name);
         Gui_Manager.Rename_Window (Old_Name, New_Name);
      end if;
   end Rename_Window;

   procedure Save_Window
     (Name : in String)
   is
      --  make sure the window is loaded
      Window : Vis_Windows.Visual_Window_Access;
   begin
      if (Projects.Is_Vis_Window_Memory_Loaded (Current_Project, Name)) then
         Window := Projects.Get_Visualisation_Window (Current_Project, Name);
         Projects.Store_Single_Visualisation_Window (Current_Project, Name);
         Logger.Info ("Saved window " & Name);
      end if;
   end Save_Window;

   ---------------------------------------------------------------------------
   --  Zoom
   ---------------------------------------------------------------------------

   procedure Center_On_Node
     (Window_Name : in String;
      Node        : in Graph_Lib.Node_Id)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Widget : Graph_Widgets.Graph_Widget
        := Vis_Windows.Get_Graph_Widget (Window);
      Location : Vis.Logic.Vector_2d;
   begin
      Location := Graph_Widgets.Get_Top_Middle (Widget, Node);
      Graph_Widgets.Set_Location (Widget, Location);
   end Center_On_Node;

   procedure Set_Zoom_Level
     (Window_Name : in     String;
      Zoom_Level  : in out Vis.Zoom_Level)
   is
      use type Vis.Zoom_Level;

      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Widget : Graph_Widgets.Graph_Widget
        := Vis_Windows.Get_Graph_Widget (Window);
   begin
      Logger.Debug ("setting zoom level for " & Window_Name & ": "
                    & Vis.Zoom_Level'Image (Zoom_Level));
      if (Zoom_Level > Graph_Widgets.Get_Maximum_Zoom_Level (Widget)) then
         Zoom_Level := Graph_Widgets.Get_Maximum_Zoom_Level (Widget);
      elsif (Zoom_Level < 0.01) then
         Zoom_Level := 0.01;
      end if;
      Graph_Widgets.Set_Zoom_Level (Widget, Zoom_Level);
      Gui_Manager.Update_Zoom_Level (Window_Name);
   end Set_Zoom_Level;

   procedure Zoom_To_All
     (Window_Name : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      Graph_Widgets.Zoom_To_All (Vis_Windows.Get_Graph_Widget (Window));
      Gui_Manager.Update_Zoom_Level (Window_Name);
   end Zoom_To_All;

   procedure Zoom_To_Edge
     (Window_Name : in String;
      Edge        : in Graph_Lib.Edge_Id)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
   begin
      Graph_Widgets.Zoom_To_Edge (Vis_Windows.Get_Graph_Widget (Window), Edge);
      Gui_Manager.Update_Zoom_Level (Window_Name);
   end Zoom_To_Edge;

   procedure Zoom_To_Selection
     (Window_Name    : in String;
      Selection_Name : in String)
   is
      Window : Vis_Windows.Visual_Window_Access
        := Projects.Get_Visualisation_Window (Current_Project, Window_Name);
      Selection : Graph_Lib.Selections.Selection
        := Get_Selection (Window_Name    => Window_Name,
                          Selection_Name => Selection_Name);
   begin
      Graph_Widgets.Zoom_To_Selection
        (Widget    => Vis_Windows.Get_Graph_Widget (Window),
         Selection => Selection);
      Gui_Manager.Update_Zoom_Level (Window_Name);
   end Zoom_To_Selection;

end Giant.Controller;

