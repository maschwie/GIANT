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
--  $RCSfile: giant-graph_window-callbacks.adb,v $, $Revision: 1.25 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--

with Ada.Unchecked_Conversion;
with System;

with Gdk.Types;
with Glib;

with Giant.Config;
with Giant.Config.Global_Data;
with Giant.Controller;
with Giant.Dialogs;
with Giant.Layout_Dialog;
with Giant.Graph_Lib.Subgraphs;
with Giant.Gui_Manager;
with Giant.Gui_Manager.Actions;
with Giant.Gsl;
with Giant.Gsl.Interpreters;
with Giant.Make_Room_Dialog;
with Giant.Node_Annotation_Dialog;
with Giant.Node_Info_Dialog;
with Giant.Selection_Operation_Dialog;

package body Giant.Graph_Window.Callbacks is

   ---------------------------------------------------------------------------
   --  Background Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Background_Make_Room
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Value : Float;
   begin
      Value := Make_Room_Dialog.Show;
      if (Value > 0.0) then
         Controller.Make_Room (Get_Window_Name (Window),
                               Center => Window.Current_Position,
                               Width  => Value,
                               Height => Value);
      end if;
   end;

   procedure On_Background_Create_Pin
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : constant Graph_Window_Access := Graph_Window_Access (Source);
      Name : constant String
        := Dialogs.Show_Input_Dialog (-"Pin Name");
   begin
      if (Name /= "") then
         Controller.Create_Pin (Get_Window_Name (Window), Name);
      end if;
   end;

   procedure On_Background_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Event.Widget);
      Params : Gsl.Interpreters.Gsl_Params
        := Gsl.Interpreters.Create_Parameter_List;
   begin
      Controller.Execute_GSL
        (Script_Name => Event.Label,
         Context     => Get_Window_Name (Window),
         Parameter   => Params);
   end On_Background_Script;

   procedure On_Background_Select_All
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Selection : Graph_Lib.Selections.Selection
        := Controller.Get_Current_Selection (Get_Window_Name (Window));
      Subgraph : Graph_Lib.Subgraphs.Subgraph
        := Graph_Widgets.Get_Content (Window.Graph);
   begin
      Graph_Lib.Selections.Add_Node_Set
        (Selection, Graph_Lib.Subgraphs.Get_All_Nodes (Subgraph));
      Graph_Lib.Selections.Add_Edge_Set
        (Selection, Graph_Lib.Subgraphs.Get_All_Edges (Subgraph));

      Graph_Widgets.Add_Local_Highlighting
        (Widget    => Window.Graph,
         Selection => Selection,
         Color     => Config.Global_Data.Current_Selection);
      Update_Selection (Window, Graph_Lib.Selections.Get_Name (Selection));
   end;

   procedure On_Background_Select_Nothing
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Selection : Graph_Lib.Selections.Selection
        := Controller.Get_Current_Selection (Get_Window_Name (Window));
   begin
      Graph_Widgets.Remove_Local_Highlighting
        (Widget    => Window.Graph,
         Selection => Selection,
         Color     => Config.Global_Data.Current_Selection);
      Graph_Lib.Selections.Clear (Selection);
      Update_Selection (Window, Graph_Lib.Selections.Get_Name (Selection));
   end;

   ---------------------------------------------------------------------------
   --  Edge Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Edge_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Event.Widget);
      Params : Gsl.Interpreters.Gsl_Params
        := Gsl.Interpreters.Create_Parameter_List;
   begin
      pragma Assert (Graph_Lib."/=" (Window.Current_Edge, null));
      Gsl.Interpreters.Add_Parameter (Params, Window.Current_Edge);

      Controller.Execute_GSL
        (Script_Name => Event.Label,
         Context     => Get_Window_Name (Window),
         Parameter   => Params);
   end On_Edge_Script;

   procedure On_Edge_Show_Source
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Node : Graph_Lib.Node_Id;
   begin
      pragma Assert (Graph_Lib."/=" (Window.Current_Edge, null));
      Node := Graph_Lib.Get_Source_Node (Window.Current_Edge);
      Controller.Center_On_Node (Get_Window_Name (Window), Node);
   end;

   procedure On_Edge_Show_Target
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Node : Graph_Lib.Node_Id;
   begin
      pragma Assert (Graph_Lib."/=" (Window.Current_Edge, null));
      Node := Graph_Lib.Get_Target_Node (Window.Current_Edge);
      Controller.Center_On_Node (Get_Window_Name (Window), Node);
   end;


   procedure On_Edge_Zoom
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      pragma Assert (Graph_Lib."/=" (Window.Current_Edge, null));
      Controller.Zoom_To_Edge (Get_Window_Name (Window), Window.Current_Edge);
   end;

   ---------------------------------------------------------------------------
   --  Graph Widget Callbacks
   ---------------------------------------------------------------------------

   procedure On_Action_Mode_Button_Pressed
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Button_Press_Action)
   is
      use type Glib.Guint;
      use type Gdk.Event.Gdk_Event_Type;
      use type Actions.Graph_Window_Action_Access;

      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      if Gdk.Event.Get_Button (Event.Event) = 1 then
         if (Gui_Manager.Actions.Is_Action_Pending) then
            Gui_Manager.Actions.Trigger (Window, Event);
         elsif (Graph_Window.Is_Local_Action_Pending (Window)) then
            Trigger_Local_Action (Window, Event);
         end if;
      elsif Gdk.Event.Get_Button (Event.Event) = 3 then
         if (Gui_Manager.Actions.Is_Action_Pending) then
            Gui_Manager.Actions.Cancel;
         elsif (Graph_Window.Is_Local_Action_Pending (Window)) then
            Cancel_Local_Action (Window);
         end if;
      end if;
   end On_Action_Mode_Button_Pressed;

   procedure On_Background_Popup
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Button_Press_Action)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      pragma Assert
        (Graph_Widgets.Handlers."=" (Event.Pressed_On,
                                     Graph_Widgets.Handlers.On_Background));
      Window.Current_Position := Event.Location;
      Gtk.Menu.Show_All (Window.Background_Menu);
      Gtk.Menu.Popup (Window.Background_Menu,
                      Button => Gdk.Event.Get_Button (Event.Event),
                      Activate_Time => Gdk.Event.Get_Time (Event.Event));
   end On_Background_Popup;

   procedure On_Edge_Popup
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Button_Press_Action)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      pragma Assert
        (Graph_Widgets.Handlers."=" (Event.Pressed_On,
                                     Graph_Widgets.Handlers.On_Edge));
      Window.Current_Edge := Event.Edge;
      Gtk.Menu.Show_All (Window.Edge_Menu);
      Gtk.Menu.Popup (Window.Edge_Menu,
                      Button => Gdk.Event.Get_Button (Event.Event),
                      Activate_Time => Gdk.Event.Get_Time (Event.Event));
   end On_Edge_Popup;

   procedure On_Node_Popup
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Button_Press_Action)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      pragma Assert
        (Graph_Widgets.Handlers."=" (Event.Pressed_On,
                                     Graph_Widgets.Handlers.On_Node));
      Window.Current_Node := Event.Node;
      Gtk.Menu.Show_All (Window.Node_Menu);
      Gtk.Menu.Popup (Window.Node_Menu,
                      Button => Gdk.Event.Get_Button (Event.Event),
                      Activate_Time => Gdk.Event.Get_Time (Event.Event));
   end On_Node_Popup;

   procedure On_Selection_Changed
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Graph_Widgets.Handlers.Selection_Change_Action)
   is
      Action : Graph_Widgets.Selection_Change_Type := Event.Action;
      Difference : Graph_Lib.Selections.Selection := Event.Difference;
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Selection : Graph_Lib.Selections.Selection
        := Controller.Get_Current_Selection (Get_Window_Name (Window));
   begin
      case (Action) is
         when Graph_Widgets.Insert =>
            Graph_Lib.Selections.Add_Node_Set
              (Selection, Graph_Lib.Selections.Get_All_Nodes (Difference));
            Graph_Lib.Selections.Add_Edge_Set
              (Selection, Graph_Lib.Selections.Get_All_Edges (Difference));
         when Giant.Graph_Widgets.Remove =>
            Graph_Lib.Selections.Remove_Node_Set
              (Selection, Graph_Lib.Selections.Get_All_Nodes (Difference));
            Graph_Lib.Selections.Remove_Edge_Set
              (Selection, Graph_Lib.Selections.Get_All_Edges (Difference));
         when Giant.Graph_Widgets.Change =>
            Graph_Lib.Selections.Clear (Selection);
            Graph_Lib.Selections.Add_Node_Set
              (Selection, Graph_Lib.Selections.Get_All_Nodes (Difference));
            Graph_Lib.Selections.Add_Edge_Set
              (Selection, Graph_Lib.Selections.Get_All_Edges (Difference));
         when Giant.Graph_Widgets.Clear =>
            Graph_Lib.Selections.Clear (Selection);
      end case;
      Update_Selection (Window, Graph_Lib.Selections.Get_Name (Selection));
   end On_Selection_Changed;

   ---------------------------------------------------------------------------
   --  Node Menu Callbacks
   ---------------------------------------------------------------------------

   procedure On_Node_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Event.Widget);
      Params : Gsl.Interpreters.Gsl_Params
        := Gsl.Interpreters.Create_Parameter_List;
   begin
      pragma Assert (Graph_Lib."/=" (Window.Current_Node, null));
      Gsl.Interpreters.Add_Parameter (Params, Window.Current_Node);

      Controller.Execute_GSL
        (Script_Name => Event.Label,
         Context     => Get_Window_Name (Window),
         Parameter   => Params);
   end On_Node_Script;

   procedure On_Node_Show_Info
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      pragma Assert (Graph_Lib."/=" (Window.Current_Node, null));
      Node_Info_Dialog.Show (Window.Current_Node);
   end;

   procedure On_Node_Show_Source
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      if (not Controller.Show_Source (Window.Current_Node)) then
         Controller.Show_Error (-"Node has no source information.");
      end if;
   end;

   procedure On_Node_Annotate
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      Node_Annotation_Dialog.Show (Window.Current_Node);
   end;

   ---------------------------------------------------------------------------
   --  Selection Callbacks
   ---------------------------------------------------------------------------

   procedure On_Apply_Layout
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      Layout_Dialog.Show (Get_Window_Name (Window),
                          Get_Selected_Selection (Window));
   end On_Apply_Layout;

   procedure On_Selection_List_Set_Operation
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
      Source_Name : String := Get_Selected_Selection (Window);
      Dialog : Selection_Operation_Dialog.Selection_Operation_Dialog_Access;
   begin
      Selection_Operation_Dialog.Create (Dialog, Get_Window_Name (Window));
      Selection_Operation_Dialog.Show_All (Dialog);
   end On_Selection_List_Set_Operation;

   procedure On_Selection_Script
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Menu_Factory.Script_Event)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Event.Widget);
      Params : Gsl.Interpreters.Gsl_Params
        := Gsl.Interpreters.Create_Parameter_List;
   begin
      Gsl.Interpreters.Add_Parameter
        (List    => Params,
         Param   => Get_Selected_Selection (Window),
         Context => Get_Window_Name (Window));

      Controller.Execute_GSL
        (Script_Name => Event.Label,
         Context     => Get_Window_Name (Window),
         Parameter   => Params);
   end On_Selection_Script;

   procedure On_Selection_Zoom_To
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Window : Graph_Window_Access := Graph_Window_Access (Source);
   begin
      Controller.Zoom_To_Selection
        (Window_Name    => Get_Window_Name (Window),
         Selection_Name => Get_Selected_Selection (Window));
   end On_Selection_Zoom_To;

end Giant.Graph_Window.Callbacks;
