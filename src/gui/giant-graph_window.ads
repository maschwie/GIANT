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
--  $RCSfile: giant-graph_window.ads,v $, $Revision: 1.29 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:26 $
--
------------------------------------------------------------------------------
--
--  Provides the graph window.
--

with Gdk.Event;
with Gtk.Clist;
with Gtk.Combo;
with Gtk.Gentry;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Paned;
with Gtk.Scrolled_Window;
with Gtk.Style;
with Gtk.Widget;
with Gtk.Window;

with Giant.Graph_Lib;
with Giant.Graph_Widgets;
with Giant.Graph_Widgets.Handlers;
with Giant.Gui_Utils;
with Giant.Mini_Maps;
with Giant.Vis;
with Giant.Vis_Windows;

package Giant.Graph_Window is

   type Graph_Window_Record is
     new Gtk.Window.Gtk_Window_Record with private;

   type Graph_Window_Access is access all Graph_Window_Record'Class;

   DEFAULT_ZOOM_STEP : constant Vis.Zoom_Level := 0.2;

   ---------------------------------------------------------------------------
   --  Package: Actions
   ---------------------------------------------------------------------------

   package Actions is

      type Graph_Window_Action_Type is abstract tagged private;

      type Graph_Window_Action_Access is
        access all Graph_Window_Action_Type'Class;

      ----------------------------------------------------------------------
      --  Invoked another action has been set or when the Graph_Widget
      --  emits the Action_Mode_Button_Press_Event signal and the
      --  right mouse button was pressed.
      --
      --  After this method was invoked, Destroy will be invoked.
      procedure Cancel
        (Action : access Graph_Window_Action_Type)
         is abstract;

      ------------------------------------------------------------------------
      --  Destroys Action.
      procedure Destroy
        (Action : access Graph_Window_Action_Type);

      ------------------------------------------------------------------------
      --  Invoked when the Graph_Widget emits the
      --  Action_Mode_Button_Press_Event signal and the left mouse
      --  button was pressed.
      --
      --  After this method was invoked and True returned, Destroy
      --  will be invoked.
      --
      --  Returns:
      --    If True, the action mode will be cancelled and Destory
      --    invoked.
      function Execute
        (Action   : access Graph_Window_Action_Type;
         Window   : access Graph_Window.Graph_Window_Record'Class;
         Event    : in     Graph_Widgets.Handlers.Button_Press_Action)
        return Boolean
         is abstract;

   private

      type Graph_Window_Action_Type is abstract tagged null record;

   end Actions;

   function Close
     (Window               : access Graph_Window_Record;
      Ask_For_Confirmation : in     Boolean)
     return Boolean;

   procedure Create
     (Window        :    out Graph_Window_Access;
      Visual_Window : in     Vis_Windows.Visual_Window_Access);

   procedure Initialize
     (Window : access Graph_Window_Record'Class);

   procedure Post_Initialize
     (Window : access Graph_Window_Record);

   ---------------------------------------------------------------------------
   --  Returns the edge that was selected by the last action mode
   --  event or edge popup.
   --
   --  Returns:
   --    Null, if no edge was selected.
   --  See:
   --    Get_Current_Node
   function Get_Current_Edge
     (Window : access Graph_Window_Record)
     return Graph_Lib.Edge_Id;

   ---------------------------------------------------------------------------
   --  Returns the node that was selected by the last action mode
   --  event or node popup.
   --
   --  Returns:
   --    Null, if no node was selected.
   --  See:
   --    Get_Current_Edge
   function Get_Current_Node
     (Window : access Graph_Window_Record)
     return Graph_Lib.Node_Id;

   function Get_Vis_Window
     (Window : access Graph_Window_Record)
     return Vis_Windows.Visual_Window_Access;

   ---------------------------------------------------------------------------
   --  Deletes the graph from the screen but does not destroy it.
   --
   --  See:
   --    Gui_Manager.Close
   procedure Remove_Graph_Widget
     (Window : access Graph_Window_Record);

   procedure Update_Title
     (Window : access Graph_Window_Record);

   procedure Set_Global_Action_Mode
     (Window : access Graph_Window_Record;
      Enable : in     Boolean);

   ---------------------------------------------------------------------------
   --  Local Action
   ---------------------------------------------------------------------------

   function Is_Local_Action_Pending
     (Window : access Graph_Window_Record)
     return Boolean;

   procedure Cancel_Local_Action
     (Window : access Graph_Window_Record);

   procedure Set_Local_Action
     (Window : access Graph_Window_Record;
      Action : access Graph_Window.Actions.Graph_Window_Action_Type'Class);

   procedure Trigger_Local_Action
     (Window   : access Graph_Window.Graph_Window_Record;
      Event    : in     Graph_Widgets.Handlers.Button_Press_Action);

   ---------------------------------------------------------------------------
   --  Node Annotations
   ---------------------------------------------------------------------------

   procedure Update_Node_Annotation
     (Window : access Graph_Window_Record;
      Node   : in     Graph_Lib.Node_Id);

   ---------------------------------------------------------------------------
   --  Pin Methods
   ---------------------------------------------------------------------------

   procedure Add_Pin
     (Window : access Graph_Window_Record;
      Name   : in     String);

   function Get_Selected_Pin
     (Window : access Graph_Window_Record)
     return String;

   procedure Update_Pin
     (Window : access Graph_Window_Record;
      Name   : in     String);

   procedure Remove_Pin
     (Window : access Graph_Window_Record;
      Name   : in     String);

   ---------------------------------------------------------------------------
   --  Selection Methods
   ---------------------------------------------------------------------------

   procedure Add_Selection
     (Window : access Graph_Window_Record;
      Name   : in     String);

   function Get_Selected_Selection
     (Window : access Graph_Window_Record)
     return String;

   procedure Update_Selection
     (Window : access Graph_Window_Record;
      Name   : in     String);

   procedure Remove_Selection
     (Window : access Graph_Window_Record;
      Name   : in     String);

   ---------------------------------------------------------------------------
   --  Vis Styles
   ---------------------------------------------------------------------------

   procedure Add_Vis_Style
     (Window : access Graph_Window_Record;
      Name   : in     String);

   procedure Update_Vis_Style
     (Window     : access Graph_Window_Record);

   ---------------------------------------------------------------------------
   --  Zoom
   ---------------------------------------------------------------------------

   procedure Update_Zoom_Level
     (Window     : access Graph_Window_Record);

private

   function Get_Window_Name
     (Source : access Gtk.Widget.Gtk_Widget_Record'Class)
     return String;

   type Style_Array_Type is array (Vis_Windows.Selection_Highlight_Status)
     of Gtk.Style.Gtk_Style;

   type Graph_Window_Record is
     new Gtk.Window.Gtk_Window_Record with record
        Split_Pane : Gtk.Paned.Gtk_Hpaned;
        Pin_List : Gui_Utils.String_Clists.Giant_Data_Clist;
        Pin_List_Menu : Gtk.Menu.Gtk_Menu;
        Selection_List : Gui_Utils.String_Clists.Giant_Data_Clist;
        Selection_List_Menu : Gtk.Menu.Gtk_Menu;
        Vis_Style_Combo : Gtk.Combo.Gtk_Combo;
        Zoom_Combo : Gtk.Combo.Gtk_Combo;

        Graph : Graph_Widgets.Graph_Widget;
        Graph_Scrolled_Window : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
        Mini_Map : Mini_Maps.Mini_Map;
        Background_Menu : Gtk.Menu.Gtk_Menu;
        Edge_Menu : Gtk.Menu.Gtk_Menu;
        Node_Menu : Gtk.Menu.Gtk_Menu;

        Is_Modified : Boolean := True;

        --  the action that is currently pending
        Local_Action : Actions.Graph_Window_Action_Access
          := null;

        --  the data record from projects
        Visual_Window : Vis_Windows.Visual_Window_Access;

        --  the selection highlight color cell styles
        Styles : Style_Array_Type := (others => null);

        --  the currently selected node for menu callbacks
        Current_Edge : Graph_Lib.Edge_Id;
        Current_Node : Graph_Lib.Node_Id;
        Current_Position : Vis.Logic.Vector_2d;
     end record;

end Giant.Graph_Window;
