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
--  First Author: Steffen Keul
--
--  $RCSfile: giant-graph_widgets-notifications.adb,v $, $Revision: 1.8 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Giant.Graph_Widgets.Handlers;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Graph_Widgets.Notifications is

   package Notification_Logger is new Logger
     (Name => "Giant.Graph_Widgets.Notifications");

   procedure Background_Popup
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d) is
   begin
      Notification_Logger.Debug
        ("Background_Popup at Location " & Vis.Logic.Image (Location));
      Handlers.Emit_Background_Popup_Event (Widget, Event, Location);
   end Background_Popup;

   procedure Edge_Popup
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Edge     : in     Vis_Data.Vis_Edge_Id) is

      Graph_Edge : Graph_Lib.Edge_Id := Vis_Data.Get_Graph_Edge (Edge);
   begin
      Notification_Logger.Debug
        ("Edge_Popup node " &
         Graph_Lib.Node_Id_Image (Graph_Lib.Get_Source_Node (Graph_Edge))  &
         "'s edge " & Graph_Lib.Get_Edge_Tag (Graph_Edge));
      Handlers.Emit_Edge_Popup_Event (Widget, Event, Location, Graph_Edge);
   end Edge_Popup;

   procedure Node_Popup
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Node     : in     Vis_Data.Vis_Node_Id) is

      Graph_Node : Graph_Lib.Node_Id := Vis_Data.Get_Graph_Node (Node);
   begin
      Notification_Logger.Debug
        ("Node_Popup on " & Graph_Lib.Node_Id_Image (Graph_Node));
      Handlers.Emit_Node_Popup_Event (Widget, Event, Location, Graph_Node);
   end Node_Popup;

   procedure Selection_Changed
     (Widget     : access Graph_Widget_Record'Class;
      Action     : in     Selection_Change_Type;
      Difference : in     Graph_Lib.Selections.Selection) is
   begin
      --  Notification_Logger.Debug
      --    ("User has performed the command " &
      --     Selection_Change_Type'Image (Action) & " on the current " &
      --     "selection. emitting " & Handlers.Selection_Change_Signal);
      Handlers.Emit_Selection_Change_Signal
        (Widget     => Widget,
         Action     => Action,
         Difference => Difference);
   end Selection_Changed;

   procedure Action_Mode_Button_Press_Event_Background
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d) is
   begin
      --  Notification_Logger.Debug
      --    ("User has pressed a mouse button on the background during " &
      --     "action mode. Location = " & Vis.Logic.Image (Location));
      Handlers.Emit_Action_Mode_Button_Press_Event_Background
        (Widget, Event, Location);
   end Action_Mode_Button_Press_Event_Background;

   procedure Action_Mode_Button_Press_Event_Edge
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Edge     : in     Vis_Data.Vis_Edge_Id) is
   begin
      --  Notification_Logger.Debug
      --    ("User has pressed a mouse button on an edge during action " &
      --     "mode Location = " & Vis.Logic.Image (Location));
      Handlers.Emit_Action_Mode_Button_Press_Event_Edge
        (Widget, Event, Location, Vis_Data.Get_Graph_Edge (Edge));
   end Action_Mode_Button_Press_Event_Edge;

   procedure Action_Mode_Button_Press_Event_Node
     (Widget   : access Graph_Widget_Record'Class;
      Event    : in     Gdk.Event.Gdk_Event_Button;
      Location : in     Vis.Logic.Vector_2d;
      Node     : in     Vis_Data.Vis_Node_Id) is
   begin
      --  Notification_Logger.Debug
      --    ("User has pressed a mouse button during action mode." &
      --     " Location = " & Vis.Logic.Image (Location));
      Handlers.Emit_Action_Mode_Button_Press_Event_Node
        (Widget, Event, Location, Vis_Data.Get_Graph_Node (Node));
   end Action_Mode_Button_Press_Event_Node;

   procedure Logical_Area_Changed
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d) is
   begin
      --  Notification_Logger.Debug
      --    ("Logical area has changed. Emitting "
      --     & Handlers.Logical_Area_Changed_Signal);
      Handlers.Emit_Logical_Area_Changed (Widget, Area);
   end Logical_Area_Changed;

   procedure Visible_Area_Changed
     (Widget : access Graph_Widget_Record'Class;
      Area   : in     Vis.Logic.Rectangle_2d) is
   begin
      --  Notification_Logger.Debug
      --    ("Visible area has changed. Emitting "
      --     & Handlers.Visible_Area_Changed_Signal);
      Handlers.Emit_Visible_Area_Changed (Widget, Area);
   end Visible_Area_Changed;


end Giant.Graph_Widgets.Notifications;
