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
--  $RCSfile: giant-graph_widgets-callbacks.ads,v $, $Revision: 1.11 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with Gdk.Event;
with Gtk.Adjustment;
with Gtk.Widget;
pragma Elaborate_All (Gtk.Widget);

package Giant.Graph_Widgets.Callbacks is

   ----------------------------------------------------------------------------
   --  Connects all Callbacks 'Widget' needs
   procedure Connect_All_Callbacks
     (Widget : access Graph_Widget_Record'Class);

   procedure Update_Scrollbars
     (Widget : access Graph_Widget_Record'Class);

   ----------------------------------------------------------------------------
   --  Updates a user action so it respects newly appeared edges. This must
   --  be called when new edges are shown on the graph widget, e.g. when
   --  nodes come into view.
   procedure Edges_Appeared
     (Widget : access Graph_Widget_Record'Class;
      Edges  : in     Vis_Data.Vis_Edge_Sets.Set);

   ----------------------------------------------------------------------------
   --  Updates the position of the mouse pointer. This must be called whenever
   --  the position of the mouse pointer changes relative to the graph, e.g.
   --  when the widget scrolls.
   procedure Mouse_Pointer_Moved_To
     (Widget          : access Graph_Widget_Record'Class;
      Motion_Position : in     Vis.Absolute.Vector_2d);

private

   procedure On_Horizontal_Scroll
     (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Widget     : in     Graph_Widget);

   procedure On_Vertical_Scroll
     (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Widget     : in     Graph_Widget);

   procedure On_Realize
     (Widget : access Graph_Widget_Record'Class);

   procedure After_Realize
     (Widget : access Graph_Widget_Record'Class);

   procedure On_Unrealize
     (Widget : access Graph_Widget_Record'Class);

   procedure On_Destroy
     (Widget : access Graph_Widget_Record'Class);

   procedure On_Set_Scroll_Adjustments
     (Widget     : access Graph_Widget_Record'Class;
      Horizontal : in     Gtk.Adjustment.Gtk_Adjustment;
      Vertical   : in     Gtk.Adjustment.Gtk_Adjustment);

   procedure On_Size_Request
     (Widget      : access Graph_Widget_Record'Class;
      Requisition : in     Gtk.Widget.Gtk_Requisition_Access);

   procedure On_Size_Allocate
     (Widget     : access Graph_Widget_Record'Class;
      Allocation : in     Gtk.Widget.Gtk_Allocation_Access);

   function On_Expose_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Expose)
     return Boolean;

   function On_Button_Press_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button)
     return Boolean;

   function On_Button_Release_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button)
     return Boolean;

   function On_Motion_Notify_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Motion)
     return Boolean;

   function On_Enter_Notify_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Crossing)
     return Boolean;

   function On_Leave_Notify_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Crossing)
     return Boolean;

   function On_Auto_Scroll_Timeout
     (Widget : in     Graph_Widget)
     return Boolean;

   Left_Button   : constant := 1;
   Middle_Button : constant := 2;
   Right_Button  : constant := 3;

   --  Points the mouse cursor must be moved to activate a drag
   Default_Click_Distance_Tolerance : constant := 3;
   --  Milliseconds between two auto scroll steps
   Default_Auto_Scroll_Delay        : constant := 100;
   --  Points the mouse cursor must be moved to reach the next speed level
   Default_Auto_Scroll_Sensitivity  : constant := 3;
   --  Points the move offset is increased in every acceleration level
   Default_Auto_Scroll_Acceleration : constant := 10;

end Giant.Graph_Widgets.Callbacks;
