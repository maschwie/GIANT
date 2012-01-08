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
--  $RCSfile: giant-graph_widgets-callbacks.adb,v $, $Revision: 1.26 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with System;

with Gdk.Event;
with Gdk.Main;
with Gdk.Rectangle;
with Gdk.Threads;
with Gdk.Window;
with Gdk.Window_Attr;
with Glib.Object;
with Gtk.Arguments;
with Gtk.Enums;
with Gtk.Style;

with Giant.Graph_Widgets.Drawing;
with Giant.Graph_Widgets.Handlers;
with Giant.Graph_Widgets.Notifications;
with Giant.Graph_Widgets.Positioning;
with Giant.Graph_Widgets.Settings;
with Giant.Graph_Widgets.States;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Graph_Widgets.Callbacks is


   package Callbacks_Logger is new Logger
     (Name => "Giant.Graph_Widgets.Callbacks");

   package Graph_Widget_Timeouts is new Gtk.Main.Timeout
     (Data_Type => Graph_Widget);

   function "or"
     (Left  : in Gdk.Event.Gdk_Event_Mask;
      Right : in Gdk.Event.Gdk_Event_Mask)
     return Gdk.Event.Gdk_Event_Mask
     renames Gdk.Event."or";

   Handled_Event_Mask : constant Gdk.Event.Gdk_Event_Mask  :=
     Gdk.Event.Button_Press_Mask or
     Gdk.Event.Button_Release_Mask or
     Gdk.Event.Pointer_Motion_Mask or
     Gdk.Event.Enter_Notify_Mask or
     Gdk.Event.Leave_Notify_Mask;


   ----------------
   -- Scrollbars --
   ----------------

   procedure Update_Scrollbars
     (Widget : access Graph_Widget_Record'Class) is

      Logic   : Vis.Logic.Rectangle_2d;
      Visible : Vis.Logic.Rectangle_2d;
      Area    : Vis.Logic.Rectangle_2d;
      Horizontal_Value : Glib.Gdouble;
      Vertical_Value   : Glib.Gdouble;
      use Vis.Logic;
      package Adj renames Gtk.Adjustment;
   begin
      if Widget.Callbacks.Accept_Scrolling /= 0 then
         return;
      end if;
      Widget.Callbacks.Accept_Scrolling :=
        Widget.Callbacks.Accept_Scrolling + 1;
      Logic := Get_Logical_Area (Widget);
      Visible := Get_Visible_Area (Widget);
      Area := Get_Surrounding (Logic, Visible);

      Horizontal_Value := Glib.Gdouble (Get_Left (Visible));
      Vertical_Value := Glib.Gdouble (Get_Top (Visible));

      if Adj."/="
        (Widget.Callbacks.Horizontal_Adjustment, Adj.Null_Adjustment) then

         Adj.Set_Lower
           (Widget.Callbacks.Horizontal_Adjustment,
            Glib.Gdouble (Get_Left (Area)));
         Adj.Set_Upper
           (Widget.Callbacks.Horizontal_Adjustment,
            Glib.Gdouble (Get_Right (Area)));
         Adj.Set_Page_Size
           (Widget.Callbacks.Horizontal_Adjustment,
            Glib.Gdouble (Get_Width (Visible)));
         Adj.Set_Page_Increment
           (Widget.Callbacks.Horizontal_Adjustment,
            Glib.Gdouble (0.9 * Get_Width (Visible)));
         Adj.Set_Step_Increment
           (Widget.Callbacks.Horizontal_Adjustment,
            Glib.Gdouble (0.05 * Get_Width (Visible)));
         Adj.Changed (Widget.Callbacks.Horizontal_Adjustment);
         Adj.Set_Value
           (Widget.Callbacks.Horizontal_Adjustment,
            Glib.Gdouble (Get_Left (Visible)));
      end if;
      if Adj."/="
        (Widget.Callbacks.Vertical_Adjustment, Adj.Null_Adjustment) then

         Adj.Set_Lower
           (Widget.Callbacks.Vertical_Adjustment,
            Glib.Gdouble (Get_Top (Area)));
         Adj.Set_Upper
           (Widget.Callbacks.Vertical_Adjustment,
            Glib.Gdouble (Get_Bottom (Area)));
         Adj.Set_Page_Size
           (Widget.Callbacks.Vertical_Adjustment,
            Glib.Gdouble (Get_Height (Visible)));
         Adj.Set_Page_Increment
           (Widget.Callbacks.Vertical_Adjustment,
            Glib.Gdouble (0.9 * Get_Height (Visible)));
         Adj.Set_Step_Increment
           (Widget.Callbacks.Vertical_Adjustment,
            Glib.Gdouble (0.05 * Get_Height (Visible)));
         Adj.Changed (Widget.Callbacks.Vertical_Adjustment);
         Adj.Set_Value
           (Widget.Callbacks.Vertical_Adjustment,
            Glib.Gdouble (Get_Top (Visible)));
      end if;
      Widget.Callbacks.Accept_Scrolling :=
        Widget.Callbacks.Accept_Scrolling - 1;
   end Update_Scrollbars;

   procedure On_Horizontal_Scroll
     (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Widget     : in     Graph_Widget) is

      Current : Vis.Logic.Vector_2d;
      Left    : Vis.Logic_Float;
      New_X   : Vis.Logic_Float;
   begin
      Widget.Callbacks.Accept_Scrolling :=
        Widget.Callbacks.Accept_Scrolling + 1;
      if Widget.Callbacks.Accept_Scrolling = 1 then
         Current := Get_Location (Widget);
         Left := Vis.Logic_Float (Gtk.Adjustment.Get_Value (Adjustment));
         New_X := Left + Vis.Logic.Get_Width (Get_Visible_Area (Widget)) / 2.0;
         Vis.Logic.Set_X (Current, New_X);
         Set_Location (Widget, Current);
      end if;
      Gtk.Handlers.Emit_Stop_By_Name
        (Object => Widget.Callbacks.Horizontal_Adjustment,
         Name   => "value_changed");
      Widget.Callbacks.Accept_Scrolling :=
        Widget.Callbacks.Accept_Scrolling - 1;
   end On_Horizontal_Scroll;

   procedure On_Vertical_Scroll
     (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Widget     : in     Graph_Widget) is

      Current : Vis.Logic.Vector_2d;
      Top     : Vis.Logic_Float;
      New_Y   : Vis.Logic_Float;
   begin
      Widget.Callbacks.Accept_Scrolling :=
        Widget.Callbacks.Accept_Scrolling + 1;
      if Widget.Callbacks.Accept_Scrolling = 1 then
         Current := Get_Location (Widget);
         Top := Vis.Logic_Float (Gtk.Adjustment.Get_Value (Adjustment));
         New_Y := Top + Vis.Logic.Get_Height (Get_Visible_Area (Widget)) / 2.0;
         Vis.Logic.Set_Y (Current, New_Y);
         Set_Location (Widget, Current);
      end if;
      Gtk.Handlers.Emit_Stop_By_Name
        (Object => Widget.Callbacks.Vertical_Adjustment,
         Name   => "value_changed");
      Widget.Callbacks.Accept_Scrolling :=
        Widget.Callbacks.Accept_Scrolling - 1;
   end On_Vertical_Scroll;


   --------------------
   -- Mouse handling --
   --------------------

   procedure Grab_Mouse
     (Widget : access Graph_Widget_Record'Class;
      Time   : in     Glib.Guint32) is

      Status : Gdk.Main.Gdk_Grab_Status;
   begin
      Widget.Callbacks.Mouse_Grab_Count :=
        Widget.Callbacks.Mouse_Grab_Count + 1;
      pragma Assert (Widget.Callbacks.Mouse_Grab_Count = 1);
      if Widget.Callbacks.Mouse_Grab_Count = 1 then
         Status := Gdk.Main.Pointer_Grab
           (Window       => Get_Window (Widget),
            Owner_Events => False,
            Event_Mask   => Handled_Event_Mask,
            Confine_To   => Gdk.Window.Null_Window,
            Cursor       => Gdk.Cursor.Null_Cursor,
            Time         => Time);
         pragma Assert (Gdk.Main."=" (Status, Gdk.Main.Grab_Success));

         --  start auto scrolling
         States.Begin_Auto_Scrolling (Widget);
      end if;
   end Grab_Mouse;

   procedure Ungrab_Mouse
     (Widget : access Graph_Widget_Record'Class;
      Time   : in     Glib.Guint32) is
   begin
      Widget.Callbacks.Mouse_Grab_Count :=
        Widget.Callbacks.Mouse_Grab_Count - 1;
      if Widget.Callbacks.Mouse_Grab_Count = 0 then
         States.End_Auto_Scrolling (Widget);
         Gdk.Main.Pointer_Ungrab (Time);
      end if;
   end Ungrab_Mouse;

   function Interpret_Modifiers
     (Modifiers : in     Gdk.Types.Gdk_Modifier_Type)
     return Selection_Modify_Type is

      use type Gdk.Types.Gdk_Modifier_Type;
   begin
      if (Modifiers and Gdk.Types.Shift_Mask) /= 0 then
         return Toggle;
      elsif (Modifiers and Gdk.Types.Control_Mask) /= 0 then
         return Add;
      else
         return Change;
      end if;
   end Interpret_Modifiers;

   --  Parameters:
   --    Restrict_Change - If set to True then 'Mode = Change' will be ignored
   --                      if performed on an edge or a node that is
   --                      already selected.
   procedure Process_Mouse_Click
     (Widget          : access Graph_Widget_Record'Class;
      Mode            : in     Selection_Modify_Type;
      Restrict_Change : in     Boolean := False) is

      Edge     : Vis_Data.Vis_Edge_Id := States.Get_Click_Edge (Widget);
      Node     : Vis_Data.Vis_Node_Id := States.Get_Click_Node (Widget);
      Selected : Boolean;
   begin
      if Vis_Data."/=" (Node, null) then
         Selected := Is_Node_In_Selection (Widget, Node);
         if Mode /= Change or else (not Restrict_Change or not Selected) then
            if Mode = Change or Mode = Add or not Selected then
               Raise_Node (Widget, Node);
            end if;
            Modify_Selection_With_Node_And_Notify (Widget, Node, Mode);
         end if;
      elsif Vis_Data."/=" (Edge, null) then
         Selected := Is_Edge_In_Selection (Widget, Edge);
         if Mode /= Change or else (not Restrict_Change or not Selected) then
            if Mode = Change or Mode = Add or not Selected then
               Raise_Edge (Widget, Edge);
            end if;
            Modify_Selection_With_Edge_And_Notify (Widget, Edge, Mode);
         end if;
      else
         if Mode = Change then
            Clear_Selection_And_Notify (Widget);
         end if;
      end if;
   end Process_Mouse_Click;

   procedure Start_Rectangle
     (Widget : access Graph_Widget_Record'Class;
      Mode   : in     Selection_Modify_Type) is

      Edge_Iterator : Vis_Edge_Sets.Iterator;
      Edge          : Vis_Data.Vis_Edge_Id;
      Node_Iterator : Vis_Node_Sets.Iterator;
      Node          : Vis_Data.Vis_Node_Id;
   begin
      States.Begin_Rectangle (Widget);
      States.Changed_Temporary (Widget);
      Redraw (Widget);

      Widget.Callbacks.Previous_Selected_Edges := Vis_Edge_Lists.Create;
      Edge_Iterator := Vis_Edge_Sets.Make_Iterator
        (Get_Selected_Edges (Widget));
      while Vis_Edge_Sets.More (Edge_Iterator) loop
         Vis_Edge_Sets.Next (Edge_Iterator, Edge);
         Vis_Edge_Lists.Attach
           (Edge, Widget.Callbacks.Previous_Selected_Edges);
      end loop;
      Vis_Edge_Sets.Destroy (Edge_Iterator);

      Widget.Callbacks.Previous_Selected_Nodes := Vis_Node_Lists.Create;
      Node_Iterator := Vis_Node_Sets.Make_Iterator
        (Get_Selected_Nodes (Widget));
      while Vis_Node_Sets.More (Node_Iterator) loop
         Vis_Node_Sets.Next (Node_Iterator, Node);
         Vis_Node_Lists.Attach
           (Node, Widget.Callbacks.Previous_Selected_Nodes);
      end loop;
      Vis_Node_Sets.Destroy (Node_Iterator);
   end Start_Rectangle;

   procedure Cancel_Click
     (Widget : access Graph_Widget_Record'Class) is
   begin
      States.End_Click (Widget);
   end Cancel_Click;

   procedure Begin_Mouse_Move_Action
     (Widget : access Graph_Widget_Record'Class;
      Mode   : in     Selection_Modify_Type) is

      Iterator : Vis_Node_Sets.Iterator;
      Node     : Vis_Data.Vis_Node_Id;
   begin
      Process_Mouse_Click
        (Widget          => Widget,
         Mode            => Mode,
         Restrict_Change => True);

      if Vis_Data."/=" (States.Get_Click_Node (Widget), null) then
         --  Drag
         States.Begin_Drag (Widget);
         States.Changed_Temporary (Widget);
         --  Remove moved nodes from region manager
         States.Changed_Visual (Widget);
         Iterator := Vis_Node_Sets.Make_Iterator (Get_Floating_Nodes (Widget));
         while Vis_Node_Sets.More (Iterator) loop
            Vis_Node_Sets.Next (Iterator, Node);
            Vis_Data.Drop_Node (Widget.Manager, Node);
         end loop;
         Vis_Node_Sets.Destroy (Iterator);
         Redraw (Widget);
      elsif Vis_Data."/=" (States.Get_Click_Edge (Widget), null) then
         --  Drag on edge --> ignore
         null;
      else
         --  Rectangle
         Start_Rectangle (Widget, Mode);
      end if;
   end Begin_Mouse_Move_Action;

   procedure Drop
     (Widget : access Graph_Widget_Record'Class) is

      Lock : Lock_Type;
   begin
      --  user has dropped some nodes
      Lock_All_Content (Widget, Lock);
      Raise_Floating (Widget);
      Move_Nodes
        (Widget => Widget,
         Nodes  => Get_Floating_Nodes (Widget),
         Offset => Positioning.Get_Logic
                     (Widget, States.Get_Mouse_Move_Distance (Widget)));
      States.End_Drag (Widget);
      States.Changed_Temporary (Widget);
      Release_Lock (Widget, Lock);
   end Drop;

   procedure Cancel_Drag
     (Widget : access Graph_Widget_Record'Class) is

      Lock : Lock_Type;
   begin
      Lock_All_Content (Widget, Lock);
      Move_Nodes
        (Widget => Widget,
         Nodes  => Get_Floating_Nodes (Widget),
         Offset => Vis.Logic.Zero_2d);
      States.End_Drag (Widget);
      States.Changed_Temporary (Widget);
      Release_Lock (Widget, Lock);
   end Cancel_Drag;

   procedure Cancel_Rectangle
     (Widget : access Graph_Widget_Record'Class) is

      Lock : Lock_Type;
   begin
      Lock_All_Content (Widget, Lock);
      States.End_Rectangle (Widget);
      States.Changed_Temporary (Widget);
      Clear_Selection (Widget);
      Modify_Selection
        (Widget => Widget,
         Edges  => Widget.Callbacks.Previous_Selected_Edges,
         Nodes  => Widget.Callbacks.Previous_Selected_Nodes,
         Mode   => Add);
      Release_Lock (Widget, Lock);
   end Cancel_Rectangle;

   procedure Finish_Rectangle
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Vis_Edge_Lists.Destroy (Widget.Callbacks.Previous_Selected_Edges);
      Vis_Node_Lists.Destroy (Widget.Callbacks.Previous_Selected_Nodes);
      States.End_Rectangle (Widget);
      States.Changed_Temporary (Widget);
      Notify_Selection_Change (Widget);
      Redraw (Widget);
   end Finish_Rectangle;

   procedure Cancel_Move
     (Widget : access Graph_Widget_Record'Class) is
   begin
      States.End_Move (Widget);
   end Cancel_Move;


   --------------------------
   -- Explicit Marshallers --
   --------------------------

   procedure Set_Scroll_Adjustments_Handler
     (Widget : access Graph_Widget_Record'Class;
      Args   : in     Gtk.Arguments.Gtk_Args) is

      function To_Adjustment (Addr : in System.Address)
        return Gtk.Adjustment.Gtk_Adjustment is

         Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
      begin
         return Gtk.Adjustment.Gtk_Adjustment
           (Glib.Object.Get_User_Data (Addr, Stub));
      end To_Adjustment;

      H_Adj : Gtk.Adjustment.Gtk_Adjustment := To_Adjustment
        (Gtk.Arguments.To_Address (Args, 1));
      V_Adj : Gtk.Adjustment.Gtk_Adjustment := To_Adjustment
        (Gtk.Arguments.To_Address (Args, 2));
   begin
      On_Set_Scroll_Adjustments (Widget, H_Adj, V_Adj);
   end Set_Scroll_Adjustments_Handler;


   -----------------------
   -- Connect Callbacks --
   -----------------------

   package Graph_Widget_Callback is new Gtk.Handlers.Callback
     (Widget_Type => Graph_Widget_Record);
   package Graph_Widget_Boolean_Callback is new Gtk.Handlers.Return_Callback
     (Widget_Type => Graph_Widget_Record,
      Return_Type => Boolean);

   package Realize_Cbs renames Graph_Widget_Callback;

   package Unrealize_Cbs renames Graph_Widget_Callback;

   package Set_Scroll_Adjustment_Cbs renames Graph_Widget_Callback;
   --  cannot provide marshaller instanciation because GtkAda supports
   --  only one argument per signal

   package Size_Request_Cbs renames Graph_Widget_Callback;
   package Requisition_Marshallers is new
     Size_Request_Cbs.Marshallers.Generic_Marshaller
       (Base_Type  => Gtk.Widget.Gtk_Requisition_Access,
        Conversion => Gtk.Widget.Get_Requisition);

   package Size_Allocate_Cbs renames Size_Request_Cbs;
   package Allocation_Marshallers is new
     Size_Allocate_Cbs.Marshallers.Generic_Marshaller
       (Base_Type  => Gtk.Widget.Gtk_Allocation_Access,
        Conversion => Gtk.Widget.Get_Allocation);

   package Expose_Event_Cbs renames Graph_Widget_Boolean_Callback;

   package Destroy_Cbs renames Graph_Widget_Callback;

   package Button_Press_Event_Cbs renames Graph_Widget_Boolean_Callback;

   package Button_Release_Event_Cbs renames Graph_Widget_Boolean_Callback;

   package Motion_Notify_Event_Cbs renames Graph_Widget_Boolean_Callback;

   package Enter_Notify_Event_Cbs renames Graph_Widget_Boolean_Callback;
   package Leave_Notify_Event_Cbs renames Graph_Widget_Boolean_Callback;


   package Adjustment_Cbs is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Adjustment.Gtk_Adjustment_Record,
      User_Type   => Graph_Widget);


   package Realize_Handling is new Gtk.Widget.Realize_Handling
     (Widget_Type  => Graph_Widget_Record,
      Realize_Proc => On_Realize);


   procedure Connect_All_Callbacks
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Set_Events
        (Widget,
         Get_Events (Widget) or Handled_Event_Mask);

      Realize_Cbs.Connect
        (Widget => Widget,
         Name   => "realize",
         Marsh  => Realize_Cbs.To_Marshaller (After_Realize'Access),
         After  => True);
      Unrealize_Cbs.Connect
        (Widget => Widget,
         Name   => "unrealize",
         Marsh  => Unrealize_Cbs.To_Marshaller (On_Unrealize'Access));
      Destroy_Cbs.Connect
        (Widget => Widget,
         Name   => "destroy",
         Marsh  => Destroy_Cbs.To_Marshaller (On_Destroy'Access));
      Set_Scroll_Adjustment_Cbs.Connect
        (Widget => Widget,
         Name   => Handlers.Set_Scroll_Adjustments_Signal,
         Cb     => Set_Scroll_Adjustments_Handler'Access);
      Size_Request_Cbs.Connect
        (Widget => Widget,
         Name   => "size_request",
         Marsh  => Requisition_Marshallers.To_Marshaller
                     (On_Size_Request'Access));
      Size_Allocate_Cbs.Connect
        (Widget => Widget,
         Name   => "size_allocate",
         Marsh  => Allocation_Marshallers.To_Marshaller
                     (On_Size_Allocate'Access));
      Expose_Event_Cbs.Connect
        (Widget => Widget,
         Name   => "expose_event",
         Marsh  => Expose_Event_Cbs.To_Marshaller (On_Expose_Event'Access));
      Button_Press_Event_Cbs.Connect
        (Widget => Widget,
         Name   => "button_press_event",
         Marsh  => Button_Press_Event_Cbs.To_Marshaller
                     (On_Button_Press_Event'Access));
      Button_Release_Event_Cbs.Connect
        (Widget => Widget,
         Name   => "button_release_event",
         Marsh  => Button_Release_Event_Cbs.To_Marshaller
                     (On_Button_Release_Event'Access));
      Motion_Notify_Event_Cbs.Connect
        (Widget => Widget,
         Name   => "motion_notify_event",
         Marsh  => Motion_Notify_Event_Cbs.To_Marshaller
                     (On_Motion_Notify_Event'Access));
      Enter_Notify_Event_Cbs.Connect
        (Widget => Widget,
         Name   => "enter_notify_event",
         Marsh  => Enter_Notify_Event_Cbs.To_Marshaller
                     (On_Enter_Notify_Event'Access));
      Leave_Notify_Event_Cbs.Connect
        (Widget => Widget,
         Name   => "leave_notify_event",
         Marsh  => Leave_Notify_Event_Cbs.To_Marshaller
                     (On_Leave_Notify_Event'Access));

      Realize_Handling.Set_Realize (Widget);
      Widget.Callbacks.Accept_Scrolling := 0;
   end Connect_All_Callbacks;


   ---------------
   -- Callbacks --
   ---------------

   procedure On_Realize
     (Widget : access Graph_Widget_Record'Class) is

      Attributes      : Gdk.Window_Attr.Gdk_Window_Attr;
      Attributes_Mask : Gdk.Window.Gdk_Window_Attributes_Type;
      Window          : Gdk.Window.Gdk_Window;

      procedure Set_User_Data
        (Window : Gdk.Gdk_Window; Widget : System.Address);
      pragma Import (C, Set_User_Data, "gdk_window_set_user_data");

   begin
      Set_Flags (Widget, Gtk.Widget.Realized);

      Gdk.Window_Attr.Gdk_New
        (Window_Attr => Attributes,
         Event_Mask  => Gdk.Event."or"
                          (Get_Events (Widget), Gdk.Event.Exposure_Mask),
         X           => Get_Allocation_X (Widget),
         Y           => Get_Allocation_Y (Widget),
         Width       => Glib.Gint (Get_Allocation_Width (Widget)),
         Height      => Glib.Gint (Get_Allocation_Height (Widget)),
         Window_Type => Gdk.Window.Window_Child,
         Visual      => Get_Visual (Widget),
         Colormap    => Get_Colormap (Widget));

      Attributes_Mask :=
        Gdk.Window."or" (Gdk.Window."or" (Gdk.Window."or"
        (Gdk.Window.Wa_X, Gdk.Window.Wa_Y), Gdk.Window.Wa_Visual),
         Gdk.Window.Wa_Colormap);
      Gdk.Window.Gdk_New
        (Window,
         Gtk.Widget.Get_Window (Get_Parent (Widget)),
         Attributes,
         Attributes_Mask);
      Set_Window
        (Widget,
         Window);
      Set_Style
        (Widget,
         Gtk.Style.Attach (Get_Style (Widget), Get_Window (Widget)));
      Gtk.Style.Set_Background
        (Get_Style (Widget),
         Get_Window (Widget),
         Gtk.Enums.State_Active);

      Set_User_Data
        (Window,
         Glib.Object.Get_Object (Widget));
   end On_Realize;

   procedure After_Realize
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Settings.Set_Up (Widget);
      Drawing.Set_Up (Widget);
      if States.Must_Flush_Locked_Content (Widget) then
         Flush_Locked (Widget);
      end if;
      Grab_Default (Widget);
      States.Realized (Widget);
      Update_Scrollbars (Widget);
   end After_Realize;

   procedure On_Unrealize
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Drawing.Shut_Down (Widget);
      Settings.Shut_Down (Widget);
      if Widget.Callbacks.Auto_Scroll_Connected then
         Gtk.Main.Timeout_Remove (Widget.Callbacks.Auto_Scroll_Handler);
         Widget.Callbacks.Auto_Scroll_Connected := False;
      end if;
      while Widget.Callbacks.Mouse_Grab_Count > 0 loop
         Ungrab_Mouse (Widget, 0);
      end loop;
      Widget.Callbacks.Horizontal_Adjustment := Gtk.Adjustment.Null_Adjustment;
      Widget.Callbacks.Vertical_Adjustment := Gtk.Adjustment.Null_Adjustment;
   end On_Unrealize;

   procedure On_Destroy
     (Widget : access Graph_Widget_Record'Class) is
   begin
      Callbacks_Logger.Debug ("Destroy Graph_Widget");
      Shut_Down_Graph_Widget (Widget);
   end On_Destroy;

   procedure On_Set_Scroll_Adjustments
     (Widget     : access Graph_Widget_Record'Class;
      Horizontal : in     Gtk.Adjustment.Gtk_Adjustment;
      Vertical   : in     Gtk.Adjustment.Gtk_Adjustment) is

      package Adj renames Gtk.Adjustment;
   begin
      if Adj."/="
        (Widget.Callbacks.Horizontal_Adjustment, Adj.Null_Adjustment) then

         Gtk.Handlers.Disconnect
           (Object => Widget.Callbacks.Horizontal_Adjustment,
            Id     => Widget.Callbacks.Horizontal_Handler);
      end if;
      Widget.Callbacks.Horizontal_Adjustment := Horizontal;
      if Adj."/=" (Horizontal, Adj.Null_Adjustment) then
         Widget.Callbacks.Horizontal_Handler := Adjustment_Cbs.Connect
           (Widget    => Horizontal,
            Name      => "value_changed",
            Marsh     => Adjustment_Cbs.To_Marshaller
                           (On_Horizontal_Scroll'Access),
            User_Data => Graph_Widget (Widget));
      end if;

      if Adj."/="
        (Widget.Callbacks.Vertical_Adjustment, Adj.Null_Adjustment) then

         Gtk.Handlers.Disconnect
           (Object => Widget.Callbacks.Vertical_Adjustment,
            Id     => Widget.Callbacks.Vertical_Handler);
      end if;
      Widget.Callbacks.Vertical_Adjustment := Vertical;
      if Adj."/=" (Vertical, Adj.Null_Adjustment) then
         Widget.Callbacks.Vertical_Handler := Adjustment_Cbs.Connect
           (Widget    => Vertical,
            Name      => "value_changed",
            Marsh     => Adjustment_Cbs.To_Marshaller
                           (On_Vertical_Scroll'Access),
            User_Data => Graph_Widget (Widget));
      end if;
      Update_Scrollbars (Widget);
   end On_Set_Scroll_Adjustments;

   procedure On_Size_Request
     (Widget      : access Graph_Widget_Record'Class;
      Requisition : in     Gtk.Widget.Gtk_Requisition_Access) is
   begin
      Requisition.Width  := Default_Width;
      Requisition.Height := Default_Height;
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "size_request");
   end On_Size_Request;

   procedure On_Size_Allocate
     (Widget     : access Graph_Widget_Record'Class;
      Allocation : in     Gtk.Widget.Gtk_Allocation_Access) is
   begin
      if Gtk.Widget.Realized_Is_Set (Widget) then
         Gdk.Window.Move_Resize
           (Get_Window (Widget),
            Allocation.X,
            Allocation.Y,
            Glib.Gint (Allocation.Width),
            Glib.Gint (Allocation.Height));
      end if;
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "size_allocate");
      Resize_Graph_Widget
        (Widget,
         Vis.Absolute.Combine_Vector
           (X => Vis.Absolute_Int (Allocation.Width),
            Y => Vis.Absolute_Int (Allocation.Height)));
   end On_Size_Allocate;

   function On_Expose_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Expose)
     return Boolean is

      Gdk_Area   : Gdk.Rectangle.Gdk_Rectangle;
      Area       : Vis.Absolute.Rectangle_2d;
   begin
      --  Clear has happened before this is called.
      --  Correct Buffer is set as background pixmap therefore
      --  nothing needs to be drawn except if graph widget was polluted

      --  Relevant Fields in Event: Area, Count, Graphics_Expose
      --  type: Expose
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "expose_event");
      --  Ignore event, if more expose events are to follow.
      if Glib.">" (Gdk.Event.Get_Count (Event), 0) then
         return True;
      end if;

      if States.Has_Display_Changed (Widget) or else
        States.Has_Temporary_Changed (Widget) then

         Gdk_Area := Gdk.Event.Get_Area (Event);
         Area := Vis.Absolute.Combine_Rectangle
           (X_1 => Vis.Absolute_Int (Gdk_Area.X),
            Y_1 => Vis.Absolute_Int (Gdk_Area.Y),
            X_2 => Vis.Absolute_Int (Gdk_Area.X) +
                     Vis.Absolute_Int (Gdk_Area.Width) - 1,
            Y_2 => Vis.Absolute_Int (Gdk_Area.Y) +
                     Vis.Absolute_Int (Gdk_Area.Height) - 1);

         Drawing.Update_Display (Widget, Area);

         if States.Has_Temporary_Changed (Widget) then
            Drawing.Update_Temporary (Widget, Area);
         end if;
         Gdk.Window.Clear_Area
           (Window => Get_Window (Widget),
            X      => Glib.Gint (Vis.Absolute.Get_Left (Area)),
            Y      => Glib.Gint (Vis.Absolute.Get_Top (Area)),
            Width  => Glib.Gint (Vis.Absolute.Get_Width (Area)),
            Height => Glib.Gint (Vis.Absolute.Get_Height (Area)));
         States.Updated_Visual (Widget);
         States.Updated_Temporary (Widget);
      end if;

      return True;
   end On_Expose_Event;

   function On_Button_Press_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button)
     return Boolean is

      Edge                    : Vis_Data.Vis_Edge_Id;
      Node                    : Vis_Data.Vis_Node_Id;
      Location                : Vis.Logic.Vector_2d;
      Relative_Click_Position : Vis.Absolute.Vector_2d;
      Click_Position          : Vis.Absolute.Vector_2d;
      Window_Origin           : Vis.Absolute.Vector_2d :=
        Vis.Absolute.Get_Top_Left (Drawing.Get_Visible_Area (Widget));
   begin
      States.Set_Mouse_Modifiers (Widget, Gdk.Event.Get_State (Event));

      if States.Is_Drag_Current (Widget) then
         Ungrab_Mouse (Widget, Gdk.Event.Get_Time (Event));
         Cancel_Drag (Widget);
         return True;
      elsif States.Is_Rectangle_Current (Widget) then
         Ungrab_Mouse (Widget, Gdk.Event.Get_Time (Event));
         Cancel_Rectangle (Widget);
         return True;
      elsif States.Is_Click_Current (Widget) then
         Ungrab_Mouse (Widget, Gdk.Event.Get_Time (Event));
         Cancel_Click (Widget);
         return True;
      elsif States.Is_Move_Current (Widget) then
         Ungrab_Mouse (Widget, Gdk.Event.Get_Time (Event));
         Cancel_Move (Widget);
      end if;

      Relative_Click_Position := Vis.Absolute.Combine_Vector
        (X => Vis.Absolute_Int (Gdk.Event.Get_X (Event)),
         Y => Vis.Absolute_Int (Gdk.Event.Get_Y (Event)));
      Click_Position := Vis.Absolute."+"
        (Window_Origin, Relative_Click_Position);

      if States.Is_Action_Mode_Current (Widget) then
         --  action mode, signal event
         Location := Positioning.Get_Logic (Widget, Click_Position);
         Node := Vis_Data.Get_Node_At (Widget.Manager, Click_Position);
         if Vis_Data."/=" (Node, null) then
            Notifications.Action_Mode_Button_Press_Event_Node
              (Widget   => Widget,
               Event    => Event,
               Location => Positioning.Get_Logic (Widget, Click_Position),
               Node     => Node);
         else
            Edge := Vis_Data.Get_Edge_At (Widget.Manager, Click_Position);
            if Vis_Data."/=" (Edge, null) then
               Notifications.Action_Mode_Button_Press_Event_Edge
                 (Widget   => Widget,
                  Event    => Event,
                  Location => Location,
                  Edge     => Edge);
            else
               Notifications.Action_Mode_Button_Press_Event_Background
                 (Widget   => Widget,
                  Event    => Event,
                  Location => Location);
            end if;
         end if;
         return True;
      elsif (Glib."=" (Gdk.Event.Get_Button (Event), Left_Button)) then
         --  left button
         Node := Vis_Data.Get_Node_At (Widget.Manager, Click_Position);
         if Vis_Data."/=" (Node, null) then
            --  pressed on node
            Grab_Mouse (Widget, Gdk.Event.Get_Time (Event));
            States.Begin_Click_On_Node
              (Widget => Widget,
               Point  => Click_Position,
               Node   => Node);
         else
            Edge := Vis_Data.Get_Edge_At (Widget.Manager, Click_Position);
            Grab_Mouse (Widget, Gdk.Event.Get_Time (Event));
            if Vis_Data."/=" (Edge, null) then
               --  pressed on edge
               States.Begin_Click_On_Edge
                 (Widget => Widget,
                  Point  => Click_Position,
                  Edge   => Edge);
            else
               --  pressed on background
               States.Begin_Click_On_Background
                 (Widget => Widget,
                  Point  => Click_Position);
            end if;
         end if;
         return True;
      elsif (Glib."=" (Gdk.Event.Get_Button (Event), Right_Button)) then
         Location := Positioning.Get_Logic (Widget, Click_Position);
         --  right button
         Node := Vis_Data.Get_Node_At (Widget.Manager, Click_Position);
         if Vis_Data."/=" (Node, null) then
            Notifications.Node_Popup (Widget, Event, Location, Node);
         else
            Edge := Vis_Data.Get_Edge_At (Widget.Manager, Click_Position);
            if Vis_Data."/=" (Edge, null) then
               Notifications.Edge_Popup (Widget, Event, Location, Edge);
            else
               Notifications.Background_Popup
                 (Widget   => Widget,
                  Event    => Event,
                  Location => Location);
            end if;
         end if;
         return True;
      elsif (Glib."=" (Gdk.Event.Get_Button (Event), Middle_Button)) then
         --  middle button
         Grab_Mouse (Widget, Gdk.Event.Get_Time (Event));
         States.Begin_Move (Widget);
         return True;
      else
         return False;
      end if;
   end On_Button_Press_Event;

   function On_Button_Release_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button)
     return Boolean is

      Modifiers : Gdk.Types.Gdk_Modifier_Type := Gdk.Event.Get_State (Event);
      Mode      : Selection_Modify_Type;
   begin
      States.Set_Mouse_Modifiers (Widget, Modifiers);

      if Glib."=" (Gdk.Event.Get_Button (Event), Left_Button) then

         if States.Is_Click_Current (Widget) then
            --  simple click
            Ungrab_Mouse (Widget, Gdk.Event.Get_Time (Event));
            Mode := Interpret_Modifiers (Modifiers);

            Process_Mouse_Click (Widget, Mode);

            States.End_Click (Widget);
         elsif States.Is_Rectangle_Current (Widget) then
            --  user has finished opening a rectangle
            Ungrab_Mouse (Widget, Gdk.Event.Get_Time (Event));

            Finish_Rectangle (Widget);
         elsif States.Is_Drag_Current (Widget) then
            Ungrab_Mouse (Widget, Gdk.Event.Get_Time (Event));

            Drop (Widget);
         end if;

         return True;
      elsif Glib."=" (Gdk.Event.Get_Button (Event), Middle_Button) then
         if States.Is_Move_Current (Widget) then
            Ungrab_Mouse (Widget, Gdk.Event.Get_Time (Event));
            States.End_Move (Widget);
         end if;

         return True;
      else
         return False;
      end if;
   end On_Button_Release_Event;

   procedure Edges_Appeared
     (Widget : access Graph_Widget_Record'Class;
      Edges  : in     Vis_Edge_Sets.Set) is

      Add_Edge_Queue : Vis_Edge_Lists.List;
      Add_Node_Queue : Vis_Node_Lists.List;
      Mode           : Selection_Modify_Type;
      Rectangle      : Vis.Absolute.Rectangle_2d;
      Edge           : Vis_Data.Vis_Edge_Id;
      Iterator       : Vis_Edge_Sets.Iterator;
   begin
      if States.Is_Rectangle_Current (Widget) then
         Mode := Interpret_Modifiers (States.Get_Mouse_Modifiers (Widget));
         if Mode = Change then
            Mode := Add;
         end if;
         Rectangle := Vis.Absolute.Combine_Rectangle
           (X_1 => Vis.Absolute.Get_X (States.Get_Click_Point (Widget)),
            Y_1 => Vis.Absolute.Get_Y (States.Get_Click_Point (Widget)),
            X_2 => Vis.Absolute.Get_X (States.Get_Mouse_Position (Widget)),
            Y_2 => Vis.Absolute.Get_Y (States.Get_Mouse_Position (Widget)));

         Add_Edge_Queue := Vis_Edge_Lists.Create;
         Add_Node_Queue := Vis_Node_Lists.Create;
         Iterator := Vis_Edge_Sets.Make_Iterator (Edges);
         while Vis_Edge_Sets.More (Iterator) loop
            Vis_Edge_Sets.Next (Iterator, Edge);
            if Vis_Data.Intersects (Edge, Rectangle) then

               Vis_Edge_Lists.Attach (Edge, Add_Edge_Queue);
            end if;
         end loop;
         Vis_Edge_Sets.Destroy (Iterator);

         Modify_Selection
           (Widget   => Widget,
            Edges    => Add_Edge_Queue,
            Nodes    => Add_Node_Queue,
            Mode     => Mode);
      end if;
   end Edges_Appeared;

   procedure Mouse_Pointer_Moved_To
     (Widget          : access Graph_Widget_Record'Class;
      Motion_Position : in     Vis.Absolute.Vector_2d) is

      use type Vis.Absolute.Vector_2d;
      Distance                 : Vis.Absolute.Vector_2d;
      Old_Rectangle            : Vis.Absolute.Rectangle_2d;
      New_Rectangle            : Vis.Absolute.Rectangle_2d;
      Add_Edge_Queue           : Vis_Edge_Lists.List;
      Remove_Edge_Queue        : Vis_Edge_Lists.List;
      Add_Node_Queue           : Vis_Node_Lists.List;
      Remove_Node_Queue        : Vis_Node_Lists.List;
      Lock                     : Lock_Type;
      Mode                     : Selection_Modify_Type;
   begin
      Old_Rectangle := Vis.Absolute.Combine_Rectangle
        (X_1 => Vis.Absolute.Get_X (States.Get_Click_Point (Widget)),
         Y_1 => Vis.Absolute.Get_Y (States.Get_Click_Point (Widget)),
         X_2 => Vis.Absolute.Get_X (States.Get_Mouse_Position (Widget)),
         Y_2 => Vis.Absolute.Get_Y (States.Get_Mouse_Position (Widget)));
      States.Set_Mouse_Position
        (Widget => Widget,
         Point  => Motion_Position);
      Mode := Interpret_Modifiers (States.Get_Mouse_Modifiers (Widget));

      if States.Is_Click_Current (Widget) then
         Distance := States.Get_Mouse_Move_Distance (Widget);
         if Distance * Distance > Default_Click_Distance_Tolerance**2 then
            Begin_Mouse_Move_Action
              (Widget          => Widget,
               Mode            => Mode);
         end if;
      end if;
      if States.Is_Rectangle_Current (Widget) then
         if Mode = Change then
            Mode := Add;
         end if;
         New_Rectangle := Vis.Absolute.Combine_Rectangle
           (X_1 => Vis.Absolute.Get_X (States.Get_Click_Point (Widget)),
            Y_1 => Vis.Absolute.Get_Y (States.Get_Click_Point (Widget)),
            X_2 => Vis.Absolute.Get_X (States.Get_Mouse_Position (Widget)),
            Y_2 => Vis.Absolute.Get_Y (States.Get_Mouse_Position (Widget)));
         States.Changed_Temporary (Widget);
         Vis_Data.Update_Area_Content
           (Manager      => Widget.Manager,
            Old_Area     => Old_Rectangle,
            New_Area     => New_Rectangle,
            Add_Edges    => Add_Edge_Queue,
            Remove_Edges => Remove_Edge_Queue,
            Add_Nodes    => Add_Node_Queue,
            Remove_Nodes => Remove_Node_Queue);
         Lock_All_Content (Widget, Lock);
         Modify_Selection
           (Widget   => Widget,
            Edges    => Remove_Edge_Queue,
            Nodes    => Remove_Node_Queue,
            Mode     => Remove);
         Modify_Selection
           (Widget   => Widget,
            Edges    => Add_Edge_Queue,
            Nodes    => Add_Node_Queue,
            Mode     => Mode);
         Release_Lock (Widget, Lock);
      elsif States.Is_Drag_Current (Widget) then
         States.Changed_Temporary (Widget);
         Redraw (Widget);
      elsif States.Is_Move_Current (Widget) then
         null;
      end if;
   end Mouse_Pointer_Moved_To;

   function On_Motion_Notify_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Motion)
     return Boolean is

      Window_Origin            : Vis.Absolute.Vector_2d :=
        Vis.Absolute.Get_Top_Left (Drawing.Get_Visible_Area (Widget));
      Relative_Motion_Position : Vis.Absolute.Vector_2d;
      Motion_Position          : Vis.Absolute.Vector_2d;
   begin
      States.Set_Mouse_Modifiers (Widget, Gdk.Event.Get_State (Event));

      Relative_Motion_Position := Vis.Absolute.Combine_Vector
        (X => Vis.Absolute_Int (Gdk.Event.Get_X (Event)),
         Y => Vis.Absolute_Int (Gdk.Event.Get_Y (Event)));
      Motion_Position := Vis.Absolute."+"
        (Window_Origin, Relative_Motion_Position);

      Mouse_Pointer_Moved_To (Widget, Motion_Position);

      return True;
   end On_Motion_Notify_Event;

   function On_Enter_Notify_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Crossing)
     return Boolean is
   begin
      States.Set_Mouse_Modifiers (Widget, Gdk.Event.Get_State (Event));
      if Widget.Callbacks.Auto_Scroll_Connected then
         Gtk.Main.Timeout_Remove (Widget.Callbacks.Auto_Scroll_Handler);
         Widget.Callbacks.Auto_Scroll_Connected := False;
      end if;
      return False;
   end On_Enter_Notify_Event;

   function On_Leave_Notify_Event
     (Widget : access Graph_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Crossing)
     return Boolean is
   begin
      States.Set_Mouse_Modifiers (Widget, Gdk.Event.Get_State (Event));
      if States.Is_Auto_Scrolling (Widget) and
        not Widget.Callbacks.Auto_Scroll_Connected then

         Widget.Callbacks.Auto_Scroll_Handler := Graph_Widget_Timeouts.Add
           (Interval => Default_Auto_Scroll_Delay,
            Func     => On_Auto_Scroll_Timeout'Access,
            D        => Graph_Widget (Widget));
         Widget.Callbacks.Auto_Scroll_Connected := True;
      end if;
      return False;
   end On_Leave_Notify_Event;

   function On_Auto_Scroll_Timeout
     (Widget : in     Graph_Widget)
     return Boolean is

      Area   : Vis.Absolute.Rectangle_2d;
      Cursor : Vis.Absolute.Vector_2d;
      X      : Vis.Absolute_Int;
      Y      : Vis.Absolute_Int;
   begin
      Gdk.Threads.Enter;
      if not States.Is_Auto_Scrolling (Widget) and
        Widget.Callbacks.Auto_Scroll_Connected then

         Gtk.Main.Timeout_Remove (Widget.Callbacks.Auto_Scroll_Handler);
         Widget.Callbacks.Auto_Scroll_Connected := False;

         Gdk.Threads.Leave;
         return False;
      else
         Area := Drawing.Get_Visible_Area (Widget);
         Cursor := States.Get_Mouse_Position (Widget);

         if Vis.Absolute.Get_X (Cursor) < Vis.Absolute.Get_Left (Area) then
            X := Vis.Absolute.Get_X (Cursor) - Vis.Absolute.Get_Left (Area);
         elsif Vis.Absolute.Get_X (Cursor) > Vis.Absolute.Get_Right (Area) then
            X := Vis.Absolute.Get_X (Cursor) - Vis.Absolute.Get_Right (Area);
         else
            X := 0;
         end if;

         if Vis.Absolute.Get_Y (Cursor) > Vis.Absolute.Get_Bottom (Area) then
            Y := Vis.Absolute.Get_Y (Cursor) - Vis.Absolute.Get_Bottom (Area);
         elsif Vis.Absolute.Get_Y (Cursor) < Vis.Absolute.Get_Top (Area) then
            Y := Vis.Absolute.Get_Y (Cursor) - Vis.Absolute.Get_Top (Area);
         else
            Y := 0;
         end if;

         X := (X / Default_Auto_Scroll_Sensitivity) *
           Default_Auto_Scroll_Acceleration;
         Y := (Y / Default_Auto_Scroll_Sensitivity) *
           Default_Auto_Scroll_Acceleration;

         Set_Visible_Center
           (Widget => Widget,
            Center => Vis.Absolute."+"
                        (Left  => Vis.Absolute.Get_Center (Area),
                         Right => Vis.Absolute.Combine_Vector (X, Y)));

         Gdk.Threads.Leave;
         return True;
      end if;
   end On_Auto_Scroll_Timeout;

end Giant.Graph_Widgets.Callbacks;
