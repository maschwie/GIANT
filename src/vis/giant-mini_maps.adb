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
--  $RCSfile: giant-mini_maps.adb,v $, $Revision: 1.13 $
--  $Author: squig $
--  $Date: 2003-12-05 16:50:27 $
--
------------------------------------------------------------------------------


with System;

with Gdk;
with Gdk.Drawable;
with Gdk.Event;
with Gdk.Main;
with Gdk.Rectangle;
with Gdk.Types;
with Gdk.Window;
with Gdk.Window_Attr;
with Glib;
with Glib.Object;
with Gtk.Arguments;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Object;
with Gtk.Style;
with Gtk.Widget;
pragma Elaborate_All (Gtk.Widget);
with Gtk.Window;
with Gtkada.Types;

with Giant.Graph_Widgets.Handlers;
with Giant.Logger;
pragma Elaborate_All (Giant.Logger);

package body Giant.Mini_Maps is

   use Vis.Absolute;
   use Vis.Logic;

   package Mini_Map_Logger is new Logger
     (Name => "Giant.Mini_Maps");

   function "or"
     (Left  : in     Gdk.Event.Gdk_Event_Mask;
      Right : in     Gdk.Event.Gdk_Event_Mask)
     return Gdk.Event.Gdk_Event_Mask
     renames Gdk.Event."or";

   Handled_Events : constant Gdk.Event.Gdk_Event_Mask :=
     Gdk.Event.Button_Press_Mask or
     Gdk.Event.Button1_Motion_Mask or
     Gdk.Event.Button_Release_Mask;


   ----------------------
   -- Reusable drawing --
   ----------------------

   procedure Draw_Rectangle
     (Drawable  : in     Gdk.Drawable.Gdk_Drawable;
      Gc        : in     Gdk.GC.Gdk_GC;
      Filled    : in     Boolean;
      Rectangle : in     Vis.Absolute.Rectangle_2d) is
   begin
      if Filled then
         Gdk.Drawable.Draw_Rectangle
           (Drawable => Drawable,
            Gc       => Gc,
            Filled   => True,
            X        => Glib.Gint (Get_Left (Rectangle)),
            Y        => Glib.Gint (Get_Top (Rectangle)),
            Width    => Glib.Gint (Get_Width (Rectangle)),
            Height   => Glib.Gint (Get_Height (Rectangle)));
      else
         Gdk.Drawable.Draw_Rectangle
           (Drawable => Drawable,
            Gc       => Gc,
            Filled   => False,
            X        => Glib.Gint (Get_Left (Rectangle)),
            Y        => Glib.Gint (Get_Top (Rectangle)),
            Width    => Glib.Gint (Get_Width (Rectangle) - 1),
            Height   => Glib.Gint (Get_Height (Rectangle) - 1));
      end if;
   end Draw_Rectangle;


   ---------------
   -- Callbacks --
   ---------------

   procedure On_Realize
     (Widget : access Mini_Map_Record'Class);

   procedure After_Realize
     (Widget : access Mini_Map_Record'Class);

   procedure On_Unrealize
     (Widget : access Mini_Map_Record'Class);

   procedure On_Destroy
     (Widget : access Mini_Map_Record'Class);

   procedure On_Size_Request
     (Widget      : access Mini_Map_Record'Class;
      Requisition : in     Gtk.Widget.Gtk_Requisition_Access);

   procedure On_Size_Allocate
     (Widget     : access Mini_Map_Record'Class;
      Allocation : in     Gtk.Widget.Gtk_Allocation_Access);

   function On_Expose_Event
     (Widget : access Mini_Map_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Expose)
     return Boolean;

   function On_Button_Press_Event
     (Widget : access Mini_Map_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button)
     return Boolean;

   function On_Motion_Notify_Event
     (Widget : access Mini_Map_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Motion)
     return Boolean;

   function On_Button_Release_Event
     (Widget : access Mini_Map_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button)
     return Boolean;

   procedure On_Logical_Area_Changed
     (Graph_Widget : access Graph_Widgets.Graph_Widget_Record'Class;
      Area         : in     Vis.Logic.Rectangle_2d;
      Mini         : in     Mini_Map);

   procedure On_Visible_Area_Changed
     (Graph_Widget : access Graph_Widgets.Graph_Widget_Record'Class;
      Area         : in     Vis.Logic.Rectangle_2d;
      Mini         : in     Mini_Map);


   ---------------------
   -- Signal Handling --
   ---------------------

   package Mini_Map_Callback is new Gtk.Handlers.Callback
     (Widget_Type => Mini_Map_Record);
   package Mini_Map_Boolean_Callback is new Gtk.Handlers.Return_Callback
     (Widget_Type => Mini_Map_Record,
      Return_Type => Boolean);

   package Realize_Cbs renames Mini_Map_Callback;

   package Unrealize_Cbs renames Mini_Map_Callback;

   package Size_Request_Cbs renames Mini_Map_Callback;
   package Requisition_Marshallers is new
     Size_Request_Cbs.Marshallers.Generic_Marshaller
       (Base_Type  => Gtk.Widget.Gtk_Requisition_Access,
        Conversion => Gtk.Widget.Get_Requisition);

   package Size_Allocate_Cbs renames Size_Request_Cbs;
   package Allocation_Marshallers is new
     Size_Allocate_Cbs.Marshallers.Generic_Marshaller
       (Base_Type  => Gtk.Widget.Gtk_Allocation_Access,
        Conversion => Gtk.Widget.Get_Allocation);

   package Expose_Event_Cbs renames Mini_Map_Boolean_Callback;

   package Destroy_Cbs renames Mini_Map_Callback;

   package Button_Press_Event_Cbs renames Mini_Map_Boolean_Callback;
   package Motion_Notify_Event_Cbs renames Mini_Map_Boolean_Callback;
   package Button_Release_Event_Cbs renames Mini_Map_Boolean_Callback;


   package Realize_Handling is new Gtk.Widget.Realize_Handling
     (Widget_Type  => Mini_Map_Record,
      Realize_Proc => On_Realize);


   package Logical_Area_Cbs is new Gtk.Handlers.User_Callback
     (Widget_Type => Graph_Widgets.Graph_Widget_Record,
      User_Type   => Mini_Map);

   package Logical_Area_Marshs is new
     Logical_Area_Cbs.Marshallers.Generic_Marshaller
     (Base_Type   => Vis.Logic.Rectangle_2d,
      Conversion  => Graph_Widgets.Handlers.To_Rectangle_2d);

   package Visible_Area_Cbs is new Gtk.Handlers.User_Callback
     (Widget_Type => Graph_Widgets.Graph_Widget_Record,
      User_Type   => Mini_Map);

   package Visible_Area_Marshs is new
     Visible_Area_Cbs.Marshallers.Generic_Marshaller
     (Base_Type   => Vis.Logic.Rectangle_2d,
      Conversion  => Graph_Widgets.Handlers.To_Rectangle_2d);


   -------------------------
   -- Internal procedures --
   -------------------------

   procedure Resize_Buffer
     (Widget : access Mini_Map_Record'Class) is

      Window : Gdk.Window.Gdk_Window;
      X      : Glib.Gint;
      Y      : Glib.Gint;
      Width  : Glib.Gint;
      Height : Glib.Gint;
      Depth  : Glib.Gint;
   begin
      Window := Get_Window (Widget);
      Gdk.Window.Get_Geometry
        (Window => Window,
         X      => X,
         Y      => Y,
         Width  => Width,
         Height => Height,
         Depth  => Depth);
      if Gdk."/=" (Widget.Buffer, Gdk.Pixmap.Null_Pixmap) then
         Gdk.Pixmap.Unref (Widget.Buffer);
      end if;
      Gdk.Pixmap.Gdk_New
        (Pixmap => Widget.Buffer,
         Window => Window,
         Width  => Width,
         Height => Height);
      Draw_Rectangle
        (Drawable  => Widget.Buffer,
         Gc        => Widget.Background_Gc,
         Filled    => True,
         Rectangle => Vis.Absolute.Combine_Rectangle
                       (X_1 => 0,
                        Y_1 => 0,
                        X_2 => Vis.Absolute_Natural (Width) - 1,
                        Y_2 => Vis.Absolute_Natural (Height) - 1));
      Gdk.Window.Set_Back_Pixmap
        (Window          => Window,
         Pixmap          => Widget.Buffer,
         Parent_Relative => False);
   end Resize_Buffer;

   procedure Destroy_Buffer
     (Widget : access Mini_Map_Record'Class) is
   begin
      if Gdk."/=" (Widget.Buffer, Gdk.Pixmap.Null_Pixmap) then
         Gdk.Pixmap.Unref (Widget.Buffer);
         Widget.Buffer := Gdk.Pixmap.Null_Pixmap;
      end if;
   end Destroy_Buffer;

   procedure Update
     (Widget : access Mini_Map_Record) is
   begin
      if Graph_Widgets."/=" (Widget.Watched, null) then
         Widget.Polluted := True;
         Draw (Widget);
      end if;
   end Update;


   --------------------
   -- Implementation --
   --------------------

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;

   procedure Initialize
     (Widget  : access Mini_Map_Record;
      Watched : in     Graph_Widgets.Graph_Widget) is
   begin
      Gtk.Drawing_Area.Initialize (Widget);

      Widget.Watched := null;
      Widget.Buffer := Gdk.Pixmap.Null_Pixmap;
      Widget.Polluted := True;
      Set_Graph_Widget (Widget, Watched);
      Gtk.Object.Initialize_Class_Record
        (Object       => Widget,
         Signals      => Gtkada.Types.Null_Array,
         Class_Record => Class_Record,
         Type_Name    => "MiniMap");
      Set_Events
        (Widget, Gdk.Event."or" (Get_Events (Widget), Handled_Events));

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
      Motion_Notify_Event_Cbs.Connect
        (Widget => Widget,
         Name   => "motion_notify_event",
         Marsh  => Motion_Notify_Event_Cbs.To_Marshaller
                    (On_Motion_Notify_Event'Access));
      Button_Release_Event_Cbs.Connect
        (Widget => Widget,
         Name   => "button_release_event",
         Marsh  => Button_Release_Event_Cbs.To_Marshaller
                    (On_Button_Release_Event'Access));

      Realize_Handling.Set_Realize (Widget);
   end Initialize;

   procedure Create
     (Widget  :    out Mini_Map;
      Watched : in     Graph_Widgets.Graph_Widget := null) is
   begin
      Widget := new Mini_Map_Record;
      Initialize (Widget, Watched);
   end Create;

   procedure Set_Graph_Widget
     (Widget  : access Mini_Map_Record;
      Watched : in     Graph_Widgets.Graph_Widget) is
   begin
      if Graph_Widgets."/=" (Widget.Watched, null) then
         Gtk.Handlers.Disconnect
           (Object => Widget.Watched, Id => Widget.Logical_Area_Handler);
         Gtk.Handlers.Disconnect
           (Object => Widget.Watched, Id => Widget.Visible_Area_Handler);
         Graph_Widgets.Unref (Widget.Watched);
      end if;
      Widget.Watched := Watched;
      Widget.Polluted := True;
      if Graph_Widgets."/=" (Widget.Watched, null) then
         Graph_Widgets.Ref (Widget.Watched);
         Widget.Logical_Area_Handler := Logical_Area_Cbs.Connect
           (Widget    => Widget.Watched,
            Name      => Graph_Widgets.Handlers.Logical_Area_Changed_Signal,
            Marsh     => Logical_Area_Marshs.To_Marshaller
                           (On_Logical_Area_Changed'Access),
            User_Data => Mini_Map (Widget));
         Widget.Visible_Area_Handler := Visible_Area_Cbs.Connect
           (Widget    => Widget.Watched,
            Name      => Graph_Widgets.Handlers.Visible_Area_Changed_Signal,
            Marsh     => Visible_Area_Marshs.To_Marshaller
                           (On_Visible_Area_Changed'Access),
            User_Data => Mini_Map (Widget));
      end if;
      if Gtk.Widget.Realized_Is_Set (Widget) then
         Update (Widget);
      end if;
   end Set_Graph_Widget;

   procedure Draw_Mini_Map
     (Widget : access Mini_Map_Record) is

      procedure Clear
        (Area : Vis.Absolute.Rectangle_2d) is
      begin
         Draw_Rectangle
           (Drawable  => Widget.Buffer,
            Gc        => Widget.Background_Gc,
            Filled    => True,
            Rectangle => Area);
      end Clear;

      X                 : Glib.Gint  := 0;
      Y                 : Glib.Gint  := 0;
      Width             : Glib.Gint  := Get_Allocation_Width (Widget);
      Height            : Glib.Gint  := Get_Allocation_Height (Widget);
      Window_Rect       : Vis.Absolute.Rectangle_2d;
      Target_Rect       : Vis.Absolute.Rectangle_2d;
      Visible_Rect      : Vis.Absolute.Rectangle_2d;
      Visible_Fill_Rect : Vis.Absolute.Rectangle_2d;
      Logical_Area      : Vis.Logic.Rectangle_2d;
      Visible_Area      : Vis.Logic.Rectangle_2d;
   begin
      Window_Rect := Combine_Rectangle
        (Vis.Absolute_Int (X),
         Vis.Absolute_Int (Y),
         Vis.Absolute_Int (Width) - 1,
         Vis.Absolute_Int (Height) - 1);

      if Graph_Widgets."=" (Widget.Watched, null) then
         Clear (Window_Rect);
         return;
      end if;

      Logical_Area := Graph_Widgets.Get_Logical_Area (Widget.Watched);
      Visible_Area := Graph_Widgets.Get_Visible_Area (Widget.Watched);

      Widget.Transformation := Vis.Get_Transformation_Rect_Into_Rect_Centered
        (Logical_Area, Window_Rect);

      if Vis.Get_Zoom_Level (Widget.Transformation) >
        Vis.Zoom_Level'Safe_Last then

         Clear (Window_Rect);
         return;
      end if;

      Target_Rect := Vis.Transform
        (Widget.Transformation, Logical_Area);

      if Get_Top (Window_Rect) < Get_Top (Target_Rect) then
         Clear
           (Combine_Rectangle
              (Top_Left     => Get_Top_Left (Window_Rect),
               Bottom_Right => Get_Top_Right (Target_Rect)
                                 - Combine_Vector (0, 1)));
      end if;
      if Get_Left (Window_Rect) < Get_Left (Target_Rect) then
         Clear
           (Combine_Rectangle
              (Top_Left     => Combine_Vector
                                 (Get_Left (Window_Rect),
                                  Get_Top  (Target_Rect)),
               Bottom_Right => Combine_Vector
                                 (Get_Left (Target_Rect) - 1,
                                  Get_Bottom (Target_Rect))));
      end if;
      if Get_Right (Window_Rect) > Get_Right (Target_Rect) then
         Clear
           (Combine_Rectangle
              (Top_Left     => Combine_Vector
                                 (Get_Right (Target_Rect) + 1,
                                  Get_Top (Target_Rect)),
               Bottom_Right => Combine_Vector
                                 (Get_Right (Window_Rect),
                                  Get_Bottom (Target_Rect))));
      end if;
      if Get_Bottom (Window_Rect) > Get_Bottom (Target_Rect) then
         Clear
           (Combine_Rectangle
              (Top_Left     => Get_Bottom_Left (Target_Rect)
                                 + Combine_Vector (0, 1),
               Bottom_Right => Get_Bottom_Right (Window_Rect)));
      end if;

      Draw_Rectangle
        (Drawable  => Widget.Buffer,
         Gc        => Widget.Logical_Area_Gc,
         Filled    => True,
         Rectangle => Target_Rect);

      Visible_Rect := Vis.Transform
        (Widget.Transformation, Visible_Area);

      Draw_Rectangle
        (Drawable  => Widget.Buffer,
         Gc        => Widget.Visible_Border_Gc,
         Filled    => False,
         Rectangle => Visible_Rect);

      if Get_Width (Visible_Rect) > 2
        and then Get_Height (Visible_Rect) > 2 then

         Visible_Fill_Rect := Combine_Rectangle
           (Get_Left (Visible_Rect) + 1,
            Get_Top (Visible_Rect) + 1,
            Get_Right (Visible_Rect) - 1,
            Get_Bottom (Visible_Rect) - 1);

         Draw_Rectangle
           (Drawable  => Widget.Buffer,
            Gc        => Widget.Visible_Fill_Gc,
            Filled    => True,
            Rectangle => Visible_Fill_Rect);
      end if;

   end Draw_Mini_Map;


   -----------------------------
   -- Callback Implementation --
   -----------------------------

   procedure On_Realize
     (Widget : access Mini_Map_Record'Class) is

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
     (Widget : access Mini_Map_Record'Class) is

      Window           : Gdk.Window.Gdk_Window := Get_Window (Widget);
      Colormap         : Gdk.Color.Gdk_Colormap := Get_Colormap (Widget);
      Number_Of_Colors : Glib.Gint;
      Success          : Glib.Boolean_Array
        (Widget.Colors'First .. Widget.Colors'Last);
   begin
      for I in Widget.Colors'Range loop
         declare
            Index      : Natural := I;
            Color_Name : String := Mini_Map_Colors'Image
              (Mini_Map_Colors'Val (I));
         begin
            Widget.Colors (Index) := Gdk.Color.Parse (Color_Name);
         exception
            when Gdk.Color.Wrong_Color =>
               Mini_Map_Logger.Error
                 ("Could not parse color name " & Color_Name & ". Will use"
                  & " undefined color ('Null_Color') instead.");
               Widget.Colors (Index) := Gdk.Color.Null_Color;
         end;
      end loop;

      Gdk.Color.Alloc_Colors
        (Colormap => Colormap,
         Colors   => Widget.Colors,
         Success  => Success,
         Result   => Number_Of_Colors);

      if Integer (Number_Of_Colors) < Widget.Colors'Length then
         for I in Success'Range loop
            if not Success (I) then
               Mini_Map_Logger.Error
                 ("Color " & Mini_Map_Colors'Image (Mini_Map_Colors'Val (I))
                  & " could not be allocated. Will use undefined color "
                  & "('Null_Color') instead");
               Widget.Colors (I) := Gdk.Color.Null_Color;
            end if;
         end loop;
      end if;

      Gdk.GC.Gdk_New (Widget.Background_Gc, Window);
      Gdk.GC.Set_Foreground
        (Widget.Background_Gc,
         Gtk.Style.Get_Background
           (Get_Style (Widget), Gtk.Enums.State_Normal));
      Gdk.GC.Gdk_New (Widget.Logical_Area_Gc, Window);
      Gdk.GC.Set_Foreground
        (Widget.Logical_Area_Gc, Widget.Colors (Mini_Map_Colors'Pos (Black)));
      Gdk.GC.Gdk_New (Widget.Visible_Border_Gc, Window);
      Gdk.GC.Set_Foreground
        (Widget.Visible_Border_Gc, Widget.Colors (Mini_Map_Colors'Pos (Red)));
      Gdk.GC.Gdk_New (Widget.Visible_Fill_Gc, Window);
      Gdk.GC.Set_Foreground
        (Widget.Visible_Fill_Gc, Widget.Colors (Mini_Map_Colors'Pos (White)));

      Resize_Buffer (Widget);
   end After_Realize;

   procedure On_Unrealize
     (Widget : access Mini_Map_Record'Class) is
   begin
      Gdk.Pixmap.Unref (Widget.Buffer);
      Gdk.GC.Destroy (Widget.Background_Gc);
      Gdk.GC.Destroy (Widget.Logical_Area_Gc);
      Gdk.GC.Destroy (Widget.Visible_Border_Gc);
      Gdk.GC.Destroy (Widget.Visible_Fill_Gc);
      Gdk.Color.Free_Colors (Get_Colormap (Widget), Widget.Colors);
   end On_Unrealize;

   procedure On_Destroy
     (Widget : access Mini_Map_Record'Class) is
   begin
      Set_Graph_Widget (Widget, null);
   end On_Destroy;

   procedure On_Size_Request
     (Widget      : access Mini_Map_Record'Class;
      Requisition : in     Gtk.Widget.Gtk_Requisition_Access) is
   begin
      Requisition.Width  := Default_Width;
      Requisition.Height := Default_Height;
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "size_request");
   end On_Size_Request;

   procedure On_Size_Allocate
     (Widget     : access Mini_Map_Record'Class;
      Allocation : in     Gtk.Widget.Gtk_Allocation_Access) is
   begin
      if Gtk.Widget.Realized_Is_Set (Widget) then
         Gdk.Window.Move_Resize
           (Get_Window (Widget),
            Allocation.X,
            Allocation.Y,
            Glib.Gint (Allocation.Width),
            Glib.Gint (Allocation.Height));

         Resize_Buffer (Widget);
      end if;
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "size_allocate");
   end On_Size_Allocate;

   function On_Expose_Event
     (Widget : access Mini_Map_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Expose)
     return Boolean is

      Area : Gdk.Rectangle.Gdk_Rectangle;
   begin
      --  Clear has happened before this is called.
      --  Correct Buffer is set as background pixmap therefore
      --  nothing needs to be drawn if 'not Widget.Polluted'

      --  Relevant Fields in Event: Area, Count, Graphics_Expose
      --  type: Expose
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "expose_event");
      --  Ignore event, if more expose events are to follow.
      if Glib.">" (Gdk.Event.Get_Count (Event), 0) then
         return True;
      end if;

      pragma Assert (Gdk."/=" (Widget.Buffer, Gdk.Pixmap.Null_Pixmap));

      if Widget.Polluted then
         Draw_Mini_Map (Widget);
         Area := Gdk.Event.Get_Area (Event);
         --  Buffer is background pixmap --> gdk redraws after clear
         Gdk.Window.Clear_Area
           (Get_Window (Widget),
            Area.X,
            Area.Y,
            Glib.Gint (Area.Width),
            Glib.Gint (Area.Height));
      end if;

      return True;
   end On_Expose_Event;

   function On_Motion_Notify_Event
     (Widget : access Mini_Map_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Motion)
     return Boolean is

      Point : Vis.Absolute.Vector_2d := Combine_Vector
        (X => Vis.Absolute_Int (Gdk.Event.Get_X (Event)),
         Y => Vis.Absolute_Int (Gdk.Event.Get_Y (Event)));
   begin
      Graph_Widgets.Set_Location
        (Widget   => Widget.Watched,
         Location => Vis.Transform_Backward (Widget.Transformation, Point));
      return True;
   end On_Motion_Notify_Event;

   function On_Button_Press_Event
     (Widget : access Mini_Map_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button)
     return Boolean is

      Status : Gdk.Main.Gdk_Grab_Status;
      Point : Vis.Absolute.Vector_2d := Combine_Vector
        (X => Vis.Absolute_Int (Gdk.Event.Get_X (Event)),
         Y => Vis.Absolute_Int (Gdk.Event.Get_Y (Event)));
   begin
      if Glib."=" (Gdk.Event.Get_Button (Event), 1) then
         Graph_Widgets.Set_Location
           (Widget   => Widget.Watched,
            Location => Vis.Transform_Backward (Widget.Transformation, Point));
         Status := Gdk.Main.Pointer_Grab
           (Window       => Get_Window (Widget),
            Owner_Events => False,
            Event_Mask   => Handled_Events,
            Confine_To   => Get_Window (Widget),
            Time         => Gdk.Event.Get_Time (Event));
         pragma Assert (Gdk.Main."=" (Status, Gdk.Main.Grab_Success));
      end if;
      return True;
   end On_Button_Press_Event;

   function On_Button_Release_Event
     (Widget : access Mini_Map_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Button)
     return Boolean is
   begin
      if Glib."=" (Gdk.Event.Get_Button (Event), 1) then
         Gdk.Main.Pointer_Ungrab (Gdk.Event.Get_Time (Event));
      end if;
      return True;
   end On_Button_Release_Event;

   procedure On_Logical_Area_Changed
     (Graph_Widget : access Graph_Widgets.Graph_Widget_Record'Class;
      Area         : in     Vis.Logic.Rectangle_2d;
      Mini         : in     Mini_Map) is
   begin
      if Mini = null then
         Mini_Map_Logger.Error
           ("On_Logical_Area_Changed called with null Mini_Map.");
      else
         Update (Mini);
      end if;
   end On_Logical_Area_Changed;

   procedure On_Visible_Area_Changed
     (Graph_Widget : access Graph_Widgets.Graph_Widget_Record'Class;
      Area         : in     Vis.Logic.Rectangle_2d;
      Mini         : in     Mini_Map) is
   begin
      if Mini = null then
         Mini_Map_Logger.Error
           ("On_Visible_Area_Changed called with null Mini_Map.");
      else
         Update (Mini);
      end if;
   end On_Visible_Area_Changed;

end Giant.Mini_Maps;

